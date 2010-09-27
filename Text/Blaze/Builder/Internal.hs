{-# LANGUAGE CPP, BangPatterns #-}
-- | Implementation of the 'Builder' monoid.
module Text.Blaze.Builder.Internal
    ( 
    -- * The Builder type
      Builder(..)
    , BuildStep
    , BuildSignal(..)

    , flush

    -- ** Obtaining the built chunks
    , toLazyByteString
    , toLazyByteStringWith
    , toByteStringIO
    , toByteStringIOWith


    -- * Default constants
    , defaultBufferSize
    , defaultMinimalChunkSize
    , defaultMaximalCopySize
    ) where


import Foreign
import Data.Monoid
import Control.Monad (unless)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
#endif


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | Intuitively, a builder denotes the construction of a lazy bytestring. 
--
-- Builders can be created from primitive buffer manipulations using the
-- 'Write' abstraction provided by in "Text.Blaze.Builder.Write". However for
-- many Haskell values, there exist predefined functions doing that already. 
-- For example, UTF-8 encoding 'Char' and 'String' values is provided by the
-- functions in "Text.Blaze.Builder.Char.Utf8". Concatenating builders is done
-- using their 'Monoid' instance.
--
-- Semantically, builders are nothing special. They just denote a sequence of
-- bytes. However, their representation is chosen such that this sequence of
-- bytes can be efficiently (in terms of CPU cycles) computed in an
-- incremental, chunk-wise fashion such that the average chunk-size is large.
-- Note that the large average chunk size allows to make good use of cache
-- prefetching in later processing steps (e.g. compression) or reduces the
-- sytem call overhead when writing the resulting lazy bytestring to a file or
-- sending it over the network.
--
-- For precisely understanding the performance of a specific 'Builder',
-- benchmarking is unavoidable. Moreover, it also helps to understand the
-- implementation of builders and the predefined combinators. This should be
-- amenable to the average Haskell programmer by reading the source code of
-- this library. The guiding implementation principle was to reduce the
-- abstraction cost per byte output as much as possible. Therefore, try to keep
-- the control flow during the evaluation of a builder as simple as possible.
-- We also try to take the pressure off the memory bus by moving variables as
-- far out of loops as possible. This leads to some duplication of code, but
-- results in sometimes dramatic increases in performance. For example, see the
-- @'fromWord8s'@ function in "Text.Blaze.Builder.Word".
--
newtype Builder = Builder (BuildStep -> BuildStep)

-- | A buildsignal is a signal returned from a write to the builder, it tells us
-- what should happen next.
--
data BuildSignal
  -- | Signal the completion of the write process.
  = Done {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte
  -- | Signal that the buffer is full and a new one needs to be allocated.
  -- It contains the minimal size required for the next buffer, a pointer to the
  -- next free byte, and a continuation.
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
      {-# UNPACK #-} !BuildStep
  -- | Signal that a ByteString needs to be wrapped or inserted as is. The
  -- concrete behaviour is up to the driver of the builder. Arguments:
  -- The next free byte in the buffer, the ByteString modification,
  -- and the next build step.
  | ModifyByteStrings
      {-# UNPACK #-} !(Ptr Word8) 
      {-# UNPACK #-} !([S.ByteString] -> [S.ByteString]) 
      {-# UNPACK #-} !BuildStep

-- | Type for a single build step. Every build step checks that
--
-- > free + bytes-written <= last
--
type BuildStep =  Ptr Word8       -- ^ Ptr to the next free byte in the buffer
               -> Ptr Word8       -- ^ Ptr to the first byte AFTER the buffer
               -> IO BuildSignal  -- ^ Signal the next step to be taken

instance Monoid Builder where
    mempty = Builder id
    {-# INLINE mempty #-}
    mappend (Builder f) (Builder g) = Builder $ f . g
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

------------------------------------------------------------------------------
-- Internal global constants.
------------------------------------------------------------------------------

-- | Copied from Data.ByteString.Lazy.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | The minimal length a buffer should have for it to be worth to be processed
-- further. For 4kb it is the case that a memcopy takes about as long as a
-- system call in Linux.
defaultMinimalChunkSize :: Int
defaultMinimalChunkSize = 4 * 1024

-- | The maximal number of bytes where copying is cheaper than direct
-- insertion. This takes into account the fragmentation that may occur in the
-- output buffer due to the early flush.
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * defaultMinimalChunkSize

------------------------------------------------------------------------------
-- Flushing and running a Builder
------------------------------------------------------------------------------


-- | Flush a 'Builder'. This means a new chunk will be started in the resulting
-- lazy 'L.ByteString'. The remaining part of the buffer is spilled.
--
flush :: Builder
flush = Builder $ \f pf _ ->
    return $ BufferFull 0 pf f

-- | Run the builder with buffers of at least the given size.
--
-- Note that the builders should guarantee that on average the desired buffer
-- size is attained almost perfectly. "Almost" because builders may decide to
-- start a new buffer and not completely fill the existing buffer, if this is
-- faster. However, they should not spill too much of the buffer, if they
-- cannot compensate for it.
--
runBuilderWith :: Int -> Builder -> [S.ByteString] -> [S.ByteString]
runBuilderWith bufSize (Builder b) k = 
    inlinePerformIO $ fillNewBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillNewBuffer !size !step0 = do
        fpbuf <- S.mallocByteString size
        withForeignPtr fpbuf $ fillBuffer fpbuf
      where
        fillBuffer fpbuf !pbuf = fill pbuf step0
          where
            pe = pbuf `plusPtr` size
            fill !pf !step = do
                next <- step pf pe
                case next of
                    Done pf' 
                      | pf' == pf -> return k
                      | otherwise -> return $ 
                          S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf) : k

                    BufferFull newSize pf' nextStep  ->
                        return $ 
                            S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf) : 
                            (inlinePerformIO $ 
                                fillNewBuffer (max newSize bufSize) nextStep)
                        
                    ModifyByteStrings  pf' bsk nextStep  ->
                        modifyByteStrings pf' bsk nextStep
              where
                modifyByteStrings pf' bsk nextStep
                    | pf' == pf                           =
                        return $ bsk (inlinePerformIO $ fill pf' nextStep)
                    | defaultMinimalChunkSize < pe `minusPtr` pf' =
                        return $ bs : bsk (inlinePerformIO $ fill pf' nextStep)
                    | otherwise                           =
                        return $ bs : 
                            bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                  where
                    bs = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                {-# INLINE modifyByteStrings #-}

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
toLazyByteStringWith :: Int          -- ^ Desired chunk size
                     -> Builder 
                     -> L.ByteString
toLazyByteStringWith bufSize = L.fromChunks . flip (runBuilderWith bufSize) []
{-# INLINE toLazyByteStringWith #-}

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith defaultBufferSize
{-# INLINE toLazyByteString #-}

-- | Run the builder with a buffer of at least the given size and execute
-- the given IO action whenever it is full.
--
--
toByteStringIOWith :: Int -> (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIOWith bufSize io (Builder b) = 
    fillNewBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillNewBuffer !size !step0 = do
        S.mallocByteString size >>= fillBuffer
      where
        fillBuffer fpbuf = fill step0
          where
            -- safe because the constructed ByteString references the foreign
            -- pointer AFTER its buffer was filled.
            pf = unsafeForeignPtrToPtr fpbuf
            fill !step = do
                next <- step pf (pf `plusPtr` size)
                case next of
                    Done pf' ->
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))

                    BufferFull newSize pf' nextStep  -> do
                        io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                        if bufSize < newSize
                          then fillNewBuffer newSize nextStep
                          else fill nextStep
                        
                    ModifyByteStrings  pf' bsk nextStep  -> do
                        unless (pf' == pf) (io $  S.PS fpbuf 0 (pf' `minusPtr` pf))
                        mapM_ io (bsk [])
                        fill nextStep

-- | Run the builder with a 'defaultSize'd buffer and execute the given IO
-- action whenever it is full.
--
-- > toByteStringIO io (x `mappend` y) == toByteStringIO io x >> toByteStringIO io y
--
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize
{-# INLINE toByteStringIO #-}

