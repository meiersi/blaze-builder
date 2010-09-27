{-# LANGUAGE CPP, BangPatterns #-}
-- | Implementation of the 'Builder' monoid.
--
-- A standard library user must never import this module directly. Instead, he
-- should import "Text.Blaze.Builder", which re-exports the 'Builder' type and
-- its associated public functions defined in this module.
--
-- Developers of other libraries may import this module to gain access to the
-- internal representation of builders. For example, in some cases, creating a
-- 'Builder' with a custom low-level 'BuildStep' may improve performance
-- considerably compared to the creating it using the public 'Builder'
-- combinators (e.g., @'fromWrite1List'@ in "Text.Blaze.Builder.Write").
-- Another example, is the use of 'runBuilderWith' or 'ModifyByteStrings' to
-- efficiently wire the 'Builder' type with another library that generates lazy
-- bytestrings.
--
-- In any case, whenever you import this module you /must/ reference the full
-- version of the 'blaze-builder' package in your cabal file, as the
-- implementation and the guarantees given in this file may change in any
-- version! The release notes will tell, if this was the case.
--
module Text.Blaze.Builder.Internal
    ( 
    -- * The @Builder@ type
      Builder(..)
    , BuildStep
    , BuildSignal(..)

    -- * Flushing the buffer
    , flush

    -- * Executing builders
    , runBuilderWith
    , toLazyByteStringWith
    , toLazyByteString
    , toByteStringIOWith
    , toByteStringIO

    -- * Default sizes
    , defaultBufferSize
    , defaultMinimalBufferSize
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
-- @'Write'@ abstraction provided by in "Text.Blaze.Builder.Write". However for
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
-- prefetching in later processing steps (e.g. compression) or to reduce the
-- sytem call overhead when writing the resulting lazy bytestring to a file or
-- sending it over the network.
--
-- For precisely understanding the performance of a specific 'Builder',
-- benchmarking is unavoidable. Moreover, it also helps to understand the
-- implementation of builders and the predefined combinators. This should be
-- amenable to the average Haskell programmer by reading the source code of
-- "Text.Blaze.Builder.Internal" and the other modules of this library. 
--
-- The guiding implementation principle was to reduce the abstraction cost per
-- output byte. We use continuation passing to achieve a constant time append.
-- The output buffer is filled by the individual builders as long as possible.
-- They call each other directly when they are done and control is returned to
-- the driver (e.g., 'toLazyByteString') only when the buffer is full, a
-- bytestring needs to be inserted directly, or no more bytes can be written.
-- We also try to take the pressure off the cache by moving variables as far
-- out of loops as possible. This leads to some duplication of code, but
-- results in sometimes dramatic increases in performance. For example, see the
-- @'fromWord8s'@ function in "Text.Blaze.Builder.Word".
--
newtype Builder = Builder (BuildStep -> BuildStep)

-- | A 'BuildSignal' signals to the driver of the 'Builder' execution the next
-- step to be taken.
--
data BuildSignal =
  -- | @Done pf@ signals that the 'BuildStep' is finished and data has been
  -- written up to the next free byte @pf@.
    Done {-# UNPACK #-} !(Ptr Word8) 
  -- | @BufferFull newSize pf nextStep@ signals that the buffer is full and
  -- data has been written up to the next free byte @pf@. Moreover, the next
  -- build step to be executed @nextStep@ requires a buffer of at least size
  -- @newSize@ to execute successfully.
  --
  -- A driver /must/ guarantee that the buffer used to call @nextStep@ is at
  -- least of size @newSize@.
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
      {-# UNPACK #-} !BuildStep
  -- | @ModifyByteStrings pf fbs nextStep@ signals that the data written up
  -- to the next free byte @pf@ must be output and the remaining stream of
  -- strict bytestrings that is produced by executing @nextStep@ must be
  -- modified using the function @fbs@.
  --
  -- This signal is used to insert bytestrings directly into the output stream.
  -- It can also be used to efficiently hand over control to another library
  -- for generating streams of strict bytestrings.
  | ModifyByteStrings
      {-# UNPACK #-} !(Ptr Word8) 
      {-# UNPACK #-} !([S.ByteString] -> [S.ByteString]) 
      {-# UNPACK #-} !BuildStep

-- | A 'BuildStep' fills a buffer from the given start pointer as long as
-- possible and returns control to the caller using a 'BuildSignal', once it is
-- required.
--
type BuildStep =  Ptr Word8       -- ^ Pointer to the next free byte in the
                                  -- buffer. A 'BuildStep' must start writing
                                  -- its data from this address.
               -> Ptr Word8       -- ^ Pointer to the first byte /after/ the
                                  -- buffer.  A 'BuildStep' must never write
                                  -- data at or after this address.
               -> IO BuildSignal  -- ^ Signal to the driver about the next step
                                  -- to be taken.

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

-- | Default size (~32kb) for the buffer that becomes a chunk of the output
-- stream once it is filled.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * k - overhead -- Copied from Data.ByteString.Lazy.
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | The minimal length (4kb) a buffer must have before filling it and
-- outputting it as a chunk of the output stream. 
--
-- This size determines when a buffer is spilled after a 'flush' or a direct
-- bytestring insertion.
defaultMinimalBufferSize :: Int
defaultMinimalBufferSize = 4 * 1024

-- | The maximal number of bytes for that copying is cheaper than direct
-- insertion into the output stream. This takes into account the fragmentation
-- that may occur in the output buffer due to the early 'flush' implied by the
-- direct bytestring insertion.
--
-- @'defaultMaximalCopySize' = 2 * 'defaultMinimalBufferSize'@
--
defaultMaximalCopySize :: Int
defaultMaximalCopySize = 2 * defaultMinimalBufferSize

------------------------------------------------------------------------------
-- Flushing and running a Builder
------------------------------------------------------------------------------


-- | Output all data written in the current buffer and start a new chunk.
--
-- The use uf this function depends on how the resulting bytestrings are
-- consumed. 'flush' is possibly not very useful in non-interactive scenarios.
-- However, it is kept for compatibility with the builder provided by
-- Data.Binary.Builder.
--
-- When using 'toLazyByteString' to extract a lazy 'L.ByteString' from a
-- 'Builder', this means that a new chunk will be started in the resulting lazy
-- 'L.ByteString'. The remaining part of the buffer is spilled, if the
-- reamining free space is smaller than the minimal desired buffer size.
--
flush :: Builder
flush = Builder $ \k pf _ -> return $ ModifyByteStrings pf id k

-- | Run a 'Builder' with the given buffer sizes.
--
-- Use this function for integrating the 'Builder' type with other libraries
-- that generate lazy bytestrings.
--
-- Note that the builders should guarantee that on average the desired chunk
-- size is attained. Builders may decide to start a new buffer and not
-- completely fill the existing buffer, if this is faster. However, they should
-- not spill too much of the buffer, if they cannot compensate for it.
--
runBuilderWith :: Int            -- ^ Buffer size (upper-bounds the resulting
                                 -- chunk size).
               -> Int            -- ^ Minimal free buffer space for continuing
                                 -- filling the same buffer after a 'flush' or
                                 -- a direct bytestring insertion. This
                                 -- corresponds to the minimal desired chunk
                                 -- size.
               -> Builder        -- ^ Builder to run.
               -> [S.ByteString] -- ^ Strict bytestrings to output after the
                                 -- builder is finished.
               -> [S.ByteString] -- ^ Resulting list of strict bytestrings.
runBuilderWith bufSize minBufSize (Builder b) k = 
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
                    | pf' == pf                      =
                        return $ bsk (inlinePerformIO $ fill pf' nextStep)
                    | minBufSize < pe `minusPtr` pf' =
                        return $ bs : bsk (inlinePerformIO $ fill pf' nextStep)
                    | otherwise                      =
                        return $ bs : 
                            bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                  where
                    bs = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                {-# INLINE modifyByteStrings #-}


-- | Extract the lazy 'L.ByteString' from the builder by running it with the
-- given buffer sizes.
--
toLazyByteStringWith :: Int           -- ^ Buffer size (upper bounds the resulting
                                      -- chunk size).
                     -> Int           -- ^ Minimal free buffer space for continuing
                                      -- filling the same buffer after a 'flush' or
                                      -- a direct bytestring insertion. This
                                      -- corresponds to the minimal desired chunk
                                      -- size.
                     -> Builder       -- ^ 'Builder' to run.
                     -> L.ByteString  -- ^ Resulting lazy 'L.ByteString'.
toLazyByteStringWith bufSize minBufSize = 
  L.fromChunks . flip (runBuilderWith bufSize minBufSize) []
{-# INLINE toLazyByteStringWith #-}

-- | Extract the lazy 'L.ByteString' from the builder by running it with default
-- buffer sizes. Use this function, if you do not have any special
-- considerations with respect to buffer sizes.
--
-- @ 'toLazyByteString' = 'toLazyByteStringWith' 'defaultBufferSize' 'defaultMinimalBufferSize'@
--
-- Note that @'toLazyByteString'@ is a 'Monoid' homomorphism.
--
-- > toLazyByteString mempty          == mempty
-- > toLazyByteString (x `mappend` y) == toLazyByteString x `mappend` toLazyByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = 
    toLazyByteStringWith defaultBufferSize defaultMinimalBufferSize
{-# INLINE toLazyByteString #-}

-- | @toByteStringIOWith bufSize io b@ runs the builder @b@ with a buffer of
-- at least the size @bufSize@ and executes the 'IO' action @io@ whenever the
-- buffer is full.
--
-- Compared to 'toLazyByteStringWith' this function requires less allocation,
-- as the output buffer is only allocated once at the start of the
-- serialization and whenever something bigger than the current buffer size has
-- to be copied into the buffer, which should happen very seldomly for the
-- default buffer size of 32kb. Hence, the pressure on the garbage collector is
-- reduced, which can be an advantage when building long sequences of bytes.
--
toByteStringIOWith :: Int                      -- ^ Buffer size (upper bounds
                                               -- the number of bytes forced
                                               -- per call to the 'IO' action).
                   -> (S.ByteString -> IO ())  -- ^ 'IO' action to execute per
                                               -- full buffer, which is
                                               -- referenced by a strict
                                               -- 'S.ByteString'.
                   -> Builder                  -- ^ 'Builder' to run.
                   -> IO ()                    -- ^ Resulting 'IO' action.
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

-- | Run the builder with a 'defaultBufferSize'd buffer and execute the given
-- 'IO' action whenever the buffer is full or gets flushed.
--
-- @ 'toByteStringIO' = 'toByteStringIOWith' 'defaultBufferSize'@
--
-- This is a 'Monoid' homomorphism in the following sense.
--
-- > toByteStringIO io mempty          == return ()
-- > toByteStringIO io (x `mappend` y) == toByteStringIO io x >> toByteStringIO io y
--
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize
{-# INLINE toByteStringIO #-}

