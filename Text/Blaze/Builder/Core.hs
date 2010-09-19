-- | Core types and functions for the 'Builder' monoid
--
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Blaze.Builder.Core
    ( 
      -- * Main builder type
      Builder

      -- * Custom writes to the builder
    , Write (..)
    , writeByte
    , writeByteString
    , writeSingleton
    , writeList

      -- * Creating builders
    , singleton
    , fromByteString
    , copyByteString
    , insertByteString

    -- , fromLazyByteString
    , copyLazyByteString
    , insertLazyByteString

      -- * Special builders
    , flush

      -- * Extracting the result from a builder
    , toLazyByteString
    ) where

import Foreign
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.ByteString.Char8 ()
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

-- | Main builder type. It simply contains a function to extract the actual
-- data.
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

-- | Write abstraction so we can avoid some gory and bloody details. A write
-- abstration holds the exact size of the write in bytes, and a function to
-- carry out the write operation.
--
data Write = Write
    {-# UNPACK #-} !Int
    (Ptr Word8 -> IO ())

-- A monoid interface for the write actions.
instance Monoid Write where
    mempty = Write 0 (const $ return ())
    {-# INLINE mempty #-}
    mappend (Write l1 f1) (Write l2 f2) = Write (l1 + l2) $ \ptr -> do
        f1 ptr
        f2 (ptr `plusPtr` l1)
    {-# INLINE mappend #-}

-- | Write a single byte.
--
writeByte :: Word8  -- ^ Byte to write
          -> Write  -- ^ Resulting write
writeByte x = Write 1 (\pf -> poke pf x)
{-# INLINE writeByte #-}

-- | Write a strict 'S.ByteString'.
--
writeByteString :: S.ByteString  -- ^ 'S.ByteString' to write
                -> Write         -- ^ Resulting write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Construct a 'Builder' from a single 'Write' abstraction.
--
writeSingleton :: (a -> Write)  -- ^ 'Write' abstraction
               -> a             -- ^ Actual value to write
               -> Builder       -- ^ Resulting 'Builder'
writeSingleton write = makeBuilder
  where 
    makeBuilder x = Builder step
      where
        step k pf pe
          | pf `plusPtr` size <= pe = do
              io pf
              let pf' = pf `plusPtr` size
              pf' `seq` k pf' pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE writeSingleton #-}

-- | Construct a builder writing a list of data from a write abstraction.
--
writeList :: (a -> Write)  -- ^ 'Write' abstraction
          -> [a]           -- ^ List of values to write
          -> Builder       -- ^ Resulting 'Builder'
writeList write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | pf `plusPtr` size <= pe0  = do
                  io pf
                  go xs' (pf `plusPtr` size)
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'
{-# INLINE writeList #-}

-- | Construct a 'Builder' from a single byte.
--
singleton :: Word8    -- ^ Byte to create a 'Builder' from
          -> Builder  -- ^ Resulting 'Builder'
singleton = writeSingleton writeByte

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to copy
               -> Builder       -- ^ Resulting 'Builder'
copyByteString = writeSingleton writeByteString
{-# INLINE copyByteString #-}

-- | /O(1)./ A Builder taking a 'S.ByteString`, inserting it directly.
--
-- This operation should only be used for large (> 8kb for IO) ByteStrings as
-- otherwise the resulting output is may be too fragmented to be processed
-- efficiently.
--
insertByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to insert
                 -> Builder       -- ^ Resulting 'Builder'
insertByteString bs = Builder $ \ k pf _ ->
    return $ ModifyByteStrings pf (bs:) k
{-# INLINE insertByteString #-}

-- | Construct a 'Builder' from a single ByteString, copying it if its
-- size is below 8kb and inserting directly otherwise.
--
fromByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to insert
               -> Builder       -- ^ Resulting 'Builder'
fromByteString bs = Builder step
  where
    step k pf pe
      | maxCopyLength < size    = return $ ModifyByteStrings pf (bs:) k
      | pf `plusPtr` size <= pe = do
          withForeignPtr fpbuf $ \pbuf -> 
              copyBytes pf (pbuf `plusPtr` offset) size
          let pf' = pf `plusPtr` size
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
      where
        (fpbuf, offset, size) = S.toForeignPtr bs
{-# INLINE fromByteString #-}

-- | /O(n)./ A 'Builder' taking a 'L.ByteString', copying all chunks.
-- Here, 'n' is the size of the input in bytes.
--
copyLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to copy
                   -> Builder       -- Resulting 'Builder'
copyLazyByteString = writeList writeByteString . L.toChunks
{-# INLINE copyLazyByteString #-}

-- | /O(n)./ A Builder taking a lazy 'L.ByteString', inserting its chunks
-- directly. Here, 'n' is the number of chunks.
--
-- This operation should only be used, if the chunks are large (> 8kb for IO)
-- on average. Otherwise the resulting output may be too fragmented to be
-- processed efficiently.
--
insertLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to insert
                     -> Builder       -- ^ Resulting 'Builder'
insertLazyByteString lbs = Builder step
  where
    step k pf _ = return $ ModifyByteStrings pf (L.toChunks lbs ++) k
{-# INLINE insertLazyByteString #-}

-- | Flush a 'Builder'. This means a new chunk will be started in the resulting
-- lazy 'L.ByteString'. The remaining part of the buffer is spilled.
--
flush :: Builder
flush = Builder $ \f pf _ ->
    return $ BufferFull 0 pf f

-- | Copied from Data.ByteString.Lazy.
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | Run the builder with the default buffer size.
--
runBuilder :: Builder -> [S.ByteString] -> [S.ByteString]
runBuilder = runBuilderWith defaultSize
{-# INLINE runBuilder #-}

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
    S.inlinePerformIO $ fillNewBuffer bufSize (b finalStep)
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
                            (S.inlinePerformIO $ 
                                fillNewBuffer (max newSize bufSize) nextStep)
                        
                    ModifyByteStrings  pf' bsk nextStep  ->
                        modifyByteStrings pf' bsk nextStep
              where
                modifyByteStrings pf' bsk nextStep
                    | pf' == pf                           =
                        return $ bsk (S.inlinePerformIO $ fill pf' nextStep)
                    | minBufferLength < pe `minusPtr` pf' =
                        return $ bs : bsk (S.inlinePerformIO $ fill pf' nextStep)
                    | otherwise                           =
                        return $ bs : 
                            bsk (S.inlinePerformIO $ fillNewBuffer bufSize nextStep)
                  where
                    bs = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                {-# INLINE modifyByteStrings #-}

maxCopyLength :: Int
maxCopyLength = 8 * 1024

minBufferLength :: Int
minBufferLength = 4 * 1024

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
toLazyByteString :: Builder       -- ^ 'Builder' to evaluate
                 -> L.ByteString  -- ^ Resulting UTF-8 encoded 'L.ByteString'
toLazyByteString = L.fromChunks . flip runBuilder []
{-# INLINE toLazyByteString #-}
