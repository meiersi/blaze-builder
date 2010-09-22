{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-- | Core types and functions for the 'Builder' monoid
--
--

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Text.Blaze.Builder.Core
    ( 
    -- * Atomic writes to a buffer
      Write (..)

    -- ** Byte writes
    , writeWord8
    , writeByteString

    -- ** Big-endian writes
    , writeWord16be           -- :: Word16 -> Write
    , writeWord32be           -- :: Word32 -> Write
    , writeWord64be           -- :: Word64 -> Write

    -- ** Little-endian writes
    , writeWord16le           -- :: Word16 -> Write
    , writeWord32le           -- :: Word32 -> Write
    , writeWord64le           -- :: Word64 -> Write

    -- ** Host-endian, unaligned writes
    , writeWordhost           -- :: Word -> Write
    , writeWord16host         -- :: Word16 -> Write
    , writeWord32host         -- :: Word32 -> Write
    , writeWord64host         -- :: Word64 -> Write

    -- * The Builder type
    , Builder
    , toLazyByteString
    , toByteStringIO

    -- ** Basic builder construction
    , empty                   -- DEPRECATED: use 'mempty' instead
    , singleton               -- DEPRECATED: use 'fromByte' instead
    , append                  -- DEPRECATED: use 'mappend' instead
    , flush
    , fromWrite
    , fromWriteSingleton
    , fromWriteList
    , fromWrite2List
    , fromWrite4List
    , fromWrite8List
    , fromWrite16List

    -- ** Optimized Builders from ByteStrings
    , copyByteString
    , insertByteString
    , fromByteString

    , copyLazyByteString
    , insertLazyByteString
    , fromLazyByteString

    -- ** Builders derived from existing writes
    
    -- *** Byte writes
    , fromWord8
    , fromWord8s

    -- *** Big-endian writes
    , fromWord16be           -- :: Word16 -> Builder
    , fromWord32be           -- :: Word32 -> Builder
    , fromWord64be           -- :: Word64 -> Builder

    -- *** Little-endian writes
    , fromWord16le           -- :: Word16 -> Builder
    , fromWord32le           -- :: Word32 -> Builder
    , fromWord64le           -- :: Word64 -> Builder

    -- *** Host-endian, unaligned writes
    , fromWordhost           -- :: Word -> Builder
    , fromWord16host         -- :: Word16 -> Builder
    , fromWord32host         -- :: Word32 -> Builder
    , fromWord64host         -- :: Word64 -> Builder

    ) where

import Foreign
import Control.Monad (unless)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 ()

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..),Word16(..),Word64(..))

#if WORD_SIZE_IN_BITS < 64 && __GLASGOW_HASKELL__ >= 608
import GHC.Word (uncheckedShiftRL64#)
#endif
#else
import Data.Word
#endif


------------------------------------------------------------------------------
-- Writing to a buffer
------------------------------------------------------------------------------

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
writeWord8 :: Word8  -- ^ 'Word8' to write
           -> Write  -- ^ Resulting write
writeWord8 x = Write 1 (\pf -> poke pf x)
{-# INLINE writeWord8 #-}

-- | Write a strict 'S.ByteString'.
--
writeByteString :: S.ByteString  -- ^ 'S.ByteString' to write
                -> Write         -- ^ Resulting write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}


------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

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

-- | /O(1)/. An empty builder. Deprecated: use 'mempty' instead.
empty :: Builder
empty = mempty

-- | /O(1)/. Append two builders. Deprecated: use 'mappend' instead.
append :: Builder -> Builder -> Builder
append = mappend

-- | Construct a 'Builder' constructor from a single 'Write' constructor.
-- This constructor should be known /statically/ such that it can be
-- eliminated. Semantically it holds
--
-- > fromWrite . write = fromWriteSingleton write
--
-- However, performance-wise the right-hand side is more efficient.
--
fromWrite :: Write   -- ^ 'Write' abstraction
          -> Builder -- ^ Resulting 'Builder'
fromWrite (Write size io) =
    Builder step
  where
    step k pf pe
      | pf `plusPtr` size <= pe = do
          io pf
          let pf' = pf `plusPtr` size
          pf' `seq` k pf' pe
      | otherwise               = return $ BufferFull size pf (step k)
{-# INLINE fromWrite #-}

-- | Construct a 'Builder' constructor from a single 'Write' constructor.
--
fromWriteSingleton :: (a -> Write)  -- ^ 'Write' abstraction
                   -> a             -- ^ Actual value to write
                   -> Builder       -- ^ Resulting 'Builder'
fromWriteSingleton write = makeBuilder
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
{-# INLINE fromWriteSingleton #-}

-- | Construct a builder writing a list of data from a write abstraction.
--
fromWriteList :: (a -> Write)  -- ^ 'Write' abstraction
              -> [a]           -- ^ List of values to write
              -> Builder       -- ^ Resulting 'Builder'
fromWriteList write = makeBuilder
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
{-# INLINE fromWriteList #-}

-- | Construct a builder writing a list of data two elements at a time from a
-- write abstraction.
--
fromWrite2List :: (a -> Write)  -- ^ 'Write' abstraction
               -> [a]           -- ^ List of values to write
               -> Builder       -- ^ Resulting 'Builder'
fromWrite2List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []       !pf = k pf pe0

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size
{-# INLINE fromWrite2List #-}

-- | Construct a builder writing a list of data four elements at a time from a
-- write abstraction.
--
fromWrite4List :: (a -> Write)  -- ^ 'Write' abstraction
               -> [a]           -- ^ List of values to write
               -> Builder       -- ^ Resulting 'Builder'
fromWrite4List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite4List #-}

-- | Construct a builder writing a list of data eight elements at a time from a
-- write abstraction.
--
fromWrite8List :: (a -> Write)  -- ^ 'Write' abstraction
               -> [a]           -- ^ List of values to write
               -> Builder       -- ^ Resulting 'Builder'
fromWrite8List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite8List #-}

-- | Construct a builder writing a list of data 16 elements at a time from a
-- write abstraction.
--
fromWrite16List :: (a -> Write)  -- ^ 'Write' abstraction
                -> [a]           -- ^ List of values to write
                -> Builder       -- ^ Resulting 'Builder'
fromWrite16List write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:x'9:x'10:x'11:x'12:x'13:x'14:x'15:x'16:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                                          `mappend` write x'9 
                                          `mappend` write x'10
                                          `mappend` write x'11
                                          `mappend` write x'12
                                          `mappend` write x'13
                                          `mappend` write x'14
                                          `mappend` write x'15
                                          `mappend` write x'16
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:x'3:x'4:x'5:x'6:x'7:x'8:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                                          `mappend` write x'5 
                                          `mappend` write x'6 
                                          `mappend` write x'7 
                                          `mappend` write x'8 
                pf' = pf `plusPtr` size


            go xs@(x'1:x'2:x'3:x'4:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                                          `mappend` write x'3 
                                          `mappend` write x'4 
                pf' = pf `plusPtr` size

            go xs@(x'1:x'2:xs') !pf
              | pf' <= pe0  = do
                  io pf
                  go xs' pf'
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1 `mappend` write x'2
                pf' = pf `plusPtr` size

            go xs@[x'1] !pf
              | pf' <= pe0  = do
                  io pf
                  k pf' pe0
              | otherwise   = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'1
                pf' = pf `plusPtr` size

            go [] !pf = k pf pe0
{-# INLINE fromWrite16List #-}

-- | Flush a 'Builder'. This means a new chunk will be started in the resulting
-- lazy 'L.ByteString'. The remaining part of the buffer is spilled.
--
flush :: Builder
flush = Builder $ \f pf _ ->
    return $ BufferFull 0 pf f

-- | The minimal length a buffer should have for it to be worth to be processed
-- further. For 4kb it is the case that a memcopy takes about as long as a
-- system call in Linux.
minBufferLength :: Int
minBufferLength = 4 * 1024

-- | The maximal number of bytes where copying is cheaper than direct
-- insertion. This takes into account the fragmentation that may occur in the
-- output buffer due to the early flush.
maxCopyLength :: Int
maxCopyLength = 2 * minBufferLength

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
                    | minBufferLength < pe `minusPtr` pf' =
                        return $ bs : bsk (inlinePerformIO $ fill pf' nextStep)
                    | otherwise                           =
                        return $ bs : 
                            bsk (inlinePerformIO $ fillNewBuffer bufSize nextStep)
                  where
                    bs = S.PS fpbuf (pf `minusPtr` pbuf) (pf' `minusPtr` pf)
                {-# INLINE modifyByteStrings #-}

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
toLazyByteString :: Builder       -- ^ 'Builder' to evaluate
                 -> L.ByteString  -- ^ Resulting UTF-8 encoded 'L.ByteString'
toLazyByteString = L.fromChunks . flip runBuilder []
{-# INLINE toLazyByteString #-}

-- | Run the builder with a buffer of at least the given size and execute
-- the given IO action whenever it is full.
toByteStringIOWith :: Int -> Builder -> (S.ByteString -> IO ()) -> IO ()
toByteStringIOWith bufSize (Builder b) io = 
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
toByteStringIO :: Builder -> (S.ByteString -> IO ()) -> IO ()
toByteStringIO = toByteStringIOWith defaultSize


-- Single bytes
------------------------------------------------------------------------------

-- | Construct a 'Builder' from a single 'Word8'.
--
fromWord8 :: Word8    -- ^ 'Word8' to create a 'Builder' from
          -> Builder  -- ^ Resulting 'Builder'
fromWord8 = fromWriteSingleton writeWord8

-- | Construct a 'Builder' from a single byte. Deprecated use 'fromByte'
-- instead.
--
singleton :: Word8    -- ^ Byte to create a 'Builder' from
          -> Builder  -- ^ Resulting 'Builder'
singleton = fromWriteSingleton writeWord8

-- | Construct a 'Builder' from a list of 'Word8s'.
--
fromWord8s :: [Word8]  -- ^ '[Word8]' to create a 'Builder' from
           -> Builder  -- ^ Resulting 'Builder'
fromWord8s = fromWrite8List writeWord8


-- Strict ByteStrings
------------------------------------------------------------------------------


-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to copy
               -> Builder       -- ^ Resulting 'Builder'
copyByteString = fromWriteSingleton writeByteString
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


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | /O(n)./ A 'Builder' taking a 'L.ByteString', copying all chunks.
-- Here, 'n' is the size of the input in bytes.
--
copyLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to copy
                   -> Builder       -- Resulting 'Builder'
copyLazyByteString = fromWriteList writeByteString . L.toChunks
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


-- | Construct a builder from a lazy 'L.ByteString' that copies chunks smaller 
-- than 'maxCopyLength' and inserts them otherwise.
--
fromLazyByteString :: L.ByteString  -- ^ Lazy 'L.ByteString' to insert/copy.
                   -> Builder       -- ^ Resulting 'Builder'
fromLazyByteString = makeBuilder . L.toChunks
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | maxCopyLength < size    = 
                  return $ ModifyByteStrings pf (x':) (step xs' k)

              | pf `plusPtr` size <= pe0 = do
                  withForeignPtr fpbuf $ \pbuf -> 
                      copyBytes pf (pbuf `plusPtr` offset) size
                  let pf' = pf `plusPtr` size
                  pf' `seq` k pf' pe0

              | otherwise               = return $ BufferFull size pf (step xs k)
              where
                (fpbuf, offset, size) = S.toForeignPtr x'
{-# INLINE fromLazyByteString #-}


------------------------------------------------------------------------------
-- Word writes
------------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
writeWord16be :: Word16 -> Write
writeWord16be w = Write 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE writeWord16be #-}

-- | Write a Word16 in little endian format
writeWord16le :: Word16 -> Write
writeWord16le w = Write 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE writeWord16le #-}

-- writeWord16le w16 = Write 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
writeWord32be :: Word32 -> Write
writeWord32be w = Write 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE writeWord32be #-}

-- | Write a Word32 in little endian format
writeWord32le :: Word32 -> Write
writeWord32le w = Write 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE writeWord32le #-}

-- on a little endian machine:
-- writeWord32le w32 = Write 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
writeWord64be :: Word64 -> Write
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
writeWord64be w =
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    in Write 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
writeWord64be w = Write 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
{-# INLINE writeWord64be #-}

-- | Write a Word64 in little endian format
writeWord64le :: Word64 -> Write

#if WORD_SIZE_IN_BITS < 64
writeWord64le w =
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    in Write 8 $ \p -> do
    poke (p)             (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
writeWord64le w = Write 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
{-# INLINE writeWord64le #-}

-- on a little endian machine:
-- writeWord64le w64 = Write 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A Write taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
writeWordhost :: Word -> Write
writeWordhost w = 
    Write (sizeOf (undefined :: Word)) (\p -> poke (castPtr p) w)
{-# INLINE writeWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
writeWord16host :: Word16 -> Write
writeWord16host w16 = 
    Write (sizeOf (undefined :: Word16)) (\p -> poke (castPtr p) w16)
{-# INLINE writeWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
writeWord32host :: Word32 -> Write
writeWord32host w32 = 
    Write (sizeOf (undefined :: Word32)) (\p -> poke (castPtr p) w32)
{-# INLINE writeWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
writeWord64host :: Word64 -> Write
writeWord64host w = 
    Write (sizeOf (undefined :: Word64)) (\p -> poke (castPtr p) w)
{-# INLINE writeWord64host #-}

------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif


------------------------------------------------------------------------------
-- Builders corresponding to the word writes
------------------------------------------------------------------------------

-- | Write a Word16 in big endian format
fromWord16be :: Word16 -> Builder
fromWord16be = fromWriteSingleton writeWord16be 
{-# INLINE fromWord16be #-}

-- | Write a Word16 in little endian format
fromWord16le :: Word16 -> Builder
fromWord16le = fromWriteSingleton writeWord16le 
{-# INLINE fromWord16le #-}

-- fromWord16le w16 = Write 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
fromWord32be :: Word32 -> Builder
fromWord32be = fromWriteSingleton writeWord32be 
{-# INLINE fromWord32be #-}

--
-- a data type to tag Put/Check. froms construct these which are then
-- inlined and flattened. matching Checks will be more robust with rules.
--

-- | Write a Word32 in little endian format
fromWord32le :: Word32 -> Builder
fromWord32le = fromWriteSingleton writeWord32le 
{-# INLINE fromWord32le #-}

-- on a little endian machine:
-- fromWord32le w32 = Write 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
fromWord64be :: Word64 -> Builder
fromWord64be = fromWriteSingleton writeWord64be 
{-# INLINE fromWord64be #-}

-- | Write a Word64 in little endian format
fromWord64le :: Word64 -> Builder
fromWord64le = fromWriteSingleton writeWord64le 
{-# INLINE fromWord64le #-}

-- on a little endian machine:
-- fromWord64le w64 = Write 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
fromWordhost :: Word -> Builder
fromWordhost = fromWriteSingleton writeWordhost 
{-# INLINE fromWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
fromWord16host :: Word16 -> Builder
fromWord16host = fromWriteSingleton writeWord16host 
{-# INLINE fromWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
fromWord32host :: Word32 -> Builder
fromWord32host = fromWriteSingleton writeWord32host 
{-# INLINE fromWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we from two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
fromWord64host :: Word64 -> Builder
fromWord64host = fromWriteSingleton writeWord64host
{-# INLINE fromWord64host #-}
