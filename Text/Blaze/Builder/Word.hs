{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-- | 'Write's and 'Builder's for serializing words.
--
-- Note that for serializing a three tuple @(x,y,z)@ of bytes (or other word
-- values) you should use the expression 
--
-- > fromWrite $ writeWord8 x `mappend` writeWord8 y `mappend` writeWord z
--
-- instead of
--
-- > fromWord8 x `mappend` fromWord8 y `mappend` fromWord z
--
-- The first expression will result in a single atomic write of three bytes,
-- while the second expression will check for each byte, if there is free space
-- left in the output buffer. Coalescing these checks can improve performance
-- quite a bit, as long as you use it sensibly.
--
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Text.Blaze.Builder.Word
    ( 
    -- * Writing words to a buffer

      writeWord8

    -- ** Big-endian writes
    , writeWord16be           -- :: Word16 -> Write
    , writeWord32be           -- :: Word32 -> Write
    , writeWord64be           -- :: Word64 -> Write

    -- ** Little-endian writes
    , writeWord16le           -- :: Word16 -> Write
    , writeWord32le           -- :: Word32 -> Write
    , writeWord64le           -- :: Word64 -> Write

    -- ** Host-endian writes
    , writeWordhost           -- :: Word -> Write
    , writeWord16host         -- :: Word16 -> Write
    , writeWord32host         -- :: Word32 -> Write
    , writeWord64host         -- :: Word64 -> Write

    -- * Creating builders from words
    
    -- | We provide serialization functions both for singleton words as well as
    -- for lists of words. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromWord/<n/>@, as the list serialization
    -- functions use a tighter inner loop and a chunked write like it is
    -- provided by functions such as 'fromWrite2List'.

    , fromWord8
    , fromWord8s

    -- ** Big-endian serialization
    , fromWord16be            -- :: Word16   -> Builder
    , fromWord32be            -- :: Word32   -> Builder
    , fromWord64be            -- :: Word64   -> Builder
    , fromWord32sbe           -- :: [Word32] -> Builder
    , fromWord16sbe           -- :: [Word16] -> Builder
    , fromWord64sbe           -- :: [Word64] -> Builder

    -- ** Little-endian serialization
    , fromWord16le            -- :: Word16   -> Builder
    , fromWord32le            -- :: Word32   -> Builder
    , fromWord64le            -- :: Word64   -> Builder
    , fromWord16sle           -- :: [Word16] -> Builder
    , fromWord32sle           -- :: [Word32] -> Builder
    , fromWord64sle           -- :: [Word64] -> Builder

    -- ** Host-endian serialization
    , fromWordhost            -- :: Word     -> Builder
    , fromWord16host          -- :: Word16   -> Builder
    , fromWord32host          -- :: Word32   -> Builder
    , fromWord64host          -- :: Word64   -> Builder
    , fromWordshost           -- :: [Word]   -> Builder
    , fromWord16shost         -- :: [Word16] -> Builder
    , fromWord32shost         -- :: [Word32] -> Builder
    , fromWord64shost         -- :: [Word64] -> Builder

    ) where

import Text.Blaze.Builder.Internal
import Text.Blaze.Builder.Write

import Foreign

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
-- Word writes
--------------
--
-- Based upon the 'putWordX' functions from "Data.Binary.Builder" from the
-- 'binary' package.
-- 
------------------------------------------------------------------------------


-- | Write a single byte.
--
writeWord8 :: Word8 -> Write
writeWord8 x = Write 1 (\pf -> poke pf x)
{-# INLINE writeWord8 #-}

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a 'Word16' in big endian format.
writeWord16be :: Word16 -> Write
writeWord16be w = Write 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE writeWord16be #-}

-- | Write a 'Word16' in little endian format.
writeWord16le :: Word16 -> Write
writeWord16le w = Write 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE writeWord16le #-}

-- writeWord16le w16 = Write 2 (\p -> poke (castPtr p) w16)

-- | Write a 'Word32' in big endian format.
writeWord32be :: Word32 -> Write
writeWord32be w = Write 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE writeWord32be #-}

-- | Write a 'Word32' in little endian format.
writeWord32le :: Word32 -> Write
writeWord32le w = Write 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE writeWord32le #-}

-- on a little endian machine:
-- writeWord32le w32 = Write 4 (\p -> poke (castPtr p) w32)

-- | Write a 'Word64' in big endian format.
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

-- | Write a 'Word64' in little endian format.
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

-- | Write a single native machine 'Word'. The 'Word' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
writeWordhost :: Word -> Write
writeWordhost w = 
    Write (sizeOf (undefined :: Word)) (\p -> poke (castPtr p) w)
{-# INLINE writeWordhost #-}

-- | Write a 'Word16' in native host order and host endianness.
writeWord16host :: Word16 -> Write
writeWord16host w16 = 
    Write (sizeOf (undefined :: Word16)) (\p -> poke (castPtr p) w16)
{-# INLINE writeWord16host #-}

-- | Write a 'Word32' in native host order and host endianness.
writeWord32host :: Word32 -> Write
writeWord32host w32 = 
    Write (sizeOf (undefined :: Word32)) (\p -> poke (castPtr p) w32)
{-# INLINE writeWord32host #-}

-- | Write a 'Word64' in native host order and host endianness.
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

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromWord8 :: Word8 -> Builder
fromWord8 = fromWriteSingleton writeWord8

-- | Serialize a list of bytes.
--
fromWord8s :: [Word8] -> Builder
fromWord8s = fromWrite8List writeWord8


-- Word16
------------------------------------------------------------------------------

-- | Serialize a 'Word16' in big endian format.
fromWord16be :: Word16 -> Builder
fromWord16be = fromWriteSingleton writeWord16be 
{-# INLINE fromWord16be #-}

-- | Serialize a list of 'Word16's in big endian format.
fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = fromWrite4List writeWord16be 
{-# INLINE fromWord16sbe #-}

-- | Serialize a 'Word16' in little endian format.
fromWord16le :: Word16 -> Builder
fromWord16le = fromWriteSingleton writeWord16le 
{-# INLINE fromWord16le #-}

-- | Serialize a list of 'Word16's in little endian format.
fromWord16sle :: [Word16] -> Builder
fromWord16sle = fromWrite4List writeWord16le 
{-# INLINE fromWord16sle #-}


-- Word32
-----------------------------------------------------------------------------

-- | Serialize a 'Word32' in big endian format.
fromWord32be :: Word32 -> Builder
fromWord32be = fromWriteSingleton writeWord32be 
{-# INLINE fromWord32be #-}

-- | Serialize a list of 'Word32's in big endian format.
fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = fromWrite2List writeWord32be 
{-# INLINE fromWord32sbe #-}

-- | Serialize a 'Word32' in little endian format.
fromWord32le :: Word32 -> Builder
fromWord32le = fromWriteSingleton writeWord32le 
{-# INLINE fromWord32le #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32sle :: [Word32] -> Builder
fromWord32sle = fromWrite2List writeWord32le 
{-# INLINE fromWord32sle #-}

-- | Serialize a 'Word64' in big endian format.
fromWord64be :: Word64 -> Builder
fromWord64be = fromWriteSingleton writeWord64be 
{-# INLINE fromWord64be #-}

-- | Serialize a list of 'Word64's in big endian format.
fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = fromWrite1List writeWord64be 
{-# INLINE fromWord64sbe #-}

-- | Serialize a 'Word64' in little endian format.
fromWord64le :: Word64 -> Builder
fromWord64le = fromWriteSingleton writeWord64le 
{-# INLINE fromWord64le #-}

-- | Serialize a list of 'Word64's in little endian format.
fromWord64sle :: [Word64] -> Builder
fromWord64sle = fromWrite1List writeWord64le 
{-# INLINE fromWord64sle #-}


------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Serialize a single native machine 'Word'. The 'Word' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or word sized machines, without
-- conversion.
--
fromWordhost :: Word -> Builder
fromWordhost = fromWriteSingleton writeWordhost 
{-# INLINE fromWordhost #-}

-- | Serialize a list of 'Word's.
-- See 'fromWordhost' for usage considerations.
fromWordshost :: [Word] -> Builder
fromWordshost = fromWrite2List writeWordhost 
{-# INLINE fromWordshost #-}

-- | Write a 'Word16' in native host order and host endianness.
fromWord16host :: Word16 -> Builder
fromWord16host = fromWriteSingleton writeWord16host 
{-# INLINE fromWord16host #-}

-- | Write a list of 'Word16's in native host order and host endianness.
fromWord16shost :: [Word16] -> Builder
fromWord16shost = fromWrite4List writeWord16host 
{-# INLINE fromWord16shost #-}

-- | Write a 'Word32' in native host order and host endianness.
fromWord32host :: Word32 -> Builder
fromWord32host = fromWriteSingleton writeWord32host 
{-# INLINE fromWord32host #-}

-- | Write a list of 'Word32's in native host order and host endianness.
fromWord32shost :: [Word32] -> Builder
fromWord32shost = fromWrite2List writeWord32host 
{-# INLINE fromWord32shost #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64host :: Word64 -> Builder
fromWord64host = fromWriteSingleton writeWord64host
{-# INLINE fromWord64host #-}

-- | Write a list of 'Word64's in native host order and host endianness.
fromWord64shost :: [Word64] -> Builder
fromWord64shost = fromWrite1List writeWord64host
{-# INLINE fromWord64shost #-}
