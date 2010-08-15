-- | A module that extends the builder monoid from BlazeHtml with a number of
-- functions to write different words.
--
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Text.Blaze.Builder.Word
    ( putWord16be
    , putWord16le
    , putWord32be
    , putWord32le
    , putWord64be
    , putWord64le
    , putWordhost
    , putWord16host
    , putWord32host
    , putWord64host
    ) where

import Foreign (plusPtr, poke, sizeOf, castPtr)
import Data.Word

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Text.Blaze.Builder.Core

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..),Word16(..),Word64(..))

#if WORD_SIZE_IN_BITS < 64 && __GLASGOW_HASKELL__ >= 608
import GHC.Word (uncheckedShiftRL64#)
#endif
#endif

-- | Write a Word16 in big endian format
--
putWord16be :: Word16 -> Builder
putWord16be = writeSingleton $ \w -> Write 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
--
putWord16le :: Word16 -> Builder
putWord16le = writeSingleton $ \w -> Write 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
--
putWord32be :: Word32 -> Builder
putWord32be = writeSingleton $ \w -> Write 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> Builder
putWord32le = writeSingleton $ \w -> Write 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
--
putWord64be :: Word64 -> Builder
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
putWord64be = writeSingleton $ \w -> Write 8 $ \p ->
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    in do poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
          poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
          poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
          poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
          poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
          poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
          poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
          poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
putWord64be = writeSingleton $ \w -> Write 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
--
putWord64le :: Word64 -> Builder
#if WORD_SIZE_IN_BITS < 64
putWord64le = writeSingleton $ \w -> Write 8 $ \p ->
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    in do poke (p)             (fromIntegral (a)               :: Word8)
          poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
          poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
          poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
          poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
          poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
          poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
          poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
putWord64le = writeSingleton $ \w -> Write 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
{-# INLINE putWord64le #-}

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost :: Word -> Builder
putWordhost = writeSingleton $ \w -> Write (sizeOf (undefined :: Word)) $ \p ->
    poke (castPtr p) w
{-# INLINE putWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
--
putWord16host :: Word16 -> Builder
putWord16host = writeSingleton $ \w ->
    Write (sizeOf (undefined :: Word16)) $ \p -> poke (castPtr p) w
{-# INLINE putWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putWord32host :: Word32 -> Builder
putWord32host = writeSingleton $ \w ->
    Write (sizeOf (undefined :: Word32)) $ \p -> poke (castPtr p) w
{-# INLINE putWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
putWord64host :: Word64 -> Builder
putWord64host = writeSingleton $ \w ->
    Write (sizeOf (undefined :: Word64)) $ \p -> poke (castPtr p) w
{-# INLINE putWord64host #-}

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
