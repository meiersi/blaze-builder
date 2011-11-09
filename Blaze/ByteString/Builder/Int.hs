{-# LANGUAGE MonoPatBinds #-}
-- |
-- Module      : Blaze.ByteString.Builder.Int
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing integers.
--
-- See "Blaze.ByteString.Builder.Word" for information about how to best write several
-- integers at once.
--
module Blaze.ByteString.Builder.Int
    ( 
    -- * Creating builders from integers
    
    -- | We provide serialization functions both for singleton integers as well as
    -- for lists of integers. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromInt/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

      fromInt8
    , fromInt8s

    -- ** Big-endian serialization
    , fromInt16be            -- :: Int16   -> Builder
    , fromInt32be            -- :: Int32   -> Builder
    , fromInt64be            -- :: Int64   -> Builder
    , fromInt32sbe           -- :: [Int32] -> Builder
    , fromInt16sbe           -- :: [Int16] -> Builder
    , fromInt64sbe           -- :: [Int64] -> Builder

    -- ** Little-endian serialization
    , fromInt16le            -- :: Int16   -> Builder
    , fromInt32le            -- :: Int32   -> Builder
    , fromInt64le            -- :: Int64   -> Builder
    , fromInt16sle           -- :: [Int16] -> Builder
    , fromInt32sle           -- :: [Int32] -> Builder
    , fromInt64sle           -- :: [Int64] -> Builder

    -- ** Host-endian serialization
    , fromInthost            -- :: Int     -> Builder
    , fromInt16host          -- :: Int16   -> Builder
    , fromInt32host          -- :: Int32   -> Builder
    , fromInt64host          -- :: Int64   -> Builder
    , fromIntshost           -- :: [Int]   -> Builder
    , fromInt16shost         -- :: [Int16] -> Builder
    , fromInt32shost         -- :: [Int32] -> Builder
    , fromInt64shost         -- :: [Int64] -> Builder

    ) where


import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import qualified Data.ByteString.Lazy.Builder.BasicEncoding as E

import           Foreign

------------------------------------------------------------------------------
-- Builders corresponding to the integer writes
------------------------------------------------------------------------------

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromInt8 :: Int8 -> Builder
fromInt8 = E.encodeWithF E.int8

-- | Serialize a list of bytes.
--
fromInt8s :: [Int8] -> Builder
fromInt8s = E.encodeListWithF E.int8


-- Int16
------------------------------------------------------------------------------

-- | Serialize an 'Int16' in big endian format.
fromInt16be :: Int16 -> Builder
fromInt16be = E.encodeWithF E.int16BE
{-# INLINE fromInt16be #-}

-- | Serialize a list of 'Int16's in big endian format.
fromInt16sbe :: [Int16] -> Builder
fromInt16sbe = E.encodeListWithF E.int16BE
{-# INLINE fromInt16sbe #-}

-- | Serialize an 'Int16' in little endian format.
fromInt16le :: Int16 -> Builder
fromInt16le = E.encodeWithF E.int16LE
{-# INLINE fromInt16le #-}

-- | Serialize a list of 'Int16's in little endian format.
fromInt16sle :: [Int16] -> Builder
fromInt16sle = E.encodeListWithF E.int16LE
{-# INLINE fromInt16sle #-}


-- Int32
-----------------------------------------------------------------------------

-- | Serialize an 'Int32' in big endian format.
fromInt32be :: Int32 -> Builder
fromInt32be = E.encodeWithF E.int32BE
{-# INLINE fromInt32be #-}

-- | Serialize a list of 'Int32's in big endian format.
fromInt32sbe :: [Int32] -> Builder
fromInt32sbe = E.encodeListWithF E.int32BE
{-# INLINE fromInt32sbe #-}

-- | Serialize an 'Int32' in little endian format.
fromInt32le :: Int32 -> Builder
fromInt32le = E.encodeWithF E.int32LE
{-# INLINE fromInt32le #-}

-- | Serialize a list of 'Int32's in little endian format.
fromInt32sle :: [Int32] -> Builder
fromInt32sle = E.encodeListWithF E.int32LE
{-# INLINE fromInt32sle #-}

-- | Serialize an 'Int64' in big endian format.
fromInt64be :: Int64 -> Builder
fromInt64be = E.encodeWithF E.int64BE
{-# INLINE fromInt64be #-}

-- | Serialize a list of 'Int64's in big endian format.
fromInt64sbe :: [Int64] -> Builder
fromInt64sbe = E.encodeListWithF E.int64BE
{-# INLINE fromInt64sbe #-}

-- | Serialize an 'Int64' in little endian format.
fromInt64le :: Int64 -> Builder
fromInt64le = E.encodeWithF E.int64LE
{-# INLINE fromInt64le #-}

-- | Serialize a list of 'Int64's in little endian format.
fromInt64sle :: [Int64] -> Builder
fromInt64sle = E.encodeListWithF E.int64LE
{-# INLINE fromInt64sle #-}


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Serialize a single native machine 'Int'. The 'Int' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or integer sized machines, without
-- conversion.
--
fromInthost :: Int -> Builder
fromInthost = E.encodeWithF E.intHost
{-# INLINE fromInthost #-}

-- | Serialize a list of 'Int's.
-- See 'fromInthost' for usage considerations.
fromIntshost :: [Int] -> Builder
fromIntshost = E.encodeListWithF E.intHost
{-# INLINE fromIntshost #-}

-- | Write an 'Int16' in native host order and host endianness.
fromInt16host :: Int16 -> Builder
fromInt16host = E.encodeWithF E.int16Host
{-# INLINE fromInt16host #-}

-- | Write a list of 'Int16's in native host order and host endianness.
fromInt16shost :: [Int16] -> Builder
fromInt16shost = E.encodeListWithF E.int16Host
{-# INLINE fromInt16shost #-}

-- | Write an 'Int32' in native host order and host endianness.
fromInt32host :: Int32 -> Builder
fromInt32host = E.encodeWithF E.int32Host
{-# INLINE fromInt32host #-}

-- | Write a list of 'Int32's in native host order and host endianness.
fromInt32shost :: [Int32] -> Builder
fromInt32shost = E.encodeListWithF E.int32Host
{-# INLINE fromInt32shost #-}

-- | Write an 'Int64' in native host order and host endianness.
fromInt64host :: Int64 -> Builder
fromInt64host = E.encodeWithF E.int64Host
{-# INLINE fromInt64host #-}

-- | Write a list of 'Int64's in native host order and host endianness.
fromInt64shost :: [Int64] -> Builder
fromInt64shost = E.encodeListWithF E.int64Host
{-# INLINE fromInt64shost #-}
