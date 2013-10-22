------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Int
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Int
    (
    -- * Writing integers to a buffer

      writeInt8

    -- ** Big-endian writes
    , writeInt16be           -- :: Int16 -> Write
    , writeInt32be           -- :: Int32 -> Write
    , writeInt64be           -- :: Int64 -> Write

    -- ** Little-endian writes
    , writeInt16le           -- :: Int16 -> Write
    , writeInt32le           -- :: Int32 -> Write
    , writeInt64le           -- :: Int64 -> Write

    -- ** Host-endian writes
    , writeInthost           -- :: Int -> Write
    , writeInt16host         -- :: Int16 -> Write
    , writeInt32host         -- :: Int32 -> Write
    , writeInt64host         -- :: Int64 -> Write

    -- * Creating builders from integers

    -- | We provide serialization functions both for singleton integers as well as
    -- for lists of integers. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromInt/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

    , fromInt8
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

import Data.Int
import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim  as P

    -- * Writing integers to a buffer

writeInt8      :: Int8 -> Write
writeInt8 = writePrimFixed P.int8

    -- ** Big-endian writes
writeInt16be   :: Int16 -> Write
writeInt16be = writePrimFixed P.int16BE

writeInt32be   :: Int32 -> Write
writeInt32be = writePrimFixed P.int32BE

writeInt64be   :: Int64 -> Write
writeInt64be = writePrimFixed P.int64BE


    -- ** Little-endian writes
writeInt16le   :: Int16 -> Write
writeInt16le = writePrimFixed P.int16LE

writeInt32le   :: Int32 -> Write
writeInt32le = writePrimFixed P.int32LE

writeInt64le   :: Int64 -> Write
writeInt64le = writePrimFixed P.int64LE

writeInthost   :: Int -> Write
writeInthost = writePrimFixed P.intHost

writeInt16host :: Int16 -> Write
writeInt16host = writePrimFixed P.int16Host

writeInt32host :: Int32 -> Write
writeInt32host = writePrimFixed P.int32Host

writeInt64host :: Int64 -> Write
writeInt64host = writePrimFixed P.int64Host

fromInt8       :: Int8    -> Builder
fromInt8 = B.int8

fromInt8s      :: [Int8]  -> Builder
fromInt8s = P.primMapListFixed P.int8

fromInt16be    :: Int16   -> Builder
fromInt16be = B.int16BE

fromInt32be    :: Int32   -> Builder
fromInt32be = B.int32BE

fromInt64be    :: Int64   -> Builder
fromInt64be = B.int64BE

fromInt32sbe   :: [Int32] -> Builder
fromInt32sbe = P.primMapListFixed P.int32BE

fromInt16sbe   :: [Int16] -> Builder
fromInt16sbe = P.primMapListFixed P.int16BE

fromInt64sbe   :: [Int64] -> Builder
fromInt64sbe = P.primMapListFixed P.int64BE

fromInt16le    :: Int16   -> Builder
fromInt16le = B.int16LE

fromInt32le    :: Int32   -> Builder
fromInt32le = B.int32LE

fromInt64le    :: Int64   -> Builder
fromInt64le = B.int64LE

fromInt16sle   :: [Int16] -> Builder
fromInt16sle = P.primMapListFixed P.int16LE

fromInt32sle   :: [Int32] -> Builder
fromInt32sle = P.primMapListFixed P.int32LE

fromInt64sle   :: [Int64] -> Builder
fromInt64sle = P.primMapListFixed P.int64LE

fromInthost    :: Int     -> Builder
fromInthost = B.intHost

fromInt16host  :: Int16   -> Builder
fromInt16host = B.int16Host

fromInt32host  :: Int32   -> Builder
fromInt32host = B.int32Host

fromInt64host  :: Int64   -> Builder
fromInt64host = B.int64Host

fromIntshost   :: [Int]   -> Builder
fromIntshost = P.primMapListFixed P.intHost

fromInt16shost :: [Int16] -> Builder
fromInt16shost = P.primMapListFixed P.int16Host

fromInt32shost :: [Int32] -> Builder
fromInt32shost = P.primMapListFixed P.int32Host

fromInt64shost :: [Int64] -> Builder
fromInt64shost = P.primMapListFixed P.int64Host
