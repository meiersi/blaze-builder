------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Word
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Word
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
    -- functions use a tighter inner loop.

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

import Data.Word
import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim  as P

writeWord8 :: Word8 -> Write
writeWord8 = writePrimFixed P.word8

writeWord16be :: Word16 -> Write
writeWord16be = writePrimFixed P.word16BE

writeWord32be :: Word32 -> Write
writeWord32be = writePrimFixed P.word32BE

writeWord64be :: Word64 -> Write
writeWord64be = writePrimFixed P.word64BE

writeWord16le :: Word16 -> Write
writeWord16le = writePrimFixed P.word16LE

writeWord32le :: Word32 -> Write
writeWord32le = writePrimFixed P.word32LE

writeWord64le :: Word64 -> Write
writeWord64le = writePrimFixed P.word64LE

writeWordhost :: Word -> Write
writeWordhost = writePrimFixed P.wordHost

writeWord16host :: Word16 -> Write
writeWord16host = writePrimFixed P.word16Host

writeWord32host :: Word32 -> Write
writeWord32host = writePrimFixed P.word32Host

writeWord64host :: Word64 -> Write
writeWord64host = writePrimFixed P.word64Host

fromWord8 :: Word8 -> Builder
fromWord8 = B.word8

fromWord8s :: [Word8] -> Builder
fromWord8s = P.primMapListFixed P.word8

fromWord16be :: Word16   -> Builder
fromWord16be = B.word16BE

fromWord32be :: Word32   -> Builder
fromWord32be = B.word32BE

fromWord64be :: Word64   -> Builder
fromWord64be = B.word64BE

fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = P.primMapListFixed P.word32BE

fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = P.primMapListFixed P.word16BE

fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = P.primMapListFixed P.word64BE

fromWord16le :: Word16   -> Builder
fromWord16le = B.word16LE

fromWord32le :: Word32   -> Builder
fromWord32le = B.word32LE

fromWord64le :: Word64   -> Builder
fromWord64le = B.word64LE

fromWord16sle :: [Word16] -> Builder
fromWord16sle = P.primMapListFixed P.word16LE

fromWord32sle :: [Word32] -> Builder
fromWord32sle = P.primMapListFixed P.word32LE

fromWord64sle :: [Word64] -> Builder
fromWord64sle = P.primMapListFixed P.word64LE

fromWordhost :: Word     -> Builder
fromWordhost = B.wordHost

fromWord16host :: Word16   -> Builder
fromWord16host = B.word16Host

fromWord32host :: Word32   -> Builder
fromWord32host = B.word32Host

fromWord64host :: Word64   -> Builder
fromWord64host = B.word64Host

fromWordshost :: [Word]   -> Builder
fromWordshost = P.primMapListFixed P.wordHost

fromWord16shost :: [Word16] -> Builder
fromWord16shost = P.primMapListFixed P.word16Host

fromWord32shost :: [Word32] -> Builder
fromWord32shost = P.primMapListFixed P.word32Host

fromWord64shost :: [Word64] -> Builder
fromWord64shost = P.primMapListFixed P.word64Host
