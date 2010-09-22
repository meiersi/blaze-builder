{-# LANGUAGE BangPatterns #-}
module Throughput.BlazeBuilder (
  serialize
) where

import Data.Monoid
import qualified Data.ByteString.Lazy as L

import Text.Blaze.Builder.Core

import Throughput.Utils

serialize :: Int -> Int -> Endian -> Int -> L.ByteString
serialize wordSize chunkSize end = toLazyByteString . 
  case (wordSize, chunkSize, end) of
    (1, 1,_)   -> writeByteN1
    (1, 2,_)   -> writeByteN2
    (1, 4,_)   -> writeByteN4
    (1, 8,_)   -> writeByteN8
    (1, 16, _) -> writeByteN16

    (2, 1,  Big)    -> writeWord16N1Big
    (2, 2,  Big)    -> writeWord16N2Big
    (2, 4,  Big)    -> writeWord16N4Big
    (2, 8,  Big)    -> writeWord16N8Big
    (2, 16, Big)    -> writeWord16N16Big
    (2, 1,  Little) -> writeWord16N1Little
    (2, 2,  Little) -> writeWord16N2Little
    (2, 4,  Little) -> writeWord16N4Little
    (2, 8,  Little) -> writeWord16N8Little
    (2, 16, Little) -> writeWord16N16Little
    (2, 1,  Host)   -> writeWord16N1Host
    (2, 2,  Host)   -> writeWord16N2Host
    (2, 4,  Host)   -> writeWord16N4Host
    (2, 8,  Host)   -> writeWord16N8Host
    (2, 16, Host)   -> writeWord16N16Host

    (4, 1,  Big)    -> writeWord32N1Big
    (4, 2,  Big)    -> writeWord32N2Big
    (4, 4,  Big)    -> writeWord32N4Big
    (4, 8,  Big)    -> writeWord32N8Big
    (4, 16, Big)    -> writeWord32N16Big
    (4, 1,  Little) -> writeWord32N1Little
    (4, 2,  Little) -> writeWord32N2Little
    (4, 4,  Little) -> writeWord32N4Little
    (4, 8,  Little) -> writeWord32N8Little
    (4, 16, Little) -> writeWord32N16Little
    (4, 1,  Host)   -> writeWord32N1Host
    (4, 2,  Host)   -> writeWord32N2Host
    (4, 4,  Host)   -> writeWord32N4Host
    (4, 8,  Host)   -> writeWord32N8Host
    (4, 16, Host)   -> writeWord32N16Host

    (8, 1,  Host)        -> writeWord64N1Host
    (8, 2,  Host)        -> writeWord64N2Host
    (8, 4,  Host)        -> writeWord64N4Host
    (8, 8,  Host)        -> writeWord64N8Host
    (8, 16, Host)        -> writeWord64N16Host
    (8, 1,  Big)         -> writeWord64N1Big
    (8, 2,  Big)         -> writeWord64N2Big
    (8, 4,  Big)         -> writeWord64N4Big
    (8, 8,  Big)         -> writeWord64N8Big
    (8, 16, Big)         -> writeWord64N16Big
    (8, 1,  Little)      -> writeWord64N1Little
    (8, 2,  Little)      -> writeWord64N2Little
    (8, 4,  Little)      -> writeWord64N4Little
    (8, 8,  Little)      -> writeWord64N8Little
    (8, 16, Little)      -> writeWord64N16Little

------------------------------------------------------------------------


------------------------------------------------------------------------

writeByteN1 bytes = loop 0 0
  where loop !s !n | n == bytes = mempty
                   | otherwise  = fromWord8 s `mappend`
                                  loop (s+1) (n+1)

writeByteN2 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord8 (s+0) `mappend`
          writeWord8 (s+1)) `mappend`
          loop (s+2) (n-2)

writeByteN4 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord8 (s+0) `mappend`
          writeWord8 (s+1) `mappend`
          writeWord8 (s+2) `mappend`
          writeWord8 (s+3)) `mappend`
          loop (s+4) (n-4)

writeByteN8 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord8 (s+0) `mappend`
          writeWord8 (s+1) `mappend`
          writeWord8 (s+2) `mappend`
          writeWord8 (s+3) `mappend`
          writeWord8 (s+4) `mappend`
          writeWord8 (s+5) `mappend`
          writeWord8 (s+6) `mappend`
          writeWord8 (s+7)) `mappend`
          loop (s+8) (n-8)

writeByteN16 = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord8 (s+0) `mappend`
          writeWord8 (s+1) `mappend`
          writeWord8 (s+2) `mappend`
          writeWord8 (s+3) `mappend`
          writeWord8 (s+4) `mappend`
          writeWord8 (s+5) `mappend`
          writeWord8 (s+6) `mappend`
          writeWord8 (s+7) `mappend`
          writeWord8 (s+8) `mappend`
          writeWord8 (s+9) `mappend`
          writeWord8 (s+10) `mappend`
          writeWord8 (s+11) `mappend`
          writeWord8 (s+12) `mappend`
          writeWord8 (s+13) `mappend`
          writeWord8 (s+14) `mappend`
          writeWord8 (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord16N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16be (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord16N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16be (s+0) `mappend`
          writeWord16be (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord16N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16be (s+0) `mappend`
          writeWord16be (s+1) `mappend`
          writeWord16be (s+2) `mappend`
          writeWord16be (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord16N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16be (s+0) `mappend`
          writeWord16be (s+1) `mappend`
          writeWord16be (s+2) `mappend`
          writeWord16be (s+3) `mappend`
          writeWord16be (s+4) `mappend`
          writeWord16be (s+5) `mappend`
          writeWord16be (s+6) `mappend`
          writeWord16be (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord16N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16be (s+0) `mappend`
          writeWord16be (s+1) `mappend`
          writeWord16be (s+2) `mappend`
          writeWord16be (s+3) `mappend`
          writeWord16be (s+4) `mappend`
          writeWord16be (s+5) `mappend`
          writeWord16be (s+6) `mappend`
          writeWord16be (s+7) `mappend`
          writeWord16be (s+8) `mappend`
          writeWord16be (s+9) `mappend`
          writeWord16be (s+10) `mappend`
          writeWord16be (s+11) `mappend`
          writeWord16be (s+12) `mappend`
          writeWord16be (s+13) `mappend`
          writeWord16be (s+14) `mappend`
          writeWord16be (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Little endian, word16 writes

writeWord16N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = 
          fromWrite (writeWord16le (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord16N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16le (s+0) `mappend`
          writeWord16le (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord16N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16le (s+0) `mappend`
          writeWord16le (s+1) `mappend`
          writeWord16le (s+2) `mappend`
          writeWord16le (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord16N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16le (s+0) `mappend`
          writeWord16le (s+1) `mappend`
          writeWord16le (s+2) `mappend`
          writeWord16le (s+3) `mappend`
          writeWord16le (s+4) `mappend`
          writeWord16le (s+5) `mappend`
          writeWord16le (s+6) `mappend`
          writeWord16le (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord16N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16le (s+0) `mappend`
          writeWord16le (s+1) `mappend`
          writeWord16le (s+2) `mappend`
          writeWord16le (s+3) `mappend`
          writeWord16le (s+4) `mappend`
          writeWord16le (s+5) `mappend`
          writeWord16le (s+6) `mappend`
          writeWord16le (s+7) `mappend`
          writeWord16le (s+8) `mappend`
          writeWord16le (s+9) `mappend`
          writeWord16le (s+10) `mappend`
          writeWord16le (s+11) `mappend`
          writeWord16le (s+12) `mappend`
          writeWord16le (s+13) `mappend`
          writeWord16le (s+14) `mappend`
          writeWord16le (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------
-- Host endian, unaligned, word16 writes

writeWord16N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16host (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord16N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16host (s+0) `mappend`
          writeWord16host (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord16N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16host (s+0) `mappend`
          writeWord16host (s+1) `mappend`
          writeWord16host (s+2) `mappend`
          writeWord16host (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord16N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16host (s+0) `mappend`
          writeWord16host (s+1) `mappend`
          writeWord16host (s+2) `mappend`
          writeWord16host (s+3) `mappend`
          writeWord16host (s+4) `mappend`
          writeWord16host (s+5) `mappend`
          writeWord16host (s+6) `mappend`
          writeWord16host (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord16N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord16host (s+0) `mappend`
          writeWord16host (s+1) `mappend`
          writeWord16host (s+2) `mappend`
          writeWord16host (s+3) `mappend`
          writeWord16host (s+4) `mappend`
          writeWord16host (s+5) `mappend`
          writeWord16host (s+6) `mappend`
          writeWord16host (s+7) `mappend`
          writeWord16host (s+8) `mappend`
          writeWord16host (s+9) `mappend`
          writeWord16host (s+10) `mappend`
          writeWord16host (s+11) `mappend`
          writeWord16host (s+12) `mappend`
          writeWord16host (s+13) `mappend`
          writeWord16host (s+14) `mappend`
          writeWord16host (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord32N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32be (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord32N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32be (s+0) `mappend`
          writeWord32be (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord32N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32be (s+0) `mappend`
          writeWord32be (s+1) `mappend`
          writeWord32be (s+2) `mappend`
          writeWord32be (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord32N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32be (s+0) `mappend`
          writeWord32be (s+1) `mappend`
          writeWord32be (s+2) `mappend`
          writeWord32be (s+3) `mappend`
          writeWord32be (s+4) `mappend`
          writeWord32be (s+5) `mappend`
          writeWord32be (s+6) `mappend`
          writeWord32be (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord32N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32be (s+0) `mappend`
          writeWord32be (s+1) `mappend`
          writeWord32be (s+2) `mappend`
          writeWord32be (s+3) `mappend`
          writeWord32be (s+4) `mappend`
          writeWord32be (s+5) `mappend`
          writeWord32be (s+6) `mappend`
          writeWord32be (s+7) `mappend`
          writeWord32be (s+8) `mappend`
          writeWord32be (s+9) `mappend`
          writeWord32be (s+10) `mappend`
          writeWord32be (s+11) `mappend`
          writeWord32be (s+12) `mappend`
          writeWord32be (s+13) `mappend`
          writeWord32be (s+14) `mappend`
          writeWord32be (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord32N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32le (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord32N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32le (s+0) `mappend`
          writeWord32le (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord32N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32le (s+0) `mappend`
          writeWord32le (s+1) `mappend`
          writeWord32le (s+2) `mappend`
          writeWord32le (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord32N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32le (s+0) `mappend`
          writeWord32le (s+1) `mappend`
          writeWord32le (s+2) `mappend`
          writeWord32le (s+3) `mappend`
          writeWord32le (s+4) `mappend`
          writeWord32le (s+5) `mappend`
          writeWord32le (s+6) `mappend`
          writeWord32le (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord32N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32le (s+0) `mappend`
          writeWord32le (s+1) `mappend`
          writeWord32le (s+2) `mappend`
          writeWord32le (s+3) `mappend`
          writeWord32le (s+4) `mappend`
          writeWord32le (s+5) `mappend`
          writeWord32le (s+6) `mappend`
          writeWord32le (s+7) `mappend`
          writeWord32le (s+8) `mappend`
          writeWord32le (s+9) `mappend`
          writeWord32le (s+10) `mappend`
          writeWord32le (s+11) `mappend`
          writeWord32le (s+12) `mappend`
          writeWord32le (s+13) `mappend`
          writeWord32le (s+14) `mappend`
          writeWord32le (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord32N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32host (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord32N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32host (s+0) `mappend`
          writeWord32host (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord32N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32host (s+0) `mappend`
          writeWord32host (s+1) `mappend`
          writeWord32host (s+2) `mappend`
          writeWord32host (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord32N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32host (s+0) `mappend`
          writeWord32host (s+1) `mappend`
          writeWord32host (s+2) `mappend`
          writeWord32host (s+3) `mappend`
          writeWord32host (s+4) `mappend`
          writeWord32host (s+5) `mappend`
          writeWord32host (s+6) `mappend`
          writeWord32host (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord32N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord32host (s+0) `mappend`
          writeWord32host (s+1) `mappend`
          writeWord32host (s+2) `mappend`
          writeWord32host (s+3) `mappend`
          writeWord32host (s+4) `mappend`
          writeWord32host (s+5) `mappend`
          writeWord32host (s+6) `mappend`
          writeWord32host (s+7) `mappend`
          writeWord32host (s+8) `mappend`
          writeWord32host (s+9) `mappend`
          writeWord32host (s+10) `mappend`
          writeWord32host (s+11) `mappend`
          writeWord32host (s+12) `mappend`
          writeWord32host (s+13) `mappend`
          writeWord32host (s+14) `mappend`
          writeWord32host (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord64N1Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64be (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord64N2Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64be (s+0) `mappend`
          writeWord64be (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord64N4Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64be (s+0) `mappend`
          writeWord64be (s+1) `mappend`
          writeWord64be (s+2) `mappend`
          writeWord64be (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord64N8Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64be (s+0) `mappend`
          writeWord64be (s+1) `mappend`
          writeWord64be (s+2) `mappend`
          writeWord64be (s+3) `mappend`
          writeWord64be (s+4) `mappend`
          writeWord64be (s+5) `mappend`
          writeWord64be (s+6) `mappend`
          writeWord64be (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord64N16Big = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64be (s+0) `mappend`
          writeWord64be (s+1) `mappend`
          writeWord64be (s+2) `mappend`
          writeWord64be (s+3) `mappend`
          writeWord64be (s+4) `mappend`
          writeWord64be (s+5) `mappend`
          writeWord64be (s+6) `mappend`
          writeWord64be (s+7) `mappend`
          writeWord64be (s+8) `mappend`
          writeWord64be (s+9) `mappend`
          writeWord64be (s+10) `mappend`
          writeWord64be (s+11) `mappend`
          writeWord64be (s+12) `mappend`
          writeWord64be (s+13) `mappend`
          writeWord64be (s+14) `mappend`
          writeWord64be (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord64N1Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64le (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord64N2Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64le (s+0) `mappend`
          writeWord64le (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord64N4Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64le (s+0) `mappend`
          writeWord64le (s+1) `mappend`
          writeWord64le (s+2) `mappend`
          writeWord64le (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord64N8Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64le (s+0) `mappend`
          writeWord64le (s+1) `mappend`
          writeWord64le (s+2) `mappend`
          writeWord64le (s+3) `mappend`
          writeWord64le (s+4) `mappend`
          writeWord64le (s+5) `mappend`
          writeWord64le (s+6) `mappend`
          writeWord64le (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord64N16Little = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64le (s+0) `mappend`
          writeWord64le (s+1) `mappend`
          writeWord64le (s+2) `mappend`
          writeWord64le (s+3) `mappend`
          writeWord64le (s+4) `mappend`
          writeWord64le (s+5) `mappend`
          writeWord64le (s+6) `mappend`
          writeWord64le (s+7) `mappend`
          writeWord64le (s+8) `mappend`
          writeWord64le (s+9) `mappend`
          writeWord64le (s+10) `mappend`
          writeWord64le (s+11) `mappend`
          writeWord64le (s+12) `mappend`
          writeWord64le (s+13) `mappend`
          writeWord64le (s+14) `mappend`
          writeWord64le (s+15)) `mappend`
          loop (s+16) (n-16)

------------------------------------------------------------------------

writeWord64N1Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64host (s+0)) `mappend`
          loop (s+1) (n-1)

writeWord64N2Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64host (s+0) `mappend`
          writeWord64host (s+1)) `mappend`
          loop (s+2) (n-2)

writeWord64N4Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64host (s+0) `mappend`
          writeWord64host (s+1) `mappend`
          writeWord64host (s+2) `mappend`
          writeWord64host (s+3)) `mappend`
          loop (s+4) (n-4)

writeWord64N8Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64host (s+0) `mappend`
          writeWord64host (s+1) `mappend`
          writeWord64host (s+2) `mappend`
          writeWord64host (s+3) `mappend`
          writeWord64host (s+4) `mappend`
          writeWord64host (s+5) `mappend`
          writeWord64host (s+6) `mappend`
          writeWord64host (s+7)) `mappend`
          loop (s+8) (n-8)

writeWord64N16Host = loop 0
  where loop s n | s `seq` n `seq` False = undefined
        loop _ 0 = mempty
        loop s n = fromWrite (
          writeWord64host (s+0) `mappend`
          writeWord64host (s+1) `mappend`
          writeWord64host (s+2) `mappend`
          writeWord64host (s+3) `mappend`
          writeWord64host (s+4) `mappend`
          writeWord64host (s+5) `mappend`
          writeWord64host (s+6) `mappend`
          writeWord64host (s+7) `mappend`
          writeWord64host (s+8) `mappend`
          writeWord64host (s+9) `mappend`
          writeWord64host (s+10) `mappend`
          writeWord64host (s+11) `mappend`
          writeWord64host (s+12) `mappend`
          writeWord64host (s+13) `mappend`
          writeWord64host (s+14) `mappend`
          writeWord64host (s+15)) `mappend`
          loop (s+16) (n-16)
