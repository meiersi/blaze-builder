{-# LANGUAGE BangPatterns #-}
module Throughput.BlazeBuilderDeclarative (
  serialize
) where

import Data.Monoid
import Data.Word
import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder

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
-- Word8
------------------------------------------------------------------------

word8List :: Int -> [Word8]
word8List n = take n $ cycle $ [0..]

------------------------------------------------------------------------

writeByteN1  = fromWrite1List  writeWord8 . word8List
writeByteN2  = fromWrite2List  writeWord8 . word8List
writeByteN4  = fromWrite4List  writeWord8 . word8List
writeByteN8  = fromWrite8List  writeWord8 . word8List
writeByteN16 = fromWrite16List writeWord8 . word8List


------------------------------------------------------------------------
-- Word16
------------------------------------------------------------------------

word16List :: Int -> [Word16]
word16List n = take n $ cycle $ [0..]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord16N1Big  = fromWrite1List  writeWord16be . word16List
writeWord16N2Big  = fromWrite2List  writeWord16be . word16List
writeWord16N4Big  = fromWrite4List  writeWord16be . word16List
writeWord16N8Big  = fromWrite8List  writeWord16be . word16List
writeWord16N16Big = fromWrite16List writeWord16be . word16List


------------------------------------------------------------------------
-- Little endian, word16 writes

writeWord16N1Little  = fromWrite1List  writeWord16le . word16List
writeWord16N2Little  = fromWrite2List  writeWord16le . word16List
writeWord16N4Little  = fromWrite4List  writeWord16le . word16List
writeWord16N8Little  = fromWrite8List  writeWord16le . word16List
writeWord16N16Little = fromWrite16List writeWord16le . word16List


------------------------------------------------------------------------
-- Host endian, unaligned, word16 writes

writeWord16N1Host  = fromWrite1List  writeWord16host . word16List
writeWord16N2Host  = fromWrite2List  writeWord16host . word16List
writeWord16N4Host  = fromWrite4List  writeWord16host . word16List
writeWord16N8Host  = fromWrite8List  writeWord16host . word16List
writeWord16N16Host = fromWrite16List writeWord16host . word16List


------------------------------------------------------------------------
-- Word32
------------------------------------------------------------------------

word32List :: Int -> [Word32]
word32List n = [0..fromIntegral (n-1)]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord32N1Big  = fromWrite1List  writeWord32be . word32List
writeWord32N2Big  = fromWrite2List  writeWord32be . word32List
writeWord32N4Big  = fromWrite4List  writeWord32be . word32List
writeWord32N8Big  = fromWrite8List  writeWord32be . word32List
writeWord32N16Big = fromWrite16List writeWord32be . word32List


------------------------------------------------------------------------
-- Little endian, word32 writes

writeWord32N1Little  = fromWrite1List  writeWord32le . word32List
writeWord32N2Little  = fromWrite2List  writeWord32le . word32List
writeWord32N4Little  = fromWrite4List  writeWord32le . word32List
writeWord32N8Little  = fromWrite8List  writeWord32le . word32List
writeWord32N16Little = fromWrite16List writeWord32le . word32List


------------------------------------------------------------------------
-- Host endian, unaligned, word32 writes

writeWord32N1Host  = fromWrite1List  writeWord32host . word32List
writeWord32N2Host  = fromWrite2List  writeWord32host . word32List
writeWord32N4Host  = fromWrite4List  writeWord32host . word32List
writeWord32N8Host  = fromWrite8List  writeWord32host . word32List
writeWord32N16Host = fromWrite16List writeWord32host . word32List


------------------------------------------------------------------------
-- Word64
------------------------------------------------------------------------

word64List :: Int -> [Word64]
word64List n = [0..fromIntegral (n-1)]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord64N1Big  = fromWrite1List  writeWord64be . word64List
writeWord64N2Big  = fromWrite2List  writeWord64be . word64List
writeWord64N4Big  = fromWrite4List  writeWord64be . word64List
writeWord64N8Big  = fromWrite8List  writeWord64be . word64List
writeWord64N16Big = fromWrite16List writeWord64be . word64List


------------------------------------------------------------------------
-- Little endian, word64 writes

writeWord64N1Little  = fromWrite1List  writeWord64le . word64List
writeWord64N2Little  = fromWrite2List  writeWord64le . word64List
writeWord64N4Little  = fromWrite4List  writeWord64le . word64List
writeWord64N8Little  = fromWrite8List  writeWord64le . word64List
writeWord64N16Little = fromWrite16List writeWord64le . word64List


------------------------------------------------------------------------
-- Host endian, unaligned, word64 writes

writeWord64N1Host  = fromWrite1List  writeWord64host . word64List
writeWord64N2Host  = fromWrite2List  writeWord64host . word64List
writeWord64N4Host  = fromWrite4List  writeWord64host . word64List
writeWord64N8Host  = fromWrite8List  writeWord64host . word64List
writeWord64N16Host = fromWrite16List writeWord64host . word64List


