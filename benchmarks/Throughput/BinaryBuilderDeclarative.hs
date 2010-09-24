module Throughput.BinaryBuilderDeclarative (
  serialize
) where

import Data.Monoid
import Data.Word
import qualified Data.ByteString.Lazy as L

import Data.Binary.Builder

import Control.Monad

import Throughput.Utils

serialize :: Int -> Int -> Endian -> Int -> Maybe L.ByteString
serialize wordSize chunkSize end iters = fmap toLazyByteString $
  case (wordSize, chunkSize, end) of
    (1, 1,_)        -> return $ writeByteN1 iters

    (2, 1,  Big)    -> return $ writeWord16N1Big iters
    (2, 1,  Little) -> return $ writeWord16N1Little iters
    (2, 1,  Host)   -> return $ writeWord16N1Host iters

    (4, 1,  Big)    -> return $ writeWord32N1Big iters
    (4, 1,  Little) -> return $ writeWord32N1Little iters
    (4, 1,  Host)   -> return $ writeWord32N1Host iters

    (8, 1,  Host)   -> return $ writeWord64N1Host iters
    (8, 1,  Big)    -> return $ writeWord64N1Big iters
    (8, 1,  Little) -> return $ writeWord64N1Little iters

    _               -> mzero

------------------------------------------------------------------------
-- Word8
------------------------------------------------------------------------

word8List :: Int -> [Word8]
word8List n = take n $ cycle $ [0..]

------------------------------------------------------------------------

writeByteN1  = mconcat . map singleton . word8List


------------------------------------------------------------------------
-- Word16
------------------------------------------------------------------------

word16List :: Int -> [Word16]
word16List n = take n $ cycle $ [0..]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord16N1Big  = mconcat . map putWord16be . word16List


------------------------------------------------------------------------
-- Little endian, word16 writes

writeWord16N1Little  = mconcat . map putWord16le . word16List


------------------------------------------------------------------------
-- Host endian, unaligned, word16 writes

writeWord16N1Host  = mconcat . map putWord16host . word16List


------------------------------------------------------------------------
-- Word32
------------------------------------------------------------------------

word32List :: Int -> [Word32]
word32List n = [0..fromIntegral (n-1)]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord32N1Big  = mconcat . map putWord32be . word32List


------------------------------------------------------------------------
-- Little endian, word32 writes

writeWord32N1Little  = mconcat . map putWord32le . word32List


------------------------------------------------------------------------
-- Host endian, unaligned, word32 writes

writeWord32N1Host  = mconcat . map putWord32host . word32List


------------------------------------------------------------------------
-- Word64
------------------------------------------------------------------------

word64List :: Int -> [Word64]
word64List n = [0..fromIntegral (n-1)]

------------------------------------------------------------------------
-- Big endian, word16 writes

writeWord64N1Big  = mconcat . map putWord64be . word64List


------------------------------------------------------------------------
-- Little endian, word64 writes

writeWord64N1Little  = mconcat . map putWord64le . word64List

------------------------------------------------------------------------
-- Host endian, unaligned, word64 writes

writeWord64N1Host  = mconcat . map putWord64host . word64List

