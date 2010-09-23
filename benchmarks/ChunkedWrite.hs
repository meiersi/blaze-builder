{-# LANGUAGE OverloadedStrings #-}
-- | Test different strategies for writing lists of simple values:
--
--  1. Using 'mconcat . map from<Value>'
--  2. Using the specialized 'fromWrite<n>List' function where 'n' denotes
--     the number of elements to write at the same time. Writing chunks of
--     elements reduces the overhead from the buffer overflow test that has
--     to be done before every write.
--
module ChunkedWrite where

import Data.Char (chr, ord)
import Data.Int (Int64)
import Data.Word (Word8, Word32)
import Data.Monoid (mconcat, mappend)

import Criterion.Main
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Text as T

import qualified Text.Blaze.Builder as BB

main = defaultMain 
    [ bench "mconcat . map fromByte: [Char] -> Builder -> L.ByteString" $ 
        whnf benchMConcatChars chars
    , bench "fromWriteList: [Char] -> Builder -> L.ByteString" $ 
        whnf bench1Chars chars
    , bench "fromWrite2List: [Char] -> Builder -> L.ByteString" $ 
        whnf bench2Chars chars
    , bench "fromWrite4List: [Char] -> Builder -> L.ByteString" $ 
        whnf bench4Chars chars
    , bench "fromWrite8List: [Char] -> Builder -> L.ByteString" $ 
        whnf bench8Chars chars
    , bench "fromWrite16List: [Char] -> Builder -> L.ByteString" $ 
        whnf bench16Chars chars

    , bench "mconcat . map fromByte: [Word8] -> Builder -> L.ByteString" $ 
        whnf benchMConcatWord8s word8s
    , bench "fromWriteList: [Word8] -> Builder -> L.ByteString" $ 
        whnf bench1Word8s word8s
    , bench "fromWrite2List: [Word8] -> Builder -> L.ByteString" $ 
        whnf bench2Word8s word8s
    , bench "fromWrite4List: [Word8] -> Builder -> L.ByteString" $ 
        whnf bench4Word8s word8s
    , bench "fromWrite8List: [Word8] -> Builder -> L.ByteString" $ 
        whnf bench8Word8s word8s
    , bench "fromWrite16List: [Word8] -> Builder -> L.ByteString" $ 
        whnf bench16Word8s word8s

    , bench "mconcat . map fromWord32host: [Word32] -> Builder -> L.ByteString" $ 
        whnf benchMConcatWord32s word32s
    , bench "fromWriteList: [Word32] -> Builder -> L.ByteString" $ 
        whnf bench1Word32s word32s
    , bench "fromWrite2List: [Word32] -> Builder -> L.ByteString" $ 
        whnf bench2Word32s word32s
    , bench "fromWrite4List: [Word32] -> Builder -> L.ByteString" $ 
        whnf bench4Word32s word32s
    , bench "fromWrite8List: [Word32] -> Builder -> L.ByteString" $ 
        whnf bench8Word32s word32s
    , bench "fromWrite16List: [Word32] -> Builder -> L.ByteString" $ 
        whnf bench16Word32s word32s
    ]
  where
    n = 100000

    word8s :: [Word8]
    word8s = take n $ map fromIntegral $ [(1::Int)..]
    {-# NOINLINE word8s #-}

    word32s :: [Word32]
    word32s = take n $ [1..]
    {-# NOINLINE word32s #-}

    chars :: String
    chars = take n $ map (chr . fromIntegral) $ word8s
    {-# NOINLINE chars #-}

-- Char

benchMConcatChars :: [Char] -> Int64
benchMConcatChars = L.length . BB.toLazyByteString . mconcat . map BB.fromChar

bench1Chars :: [Char] -> Int64
bench1Chars = L.length . BB.toLazyByteString . BB.fromWriteList BB.writeChar

bench2Chars :: [Char] -> Int64
bench2Chars = L.length . BB.toLazyByteString . BB.fromWrite2List BB.writeChar

bench4Chars :: [Char] -> Int64
bench4Chars = L.length . BB.toLazyByteString . BB.fromWrite4List BB.writeChar

bench8Chars :: [Char] -> Int64
bench8Chars = L.length . BB.toLazyByteString . BB.fromWrite8List BB.writeChar

bench16Chars :: [Char] -> Int64
bench16Chars = L.length . BB.toLazyByteString . BB.fromWrite16List BB.writeChar

-- Word8

benchMConcatWord8s :: [Word8] -> Int64
benchMConcatWord8s = L.length . BB.toLazyByteString . mconcat . map BB.fromWord8

bench1Word8s :: [Word8] -> Int64
bench1Word8s = L.length . BB.toLazyByteString . BB.fromWriteList BB.writeWord8

bench2Word8s :: [Word8] -> Int64
bench2Word8s = L.length . BB.toLazyByteString . BB.fromWrite2List BB.writeWord8

bench4Word8s :: [Word8] -> Int64
bench4Word8s = L.length . BB.toLazyByteString . BB.fromWrite4List BB.writeWord8

bench8Word8s :: [Word8] -> Int64
bench8Word8s = L.length . BB.toLazyByteString . BB.fromWrite8List BB.writeWord8

bench16Word8s :: [Word8] -> Int64
bench16Word8s = L.length . BB.toLazyByteString . BB.fromWrite16List BB.writeWord8

-- Word32

benchMConcatWord32s :: [Word32] -> Int64
benchMConcatWord32s = L.length . BB.toLazyByteString . mconcat . map BB.fromWord32host

bench1Word32s :: [Word32] -> Int64
bench1Word32s = L.length . BB.toLazyByteString . BB.fromWriteList BB.writeWord32host

bench2Word32s :: [Word32] -> Int64
bench2Word32s = L.length . BB.toLazyByteString . BB.fromWrite2List BB.writeWord32host

bench4Word32s :: [Word32] -> Int64
bench4Word32s = L.length . BB.toLazyByteString . BB.fromWrite4List BB.writeWord32host

bench8Word32s :: [Word32] -> Int64
bench8Word32s = L.length . BB.toLazyByteString . BB.fromWrite8List BB.writeWord32host

bench16Word32s :: [Word32] -> Int64
bench16Word32s = L.length . BB.toLazyByteString . BB.fromWrite16List BB.writeWord32host

