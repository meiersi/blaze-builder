-- |
-- Module      : Compression
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Benchmark the effect of first compacting the input stream for the 'zlib'
-- compression package.
--
-- On a Core2 Duo T7500 with Linux 2.6.32-24 i686 and GHC 6.12.3 compacting
-- first is worth its price up to chunks of 2kb size. Hence, in most
-- serialization scenarios it is better to first use a builder and only then
-- compress the output.
--
module Compression where

import Data.Int
import Data.Monoid (mconcat, mappend)

import Criterion.Main
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S

import qualified Text.Blaze.Builder as B
import Codec.Compression.GZip

main = defaultMain 
    [ bench "compress directly (chunksize 10)" $
        whnf benchCompressDirectly byteString10
    , bench "compress compacted (chunksize 10)" $
        whnf benchCompressCompacted byteString10
    , bench "compress directly (chunksize 2kb)" $
        whnf benchCompressDirectly byteString2kb
    , bench "compress compacted (chunksize 2kb)" $
        whnf benchCompressCompacted byteString2kb
    ]
  where
    n = 100000

    byteString10 = L.fromChunks $ replicate n $ S.pack $ take 10 ['\x0'..]
    {-# NOINLINE byteString10 #-}

    byteString2kb = L.fromChunks $ replicate (n `div` 200) $ S.pack $ take 2048 ['\x0'..]
    {-# NOINLINE byteString2kb #-}


benchCompressDirectly :: L.ByteString -> Int64
benchCompressDirectly = L.length . compress

benchCompressCompacted :: L.ByteString -> Int64
benchCompressCompacted = 
  L.length . compress . B.toLazyByteString . B.fromLazyByteString
