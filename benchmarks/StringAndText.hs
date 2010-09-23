{-# LANGUAGE OverloadedStrings #-}
-- | Benchmarking of String and Text serialization.
module StringAndText (main)  where

import Data.Char (ord)
import Data.Monoid (mconcat)

import Criterion.Main
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Text.Blaze.Builder.Core as Blaze
import qualified Text.Blaze.Builder.Utf8 as Blaze

main :: IO ()
main = defaultMain 
    [ bench "fromString :: String --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromString) benchString

    , bench "fromTextUnpacked :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromTextUnpacked) benchText
     
    , bench "fromTextFolded :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromTextFolded) benchText

    , bench "fromTextSingleWrite :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromTextSingleWrite) benchText

    , bench "fromTextEncoded :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromTextEncoded) benchText

    , bench "encodeUtf8 :: Text --[Utf8 encoding]--> S.ByteString" $ whnf
        (T.encodeUtf8) benchText

    ]
  where
    n :: Int
    n = 100000

    benchString :: String
    benchString = take n $ concatMap show [(1::Int)..]
    {-# NOINLINE benchString #-}

    benchText :: Text
    benchText = T.pack benchString
    {-# NOINLINE benchText #-}
