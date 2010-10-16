{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : BlazeVsBinary
-- Copyright   : (c) 2010 Jasper Van der Jeught & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A comparison between 'blaze-builder' and the Data.Binary.Builder from
-- 'binary'. The goal is to measure the performance on serializing dynamic
-- data referenced by a list.
--
-- Note that some of the benchmarks are a bit unfair with respect to
-- blaze-builder, as it does more than 'binary':
--
--   1. It encodes chars as utf-8 strings and does not just truncate character
--      value to one byte.
--
--   2. It copies the contents of the lazy bytestring chunks if they are
--      shorter than 4kb. This ensures efficient processing of the resulting
--      lazy bytestring. 'binary' just inserts the chunks directly in the
--      resulting output stream.
--
module BlazeVsBinary where

import Data.Char (ord)
import Data.Monoid (mconcat)
import Data.Word (Word8)

import qualified Data.Binary.Builder as Binary
import Criterion.Main
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Text.Blaze.Builder           as Blaze
import qualified Text.Blaze.Builder.Char.Utf8 as Blaze

main :: IO ()
main = defaultMain $ concat
    [ benchmark "[String]"
        (mconcat . concatMap (map $ Binary.singleton .  fromIntegral . ord))
        (mconcat . map Blaze.fromString)
        strings
    , benchmark "L.ByteString"
        (Binary.fromLazyByteString)
        (Blaze.fromLazyByteString)
        byteStrings
    , benchmark "[Text]"
        (mconcat . map (Binary.fromByteString . encodeUtf8))
        (mconcat . map Blaze.fromText)
        texts
    , benchmark "[Word8]"
        (mconcat . map Binary.singleton)
        (Blaze.fromWord8s)
        word8s
    ]
  where
    benchmark name binaryF blazeF x =
        [ bench (name ++ " (Data.Binary builder)") $
            whnf (L.length . Binary.toLazyByteString . binaryF) x
        , bench (name ++ " (blaze builder)") $
            whnf (L.length . Blaze.toLazyByteString . blazeF) x
        ]

strings :: [String]
strings = replicate 10000 "<img>"
{-# NOINLINE strings #-}

byteStrings :: L.ByteString
byteStrings = L.fromChunks $ replicate 10000 "<img>"
{-# NOINLINE byteStrings #-}

texts :: [Text]
texts = replicate 10000 "<img>"
{-# NOINLINE texts #-}

word8s :: [Word8]
word8s = replicate 10000 $ fromIntegral $ ord 'a'
{-# NOINLINE word8s #-}
