{-# LANGUAGE OverloadedStrings #-}
module Builder where

import Data.Char (ord)
import Data.Monoid (mconcat)

import qualified Data.Binary.Builder as DB
import Criterion.Main
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Text.Blaze.Builder.Core as BB
import qualified Text.Blaze.Builder.Utf8 as BB

main :: IO ()
main = defaultMain $ concat
    [ benchmark "[String]"
        (mconcat . concatMap (map $ DB.singleton .  fromIntegral . ord))
        (mconcat . map BB.fromString)
        strings
    , benchmark "[S.ByteString]"
        (mconcat . map DB.fromByteString)
        (mconcat . map BB.fromByteString)
        byteStrings
    , benchmark "[Text]"
        (mconcat . map (DB.fromByteString . encodeUtf8))
        (mconcat . map BB.fromText)
        texts
    ]
  where
    benchmark name binaryF blazeF x =
        [ bench (name ++ " (Data.Binary builder)") $
            whnf (LB.length . DB.toLazyByteString . binaryF) x
        , bench (name ++ " (blaze builder)") $
            whnf (LB.length . BB.toLazyByteString . blazeF) x
        ]

    strings :: [String]
    strings = replicate 10000 "<img>"
    {-# NOINLINE strings #-}

    byteStrings :: [S.ByteString]
    byteStrings = replicate 10000 "<img>"
    {-# NOINLINE byteStrings #-}

    texts :: [Text]
    texts = replicate 10000 "<img>"
    {-# NOINLINE texts #-}
