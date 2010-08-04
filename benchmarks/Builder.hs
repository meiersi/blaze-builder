{-# LANGUAGE OverloadedStrings #-}
module Builder where

import Data.Char (ord)
import Data.Monoid (mconcat)

import qualified Data.Binary.Builder as Binary
import Criterion.Main
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Text.Blaze.Builder.Core as Blaze
import qualified Text.Blaze.Builder.Utf8 as Blaze

main :: IO ()
main = defaultMain $ concat
    [ benchmark "[String]"
        (mconcat . concatMap (map $ Binary.singleton .  fromIntegral . ord))
        (mconcat . map Blaze.fromString)
        strings
    , benchmark "[S.ByteString]"
        (mconcat . map Binary.fromByteString)
        (mconcat . map Blaze.fromByteString)
        byteStrings
    , benchmark "[Text]"
        (mconcat . map (Binary.fromByteString . encodeUtf8))
        (mconcat . map Blaze.fromText)
        texts
    ]
  where
    benchmark name binaryF blazeF x =
        [ bench (name ++ " (Data.Binary builder)") $
            whnf (LB.length . Binary.toLazyByteString . binaryF) x
        , bench (name ++ " (blaze builder)") $
            whnf (LB.length . Blaze.toLazyByteString . blazeF) x
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
