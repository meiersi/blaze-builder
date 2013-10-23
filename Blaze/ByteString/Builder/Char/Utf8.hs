------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Char.Utf8
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Char.Utf8
    (
      -- * Writing UTF-8 encoded characters to a buffer
      writeChar

      -- * Creating Builders from UTF-8 encoded characters
    , fromChar
    , fromString
    , fromShow
    , fromText
    , fromLazyText
    ) where

import Blaze.ByteString.Builder.Compat.Write (Write, writePrimBounded)
import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Text      as TS
import qualified Data.Text.Lazy as TL

writeChar :: Char -> Write
writeChar = writePrimBounded P.charUtf8

fromChar :: Char -> Builder
fromChar = B.charUtf8

fromString :: String -> Builder
fromString = B.stringUtf8

fromShow :: Show a => a -> Builder
fromShow = fromString . show

fromText :: TS.Text -> Builder
fromText = fromString . TS.unpack

fromLazyText :: TL.Text -> Builder
fromLazyText = fromString . TL.unpack
