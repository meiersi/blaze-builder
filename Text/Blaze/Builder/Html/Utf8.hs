{-# LANGUAGE OverloadedStrings #-}
-- | 'Write's and 'Builder's for serializing HTML escaped and UTF-8 encoded
-- characters.
--
-- This module is used by both the 'blaze-html' and the \'hamlet\' HTML
-- templating libraries. If the 'Builder' from 'blaze-builder' replaces the
-- 'Data.Binary.Builder' implementation, this module will most likely keep its
-- place, as it provides a set of very specialized functions.
module Text.Blaze.Builder.Html.Utf8
    ( 
      module Text.Blaze.Builder.Char.Utf8

      -- * Writing HTML escaped and UTF-8 encoded characters to a buffer
    , writeHtmlEscapedChar

      -- * Creating Builders from HTML escaped and UTF-8 encoded characters
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    , fromHtmlEscapedShow
    , fromHtmlEscapedText
    ) where

import Data.ByteString.Char8 ()  -- for the 'IsString' instance of bytesrings

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Builder
import Text.Blaze.Builder.Char.Utf8

-- | Write a HTML escaped and UTF-8 encoded Unicode character to a bufffer.
--
writeHtmlEscapedChar :: Char -> Write
writeHtmlEscapedChar '<'  = writeByteString "&lt;"
writeHtmlEscapedChar '>'  = writeByteString "&gt;"
writeHtmlEscapedChar '&'  = writeByteString "&amp;"
writeHtmlEscapedChar '"'  = writeByteString "&quot;"
writeHtmlEscapedChar '\'' = writeByteString "&apos;"
writeHtmlEscapedChar c    = writeChar c
{-# INLINE writeHtmlEscapedChar #-}

-- | /O(1)./ Serialize a HTML escaped Unicode character using the UTF-8
-- encoding.
--
fromHtmlEscapedChar :: Char -> Builder
fromHtmlEscapedChar = fromWriteSingleton writeHtmlEscapedChar

-- | /O(n)/. Serialize a HTML escaped Unicode 'String' using the UTF-8
-- encoding.
--
fromHtmlEscapedString :: String -> Builder
fromHtmlEscapedString = fromWrite1List writeHtmlEscapedChar

-- | /O(n)/. Serialize a value by 'Show'ing it and then, HTML escaping and
-- UTF-8 encoding the resulting 'String'.
--
fromHtmlEscapedShow :: Show a => a -> Builder
fromHtmlEscapedShow = fromHtmlEscapedString . show


-- | /O(n)/. Serialize a HTML escaped Unicode 'Text' using the UTF-8 encoding.
--
fromHtmlEscapedText :: Text -> Builder
fromHtmlEscapedText = fromHtmlEscapedString . T.unpack

