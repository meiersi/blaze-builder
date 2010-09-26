-- | A module that extends the builder monoid from BlazeHtml with function to
-- insert HTML, including HTML escaping and the like.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Builder.Html.Utf8
    ( 
      module Text.Blaze.Builder.Char.Utf8

      -- * Custom writes to the builder
    , writeHtmlEscapedChar

      -- * Creating builders
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    , fromHtmlEscapedText
    ) where

import Data.ByteString.Char8 ()

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Char.Utf8
import Text.Blaze.Builder.ByteString

-- | Write an unicode character to a 'Builder', doing HTML escaping.
--
writeHtmlEscapedChar :: Char   -- ^ Character to write
                     -> Write  -- ^ Resulting write
writeHtmlEscapedChar '<'  = writeByteString "&lt;"
writeHtmlEscapedChar '>'  = writeByteString "&gt;"
writeHtmlEscapedChar '&'  = writeByteString "&amp;"
writeHtmlEscapedChar '"'  = writeByteString "&quot;"
writeHtmlEscapedChar '\'' = writeByteString "&apos;"
writeHtmlEscapedChar c    = writeChar c
{-# INLINE writeHtmlEscapedChar #-}

-- | A HTML escaped 'Char'.
--
fromHtmlEscapedChar :: Char     -- ^ Character to write
                    -> Builder  -- ^ Resulting 'Builder'
fromHtmlEscapedChar = fromWriteSingleton writeHtmlEscapedChar

-- | A HTML escaped 'String'.
--
fromHtmlEscapedString :: String   -- ^ String to create a 'Builder' from
                      -> Builder  -- ^ Resulting 'Builder'
fromHtmlEscapedString = fromWriteList writeHtmlEscapedChar

-- | An HTML escaped piece of 'Text'.
--
fromHtmlEscapedText :: Text     -- ^ 'Text' to insert
                    -> Builder  -- ^ Resulting 'Builder'
fromHtmlEscapedText = fromHtmlEscapedString . T.unpack
