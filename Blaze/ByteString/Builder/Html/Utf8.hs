{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Blaze.ByteString.Builder.Html.Utf8
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing HTML escaped and UTF-8 encoded
-- characters.
--
-- This module is used by both the 'blaze-html' and the \'hamlet\' HTML
-- templating libraries. If the 'Builder' from 'blaze-builder' replaces the
-- 'Data.Binary.Builder' implementation, this module will most likely keep its
-- place, as it provides a set of very specialized functions.
module Blaze.ByteString.Builder.Html.Utf8
    ( 
      module Blaze.ByteString.Builder.Char.Utf8

      -- * Creating Builders from HTML escaped and UTF-8 encoded characters
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    , fromHtmlEscapedShow
    , fromHtmlEscapedText
    , fromHtmlEscapedLazyText
    ) where

import qualified Data.Text      as TS
import qualified Data.Text.Lazy as TL

import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Builder.BasicEncoding as E
import           Data.ByteString.Lazy.Builder.BasicEncoding
                   ( ifB, fromF, (>*<), (>$<) )

import Blaze.ByteString.Builder.Char.Utf8

-- | Write a HTML escaped and UTF-8 encoded Unicode character to a
-- bufffer.
--
{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: E.BoundedEncoding Char
charUtf8HtmlEscaped =
    -- the maximum of the characters to escape is '>'; i.e.,
    --   maximum "<>&\"'" == '>'
    ifB (>  '>')  E.charUtf8 $
    ifB (== '<' ) (fixed4 ('&',('l',('t',';')))) $
    ifB (== '>' ) (fixed4 ('&',('g',('t',';')))) $
    ifB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $
    ifB (== '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $
    ifB (== '\'') (fixed5 ('&',('#',('3',('9',';'))))) $
    (fromF E.char8) -- fallback for chars smaller than '>' 
  where
    {-# INLINE fixed4 #-}
    fixed4 x = fromF $ const x >$< 
        E.char8 >*< E.char8 >*< E.char8 >*< E.char8

    {-# INLINE fixed5 #-}
    fixed5 x = fromF $ const x >$< 
        E.char8 >*< E.char8 >*< E.char8 >*< E.char8 >*< E.char8

    {-# INLINE fixed6 #-}
    fixed6 x = fromF $ const x >$< 
        E.char8 >*< E.char8 >*< E.char8 >*< E.char8 >*< E.char8 >*< E.char8


-- | /O(1)./ Serialize a HTML escaped Unicode character using the UTF-8
-- encoding.
--
fromHtmlEscapedChar :: Char -> Builder
fromHtmlEscapedChar = E.encodeWithB charUtf8HtmlEscaped

-- | /O(n)/. Serialize a HTML escaped Unicode 'String' using the UTF-8
-- encoding.
--
fromHtmlEscapedString :: String -> Builder
fromHtmlEscapedString = E.encodeListWithB charUtf8HtmlEscaped

-- | /O(n)/. Serialize a value by 'Show'ing it and then, HTML escaping and
-- UTF-8 encoding the resulting 'String'.
--
fromHtmlEscapedShow :: Show a => a -> Builder
fromHtmlEscapedShow = fromHtmlEscapedString . show


-- | /O(n)/. Serialize a HTML escaped strict Unicode 'TS.Text' value using the
-- UTF-8 encoding.
--
fromHtmlEscapedText :: TS.Text -> Builder
fromHtmlEscapedText = fromHtmlEscapedString . TS.unpack

-- | /O(n)/. Serialize a HTML escaped Unicode 'TL.Text' using the UTF-8 encoding.
--
fromHtmlEscapedLazyText :: TL.Text -> Builder
fromHtmlEscapedLazyText = fromHtmlEscapedString . TL.unpack

