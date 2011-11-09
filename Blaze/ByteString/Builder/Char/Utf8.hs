-- |
-- Module      : Blaze.ByteString.Builder.Char.Utf8
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing Unicode characters using the UTF-8
-- encoding. 
--
module Blaze.ByteString.Builder.Char.Utf8
    ( 
      -- * Creating Builders from UTF-8 encoded characters
      fromChar
    , fromString
    , fromShow
    , fromText
    , fromLazyText
    ) where

import qualified Data.Text               as TS
import qualified Data.Text.Lazy          as TL

import Data.ByteString.Lazy.Builder (Builder, charUtf8, stringUtf8)

-- | /O(1)/. Serialize a Unicode character using the UTF-8 encoding.
--
fromChar :: Char -> Builder
fromChar = charUtf8

-- | /O(n)/. Serialize a Unicode 'String' using the UTF-8 encoding.
--
fromString :: String -> Builder
fromString = stringUtf8

-- | /O(n)/. Serialize a value by 'Show'ing it and UTF-8 encoding the resulting
-- 'String'.
--
fromShow :: Show a => a -> Builder
fromShow = stringUtf8 . show

-- | /O(n)/. Serialize a strict Unicode 'TS.Text' value using the UTF-8 encoding.
--
fromText :: TS.Text -> Builder
fromText = stringUtf8 . TS.unpack
{-# INLINE fromText #-}


-- | /O(n)/. Serialize a lazy Unicode 'TL.Text' value using the UTF-8 encoding.
--
fromLazyText :: TL.Text -> Builder
fromLazyText = stringUtf8 . TL.unpack
{-# INLINE fromLazyText #-}
