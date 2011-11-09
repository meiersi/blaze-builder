{-# LANGUAGE MonoPatBinds #-}
-- |
-- Module      : Blaze.ByteString.Builder.Char8
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- //Note:// This package is intended for low-level use like implementing
-- protocols. If you need to //serialize// Unicode characters use one of the 
-- UTF encodings (e.g. 'Blaze.ByteString.Builder.Char.UTF-8').
--
-- 'Write's and 'Builder's for serializing the lower 8-bits of characters.
--
-- This corresponds to what the 'bytestring' package offer in
-- 'Data.ByteString.Char8'.
--
module Blaze.ByteString.Builder.Char8
    ( 
      -- * Creating Builders from Latin-1 (ISO 8859-1) encodable characters
      fromChar
    , fromString
    , fromShow
    , fromText
    , fromLazyText
    ) where

import qualified Data.Text               as TS
import qualified Data.Text.Lazy          as TL

import Data.ByteString.Lazy.Builder (Builder, char8, string8)

-- | /O(1)/. Serialize the lower 8-bits of a character.
--
fromChar :: Char -> Builder
fromChar = char8

-- | /O(n)/. Serialize the lower 8-bits of all characters of a string
--
fromString :: String -> Builder
fromString = string8

-- | /O(n)/. Serialize a value by 'Show'ing it and serializing the
-- lower 8-bits of the resulting string.
--
fromShow :: Show a => a -> Builder
fromShow = string8 . show

-- | /O(n)/. Serialize the lower 8-bits of all characters in the
-- strict text.
--
{-# INLINE fromText #-}
fromText :: TS.Text -> Builder
fromText = string8 . TS.unpack

-- | /O(n)/. Serialize the lower 8-bits of all characters in the
-- lazy text.
--
{-# INLINE fromLazyText #-}
fromLazyText :: TL.Text -> Builder
fromLazyText = string8 . TL.unpack

