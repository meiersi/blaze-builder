{-# OPTIONS_GHC -fno-warn-unused-imports #-} 
-- ignore warning from 'import Data.Text.Encoding'

-- |
-- Module      : Blaze.ByteString.Builder.Char.Utf8
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
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
-- 'Write's and 'Builder's for serializing Unicode characters using the *lossy*
-- Latin-1 (ISO 8859-1) encoding. Non-representable characters are dropped.
--
-- This corresponds to what the 'bytestring' package offer in
-- 'Data.ByteString.Char8'.
--
module Blaze.ByteString.Builder.Char.Latin1
    ( 
      -- * Writing Latin-1 (ISO 8859-1) encodable characters to a buffer
      writeChar

      -- * Creating Builders from Latin-1 (ISO 8859-1) encodable characters
    , fromChar
    , fromString
    , fromShow
    , fromText
    , fromLazyText
    ) where

import Foreign
import Data.Char (ord)

import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TS -- imported for documentation links
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TS -- imported for documentation links

import qualified Data.ByteString.Internal as S (c2w)

import Blaze.ByteString.Builder.Internal

-- | Write a UTF-8 encoded Unicode character to a buffer.
--
{-# INLINE writeChar #-}
writeChar :: Char -> Write
writeChar c0 = boundedWrite 1 (wio c0)
  where
    wio c | c < '\xFF' = writeN 1 $ \op -> poke op (S.c2w c)
          | otherwise  = writeN 0 $ \_  -> return ()

-- | /O(1)/. Serialize a Unicode character using the UTF-8 encoding.
--
fromChar :: Char -> Builder
fromChar = fromWriteSingleton writeChar

-- | /O(n)/. Serialize the Latin-1 encodable characters of a Unicode 'String'.
--
fromString :: String -> Builder
fromString = fromWriteList writeChar

-- | /O(n)/. Serialize a value by 'Show'ing it and Latin-1 encoding the
-- encodable characters of the resulting 'String'.
--
fromShow :: Show a => a -> Builder
fromShow = fromString . show

-- | /O(n)/. Serialize the encodable characters of a strict Unicode 'TS.Text'
-- value using the Latin-1 encoding.
--
{-# INLINE fromText #-}
fromText :: TS.Text -> Builder
fromText = fromString . TS.unpack



-- | /O(n)/. Serialize the encodable characters of a lazy Unicode 'TS.Text'
-- value using the Latin-1 encoding.
--
{-# INLINE fromLazyText #-}
fromLazyText :: TL.Text -> Builder
fromLazyText = fromString . TL.unpack
