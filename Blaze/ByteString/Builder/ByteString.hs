------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.ByteString
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.ByteString
    (
    -- * Strict bytestrings
      writeByteString
    , fromByteString
    , fromByteStringWith
    , copyByteString
    , insertByteString

    -- * Lazy bytestrings
    , fromLazyByteString
    , fromLazyByteStringWith
    , copyLazyByteString
    , insertLazyByteString

    ) where


import Blaze.ByteString.Builder.Internal.Write ( Write, exactWrite )
import Foreign
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L


writeByteString :: S.ByteString -> Write
writeByteString bs = exactWrite l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

fromByteString :: S.ByteString -> B.Builder
fromByteString = B.byteString

fromByteStringWith :: Int -> S.ByteString -> B.Builder
fromByteStringWith = B.byteStringThreshold

copyByteString :: S.ByteString -> B.Builder
copyByteString = B.byteStringCopy

insertByteString :: S.ByteString -> B.Builder
insertByteString = B.byteStringInsert

fromLazyByteString :: L.ByteString -> B.Builder
fromLazyByteString = B.lazyByteString

fromLazyByteStringWith :: Int -> L.ByteString -> B.Builder
fromLazyByteStringWith = B.lazyByteStringThreshold

copyLazyByteString :: L.ByteString -> B.Builder
copyLazyByteString = B.lazyByteStringCopy

insertLazyByteString :: L.ByteString -> B.Builder
insertLazyByteString = B.lazyByteStringInsert
