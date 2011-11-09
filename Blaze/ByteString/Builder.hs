{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blaze.ByteString.Builder
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- "Blaze.ByteString.Builder" is the main module, which you should import as a user
-- of the @blaze-builder@ library.
--
-- > import Blaze.ByteString.Builder
-- 
-- It provides you with a type 'Builder' that allows to efficiently construct
-- lazy bytestrings with a large average chunk size.
--
-- Intuitively, a 'Builder' denotes the construction of a part of a lazy
-- bytestring. Builders can either be created using one of the primitive
-- combinators in "Blaze.ByteString.Builder.Write" or by using one of the predefined
-- combinators for standard Haskell values (see the exposed modules of this
-- package).  Concatenation of builders is done using 'mappend' from the
-- 'Monoid' typeclass.
--
-- Here is a small example that serializes a list of strings using the UTF-8
-- encoding.
--
-- @ import "Blaze.ByteString.Builder.Char.Utf8"@
--
-- > strings :: [String]
-- > strings = replicate 10000 "Hello there!"
--
-- The function @'fromString'@ creates a 'Builder' denoting the UTF-8 encoded
-- argument. Hence, UTF-8 encoding and concatenating all @strings@ can be done
-- follows.
--
-- > concatenation :: Builder
-- > concatenation = mconcat $ map fromString strings
--
-- The function 'toLazyByteString'  can be used to execute a 'Builder' and
-- obtain the resulting lazy bytestring.
--
-- > result :: L.ByteString
-- > result = toLazyByteString concatenation
--
-- The @result@ is a lazy bytestring containing 10000 repetitions of the string
-- @\"Hello there!\"@ encoded using UTF-8. The corresponding 120000 bytes are
-- distributed among three chunks of 32kb and a last chunk of 6kb.
--
-- /A note on history./ This serialization library was inspired by the
-- @Data.Binary.Builder@ module provided by the @binary@ package. It was
-- originally developed with the specific needs of the @blaze-html@ package in
-- mind. Since then it has been restructured to serve as a drop-in replacement
-- for @Data.Binary.Builder@, which it improves upon both in speed as well as
-- expressivity.
-----------------------------------------------------------------------------

module Blaze.ByteString.Builder
    ( 
      -- * The 'Builder' type
      Builder

      -- * Creating builders
    , module Blaze.ByteString.Builder.Int
    , module Blaze.ByteString.Builder.Word
    , module Blaze.ByteString.Builder.ByteString
    , flush

      -- * Executing builders
    , toLazyByteString
    -- , toLazyByteStringWith
    , toByteString
    -- , toByteStringIO
    -- , toByteStringIOWith

    ) where

import qualified Data.ByteString                              as S
import qualified Data.ByteString.Internal                     as S
import qualified Data.ByteString.Lazy                         as L
import qualified Data.ByteString.Lazy.Internal                as L
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras (flush)
                 
import           Blaze.ByteString.Builder.Int
import           Blaze.ByteString.Builder.Word
import           Blaze.ByteString.Builder.ByteString

import           Foreign



-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = do
    S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)

-- | Run the builder to construct a strict bytestring containing the sequence
-- of bytes denoted by the builder. This is done by first serializing to a lazy bytestring and then packing its
-- chunks to a appropriately sized strict bytestring.
--
-- > toByteString = packChunks . toLazyByteString
--
-- Note that @'toByteString'@ is a 'Monoid' homomorphism.
--
-- > toByteString mempty          == mempty
-- > toByteString (x `mappend` y) == toByteString x `mappend` toByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toByteString :: Builder -> S.ByteString
toByteString = packChunks . toLazyByteString

