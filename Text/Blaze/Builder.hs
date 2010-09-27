-----------------------------------------------------------------------------
-- |
-- Module      : Text.Blaze.Builder
-- Copyright   : Jasper Van der Jeugt, Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Jasper Van der Jeugt <jaspervdj@gmail.com>, 
--               Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- @'Text.Blaze.Builder'@ is the main module, which you should import as a user
-- of the library.
--
-- > import Text.Blaze.Builder
-- 
-- It provides you with a type 'Builder' that allows to efficiently construct
-- lazy bytestrings. 
--
-- Intuitively, a 'Builder' denotes the construction of a part of a lazy
-- bytestring. Builders can either be created using one of the primitive
-- combinators in "Text.Blaze.Builder.Write" or by using one of the predefined
-- combinators for standard Haskell values (see the exposed modules of this
-- package).  Concatenation of Builders is done using 'mappend' from the
-- 'Monoid' typeclass.
--
-- Here is a small example that serializes a list of strings using the UTF-8
-- encoding.
--
-- > import Text.Blaze.Builder.Char.Utf8
--
-- > strings :: [String]
-- > strings = replicate 10000 "Hello there!"
--
-- The function 'fromString' creates a 'Builder' denoting the UTF-8 encoded
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
-- /A note on history./ This serialization library was inspired by the module
-- @Data.Binary.Builder@ provided by the @binary@ package. It was originally
-- developed with the specific needs of the @blaze-html@ package in mind. Since
-- then it has been restructured to serve as a drop-in replacement for
-- @Data.Binary.Builder@, which it improves upon both in speed as well as
-- expressivity.
-----------------------------------------------------------------------------

module Text.Blaze.Builder
    ( 
      -- * Builder combinators and constructors
      module Text.Blaze.Builder.Write 
    , module Text.Blaze.Builder.ByteString
    , module Text.Blaze.Builder.Word

    , Builder
    , flush

    -- ** Obtaining the built chunks
    , toLazyByteString
    , toLazyByteStringWith
    , toByteStringIO
    , toByteStringIOWith
    

      -- * Compatibility to Data.Binary.Builder from the binary package
      --
      -- | The following functions ensure that @"Text.Blaze.Builder"@ is a
      -- drop-in replacement for @Data.Binary.Builder@ from the @binary@
      -- package. Note that these functions are deprecated and will be removed
      -- in future versions of the @blaze-builder@ package.
      --
    , empty                   -- DEPRECATED: use 'mempty' instead
    , singleton               -- DEPRECATED: use 'fromByte' instead
    , append                  -- DEPRECATED: use 'mappend' instead
    ) where

import Text.Blaze.Builder.Internal
import Text.Blaze.Builder.Write
import Text.Blaze.Builder.ByteString
import Text.Blaze.Builder.Word

import Data.Monoid
import Data.Word

------------------------------------------------------------------------------
-- API Compatibility to Data.Binary.Builder from 'binary'
------------------------------------------------------------------------------

-- | /O(1)/. An empty builder. 
--
-- /Deprecated:/ use 'mempty' instead.
empty :: Builder
empty = mempty
{-# DEPRECATED empty "Use 'mempty' instead." #-}

-- | /O(1)/. Append two builders. 
--
-- /Deprecated:/ use 'mappend' instead.
append :: Builder -> Builder -> Builder
append = mappend
{-# DEPRECATED append "Use 'mappend' instead." #-}

-- | /O(1)/. Serialize a single byte.
--
-- /Deprecated:/ use 'fromWord8' instead.
singleton :: Word8 -> Builder
singleton = fromWriteSingleton writeWord8
{-# DEPRECATED singleton "Use 'fromWord8' instead." #-}

