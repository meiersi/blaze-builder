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
      -- * The @Builder@ type
      Builder

      -- * Creating builders
    , module Blaze.ByteString.Builder.Write 
    , module Blaze.ByteString.Builder.Int
    , module Blaze.ByteString.Builder.Word
    , module Blaze.ByteString.Builder.ByteString
    , flush

      -- * Executing builders
    , toLazyByteString
    , toLazyByteStringWith
    , toByteString
    , toByteStringIO
    , toByteStringIOWith
    

      -- * Compatibility to Data.Binary.Builder from the binary package
      --
      -- | The following functions ensure that @"Blaze.ByteString.Builder"@ is a
      -- drop-in replacement for @Data.Binary.Builder@ from the @binary@
      -- package. Note that these functions are deprecated and may be removed
      -- in future versions of the @blaze-builder@ package.
      --
    , empty                   -- DEPRECATED: use 'mempty' instead
    , singleton               -- DEPRECATED: use 'fromByte' instead
    , append                  -- DEPRECATED: use 'mappend' instead
                              
    , putWord16be             -- DEPRECATED: use 'fromWord<n><endian>' instead
    , putWord32be             --
    , putWord64be             --
    , putWord16le             --
    , putWord32le             --
    , putWord64le             --   for all these functions
    , putWordhost             --
    , putWord16host           --
    , putWord32host           --
    , putWord64host           --
    ) where

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Write
import Blaze.ByteString.Builder.Int
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.ByteString

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

-- | /O(1)/. Serialize a 'Word16' in big endian format.
--
-- /Deprecated:/ use 'fromWord16be' instead.
putWord16be :: Word16 -> Builder
putWord16be = fromWord16be 
{-# DEPRECATED putWord16be "Use 'fromWord16be' instead." #-}

-- | /O(1)/. Serialize a 'Word32' in big endian format.
--
-- /Deprecated:/ use 'fromWord32be' instead.
putWord32be :: Word32 -> Builder
putWord32be = fromWord32be
{-# DEPRECATED putWord32be "Use 'fromWord32be' instead." #-}

-- | /O(1)/. Serialize a 'Word64' in big endian format.
--
-- /Deprecated:/ use 'fromWord64be' instead.
putWord64be :: Word64 -> Builder
putWord64be = fromWord64be
{-# DEPRECATED putWord64be "Use 'fromWord64be' instead." #-}

-- | /O(1)/. Serialize a 'Word16' in little endian format.
--
-- /Deprecated:/ use 'fromWord16le' instead.
putWord16le :: Word16 -> Builder
putWord16le = fromWord16le
{-# DEPRECATED putWord16le "Use 'fromWord16le' instead." #-}

-- | /O(1)/. Serialize a 'Word32' in little endian format.
--
-- /Deprecated:/ use 'fromWord32le' instead.
putWord32le :: Word32 -> Builder
putWord32le = fromWord32le
{-# DEPRECATED putWord32le "Use 'fromWord32le' instead." #-}

-- | /O(1)/. Serialize a 'Word64' in little endian format.
--
-- /Deprecated:/ use 'fromWord64le' instead.
putWord64le :: Word64 -> Builder
putWord64le = fromWord64le
{-# DEPRECATED putWord64le "Use 'fromWord64le' instead." #-}

-- | /O(1)/. Serialize a 'Word' in host endian format.
--
-- /Deprecated:/ use 'fromWordhost' instead.
putWordhost :: Word -> Builder
putWordhost = fromWordhost
{-# DEPRECATED putWordhost "Use 'fromWordhost' instead." #-}

-- | /O(1)/. Serialize a 'Word16' in host endian format.
--
-- /Deprecated:/ use 'fromWord16host' instead.
putWord16host :: Word16 -> Builder
putWord16host = fromWord16host
{-# DEPRECATED putWord16host "Use 'fromWord16host' instead." #-}

-- | /O(1)/. Serialize a 'Word32' in host endian format.
--
-- /Deprecated:/ use 'fromWord32host' instead.
putWord32host :: Word32 -> Builder
putWord32host = fromWord32host
{-# DEPRECATED putWord32host "Use 'fromWord32host' instead." #-}

-- | /O(1)/. Serialize a 'Word64' in host endian format.
--
-- /Deprecated:/ use 'fromWord64host' instead.
putWord64host :: Word64 -> Builder
putWord64host = fromWord64host
{-# DEPRECATED putWord64host "Use 'fromWord64host' instead." #-}

