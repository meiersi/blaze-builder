-- | The builder monoid from BlazeHtml.
--
-- Usage is fairly straightforward. Builders can be constructed from many
-- values, including 'String' and 'Text' values.
--
-- > strings :: [String]
-- > strings = replicate 10000 "Hello there!"
--
-- Concatenation should happen through the 'Monoid' interface.
--
-- > concatenation :: Builder
-- > concatenation = mconcat $ map fromString strings
--
-- There is only one way to efficiently obtain the result: to convert the
-- 'Builder' to a lazy 'L.ByteString' using 'toLazyByteString'.
--
-- > result :: L.ByteString
-- > result = toLazyByteString concatenation
--
module Text.Blaze.Builder
    ( module Text.Blaze.Builder.Core
    , module Text.Blaze.Builder.ByteString
    , module Text.Blaze.Builder.Word
    , module Text.Blaze.Builder.Utf8
    , module Text.Blaze.Builder.Html

    , empty                   -- DEPRECATED: use 'mempty' instead
    , singleton               -- DEPRECATED: use 'fromByte' instead
    , append                  -- DEPRECATED: use 'mappend' instead
    ) where

import Text.Blaze.Builder.Core
import Text.Blaze.Builder.ByteString
import Text.Blaze.Builder.Word
import Text.Blaze.Builder.Utf8
import Text.Blaze.Builder.Html

import Data.Monoid
import Data.Word

------------------------------------------------------------------------------
-- API Compatibility to Data.Binary.Builder from 'binary'
------------------------------------------------------------------------------

-- | /O(1)/. An empty builder. Deprecated: use 'mempty' instead.
empty :: Builder
empty = mempty

-- | Construct a 'Builder' from a single byte. Deprecated use 'fromByte'
-- instead.
--
singleton :: Word8    -- ^ Byte to create a 'Builder' from
          -> Builder  -- ^ Resulting 'Builder'
singleton = fromWriteSingleton writeWord8

-- | /O(1)/. Append two builders. Deprecated: use 'mappend' instead.
append :: Builder -> Builder -> Builder
append = mappend

