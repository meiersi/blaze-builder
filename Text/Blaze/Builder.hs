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
    , module Text.Blaze.Builder.Utf8
    , module Text.Blaze.Builder.Html
    , module Text.Blaze.Builder.Word
    ) where

import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8
import Text.Blaze.Builder.Html
import Text.Blaze.Builder.Word
