-- | Tests for the Blaze builder
--
{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Control.Applicative ((<$>))
import Data.Monoid (mempty, mappend, mconcat)
import Data.Word (Word8)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Codec.Binary.UTF8.String (decode)

import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8
import Text.Blaze.Builder.Html

main :: IO ()
main = defaultMain $ return $ testGroup "Tests" tests

tests :: [Test]
tests =
    [ testProperty "left identity Monoid law"  monoidLeftIdentity
    , testProperty "right identity Monoid law" monoidRightIdentity
    , testProperty "associativity Monoid law"  monoidAssociativity
    , testProperty "mconcat Monoid law"        monoidConcat
    , testProperty "string → builder → string" fromStringToString
    , testProperty "string and text"           stringAndText
    , testProperty "lazy bytestring identity"  identityLazyByteString
    , testProperty "flushing identity"         identityFlushing
    , testCase     "escaping case 1"           escaping1
    , testCase     "escaping case 2"           escaping2
    , testCase     "escaping case 3"           escaping3
    ]

monoidLeftIdentity :: Builder -> Bool
monoidLeftIdentity b = mappend mempty b == b

monoidRightIdentity :: Builder -> Bool
monoidRightIdentity b = mappend b mempty == b

monoidAssociativity :: Builder -> Builder -> Builder -> Bool
monoidAssociativity x y z = mappend x (mappend y z) == mappend (mappend x y) z

monoidConcat :: [Builder] -> Bool
monoidConcat xs = mconcat xs == foldr mappend mempty xs

fromStringToString :: String -> Bool
fromStringToString string = string == convert string
  where
    convert = decode . LB.unpack . toLazyByteString . fromString

stringAndText :: String -> Bool
stringAndText string = fromString string == fromText (T.pack string)

identityLazyByteString :: LB.ByteString -> Bool
identityLazyByteString lbs = lbs == toLazyByteString (fromLazyByteString lbs)

identityFlushing :: String -> String -> Bool
identityFlushing s1 s2 =
    let b1 = fromString s1
        b2 = fromString s2
    in b1 `mappend` b2 == b1 `mappend` flush `mappend` b2

escaping1 :: Assertion
escaping1 = fromString "&lt;hello&gt;" @?= fromHtmlEscapedString "<hello>"

escaping2 :: Assertion
escaping2 = fromString "f &amp;&amp;&amp; g" @?= fromHtmlEscapedString "f &&& g"

escaping3 :: Assertion
escaping3 = fromString "&quot;&apos;" @?= fromHtmlEscapedString "\"'"

instance Show Builder where
    show = show . toLazyByteString

instance Eq Builder where
    b1 == b2 = toLazyByteString b1 == toLazyByteString b2

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary

instance Arbitrary LB.ByteString where
    arbitrary = LB.pack <$> arbitrary
