-- | Tests for the Blaze builder
--
{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Control.Applicative ((<$>))
import Data.Monoid (mempty, mappend, mconcat)

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

instance Show Builder where
    show = show . toLazyByteString

instance Eq Builder where
    b1 == b2 = toLazyByteString b1 == toLazyByteString b2

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary
