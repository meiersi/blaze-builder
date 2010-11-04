{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : StringAndText
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmarking of String and Text serialization.
module StringAndText (main)  where

import Data.Char (ord)
import Data.Monoid 

import Criterion.Main

import Foreign (plusPtr)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TS
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Blaze.ByteString.Builder           as Blaze
import qualified Blaze.ByteString.Builder.Internal  as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze

main :: IO ()
main = defaultMain 
    [ bench "TL.unpack :: LazyText -> String" $ nf
        TL.unpack benchLazyText

    , bench "TL.foldr  :: LazyText -> String" $ nf
        (TL.foldr (:) []) benchLazyText
    
    , bench "fromString :: String --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromString) benchString

    , bench "fromStrictTextUnpacked :: StrictText --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromText) benchStrictText
     
    , bench "fromStrictTextFolded :: StrictText --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromStrictTextFolded) benchStrictText

    , bench "TS.encodeUtf8 :: StrictText --[Utf8 encoding]--> S.ByteString" $ whnf
        (TS.encodeUtf8) benchStrictText

    , bench "fromLazyTextUnpacked :: LazyText --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromLazyText) benchLazyText

    , bench "fromLazyTextFolded :: LazyText --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromLazyTextFolded) benchLazyText

    , bench "TL.encodeUtf8 :: LazyText --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . TL.encodeUtf8) benchLazyText

    , bench "fromHtmlEscapedString :: String --[Html esc. & Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromHtmlEscapedString) benchString

    , bench "fromHtmlEscapedStrictTextUnpacked :: StrictText --[HTML esc. & Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromHtmlEscapedText) benchStrictText
     
    , bench "fromHtmlEscapedLazyTextUnpacked :: LazyText --[HTML esc. & Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromHtmlEscapedLazyText) benchLazyText
     
    ]

n :: Int
n = 100000

benchString :: String
benchString = take n $ concatMap show [(1::Int)..]
{-# NOINLINE benchString #-}

benchStrictText :: TS.Text
benchStrictText = TS.pack benchString
{-# NOINLINE benchStrictText #-}

benchLazyText :: TL.Text
benchLazyText = TL.pack benchString
{-# NOINLINE benchLazyText #-}


-- | Encode the 'TS.Text' as UTF-8 by folding it and filling the raw buffer
-- directly.
fromStrictTextFolded :: TS.Text -> Blaze.Builder
fromStrictTextFolded t = Blaze.Builder $ \k -> TS.foldr step k t
  where
    step c k pf pe
      | pf' <= pe = do
          io pf
          k pf' pe  -- here it would be great, if we wouldn't have to pass
                    -- around pe: requires a more powerful fold for StrictText.
      | otherwise =
          return $ Blaze.BufferFull size pf $ \pfNew peNew -> do
            let pfNew' = pfNew `plusPtr` size
            io pfNew
            k pfNew' peNew
      where
        pf' = pf `plusPtr` size
        Blaze.Write size io = Blaze.writeChar c
{-# INLINE fromStrictTextFolded #-}

-- | Encode the 'TL.Text' as UTF-8 by folding it and filling the raw buffer
-- directly.
fromLazyTextFolded :: TL.Text -> Blaze.Builder
fromLazyTextFolded t = Blaze.Builder $ \k -> TL.foldr step k t
  where
    step c k pf pe
      | pf' <= pe = do
          io pf
          k pf' pe  -- here it would be great, if we wouldn't have to pass
                    -- around pe: requires a more powerful fold for StrictText.
      | otherwise =
          return $ Blaze.BufferFull size pf $ \pfNew peNew -> do
            let pfNew' = pfNew `plusPtr` size
            io pfNew
            k pfNew' peNew
      where
        pf' = pf `plusPtr` size
        Blaze.Write size io = Blaze.writeChar c
{-# INLINE fromLazyTextFolded #-}
