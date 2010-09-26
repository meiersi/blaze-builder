{-# LANGUAGE OverloadedStrings #-}
-- | Benchmarking of String and Text serialization.
module StringAndText (main)  where

import Data.Char (ord)
import Data.Monoid 

import Criterion.Main

import Foreign (plusPtr)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Text.Blaze.Builder               as Blaze
import qualified Text.Blaze.Builder.Core.Internal as Blaze
import qualified Text.Blaze.Builder.Char.Utf8     as Blaze

main :: IO ()
main = defaultMain 
    [ bench "fromString :: String --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . Blaze.fromString) benchString

    , bench "fromTextUnpacked :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromTextUnpacked) benchText
     
    , bench "fromTextFolded :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromTextFolded) benchText

    , bench "fromTextSingleWrite :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromTextSingleWrite) benchText

    , bench "fromTextEncoded :: Text --[Utf8 encoding]--> L.ByteString" $ whnf
        (L.length . Blaze.toLazyByteString . fromTextEncoded) benchText

    , bench "encodeUtf8 :: Text --[Utf8 encoding]--> S.ByteString" $ whnf
        (T.encodeUtf8) benchText

    ]
  where
    n :: Int
    n = 100000

    benchString :: String
    benchString = take n $ concatMap show [(1::Int)..]
    {-# NOINLINE benchString #-}

    benchText :: Text
    benchText = T.pack benchString
    {-# NOINLINE benchText #-}


-- | Encode the 'Text' as UTF-8 by building a single write and writing all
-- characters in one go.
fromTextSingleWrite :: Text -> Blaze.Builder
fromTextSingleWrite = 
    Blaze.fromWriteSingleton (T.foldl (\w c -> w `mappend` Blaze.writeChar c) mempty)

-- | Encode the 'Text' as UTF-8 by folding it and filling the raw buffer
-- directly.
fromTextFolded :: Text -> Blaze.Builder
fromTextFolded t = Blaze.Builder $ \k -> T.foldr step k t
  where
    step c k pf pe
      | pf' <= pe = do
          io pf
          k pf' pe  -- here it would be great, if we wouldn't have to pass
                    -- around pe: requires a more powerful fold for Text.
      | otherwise =
          return $ Blaze.BufferFull size pf $ \pfNew peNew -> do
            let pfNew' = pfNew `plusPtr` size
            io pfNew
            k pfNew' peNew
      where
        pf' = pf `plusPtr` size
        Blaze.Write size io = Blaze.writeChar c
{-# INLINE fromTextFolded #-}

-- | Encode the 'Text' as UTF-8 by unpacking it and encoding the resulting
-- 'String'. This is currently the fastest method!
fromTextUnpacked :: Text -> Blaze.Builder
fromTextUnpacked = Blaze.fromString . T.unpack
{-# INLINE fromTextUnpacked #-}

fromTextEncoded :: Text -> Blaze.Builder
fromTextEncoded = Blaze.fromByteString . T.encodeUtf8
{-# INLINE fromTextEncoded #-}
