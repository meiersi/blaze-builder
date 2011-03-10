{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmarking IO output speed of writing a string in Utf8 encoding to a file.
module Utf8IO (main)  where

import           Control.Monad
import           Control.Exception (evaluate)

import qualified Codec.Binary.UTF8.Light as Utf8Light

import           Data.Char (chr)
import           Data.Time.Clock
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as Utf8String
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           System.IO
import           System.Environment

import           Blaze.ByteString.Builder           
import           Blaze.ByteString.Builder.Internal (defaultBufferSize)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze


-- | Write using the standard text utf8 encoding function built into 'base'.
writeUtf8_base :: String -> FilePath -> IO ()
writeUtf8_base cs file = 
    withFile file WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h cs

-- | Write using utf8 encoding as provided by the 'blaze-builder' library.
writeUtf8_blaze :: String -> FilePath -> IO ()
writeUtf8_blaze cs file = L.writeFile file $ toLazyByteString $ Blaze.fromString cs

-- | Write using utf8 encoding as provided by the 'text' library.
writeUtf8_text :: TL.Text -> FilePath -> IO ()
writeUtf8_text tx file = L.writeFile file $ TL.encodeUtf8 tx

-- | Write using utf8 encoding as provided by the 'utf8-string' library.
writeUtf8_string :: String -> FilePath -> IO ()
writeUtf8_string cs file = L.writeFile file $ Utf8String.fromString cs

-- | Write using utf8 encoding as provided by the 'utf8-light' library. Note
-- that this library only allows encoding the whole string as a strict
-- bytestring. That might make it unusable in some circumstances.
{-# NOINLINE writeUtf8_light #-}
writeUtf8_light :: String -> FilePath -> IO ()
writeUtf8_light cs file = Utf8Light.writeUTF8File file cs


main :: IO ()
main = do
    [how, len, file] <- getArgs
    let blocksize = 32000
        block     = map chr [0..blocksize]
        n         = read len
        cs        = take n $ cycle $ block
        tx        = TL.pack cs
    writer <- case how of
        "base"        -> return $ writeUtf8_base cs
        "blaze"       -> return $ writeUtf8_blaze cs
        "utf8-string" -> return $ writeUtf8_string cs

        -- utf8-light is missing support for lazy bytestrings => test 100 times
        -- writing a 100 times smaller string to avoid out-of-memory errors.
        "utf8-light"  -> return $ \f -> sequence_ $ replicate 100 $ 
                                        writeUtf8_light (take (n `div` 100) cs) f

        "via-text"    -> do return $ writeUtf8_text tx

        -- Here, we ensure that the text tx is already packed before timing.
        "text"        -> do _ <- evaluate (TL.length tx) 
                            return $ writeUtf8_text tx
        _             -> error $ "unknown writer '" ++ how ++ "'"
    t <- timed_ $ writer file
    putStrLn $ how ++ ": " ++ show t

------------------------------------------------------------------------------
-- Timing
------------------------------------------------------------------------------

-- | Execute an IO action and return its result plus the time it took to execute it.
timed :: IO a -> IO (a, NominalDiffTime)
timed io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- getCurrentTime
  return (x, diffUTCTime t1 t0)

-- | Execute an IO action and return the time it took to execute it.
timed_ :: IO a -> IO NominalDiffTime
timed_ = (snd `liftM`) . timed

