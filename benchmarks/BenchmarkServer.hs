{- Benchmark server based upon Jasper van der Jeugt's 'BenchmarkServer.lhs'
   from blaze-html. Modified for network-2.3 by Simon Meier <iridcode@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
module BenchmarkServer where

import Prelude hiding (putStrLn)

import Data.Char   (ord)
import Data.Monoid 
import Data.ByteString.Char8 () -- IsString instance only
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L

import Control.Concurrent (forkIO, putMVar, takeMVar, newEmptyMVar)
import Control.Exception  (bracket)
import Control.Monad 

import Network.Socket   (Socket, accept, sClose)
import Network          (listenOn, PortID (PortNumber))
import Network.Socket.ByteString      as S
import Network.Socket.ByteString.Lazy as L

import System (getArgs)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal (defaultBufferSize, defaultMinimalBufferSize)
import Blaze.ByteString.Builder.Char.Utf8

import Criterion.Main

httpOkHeader :: S.ByteString 
httpOkHeader = S.concat 
    [ "HTTP/1.1 200 OK\r\n"
    , "Content-Type: text/html; charset=UTF-8\r\n"
    , "\r\n" ]

response :: Int -> Builder
response n = 
  fromByteString httpOkHeader `mappend` 
  fromString (take n $ cycle "hello λ-world! ")

sendVectoredBuilderLBS :: Socket -> Builder -> IO ()
sendVectoredBuilderLBS s = L.sendAll s . toLazyByteString
{-# NOINLINE sendVectoredBuilderLBS #-}

sendBuilderLBS :: Socket -> Builder -> IO ()
sendBuilderLBS s = 
  -- mapM_ (S.sendAll s) . L.toChunks . toLazyByteString
  L.foldrChunks (\c -> (S.sendAll s c >>)) (return ()). toLazyByteString
{-# NOINLINE sendBuilderLBS #-}

sendBuilderBSIO :: Socket -> Builder -> IO ()
sendBuilderBSIO s = toByteStringIO $ S.sendAll s
{-# NOINLINE sendBuilderBSIO #-}

-- criterion benchmark determining the speed of response
main2 = defaultMain
    [ bench ("response " ++ show n) $ whnf 
        (L.length . toLazyByteString . response) n
    ]
  where
    n :: Int
    n = 1000000

main :: IO ()
main = do
    [port, nChars] <- map read `liftM` getArgs
    killSignal <- newEmptyMVar
    bracket (listenOn . PortNumber . fromIntegral $ port) sClose 
        (\socket -> do
            _ <- forkIO $ loop (putMVar killSignal ()) nChars socket
            takeMVar killSignal)
  where
    loop killServer nChars socket = forever $ do 
        (s, _) <- accept socket
        forkIO (respond s nChars)
      where
        respond s n = do
            input <- S.recv s 1024
            let requestUrl = (S.split (fromIntegral $ ord ' ') input) !! 1
            case tail (S.split (fromIntegral $ ord '/') requestUrl) of
                ["lbs"]     -> sendBuilderLBS s         $ response n
                ["lbs-vec"] -> sendVectoredBuilderLBS s $ response n
                ["bs-io"]   -> sendBuilderBSIO   s      $ response n
                ["kill"]    -> notFound s >> killServer
                _           -> notFound s
            sClose s

    notFound s = do
        _ <- S.sendAll s $ "HTTP/1.1 404 Not Found\r\n"
            `mappend` "Content-Type: text/html; charset=UTF-8\r\n"
            `mappend` "\r\n"
            `mappend` "<h1>Page not found</h1>"
        return ()
