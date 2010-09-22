{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Throughput.Memory (memBench) where

import Foreign
import Foreign.C

import Control.Exception
import System.CPUTime
import Numeric

memBench :: Int -> IO ()
memBench mb = do
  let bytes = mb * 2^20
  allocaBytes bytes $ \ptr -> do
    let bench label test = do
          seconds <- time $ test (castPtr ptr) (fromIntegral bytes)
          let throughput = fromIntegral mb / seconds
          putStrLn $ show mb ++ "MB of " ++ label
                  ++ " in " ++ showFFloat (Just 3) seconds "s, at: "
                  ++ showFFloat (Just 1) throughput "MB/s"
    bench "setup        " c_wordwrite
    putStrLn ""
    putStrLn "C memory throughput benchmarks:"
    bench "bytes written                     " c_bytewrite
    bench "bytes read                        " c_byteread
    bench "words written                     " c_wordwrite
    bench "words read                        " c_wordread
    putStrLn ""
    putStrLn "Haskell memory throughput benchmarks:"
    bench "bytes written                     " hs_bytewrite
    bench "bytes written (loop unrolled once)" hs_bytewrite2
    bench "bytes read                        " hs_byteread
    bench "words written                     " hs_wordwrite
    bench "words read                        " hs_wordread

hs_bytewrite  :: Ptr CUChar -> Int -> IO ()
hs_bytewrite !ptr bytes = loop 0 0
  where iterations = bytes
        loop :: Int -> CUChar -> IO ()
        loop !i !n | i == iterations = return ()
                   | otherwise = do pokeByteOff ptr i n
                                    loop (i+1) (n+1)

hs_bytewrite2  :: Ptr CUChar -> Int -> IO ()
hs_bytewrite2 !start bytes = loop start 0
  where end = start `plusPtr` bytes
        loop :: Ptr CUChar -> CUChar -> IO ()
        loop !ptr !n | ptr `plusPtr` 2 < end = do
                         poke ptr               n
                         poke (ptr `plusPtr` 1) (n+1)
                         loop (ptr `plusPtr` 2) (n+2)
                     | ptr `plusPtr` 1 < end =
                         poke ptr               n
                     | otherwise             = return ()

hs_byteread  :: Ptr CUChar -> Int -> IO CUChar
hs_byteread !ptr bytes = loop 0 0
  where iterations = bytes
        loop :: Int -> CUChar -> IO CUChar
        loop !i !n | i == iterations = return n
                   | otherwise = do x <- peekByteOff ptr i
                                    loop (i+1) (n+x)

hs_wordwrite :: Ptr CULong -> Int -> IO ()
hs_wordwrite !ptr bytes = loop 0 0
  where iterations = bytes `div` sizeOf (undefined :: CULong)
        loop :: Int -> CULong -> IO ()
        loop !i !n | i == iterations = return ()
                   | otherwise = do pokeByteOff ptr i n
                                    loop (i+1) (n+1)

hs_wordread  :: Ptr CULong -> Int -> IO CULong
hs_wordread !ptr bytes = loop 0 0
  where iterations = bytes `div` sizeOf (undefined :: CULong)
        loop :: Int -> CULong -> IO CULong
        loop !i !n | i == iterations = return n
                   | otherwise = do x <- peekByteOff ptr i
                                    loop (i+1) (n+x)


foreign import ccall unsafe "CBenchmark.h byteread"
  c_byteread :: Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "CBenchmark.h bytewrite"
  c_bytewrite :: Ptr CUChar -> CInt -> IO ()

foreign import ccall unsafe "CBenchmark.h wordread"
  c_wordread :: Ptr CUInt -> CInt -> IO ()

foreign import ccall unsafe "CBenchmark.h wordwrite"
  c_wordwrite :: Ptr CUInt -> CInt -> IO ()

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)
