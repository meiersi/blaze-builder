-----------------------------------------------------------------------------
-- |
-- Module      : BenchThroughput
-- Copyright   : Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- This benchmark is based on 'tests/Benchmark.hs' from the 'binary-0.5.0.2'
-- package.
--
-- Benchmark the throughput of 'blaze-builder' and 'binary' for serializing
-- sequences of 'Word8' .. 'Word64' values in little-endian, big-endian, and
-- "host-endian" formats.
--
-- The results on a Core2 Duo T7500 with Linux 2.6.32-24 i686 and GHC 6.12.3
-- are as follows:
--
--   Using the Blaze.Builder directly (i.e. not encapsulated in a writer monad
--   as Put is doing it) gives the best scalability. Up to 'Word32', it holds
--   that the bigger the chunk size, the bigger the relative speedup of using
--   the Blaze.Builder. For 'Word64', the speedup is not as impressive.
--   Probably due to the more expensive writes.
--
-----------------------------------------------------------------------------

module BenchThroughput (main) where

import qualified Throughput.BinaryBuilder as BinaryBuilder
import qualified Throughput.BinaryPut     as BinaryPut
import qualified Throughput.BinaryBuilderDeclarative as BinaryBuilderDecl

import qualified Throughput.BlazeBuilder as BlazeBuilder
import qualified Throughput.BlazePut     as BlazePut
import qualified Throughput.BlazeBuilderDeclarative as BlazeBuilderDecl

import Throughput.Utils
import Throughput.Memory

import qualified Data.ByteString.Lazy as L
import Debug.Trace
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Control.Exception
import Control.Monad
import System.CPUTime
import Numeric
import Text.Printf
import System.Environment
import System.IO

import Data.Maybe
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk


-- The different serialization functions
----------------------------------------

supportAllSizes f wS cS e i = return $ f wS cS e i

blazeLineStyle = solidLine 1 . opaque
binaryLineStyle = dashedLine 1 [5, 5] . opaque

blazeBuilder      = 
  ( "BlazeBuilder"
  , blazeLineStyle green
  , supportAllSizes $ BlazeBuilder.serialize)

blazeBuilderDecl  = 
  ( "BlazeBuilderDecl"
  , blazeLineStyle blue
  , supportAllSizes $ BlazeBuilderDecl.serialize)

blazePut          = 
  ( "BlazePut"
  , blazeLineStyle red
  , supportAllSizes $ BlazePut.serialize)

binaryBuilder     = 
  ( "BinaryBuilder"
  , binaryLineStyle green
  , supportAllSizes $ BinaryBuilder.serialize)

binaryBuilderDecl = 
  ( "BinaryBuilderDecl"
  , binaryLineStyle blue
  , BinaryBuilderDecl.serialize)

binaryPut  = 
  ( "BinaryPut"
  , binaryLineStyle red
  , supportAllSizes $ BinaryPut.serialize)


main :: IO ()
main = do
  mb <- getArgs >>= readIO . head
  -- memBench (mb*10) 
  putStrLn ""
  putStrLn "Binary serialisation benchmarks:"

  -- do bytewise 
  -- sequence_
    -- [ test wordSize chunkSize Host mb
    -- | wordSize  <- [1]
    -- , chunkSize <- [1,2,4,8,16]
    -- ]

  -- now Word16 .. Word64
  let lift f wS cS e i = return $ f wS cS e i
      serializers = 
        [ blazeBuilder , blazeBuilderDecl , blazePut
        , binaryBuilder, binaryBuilderDecl, binaryPut
        ]
      wordSizes  = [1,2,4,8]
      chunkSizes = [1,2,4,8,16] 
      endians    = [Host,Big,Little]

  let compares = 
        [ compareResults serialize wordSize chunkSize end mb
        | wordSize  <- wordSizes
        , chunkSize <- chunkSizes
        , end       <- endians
        , serialize <- serializers
        , wordSize /= 1 || end == Host -- no endianess for Word8
        ]
  -- putStrLn "checking equality of serialization results:"
  -- sequence_ compares


  let serializes = 
        [  [ ( serialize
             , [ (chunkSize, test serialize wordSize chunkSize end mb)
               | chunkSize <- [1,2,4,8,16]
               ]
             )
           | serialize <- serializers
           ]
        | wordSize  <- [1] -- ,2,4,8]
        , end       <- [Host] -- ,Big,Little]
        , wordSize /= 1 || end == Host -- no endianess for Word8
        ]


  putStrLn "\n\nbenchmarking serialization speed:"
  results <- mapM mkChart serializes
  print results

mkChart :: [((String,CairoLineStyle,a), [(Int, IO (Maybe Double))])] -> IO ()
mkChart task = do
  lines <- catMaybes `liftM` mapM measureSerializer task
  let plottedLines = flip map lines $ \ ((name,lineStyle,_), points) ->
          plot_lines_title ^= name $
          plot_lines_style ^= lineStyle $
          plot_lines_values ^= [points] $ 
          defaultPlotLines
  let layout = 
        defaultLayout1
          { layout1_plots_ = map (Right . toPlot) plottedLines }
  renderableToWindow (toRenderable layout) 640 480  


measureSerializer :: (a, [(Int, IO (Maybe Double))]) -> IO (Maybe (a, [(Int,Double)]))
measureSerializer (info, tests) = do
  optPoints <- forM tests $ \ (x, test) -> do
    optY <- test
    case optY of 
      Nothing -> return Nothing
      Just y  -> return $ Just (x, y)
  case catMaybes optPoints of
    []     -> return Nothing
    points -> return $ Just (info, points)

------------------------------------------------------------------------

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)

------------------------------------------------------------------------

test :: (String, a, Int -> Int -> Endian -> Int -> Maybe L.ByteString) 
     -> Int -> Int -> Endian -> Int -> IO (Maybe Double)
test (serializeName, _, serialize) wordSize chunkSize end mb = do
    let bytes :: Int
        bytes = mb * 2^20
        iterations = bytes `div` wordSize
    case serialize wordSize chunkSize end iterations of
      Nothing -> return Nothing
      Just bs -> do
        _ <- printf "%17s: %dMB of Word%-2d in chunks of %2d (%6s endian):"
            serializeName (mb :: Int) (8 * wordSize :: Int) (chunkSize :: Int) (show end)

        putSeconds <- time $ evaluate (L.length bs)
        -- getSeconds <- time $ evaluate sum
    --    print (L.length bs, sum)
        let putThroughput = fromIntegral mb / putSeconds
            -- getThroughput = fromIntegral mb / getSeconds

        _ <- printf "%6.1f MB/s write\n"
               putThroughput
               -- getThroughput
               -- (getThroughput/putThroughput)
     
        hFlush stdout
        return $ Just putThroughput

------------------------------------------------------------------------

compareResults :: (String, a, Int -> Int -> Endian -> Int -> Maybe L.ByteString) 
     -> Int -> Int -> Endian -> Int -> IO ()
compareResults (serializeName, _, serialize) wordSize chunkSize end mb0 = do
    let mb :: Int
        mb = max 1 (mb0 `div` 100)
        bytes :: Int
        bytes = mb * 2^20
        iterations = bytes `div` wordSize
        bs0 = BinaryBuilder.serialize wordSize chunkSize end iterations
    case serialize wordSize chunkSize end iterations of
      Nothing -> return ()
      Just bs1 -> do
        _ <- printf "%17s: %dMB of Word%-2d in chunks of %2d (%6s endian):"
          serializeName (mb :: Int) (8 * wordSize :: Int) (chunkSize :: Int) (show end)
        if (bs0 == bs1) 
          then putStrLn " Ok"
          else putStrLn " Failed"
        hFlush stdout
      
