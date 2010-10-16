{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Criterion.ScalingBenchmark
-- Copyright   : Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Measure the scaling behaviour of a benchmarkable function.
--
-----------------------------------------------------------------------------

module Criterion.ScalingBenchmark where

import Prelude hiding (lines)

import Data.List (unfoldr)
import Data.Word (Word8)

import Text.Blaze.Builder

import Data.Maybe
import Data.Accessor
import Data.Colour
import Data.Colour.Names

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Gtk

import Criterion
import Criterion.Environment
import Criterion.Monad
import Criterion.Types
import Criterion.Config

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Statistics.Types
import Statistics.Sample

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified System.Random as R

-- | A pseudo-random stream of 'Word8' always started from the same initial
-- seed.
randomWord8s :: [Word8]
randomWord8s = map fromIntegral $ unfoldr (Just . R.next) (R.mkStdGen 666) 

mapDataPoints f (ScalingSample info samples) = ScalingSample info (map (first f) samples)

-- Main function
----------------

main :: IO ()
main = do
    let config = defaultConfig
    env <- withConfig config measureEnvironment
    samples <- withConfig config (runScalingBenchmarks env benchs testPoints)
    let samples' :: [ScalingSample String Double Sample]
        samples' = map (mapDataPoints fromIntegral) samples
        renderable = renderScalingMeans "Packing lists of bytes" "bytes" samples'
    renderableToWindow renderable 640 480
  where
    testPoints :: [Int]
    testPoints = map (2^) [0..10]
    benchs =
      [ ScalingBenchmark "S.pack"     (\x-> whnf S.pack                (take x randomWord8s))
      , ScalingBenchmark "packStrict" (\x-> whnf packStrict            (take x randomWord8s))
      , ScalingBenchmark "L.pack"     (\x-> whnf (L.length . L.pack)   (take x randomWord8s))
      , ScalingBenchmark "packLazy"   (\x-> whnf (L.length . packLazy) (take x randomWord8s))
      ]

packLazy :: [Word8] -> L.ByteString
packLazy = toLazyByteString . fromWord8s

packStrict :: [Word8] -> S.ByteString
packStrict = toStrictByteString . fromWord8s

    

-- Scaling Benchmark Infrastructure
-----------------------------------

-- | A scaling benchmark denotes a named benchmark constructor, which can be
-- used to investigate the scaling behaviour of a 'Benchmarkable' function.
data ScalingBenchmark i a where
    ScalingBenchmark :: (Benchmarkable b)
                     => i                   -- ^ Additional benchmark information
                     -> (a -> b)            -- ^ Benchmark constructor
                     -> ScalingBenchmark i a  -- ^ Scaling benchmark

data ScalingSample i a s where
    ScalingSample :: i                -- ^ Additional benchmark information
                  -> [(a,s)]          -- ^ Test points annotated with measured sample
                  -> ScalingSample i a s -- ^ Scaling sample

-- | Run a 'ScalingBenchmark'.
runScalingBenchmark :: Environment                   -- ^ Criterion environment
                    -> ScalingBenchmark i a          -- ^ Scaling benchmark
                    -> [a]                           -- ^ Test points to run benchmark on
                    -> Criterion (ScalingSample i a Sample) -- ^ Measured scaling sample
runScalingBenchmark env (ScalingBenchmark info mkBench) xs = do
    samples <- mapM (runBenchmark env . mkBench) xs
    return $ ScalingSample info (zip xs samples)


-- | Run a series of 'ScalingBenchmark's on the same test points.
runScalingBenchmarks :: Environment                   -- ^ Criterion environment
                     -> [ScalingBenchmark i a]        -- ^ Scaling benchmarks
                     -> [a]                           -- ^ Test points to run benchmark on
                     -> Criterion [ScalingSample i a Sample] -- ^ Measured scaling samples
runScalingBenchmarks env bs xs = 
    sequence [runScalingBenchmark env b xs | b <- bs]

-- Transforming scaling samples
-------------------------------

instance Functor (ScalingSample i a) where
    fmap f (ScalingSample info samples) = 
        ScalingSample info (map (second f) samples)

-- | Access the info of a 'ScalingSample'.
scalingInfo :: ScalingSample i a s -> i
scalingInfo (ScalingSample info _) = info

-- | Access the samples of a 'ScalingSample'.
scalingSamples :: ScalingSample i a s -> [(a,s)]
scalingSamples (ScalingSample _ samples) = samples


-- Plotting scaling benchmarks.
------------------------------

-- NOTE: Data points should also be annotated with their unit.


renderScalingMeans :: (RealFloat a, PlotValue a)
                   => String               -- ^ Plot title
                   -> String               -- ^ Bottom axis description
                   -> [ScalingSample String a Sample]   -- ^ Samples whose mean we want to plot
                   -> Renderable ()
renderScalingMeans title xaxis samples = 
    toRenderable $
      layout1_plots ^= map (Right . toPlot) plots $ 
      layout1_title ^= title $
      layout1_bottom_axis ^= mkLogAxis xaxis $
      layout1_right_axis ^= mkLogAxis "seconds" $
      defaultLayout1
  where
    linesName = map scalingInfo samples
    linesData = map (scalingSamples . fmap mean) samples
    plots = zipWith3 plotLine linesName (cycle lineStylePalette) linesData

-- Plotting Infrastructure
--------------------------

colorPalette :: [Colour Double]
colorPalette = [blue, green, red, yellow, magenta, cyan]

lineStylePalette :: [CairoLineStyle]
lineStylePalette = 
    map (solidLine 1 . opaque)         colorPalette ++
    map (dashedLine 1 [5, 5] . opaque) colorPalette

-- | Plot a single named line using the given line style.
plotLine :: String -> CairoLineStyle -> [(a,b)] -> PlotLines a b
plotLine name style points = 
    plot_lines_title ^= name $
    plot_lines_style ^= style $
    plot_lines_values ^= [points] $ 
    defaultPlotLines

mkLinearAxis :: PlotValue x => String -> LayoutAxis x
mkLinearAxis name = laxis_title ^= name $ defaultLayoutAxis

mkLogAxis :: (RealFloat x, PlotValue x) => String -> LayoutAxis x
mkLogAxis name = 
  laxis_title ^= name $ 
  laxis_generate ^= autoScaledLogAxis defaultLogAxis $
  defaultLayoutAxis



{-
-- Plots to be generated
------------------------

{-

Compression:
  1 plot (title "compressing <n> MB of random data using 'zlib')
    3 lines (direct, compacted using a Builder, compaction time) [chunk size/ms]


ChunkedWrite:
  1 plot (title "serializing a list of <n> elements")
    1 line per type of element [chunk size/ms]


Throughput:
  5 x 3 plots (word type x endianness) (title "<n> MB of <type> (<endianness>)")
    1 line per type of Word [chunk size/ MB/s]

-}


-- Benchmarking Infrastructure
------------------------------

type MyCriterion a = ReaderT Environment Criterion a

-- | Run a list of benchmarks; flattening benchmark groups to a path of strings.
runFlattenedBenchmarks :: [Benchmark] -> MyCriterion [([String],Sample)]
runFlattenedBenchmarks = 
    (concat `liftM`) . mapM (go id)
  where
    go path (Benchmark name b)   = do
      env <- ask
      sample <- lift $ runBenchmark env b
      return [(path [name], sample)]
    go path (BenchGroup name bs) = 
      concat `liftM` mapM (go (path . (name:))) bs

-- | Run a benchmark for a series of data points; e.g. to measure scalability
-- properties.
runSeriesBenchmark :: (a -> Benchmark) -> [a] -> MyCriterion [(a,Sample)]
runSeriesBenchmark mkBench xs =
    (zip xs . map snd) `liftM` runFlattenedBenchmarks (map mkBench xs)


-- | Use the given config to measure the environment and then run the embedded
-- criterion operation with this information about the environment.
runMyCriterion :: Config -> MyCriterion a -> IO a
runMyCriterion config criterion = do
    env <- withConfig config measureEnvironment
    withConfig config (runReaderT criterion env)
    




{-
-- Plot Experiments
-------------------


testData :: [(Int,Double)]
testData = zip xs (map (fromIntegral . (^2)) xs)
  where xs = [1,2,4,8,16,32]


blazeLineStyle = solidLine 1 . opaque
binaryLineStyle = dashedLine 1 [5, 5] . opaque


plots :: [PlotLines Int Double]
plots = [ plotLine [c] style testData 
        | (c, style) <- zip ['a'..] (cycle lineStylePalette) ]


mkLayout xname yname title p = 
    layout1_plots ^= [Right p] $ 
    layout1_title ^= title $
    layout1_bottom_axis ^= mkLinearAxis xname $
    layout1_right_axis ^= mkLogAxis yname $
    defaultLayout1

layouts = zipWith (mkLayout "chunksize" "MB/s") (map return ['A'..]) (map toPlot plots)

testGrid = aboveN $ map (besideN . map (flip tspan (1,1) . toRenderable)) [l1,l2]
  where
  (l1,l2) = splitAt 3 layouts

testIt = renderableToWindow (gridToRenderable testGrid) 640 480
-}

{-
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

-}
-}
