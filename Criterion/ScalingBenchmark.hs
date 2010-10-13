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

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Statistics.Types

import qualified System.Random as R


-- | A scaling benchmark denotes a named benchmark constructor, which can be
-- used to investigate the scaling behaviour of a 'Benchmarkable' function.
data ScalingBenchmark i a where
    ScalingBenchmark :: (Benchmarkable b)
                     => i                   -- ^ Additional benchmark information
                     -> (a -> b)            -- ^ Benchmark constructor
                     -> ScalingBenchmark a  -- ^ Scaling benchmark

data ScalingSample i a where
    ScalingSample :: i                -- ^ Additional benchmark information
                  -> [(a,Sample)]     -- ^ Test points annotated with measured sample
                  -> ScalingSample a  -- ^ Scaling sample

-- scalingBench

-- | Run a 'ScalingBenchmark'.
runScalingBenchmark :: Environment                   -- ^ Criterion environment
                    -> [a]                           -- ^ Test points to run benchmark on
                    -> ScalingBenchmark i a          -- ^ Scaling benchmark
                    -> Criterion (ScalingSample i a) -- ^ Measured scaling sample
runScalingBenchmark env xs (ScalingBenchmark info mkBench) = do
    samples <- mapM (runBenchmark env . mkBench) xs
    return $ ScalingSample info (zip xs samples)


-- | Run a series of 'ScalingBenchmark's on the same test points.
runScalingBenchmarks :: Environment                   -- ^ Criterion environment
                     -> [a]                           -- ^ Test points to run benchmark on
                     -> [ScalingBenchmark i a]        -- ^ Scaling benchmarks
                     -> Criterion [ScalingSample i a] -- ^ Measured scaling samples
runScalingBenchmarks env xs = mapM (runScalingBenchmark env xs)

-- Plotting scaling bencmarks.
------------------------------

renderScalingMeans :: Floating a 
                   => String                -- ^ Bottom axis description
                   -> String                -- ^ Plot title
                   -> [ScalingSample i a]   -- ^ Samples whose mean we want to plot
                   -> Renderable ()
renderScalingMeans 


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

-- | A pseudo-random stream of 'Word8' always started from the same initial
-- seed.
randomWord8s :: [Word8]
randomWord8s = map fromIntegral $ unfoldr (Just . R.next) (R.mkStdGen 666) 

-- Main function
----------------

main :: IO ()
main = undefined


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
    


-- Plotting Infrastructure
--------------------------

colorPalette :: [Colour Double]
colorPalette = [blue, green, red, yellow, magenta, cyan]

lineStylePalette :: [CairoLineStyle]
lineStylePalette = 
    map (solidLine 1 . opaque)         colorPalette ++
    map (dashedLine 1 [5, 5] . opaque) colorPalette

-- | > ((title, xName, yName), [(lineName,[(x,y)])])
type PlotData = ((String, String, String), [(String, [(Int, Double)])])

layoutPlot :: PlotData -> Layout1 Int Double
layoutPlot ((title, xName, yName), lines) =
    layout1_plots ^= map (Right . toPlot) plots $ 
    layout1_title ^= title $
    layout1_bottom_axis ^= mkLinearAxis xName $
    layout1_right_axis ^= mkLogAxis yName $
    defaultLayout1
  where
    (linesName, linesData) = unzip lines
    plots = zipWith3 plotLine linesName (cycle lineStylePalette) linesData

-- | Plot a single named line using the given line style.
plotLine :: String -> CairoLineStyle -> [(Int,Double)] -> PlotLines Int Double
plotLine name style points = 
    plot_lines_title ^= name $
    plot_lines_style ^= style $
    plot_lines_values ^= [points] $ 
    defaultPlotLines

mkLinearAxis :: String -> LayoutAxis Int
mkLinearAxis name = laxis_title ^= name $ defaultLayoutAxis

mkLogAxis :: String -> LayoutAxis Double
mkLogAxis name = 
  laxis_title ^= name $ 
  laxis_generate ^= autoScaledLogAxis defaultLogAxis $
  defaultLayoutAxis




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
