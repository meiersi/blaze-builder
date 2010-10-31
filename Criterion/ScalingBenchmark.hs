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

import Data.Function (on)
import Data.List (unfoldr, group, transpose, sortBy)
import Data.Word (Word8)
import Data.Monoid
import Data.Int (Int64)

import qualified Data.Vector.Generic as V

import Text.Blaze.Builder
import Text.Blaze.Builder.Internal

import qualified Data.Binary.Builder as B

import Data.Maybe
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Data.Char (isSpace, toLower)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Gtk

import Criterion
import Criterion.Environment
import Criterion.Monad
import Criterion.Types
import Criterion.Config hiding (Plot)
import Criterion.Measurement (secs)
import Criterion.Analysis (countOutliers, classifyOutliers)

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Statistics.Types
import Statistics.Sample
import Statistics.Quantile as Statistics 
import Statistics.Function as Statistics

import System.FilePath

import Text.Printf (printf)

import Codec.Compression.GZip

import qualified System.Random as R

-- Main function
----------------

main :: IO ()
main = do
    let config = defaultConfig
    env <- withConfig config measureEnvironment
    runAndPlot config env compressComparison
    runAndPlot config env packComparison
  where
    runAndPlot config env sc = do
      sc' <- withConfig config $ runScalingComparison env sc
      let conv = fromIntegral :: Int -> Double
      plotScalingComparison (PDF 555 416) conv sc'
      plotScalingComparison (PNG 640 480) conv sc'

-- | Comparison of different implementations of packing [Word8].
packComparison :: ScalingComparison Int
packComparison = compareBenchmarks "Packing [Word8]" "bytes" vs $
    [ ScalingBenchmark "S.pack"           (\x-> whnf S.pack                      (take x word8s))
    , ScalingBenchmark "L.pack"           (\x-> whnf (L.length . L.pack)         (take x word8s))
    , ScalingBenchmark "packStrict"       (\x-> whnf packStrict                  (take x word8s))
    , ScalingBenchmark "packLazy"         (\x-> whnf (L.length . packLazy)       (take x word8s))
    , ScalingBenchmark "Binary.packLazy"  (\x-> whnf (L.length . binaryPackLazy) (take x word8s))
    ]
  where
    vs :: [Int]
    vs =  map head . group . map round . takeWhile (<= 200) $ iterate (*(1.5::Double)) 1
    -- vs =  [0..19] -- map (2^) [5..8]

    packLazy :: [Word8] -> L.ByteString
    packLazy = toLazyByteString . fromWord8s

    packStrict :: [Word8] -> S.ByteString
    packStrict = toByteString . fromWord8s

    binaryPackLazy :: [Word8] -> L.ByteString
    binaryPackLazy = B.toLazyByteString . mconcat . map B.singleton

    word8s :: [Word8]
    word8s = cycle [0..]
    {-# NOINLINE word8s #-}


chunk :: Int -> [a] -> [[a]]
chunk size xs = c : case xs' of [] -> []; _ -> chunk size xs'
  where
    (c, xs') = splitAt size xs

-- | Compare compressing a chunked sequence of bytes with and without
-- compaction on redundant and random data.
compressComparison :: ScalingComparison Int
compressComparison = compareBenchmarks ("Compressing "++show kb++"kb of data") "chunk size in bytes" vs $
    [ ScalingBenchmark "random / direct"             (whnf compressDirectly  . randomByteStrings)
    , ScalingBenchmark "random / with compaction"    (whnf compressCompacted . randomByteStrings)
    , ScalingBenchmark "redundant / direct"          (whnf compressDirectly  . redundantByteStrings)
    , ScalingBenchmark "redundant / with compaction" (whnf compressCompacted . redundantByteStrings)
    ]
  where
    kb = 200
    n = kb * 1024

    vs :: [Int]
    vs = takeWhile (<= 100000) $ iterate (2*) 1

    randomWord8s = map fromIntegral . take n $ unfoldr (Just . R.next) (R.mkStdGen 666) 
    randomByteStrings c = L.fromChunks . map S.pack . chunk c $ randomWord8s
    {-# NOINLINE randomByteStrings #-}

    redundantWord8s = take n $ cycle [0..]
    redundantByteStrings c = L.fromChunks . map S.pack . chunk c $ redundantWord8s
    {-# NOINLINE redundantByteStrings #-}

    compressDirectly :: L.ByteString -> Int64
    compressDirectly = L.length . compress

    compressCompacted :: L.ByteString -> Int64
    compressCompacted = 
      L.length . compress . toLazyByteString . fromLazyByteString


-- BoxPlots
-----------
    
data BoxPlot = BoxPlot
       { bpMean               :: Double
       , bpHighSevereOutliers :: Sample
       , bpHighMildOutliers   :: Sample
       , bpHighWhisker        :: Double
       , bpHighQuartile       :: Double
       , bpMedian             :: Double
       , bpLowQuartile        :: Double
       , bpLowWhisker         :: Double
       , bpLowMildOutliers    :: Sample
       , bpLowSevereOutliers  :: Sample
       }

boxPlot :: Sample -> BoxPlot
boxPlot sa = BoxPlot
    { bpMean               = mean ssa
    , bpHighSevereOutliers = V.filter (hiS <) ssa
    , bpHighMildOutliers   = V.filter (\x -> hiM < x && x <= hiS) ssa
    , bpHighWhisker        = fromMaybe hiM $ V.find (hiM >=) (V.reverse ssa)
    , bpHighQuartile       = q3
    , bpMedian             = Statistics.weightedAvg 2 4 ssa
    , bpLowQuartile        = q1
    , bpLowWhisker         = fromMaybe loM $ V.find (loM <=) ssa
    , bpLowMildOutliers    = V.filter (\x -> loS <= x && x < loM) ssa
    , bpLowSevereOutliers  = V.filter (< loS) ssa
    }
  where
    ssa = Statistics.sort sa
    loS = q1 - (iqr * 3)
    loM = q1 - (iqr * 1.5)
    hiM = q3 + (iqr * 1.5)
    hiS = q3 + (iqr * 3)
    q1  = Statistics.weightedAvg 1 4 ssa
    q3  = Statistics.weightedAvg 3 4 ssa
    iqr = q3 - q1

-- | Compute the k-th percentile of a sample.
percentile :: Int -> Sample -> Double
percentile k = Statistics.weightedAvg k 100

-- Scaling Benchmark Infrastructure
-----------------------------------

-- | A scaling benchmark denotes a named benchmark constructor, which can be
-- used to investigate the scaling behaviour of a 'Benchmarkable' function.
data ScalingBenchmark a where
    ScalingBenchmark :: (Benchmarkable b)
                     => String              -- ^ Benchmark name
                     -> (a -> b)            -- ^ Benchmark constructor
                     -> ScalingBenchmark a  -- ^ Scaling benchmark

-- | Extract the name of a 'ScalingBenchmark'.
scalingBenchmarkName :: ScalingBenchmark a -> String
scalingBenchmarkName (ScalingBenchmark name _) = name

-- | A comparison of several benchmarks on a common set of test points.
data ScalingComparison a = ScalingComparison
      { scName         :: String
      , scTestUnit     :: String
      , scTestValues   :: [a]                   -- INV: non-empty
      , scBenchmarks   :: [ScalingBenchmark a]  -- INV: non-empty
      , scMeasurements :: [[Sample]]      -- measurements: benchmarks * test points;
                                          -- [] iff the benchmarks haven't been run yet
      }

-- | Compare several benchmarks on a set of test values.
compareBenchmarks :: String               -- ^ Name of comparison.
                  -> String               -- ^ Unit of test values.
                  -> [a]                  -- ^ Test values.
                  -> [ScalingBenchmark a] -- ^ Benchmarks to compare.
                  -> ScalingComparison a  -- ^ Resulting scaling comparison.
compareBenchmarks name unit vs bs = 
    ScalingComparison name unit vs bs []

-- | Annotate the measurements stored in a 'ScalingComparison'.
annotateMeasurements :: (a -> b)
                     -> ScalingComparison a 
                     -> [(String, [(b,Sample)])]
annotateMeasurements f sc =
    zip (map scalingBenchmarkName $ scBenchmarks sc) 
        (map (zip (map f $ scTestValues sc)) (scMeasurements sc))

-- | Run a 'ScalingComparison'.
runScalingComparison :: Show a
                     => Environment                -- ^ Criterion environment.
                     -> ScalingComparison a        -- ^ Comparison to run (may or may not contain any measurements already).
                     -> Criterion (ScalingComparison a) -- ^ Comparison with 'scMeasurements' field set to new measurements.
runScalingComparison env sc = do
    samples <- mapM runBenchmarks $ scTestValues sc
    return $ sc { scMeasurements = transpose samples }
  where

    runBenchmarks x = do
        let testPointStr = show x ++ " " ++ scTestUnit sc
        liftIO $ putStrLn $ ""
        liftIO $ putStrLn $ "running benchmarks for " ++ testPointStr
        samples <- sequence 
            [ ((,) name) `liftM` runBenchmark env (mkBench x) 
            | ScalingBenchmark name mkBench <- scBenchmarks sc ]
        liftIO $ putStrLn $ ""
        liftIO $ putStrLn $ "ranking for " ++ testPointStr
        liftIO $ quickAnalysis samples
        return $ map snd samples

    quickAnalysis samples = do
        let indent = length (show $ length samples + 1) + 2
            extent = maximum (map (length . fst) samples) + 2
        mapM_ (printStatistics indent extent) 
              (zip [1..] $ sortBy (compare `on` (mean . snd)) samples)
 
    printStatistics :: Int -> Int -> (Int, (String, Sample)) -> IO ()
    printStatistics indent extent (i, (info, sample)) = putStrLn $
        rightAlign indent (show i) ++ ". " ++
        leftAlign extent (info ++ ":") ++
        "mean " ++ secs (mean sample) ++ 
        " (2p " ++ secs p2 ++ 
        ", 98p " ++ secs p98 ++
        ", out " ++ rightAlign 2 (show outliers) ++
        ")"
      where
        -- percentiles
        p2  = percentile  2 sample
        p98 = percentile 98 sample
        -- outliers
        outliers = countOutliers . classifyOutliers $ sample

    rightAlign n cs = take (n - length cs) (repeat ' ') ++ cs
    leftAlign  n cs = cs ++ take (n - length cs) (repeat ' ')


-- | Plot a scaling comparison.
plotScalingComparison :: (PlotValue b, RealFloat b) 
                      => PlotOutput           -- ^ Output format.
                      -> (a -> b)             -- ^ Test point conversion function.
                      -> ScalingComparison a  -- ^ Comparison to plot.
                      -> IO ()

plotScalingComparison (PDF x y) conv sc =
  renderableToPDFFile (renderScalingComparison conv sc) x y
                      (mangle $ printf "%s scaling %dx%d.pdf" (scName sc) x y)

plotScalingComparison (PNG x y) conv sc =
  renderableToPNGFile (renderScalingComparison conv sc) x y
                      (mangle $ printf "%s scaling %dx%d.png" (scName sc) x y)

plotScalingComparison (SVG x y) conv sc =
  renderableToSVGFile (renderScalingComparison conv sc) x y
                      (mangle $ printf "%s scaling %dx%d.svg" (scName sc) x y)

plotScalingComparison (Window x y) conv sc =
  renderableToWindow (renderScalingComparison conv sc) x y


-- | Render a scaling comparison using an adaption of the boxplot technique to
-- lineplots.
renderScalingComparison :: (PlotValue b, RealFloat b) 
                        => (a -> b) -> ScalingComparison a -> Renderable ()
renderScalingComparison f sc = 
    toRenderable $
      layout1_plots ^= plots $
      layout1_title ^= scName sc $
      layout1_bottom_axis ^= mkLogAxis (scTestUnit sc) $
      layout1_right_axis ^= mkLogAxis "seconds" $
      defaultLayout1
  where
    plots = concat $ zipWith plotAnnotatedSamples
      (map opaque $ colorPalette)
      (annotateMeasurements f sc)



-- | Plot the annotated samples with several lines for the 
--
plotAnnotatedSamples :: AlphaColour Double 
                     -> (String, [(a,Sample)]) 
                     -> [Either (Plot a Double) (Plot a Double)]
plotAnnotatedSamples colour (name, points) =
    map (Right . noLegend . uncurry (line (solidLine 1)))
        [ (0.2, bpLowWhisker)
        , (0.4, bpLowQuartile)
        , (0.4, bpHighQuartile)
        , (0.2, bpHighWhisker)
        ] ++
        [ Right $ line (solidLine 1) 0.9 bpMedian] ++
    map (Right . noLegend)
        [ toPlot $ plotPoints name meanStyle (map (second bpMean) points')
        , outliers severeOutStyle bpHighSevereOutliers
        , outliers severeOutStyle bpLowSevereOutliers
        , outliers mildOutStyle bpHighMildOutliers
        , outliers mildOutStyle bpLowMildOutliers
        ] 
  where
    points' = map (second boxPlot) points
    -- line :: Double -> (Sample -> Double) -> Plot a Double
    line style trans proj = toPlot $ plotLine name
        (solidLine 1 $ dissolve trans colour)
        (map (second proj) points')

    outliers style proj = toPlot $ plotPoints name style
        (concat [zip (repeat x) (V.toList $ proj bp) | (x, bp) <- points'])

    severeOutStyle = filledCircles 2   (dissolve 0.4 colour)
    mildOutStyle   = hollowCircles 2 1 (dissolve 0.4 colour)
    meanStyle      = exes 3 1 colour


noLegend :: Plot x y -> Plot x y
noLegend = plot_legend ^= []

-- | Plot a single named line using the given line style.
plotLine :: String -> CairoLineStyle -> [(a,b)] -> PlotLines a b
plotLine name style points = 
    plot_lines_title ^= name $
    plot_lines_style ^= style $
    plot_lines_values ^= [points] $ 
    defaultPlotLines

-- | Plot a single named line using the given line style.
plotPoints :: String -> CairoPointStyle -> [(a,b)] -> PlotPoints a b
plotPoints name style points = 
    plot_points_title ^= name $
    plot_points_style ^= style $
    plot_points_values ^= points $ 
    defaultPlotPoints


colorPalette :: [Colour Double]
colorPalette = [blue, green, red, cyan, magenta, yellow]

mkLinearAxis :: PlotValue x => String -> LayoutAxis x
mkLinearAxis name = laxis_title ^= name $ defaultLayoutAxis

mkLogAxis :: (RealFloat x, PlotValue x) => String -> LayoutAxis x
mkLogAxis name = 
  laxis_title ^= name $ 
  laxis_generate ^= autoScaledLogAxis defaultLogAxis $
  defaultLayoutAxis

-- Filehandling 
---------------


-- | Get rid of spaces and other potentially troublesome characters
-- from output.
--
-- Copied from: Criterion.Plot
mangle :: String -> FilePath
mangle = concatMap (replace ((==) '-' . head) "-")
       . group
       . map (replace isSpace '-' . replace (==pathSeparator) '-' . toLower)
    where replace p r c | p c       = r
                        | otherwise = c

