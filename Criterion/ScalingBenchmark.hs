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
import Data.List (unfoldr, group, transpose, sortBy, intersperse)
import Data.Word (Word8)
import Data.Monoid
import Data.Int (Int64)

import qualified Data.Vector.Generic as V

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal

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
    mapM_ (runAndPlot config env)
      [ (compressComparison, defaultPlotConfig { pcLogYAxis = False })
      , (packComparison, defaultPlotConfig)
      , (zoomedPackComparison, defaultPlotConfig)
      ] 
  where
    runAndPlot config env (sc, plotConfig) = do
      sc' <- withConfig config $ runScalingComparison env sc
      mkPlots sc' plotConfig

    mkPlots sc plotConfig = sequence_
        [ plotScalingComparison outType 
            (plotConfig {pcBoxPlot = doBoxPlot}) conv sc
        | outType <- outTypes PDF ++ outTypes PNG,
          doBoxPlot <- [True, False]
        ] 
      where
        conv = fromIntegral :: Int -> Double
        outTypes f = map (uncurry f) [(640,480),(800,600),(1280,1024)]

-- | Comparison of different implementations of packing [Word8].
packComparison, zoomedPackComparison :: ScalingComparison Int
(packComparison, zoomedPackComparison) = 
  ( cmp "Packing [Word8]"             broadVs (bsFaster ++ bsSlower)
  , cmp "Packing short [Word8] lists" zoomedVs (bsFaster)
  )
  where
    cmp name = compareBenchmarks name "bytes"

    bsFaster = 
      [ ScalingBenchmark "S.pack"               (\x-> whnf S.pack                          (take x word8s))
      , ScalingBenchmark "packLazy"             (\x-> whnf (L.length . packLazy)           (take x word8s))
      , ScalingBenchmark "packStrict"           (\x-> whnf packStrict                      (take x word8s))
      ]

    bsSlower = 
      [ ScalingBenchmark "L.pack"               (\x-> whnf (L.length . L.pack)             (take x word8s))
      , ScalingBenchmark "declPackLazy"         (\x-> whnf (L.length . declPackLazy)       (take x word8s))
      , ScalingBenchmark "Binary.declPackLazy"  (\x-> whnf (L.length . binaryDeclPackLazy) (take x word8s))
      ]
    
    mkLogVs :: Double -> Double -> [Int]
    mkLogVs factor upperBound = 
      map head . group . map round . takeWhile (<= upperBound) $ 
      iterate (*factor) 1

    broadVs  = mkLogVs 1.5 (200 * 1024)
    zoomedVs = mkLogVs 1.1  256

    byteStringPackLazy :: [Word8] -> L.ByteString
    byteStringPackLazy = L.pack

    byteStringPackStrict :: [Word8] -> S.ByteString
    byteStringPackStrict = S.pack

    packLazy :: [Word8] -> L.ByteString
    packLazy = toLazyByteString . fromWord8s

    packStrict :: [Word8] -> S.ByteString
    packStrict = toByteString . fromWord8s

    declPackLazy :: [Word8] -> L.ByteString
    declPackLazy = toLazyByteString . mconcat . map fromWord8

    binaryDeclPackLazy :: [Word8] -> L.ByteString
    binaryDeclPackLazy = B.toLazyByteString . mconcat . map B.singleton

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
    {-
    [ ScalingBenchmark "rnd. data / direct"             (whnf compressDirectly  . randomByteStrings)
    , ScalingBenchmark "rnd. data / with compaction"    (whnf compressCompacted . randomByteStrings)
    , ScalingBenchmark "red. data / direct"          (whnf compressDirectly  . redundantByteStrings)
    , ScalingBenchmark "red. data / with compaction" (whnf compressCompacted . redundantByteStrings)
    ]
    -}
    [ ScalingBenchmark "direct"          (whnf compressDirectly  . redundantByteStrings)
    , ScalingBenchmark "with compaction" (whnf compressCompacted . redundantByteStrings)
    , ScalingBenchmark "compaction only" (whnf compaction        . redundantByteStrings)
    ]
  where
    kb = 200
    n = kb * 1024

    vs :: [Int]
    vs = takeWhile (<= 100000) $ iterate (2*) 1

    -- randomWord8s = map fromIntegral . take n $ unfoldr (Just . R.next) (R.mkStdGen 666) 
    -- randomByteStrings c = L.fromChunks . map S.pack . chunk c $ randomWord8s
    -- {-# NOINLINE randomByteStrings #-}

    redundantWord8s = take n $ cycle [0..]
    redundantByteStrings c = L.fromChunks . map S.pack . chunk c $ redundantWord8s
    {-# NOINLINE redundantByteStrings #-}

    compressDirectly :: L.ByteString -> Int64
    compressDirectly = L.length . compress

    compressCompacted :: L.ByteString -> Int64
    compressCompacted = 
      L.length . compress . toLazyByteString . fromLazyByteString

    compaction :: L.ByteString -> Int64
    compaction = L.length . toLazyByteString . fromLazyByteString


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

data PlotConfig = PlotConfig {
    pcBoxPlot  :: Bool
  , pcLogYAxis :: Bool
  , pcLogXAxis :: Bool
  } 
  deriving( Eq, Ord, Show )

defaultPlotConfig :: PlotConfig
defaultPlotConfig = PlotConfig True True True

prettyPlotConfig (PlotConfig boxPlot logY logX) =
    concat $ intersperse "," $ msum
      [f boxPlot "boxplot", f logY "log-y", f logX "log-x"]
  where
    f b info = if b then return info else mzero

-- | Plot a scaling comparison.
plotScalingComparison :: (PlotValue b, RealFloat b) 
                      => PlotOutput           -- ^ Output format.
                      -> PlotConfig           -- ^ Plot configuration.
                      -> (a -> b)             -- ^ Test point conversion function.
                      -> ScalingComparison a  -- ^ Comparison to plot.
                      -> IO ()

plotScalingComparison output config conv sc =
    renderableToFile $ renderScalingComparison config conv sc
  where
    renderableToFile = case output of
      PDF x y    -> \r -> renderableToPDFFile r x y (mkName "pdf" x y)
      PNG x y    -> \r -> renderableToPNGFile r x y (mkName "png" x y)
      SVG x y    -> \r -> renderableToSVGFile r x y (mkName "svg" x y)
      Window x y -> \r -> renderableToWindow  r x y 

    mkName fileType x y = mangle $ 
      printf "%s scaling %s%dx%d.%s" (scName sc) plotType x y fileType

    plotType = case prettyPlotConfig config of
      ""   -> ""
      info -> "("++info++")"

-- plotScalingComparison (PNG x y) doBoxplot conv sc =
--   renderableToPNGFile (renderScalingComparison doBoxplot conv sc) x y
--                       (mangle $ printf "%s scaling %dx%d.png" (scName sc) x y)
-- 
-- plotScalingComparison (SVG x y) doBoxplot conv sc =
--   renderableToSVGFile (renderScalingComparison doBoxplot conv sc) x y
--                       (mangle $ printf "%s scaling %dx%d.svg" (scName sc) x y)
-- 
-- plotScalingComparison (Window x y) doBoxplot conv sc =
--   renderableToWindow (renderScalingComparison doBoxplot conv sc) x y
-- 

-- | Render a scaling comparison using an adaption of the boxplot technique to
-- lineplots.
renderScalingComparison :: (PlotValue b, RealFloat b) 
                        => PlotConfig -> (a -> b) -> ScalingComparison a -> Renderable ()
renderScalingComparison config f sc = 
    toRenderable $
      layout1_plots ^= plots $
      layout1_title ^= scName sc $
      layout1_bottom_axis ^= mkAxis pcLogXAxis (scTestUnit sc) $
      layout1_right_axis ^= mkAxis pcLogYAxis "seconds" $
      defaultLayout1
  where
    mkAxis proj | proj config = mkLogAxis
                | otherwise   = mkLinearAxis
    plotFunction | pcBoxPlot config = boxplotAnnotatedSamples
                 | otherwise        = plotAnnotatedSamples
    plots = concat $ zipWith plotFunction
      (map opaque $ colorPalette)
      (annotateMeasurements f sc)



-- | Plot the means of the annotated samples
--
plotAnnotatedSamples :: AlphaColour Double 
                     -> (String, [(a,Sample)]) 
                     -> [Either (Plot a Double) (Plot a Double)]
plotAnnotatedSamples colour (name, points) =
    return . Right $ line (solidLine 1) 1 mean
  where
    line style trans proj = toPlot $ plotLine name
        (solidLine 1 $ dissolve trans colour)
        (map (second proj) points)

-- | Plot the annotated samples as a boxplot. This should be used to check the
-- soundness of the measured results.
--
boxplotAnnotatedSamples :: AlphaColour Double 
                     -> (String, [(a,Sample)]) 
                     -> [Either (Plot a Double) (Plot a Double)]
boxplotAnnotatedSamples colour (name, points) =
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

