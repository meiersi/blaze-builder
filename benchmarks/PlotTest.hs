-----------------------------------------------------------------------------
-- |
-- Module      : PlotTest
-- Copyright   : Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Test plotting for the benchmarks.
-- package.
--
-----------------------------------------------------------------------------

module PlotTest (main) where


import Data.Maybe
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Gtk


-- The different serialization functions
----------------------------------------
main = undefined

testData :: [(Int,Double)]
testData = zip xs (map (fromIntegral . (^2)) xs)
  where xs = [1,2,4,8,16,32]


blazeLineStyle = solidLine 1 . opaque
binaryLineStyle = dashedLine 1 [5, 5] . opaque

linePlot :: String -> Colour Double -> [(Int,Double)] -> PlotLines Int Double
linePlot name colour points = 
          plot_lines_title ^= name $
          plot_lines_style ^= (solidLine 1 $ opaque colour) $
          plot_lines_values ^= [points] $ 
          defaultPlotLines

plots :: [PlotLines Int Double]
plots = [ linePlot [c] colour testData 
        | (c, colour) <- zip ['a'..] [blue,red,green,yellow,gray,black] ]

mkLinearAxis name = laxis_title ^= name $ defaultLayoutAxis
mkLogAxis name = 
  laxis_title ^= name $ 
  laxis_generate ^= autoScaledLogAxis defaultLogAxis $
  defaultLayoutAxis


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
