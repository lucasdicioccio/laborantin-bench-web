
module Experiment.Bench.CDF (
    plotCDF
) where

import Data.List (sort)
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens

import Data.Maybe (catMaybes)
import qualified Data.Map as M

import Laborantin.Types
import Laborantin.Implementation (EnvIO)
import Experiment.Bench.Missing

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

plotCDF :: FilePath      -- ^ result name
  -> BoundResult a -- ^ result to load across from all ancestor experiments
  -> (a -> Double) -- ^ projection function
  -> String        -- ^ title
  -> String        -- ^ x-label (should be "<name of a><unit of a>")
  -> String        -- ^ y-label (something like "fraction of experiments")
  -> Step EnvIO ()
plotCDF outName consumable f title xlabel ylabel = do
    svgPath <- resultPath outName
    rawXs <- consume consumable
    let xs = fmap f $ catMaybes $ map snd rawXs
    let n = length xs
    let ys = map (\y -> (fromIntegral y / fromIntegral n)) [1..]
    chart <- cdfChart (zip (sort xs) ys)
    void . liftIO $ renderableToFile opts chart svgPath
    where opts = FileOptions (800,600) SVG M.empty
          cdfChart :: [(Double, Double)] -> Step EnvIO (Renderable ())
          cdfChart xys = do
                  return $ toRenderable layout
                  where
                    cdfLine = plot_lines_values .~ [xys]
                              $ plot_lines_style . line_color .~ opaque blue
                              $ def
                    layout = layout_title .~ title
                           $ layout_plots .~ [toPlot cdfLine]
                           $ layout_x_axis . laxis_title .~  xlabel
                           $ layout_y_axis . laxis_title .~  ylabel
                           $ def
