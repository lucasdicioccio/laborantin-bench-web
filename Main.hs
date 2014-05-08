

module Main where

import Laborantin.CLI (defaultMain)
import Experiment.Bench.Web (benchWeb, plotWithR, plotWithChart)

main :: IO ()
main = defaultMain [benchWeb, plotWithR, plotWithChart]
