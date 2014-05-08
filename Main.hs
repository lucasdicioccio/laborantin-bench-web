

module Main where

import Laborantin.CLI (defaultMain)
import Experiment.Bench.Web (benchWeb, plotWeb, plotChart)

main :: IO ()
main = defaultMain [benchWeb, plotWeb, plotChart]
