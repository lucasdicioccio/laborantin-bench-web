

module Main where

import Laborantin.CLI (defaultMain)
import Experiment.Bench.Web (benchWeb, plotWeb)

main :: IO ()
main = defaultMain [benchWeb, plotWeb]
