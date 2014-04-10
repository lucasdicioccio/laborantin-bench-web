

module Main where

import Laborantin.CLI (defaultMain)
import Experiment.Bench.Web (benchWeb)

main :: IO ()
main = defaultMain [benchWeb]
