{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- Please read http://gwan.com/en_apachebench_httperf.html -}

module Experiment.Bench.Web (
    benchWeb
  , plotWithR
  , plotWithChart
) where


import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation (EnvIO)
import Experiment.Bench.Process
import Experiment.Bench.CSV
import Experiment.Bench.CDF
import Experiment.Bench.Missing

import Data.Csv (ToNamedRecord (..), namedRecord, (.=))
import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (liftIO)
import System.Directory (copyFile)
import Control.Concurrent (threadDelay)


type URL = Text
type IterationCount = Int
type ConcurrencyLevel = Int
type ProcessCount = Int
type RAM = Int
type GenerationsNumber = Int

{- Server-side -}

data GC = GC RAM GenerationsNumber
  deriving (Show)

data Server = Mighty ConcurrencyLevel GC
  deriving (Show)

httpServerShellCommand :: Server -> (Text, [Text])
httpServerShellCommand (Mighty conc (GC ram gens)) = ("mighty", ["+RTS"
                                                  , "-A" <> T.pack (show ram) <> "M"
                                                  , "-G" <> T.pack (show gens)
                                                  , "-N" <> T.pack (show conc)
                                                  , "-RTS"])

httpServerParams = do

  ghcRTSParams

  parameter "server-name" $ do 
    describe "Name of the server to use."
    values [str "mighty"]

  parameter "server-concurrency" $ do 
    describe "Number of concurrent server processes."
    values [num 1]

ghcRTSParams = do

  parameter "gc-area-size" $ do 
    describe "Initial RAM passed to the RTS (in MB)"
    values [num 4]

  parameter "gc-generations" $ do 
    describe "Number of garbage-collector generations"
    values [num 2]

httpServer = do
  (StringParam name) <- param "server-name"
  (NumberParam ram)  <- param "gc-area-size"
  (NumberParam gens)  <- param "gc-generations"
  (NumberParam conc) <- param "server-concurrency"
  case name of
    "mighty"  -> return $ Mighty (round conc) (GC (round gens) (round ram))
    _         -> error "unknwon server name"

{- Client-side -}

data Client = Weighttp URL IterationCount ConcurrencyLevel ProcessCount
  deriving (Show)

httpClientShellCommand :: Client -> (Text, [Text])
httpClientShellCommand (Weighttp url cnt conc procs) = ("weighttp", args ++ [url])
  where args = map T.pack ["-n", show cnt, "-c", show conc, "-t", show procs]

httpClientParams = do
  parameter "client-name" $ do 
    describe "Name of the client tool to use."
    values [str "weighttp"]

  parameter "client-concurrency" $ do
    describe "Number of concurrent request to run."
    values [num 1]

  parameter "client-processes" $ do
    describe "Number of concurrent processes to run."
    values [num 1]

  parameter "requests-count" $ do
    describe "Number of requests to execute."
    values [num 1]

  parameter "probed-url" $ do
    describe "URL to probe"
    values [str "http://localhost:8080/index.html"]

httpClient = do
  (StringParam name)  <- param "client-name"
  (StringParam url)   <- param "probed-url"
  (NumberParam conc)  <- param "client-concurrency"
  (NumberParam procs) <- param "client-processes"
  (NumberParam reqs)  <- param "requests-count"
  case name of
    "weighttp"  -> return $ Weighttp url (round reqs) (round conc) (round procs)
    _           -> error "unknwon server name"

{- analysis -}

data HttpPerformance = HttpPerformance {
    requestsPerSeconds  :: Int
  , nSuccessfulRequests :: Int
  } deriving (Show)

instance ToNamedRecord (Maybe HttpPerformance) where
  toNamedRecord (Just (HttpPerformance rps n)) = namedRecord [
                                        "rps" .= rps, "n.successes" .= n]
  toNamedRecord Nothing = namedRecord []

weightHttpOutput :: BoundResult HttpPerformance
weightHttpOutput = BoundResult "client-process.out"
                               (error "no encoder")
                               (parseWeighttpOutput)

parseClientResults :: Client -> Step EnvIO (Maybe HttpPerformance)
parseClientResults (Weighttp _ _ _ _) = do
  content <- pRead =<< result "client-process.out"
  return $ parseWeighttpOutput content

parseWeighttpOutput :: Text -> Maybe HttpPerformance
parseWeighttpOutput content = HttpPerformance <$> rps <*> statuses
              where lines = T.lines content
                    findLine fstWord = filter ((fstWord ==) . (T.take (T.length fstWord))) lines
                    findLine' fstWord = if length (findLine fstWord) == 1
                                        then Just (findLine fstWord !! 0)
                                        else Nothing
                    safeAtIndex n xs = if length xs >= (n+1)
                                       then Just (xs !! n)
                                       else Nothing
                    safeParseInt txt = either (const Nothing) (Just . fst) (decimal txt)
                    rps = findLine' "finished" >>= safeAtIndex 9 . T.words >>= safeParseInt
                    statuses = findLine' "status" >>= safeAtIndex 2 . T.words >>= safeParseInt

{- actual experiment -}

benchWeb :: ScenarioDescription EnvIO
benchWeb = scenario "bench-web" $ do
  describe "Benchmark web-servers static pages."
  httpServerParams
  httpClientParams
  run $ do
    dbg "starting server"
    (srvCmd, srvCmdArgs) <- httpServerShellCommand <$> httpServer
    endServer <- runProcess "server-process" srvCmd srvCmdArgs False

    -- cheat to make sure that the server is ready
    dbg "waiting half a second"
    liftIO $ threadDelay 500000

    dbg "starting client"
    (cliCmd, cliCmdArgs) <- httpClientShellCommand <$> httpClient
    endClient <- runProcess "client-process" cliCmd cliCmdArgs False

    endClient Wait >> endServer Kill

  analyze $ do
    liftIO . print =<< parseClientResults =<< httpClient

{- analysis -}

plotWithR :: ScenarioDescription EnvIO
plotWithR = scenario "plot-with-R" $ do
  describe "Demonstrates how to call hand-written R scripts."
  require benchWeb "@sc.param 'client-name' == 'weighttp'"
  run $ do
    aggregateCSV "performance.csv" "client-process.out" parseWeighttpOutput ["rps" , "n.successes"]
    callR
    where callR = do
                  destPath <- resultPath "plot.R"
                  let srcPath = "./scripts/r/plot-web/plot.R" 
                  liftIO $ copyFile srcPath destPath
                  runProcess "rplots" "R" ["-f", "plot.R"] True >>= ($ Wait)

plotWithChart :: ScenarioDescription EnvIO
plotWithChart = scenario "plot-with-chart" $ do
  describe "Demonstrated the CDF helper with Chart."
  require benchWeb "@sc.param 'client-name' == 'weighttp'"
  run $ do
    plotCDF "performance-cdf.svg"
            weightHttpOutput (fromIntegral . requestsPerSeconds)
            "bench-web results"
            "Serving speed (req/s)" "Fraction of experiments"
