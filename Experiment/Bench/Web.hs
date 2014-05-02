{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- Please read http://gwan.com/en_apachebench_httperf.html -}

module Experiment.Bench.Web (
    benchWeb
  , plotWeb
) where

import Data.Csv (ToNamedRecord (..), toField, namedRecord, namedField, (.=), encodeByName)
import Data.Monoid (mconcat, (<>))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation
import Control.Monad (void, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, IOException (..))
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Vector as V

import System.Directory (copyFile)
import System.FilePath.Posix ((</>))
import System.Process (createProcess, proc, terminateProcess, waitForProcess, CreateProcess (..), StdStream (..) )
import System.IO (hClose, openFile, IOMode (..))
import Control.Concurrent (threadDelay)

type URL = Text
type IterationCount = Int
type ConcurrencyLevel = Int
type ProcessCount = Int
type RAM = Int
type GenerationsNumber = Int

{- Process handling -}

data Termination = Kill | Wait 

shellCommand name cmd args chdir = do
  dir <- if chdir then Just . ePath <$> self else return Nothing
  let env = Nothing
  let cmd' = T.unpack cmd
  let args' = map T.unpack args

  outRes <- result (T.unpack $ name <> ".out")
  errRes <- result (T.unpack $ name <> ".err")

  let outPath = pPath outRes
  let errPath = pPath errRes

  cmdOut <- liftIO $ openFile outPath WriteMode
  cmdErr <- liftIO $ openFile errPath WriteMode

  let action = createProcess (proc cmd' args') { std_out = UseHandle cmdOut
                                               , std_err = UseHandle cmdErr
                                               , cwd = dir
                                               }

  dbg $ "executing " <> name <> " with `" <> cmd <> "` " <> T.pack (show args)
  proc <- liftIO $ ((Right <$> action) `catch` (\e -> return $ Left (e :: IOException)))
  case proc of
            Left _ -> do
                  dbg $ "IOException with " <> name <> ", ensure you have " <> name <> " in your $PATH"
                  err $ "IOException with " ++ T.unpack name
                  return (\_ -> return ())

            Right (Nothing, Nothing, Nothing, pHandle) -> do

                  let terminateMessage Kill = "terminating " <> name
                      terminateMessage Wait = "waiting for " <> name

                  let terminateAction Kill handle = terminateProcess handle >> waitForProcess handle
                      terminateAction Wait handle = waitForProcess handle

                  let terminator term = do
                          dbg $ terminateMessage term
                          liftIO $ void (terminateAction term pHandle)

                  liftIO $ mapM_ hClose [cmdOut, cmdErr] 
                  return terminator

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
    endServer <- shellCommand "server-process" srvCmd srvCmdArgs False

    -- cheat to make sure that the server is ready
    dbg "waiting half a second"
    liftIO $ threadDelay 500000

    dbg "starting client"
    (cliCmd, cliCmdArgs) <- httpClientShellCommand <$> httpClient
    endClient <- shellCommand "client-process" cliCmd cliCmdArgs False

    endClient Wait >> endServer Kill

  analyze $ do
    liftIO . print =<< parseClientResults =<< httpClient

{- analysis -}


instance ToNamedRecord ParameterSet where
  toNamedRecord prms = let pairs = M.toList prms
                           validPairs = filter (isRecord . snd) pairs in
    namedRecord $ map toBSpair validPairs
  -- map toRecordPair . filter _ prms
    where isRecord (NumberParam _) = True
          isRecord (StringParam _) = True
          isRecord _               = False

          toBSpair (k, StringParam v) = namedField (encodeUtf8 k) v
          toBSpair (k, NumberParam v) = let v' = fromRational v :: Double
                                        in namedField (encodeUtf8 k) v'

plotWeb :: ScenarioDescription EnvIO
plotWeb = scenario "plot-web" $ do
  describe "Plots the results for the web server benchmarks"
  require benchWeb "@sc.param 'client-name' == 'weighttp'"
  -- todo: autogenerate this kind of scenario
  -- basically we need a list of params to cherry-pick, the ancestor path, and
  -- a function to turn an ancestor to a (list of) ToNamedRecord instance
  run $ do
    generateAggregateCSV
    runPlots

runPlots = do
  destPath <- liftM (\x -> ePath x </> "plot.R") self
  let srcPath = "./scripts/r/plot-web/plot.R" 
  liftIO $ copyFile srcPath destPath
  shellCommand "rplots" "R" ["-f", "plot.R"] True >>= ($ Wait)

generateAggregateCSV = do
    b <- backend
    ancestors <- eAncestors <$> self
    results <- map transform <$> mapM (extract b) ancestors
    let header = V.fromList ["scenario.path"
              , "server-name"
              , "server-concurrency"
              , "gc-area-size"
              , "gc-generations"
              , "client-name"
              , "client-concurrency"
              , "client-processes"
              , "requests-count"
              , "probed-url"
              , "rps"
              , "n.successes"
              ]

    writeResult "aggregate-csv" (toStrict . decodeUtf8 $ encodeByName header results)
    where extract b exec = do
                out <- pRead =<< (bResult b exec "client-process.out")
                let path = ePath exec
                let prms = eParamSet exec
                let perf = parseWeighttpOutput out
                return (path, prms, perf)
          transform (path, prms, perf) = let pathRecord = namedRecord [
                                                "scenario.path" .= path]
                          in pathRecord <> toNamedRecord prms <> toNamedRecord perf
