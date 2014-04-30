{-# LANGUAGE OverloadedStrings #-}

{- Please read http://gwan.com/en_apachebench_httperf.html -}

module Experiment.Bench.Web (
  benchWeb
) where

import Data.Monoid (mconcat, (<>))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, IOException (..))

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

shellCommand name cmd args = do
  let dir = Nothing
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

parseClientResults :: Client -> Step EnvIO ()
parseClientResults (Weighttp _ _ _ _) = do
  content <- pRead =<< result "client-process.out"
  (liftIO . print) =<< eParamSet <$> self
  liftIO . print $ parse content
  where parse content = (,) <$> rps <*> statuses
              where lines = T.lines content
                    findLine fstWord = filter ((fstWord ==) . (T.take (T.length fstWord))) lines
                    findLine' fstWord = if length (findLine fstWord) == 1
                                        then Just (findLine fstWord !! 0)
                                        else Nothing
                    rps = T.words <$> findLine' "finished"
                    statuses = T.words <$> findLine' "status"

{- actual experiment -}

benchWeb :: ScenarioDescription EnvIO
benchWeb = scenario "bench-web" $ do
  describe "Benchmark web-servers static pages."
  httpServerParams
  httpClientParams
  run $ do
    dbg "starting server"
    (srvCmd, srvCmdArgs) <- httpServerShellCommand <$> httpServer
    endServer <- shellCommand "server-process" srvCmd srvCmdArgs

    dbg "starting client"
    (cliCmd, cliCmdArgs) <- httpClientShellCommand <$> httpClient
    endClient <- shellCommand "client-process" cliCmd cliCmdArgs

    endClient Wait >> endServer Kill
  analyze $ do
    httpClient >>= parseClientResults 
