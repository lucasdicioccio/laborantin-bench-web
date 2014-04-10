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
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, IOException (..))

import System.Process (createProcess, proc, terminateProcess, waitForProcess, CreateProcess (..), StdStream (..) )
import System.IO (hClose, openFile, IOMode (..))
import Control.Concurrent (threadDelay)

type URL = Text
type IterationCount = Int
type ConcurrencyLevel = Int
type ProcessCount = Int

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
                          liftIO $ do
                            terminateAction term pHandle
                            mapM_ hClose [cmdOut, cmdErr] 

                  return terminator

{- Server-side -}

data Server = Mighty ConcurrencyLevel
  deriving (Show)

httpServerShellCommand :: Server -> (Text, [Text])
httpServerShellCommand (Mighty conc) = ("mighty", ["+RTS"
                                                  , "-N" <> T.pack (show conc)
                                                  , "-RTS"])

httpServerParams = do
  parameter "server-name" $ do 
    describe "Name of the server to use."
    values [str "mighty"]

  parameter "server-concurrency" $ do 
    describe "Number of concurrent server processes."
    values [num 1]

httpServer = do
  (StringParam name) <- param "server-name"
  (NumberParam conc) <- param "server-concurrency"
  case name of
    "mighty"  -> return $ Mighty (round conc)
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
