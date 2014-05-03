{-# LANGUAGE OverloadedStrings #-}

module Experiment.Bench.Process (
    runProcess
  , TerminateStep
  , Termination (..)
) where

import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation (EnvIO)
import System.Process (createProcess, proc, terminateProcess, waitForProcess, StdStream (..), CreateProcess (..))

import System.IO (hClose, openFile, IOMode (..))
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, IOException (..))

data Termination = Kill | Wait 

type TerminateStep = Termination -> Step EnvIO ()

runProcess :: Text -> Text -> [Text] -> Bool -> Step EnvIO TerminateStep
runProcess name cmd args chdir = do
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

