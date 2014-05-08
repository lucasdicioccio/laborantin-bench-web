{-# LANGUAGE OverloadedStrings #-}

module Experiment.Bench.Missing where

import System.FilePath.Posix ((</>))
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation (EnvIO)
import Data.Text (Text)
import qualified Data.Text as T

resultPath :: FilePath -> Step EnvIO FilePath
resultPath basename = (\x -> ePath x </> basename) <$> self

data BoundResult a = BoundResult FilePath (a -> Text) (Text -> Maybe a)

produce :: BoundResult a -> a -> Step EnvIO ()
produce (BoundResult name encode _) x = do
    dbg $ "writing result " <> T.pack name
    writeResult name . encode $ x

consume :: BoundResult a -> Step EnvIO [(Execution EnvIO, Maybe a)]
consume (BoundResult name _ decode) = do
    ancestors <- eAncestors <$> self
    mapM go ancestors
    where go exec = do
            dbg $ "reading " <> T.pack name <> " from " <> T.pack (ePath exec)
            b <- backend
            dat <- pRead =<< (bResult b exec name)
            return (exec, decode dat)
