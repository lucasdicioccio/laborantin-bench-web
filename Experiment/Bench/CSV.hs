{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Experiment.Bench.CSV (
  aggregateCSV
) where

import Laborantin.DSL
import Laborantin.Types
import Laborantin.Implementation (EnvIO)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Text (Text)
import Data.Csv (ToNamedRecord (..), toField, namedRecord, namedField, (.=), encodeByName)

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

paramNames = S.fromList . concatMap (\e -> M.keys $ eParamSet e)

headerNames extras = 
    fmap encodeUtf8 . V.fromList . S.toList . S.union (S.fromList extras) . paramNames

aggregateCSV :: (ToNamedRecord a)
    => String
    -> String
    -> (Text -> a)
    -> [Text]
    -> Step EnvIO ()
aggregateCSV res srcRes parser keys = do
    b <- backend
    ancestors <- eAncestors <$> self
    let header = headerNames ("scenario.path":keys) ancestors
    dump header =<< map transform <$> mapM (extract b) ancestors
    where extract b exec = do
            out <- pRead =<< (bResult b exec srcRes)
            let path = ePath exec
            let prms = eParamSet exec
            let parsed = parser out
            return (path, prms, parsed)
          transform (path, prms, parsed) =
            let pathRecord = namedRecord ["scenario.path" .= path]
            in pathRecord <> toNamedRecord prms <> toNamedRecord parsed
          dump header = writeResult res . toStrict . decodeUtf8 . encodeByName header 
