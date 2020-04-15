{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CsvParse (readResults) where

import           Prelude.Compat

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Csv                   as Csv
import qualified Data.Vector                as V

import           Types

data BenchResult = BenchResult
    { _benchName  :: BenchName
    , _benchStats :: Stats
    }

instance Csv.FromNamedRecord BenchResult where
    parseNamedRecord m = bench
        <$> m Csv..: "Name"
        <*> m Csv..: "Mean"
        <*> m Csv..: "MeanLB"
        <*> m Csv..: "MeanUB"
        <*> m Csv..: "Stddev"
        <*> m Csv..: "StddevLB"
        <*> m Csv..: "StddevUB"
      where
        bench a b c d e f g = BenchResult a (Stats b c d e f g)

readResults :: FilePath -> IO [(BenchName, Stats)]
readResults fname = do
    mxs <- parseResults <$> BSL.readFile fname
    case mxs of
      Left err -> fail err
      Right xs -> return $ map (\(BenchResult a b) -> (a, b)) $ V.toList xs

parseResults :: BSL.ByteString -> Either String (V.Vector BenchResult)
parseResults = fmap snd . Csv.decodeByName
