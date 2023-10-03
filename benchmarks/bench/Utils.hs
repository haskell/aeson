{-# LANGUAGE NoImplicitPrelude #-}
module Utils where

import           Prelude.Compat

import           Data.Maybe            (fromMaybe)
import           System.Environment    (lookupEnv)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>))
import           System.IO             (hPutStrLn, stderr)

import qualified Data.Aeson            as A
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

readStr :: FilePath -> IO String
readStr fp = do
    fmap BS8.unpack $ readS fp

readS :: FilePath -> IO BS.ByteString
readS fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    BS.readFile $ fromMaybe "benchmarks/json-data" dataDir </> fp

readL :: FilePath -> IO LBS.ByteString
readL fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    LBS.readFile $ fromMaybe "benchmarks/json-data" dataDir </> fp

readT :: FilePath -> IO T.Text
readT fp = fmap TE.decodeUtf8 $ readS fp

readV :: A.FromJSON a => FilePath -> IO a
readV fileName = do
    mv <- A.eitherDecodeStrict' <$> readS fileName
    case mv of
        Right v -> return v
        Left err -> do
            hPutStrLn stderr $ fileName ++ ": JSON decode failed - " ++ err
            exitFailure
