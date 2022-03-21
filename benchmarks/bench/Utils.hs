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

readStr :: FilePath -> IO String
readStr fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    fmap BS8.unpack $ BS.readFile $ fromMaybe "json-data" dataDir </> fp

readS :: FilePath -> IO BS.ByteString
readS fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    BS.readFile $ fromMaybe "json-data" dataDir </> fp

readL :: FilePath -> IO LBS.ByteString
readL fp = do
    dataDir <- lookupEnv "AESON_BENCH_DATADIR"
    LBS.readFile $ fromMaybe "json-data" dataDir </> fp

readV :: A.FromJSON a => FilePath -> IO a
readV fileName = do
    mv <- A.eitherDecodeStrict' <$> readS fileName
    case mv of
        Right v -> return v
        Left err -> do
            hPutStrLn stderr $ fileName ++ ": JSON decode failed - " ++ err
            exitFailure
