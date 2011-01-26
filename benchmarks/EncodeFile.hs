{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode
import Data.Aeson.Parser
import Data.Attoparsec
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

main = do
  (cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !n
            | n >= count = return ()
            | otherwise = {-# SCC "loop" #-} do
          hSeek h AbsoluteSeek 0
          let refill = B.hGet h 16384
          result <- parseWith refill json =<< refill
          case result of
            Done _ r -> L.length (encode r) `seq` loop (n+1)
            _        -> error $ "failed to read " ++ show arg
    loop 0
    end <- getCurrentTime
    putStrLn $ "  " ++ show (diffUTCTime end start)
