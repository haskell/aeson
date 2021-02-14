{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude.Compat

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Parser
import Data.Attoparsec
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B

main = do
  (cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> withFile arg ReadMode $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !n
            | n >= count = return ()
            | otherwise = do
          let go = do
                s <- B.hGet h 16384
                if B.null s
                  then loop (n+1)
                  else go
          go
    loop 0
    end <- getCurrentTime
    putStrLn $ "  " ++ show (diffUTCTime end start)
