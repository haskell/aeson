{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude.Compat

import Control.DeepSeq
import Control.Monad (forM_)
import Data.Aeson
import Data.Attoparsec.ByteString (IResult(..), parseWith)
import Data.Char (isDigit)
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B

main :: IO ()
main = do
  args0 <- getArgs
  let (cnt,args) = case args0 of
        (i:c:a) | all isDigit i && all isDigit c -> (c,a)
        (c:a) -> (c,a)
        [] -> error "Unexpected empty list"
  let count = read cnt :: Int
  forM_ args $ \arg -> withFile arg ReadMode $ \h -> do
    putStrLn $ arg ++ ":"
    let refill = B.hGet h 16384
    result0 <- parseWith refill json =<< refill
    r0 <- case result0 of
            Done _ r -> return r
            _        -> fail $ "failed to read " ++ show arg
    start <- getCurrentTime
    let loop !n r
            | n >= count = return ()
            | otherwise = {-# SCC "loop" #-}
          rnf (encode r) `seq` loop (n+1) r
    loop 0 r0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ cnt ++ " good, " ++ show delta
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
