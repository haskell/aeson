{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Control.Monad
import Data.Attoparsec.ByteString (IResult(..), parseWith)
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B

main :: IO ()
main = do
  (bs:cnt:args) <- getArgs
  let count = read cnt :: Int
      blkSize = read bs
  forM_ args $ \arg -> withFile arg ReadMode $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          hSeek h AbsoluteSeek 0
          let refill = B.hGet h blkSize
          result <- parseWith refill json =<< refill
          case result of
            Done _ _ -> loop (good+1) bad
            _        -> loop good (bad+1)
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
