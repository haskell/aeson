{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import "aeson-benchmarks" Data.Aeson
import Control.Exception
import Control.Monad
import Data.Binary.Parser (parse, Parser, Decoder(..))
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B

main :: IO ()
main = do
  (bs:cnt:args) <- getArgs
  let count = read cnt :: Int
      blkSize = read bs :: Int
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          hSeek h AbsoluteSeek 0
          let refill = B.hGet h blkSize
          result <- parseWith refill json =<< refill
          case result of
            Done _ _ _ -> loop (good+1) bad
            _          -> loop good (bad+1)
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith :: Monad m =>
             (m B.ByteString)
          -- ^ An action that will be executed to provide the parser
          -- with more input, if necessary.  The action must return an
          -- 'B.empty' string when there is no more input available.
          -> Parser a
          -> B.ByteString
          -- ^ Initial input for the parser.
          -> m (Decoder a)
parseWith refill p s = step $ parse p s
  where step (Partial k) = do
            bs <- refill
            if B.null bs then step (k Nothing)
                         else step (k (Just bs))
        step r           = return r
{-# INLINE parseWith #-}
