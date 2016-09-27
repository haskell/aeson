{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Binary.Parser (parse, Parser, Decoder(..))
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
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    let refill = B.hGet h 16384
    result0 <- parseWith refill json =<< refill
    r0 <- case result0 of
            Done _ _ r -> return r
            _        -> fail $ "failed to read " ++ show arg
    start <- getCurrentTime
    let loop !n r
            | n >= count = return ()
            | otherwise = {-# SCC "loop" #-} do
          rnf (encode r) `seq` loop (n+1) r
    loop 0 r0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ cnt ++ " good, " ++ show delta
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
