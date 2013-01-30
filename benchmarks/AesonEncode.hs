{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B
import Control.DeepSeq

#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal as L

instance NFData L.ByteString where
    rnf = go
      where go (L.Chunk _ cs) = go cs
            go L.Empty        = ()
    {-# INLINE rnf #-}
#endif

main :: IO ()
main = do
  (cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    let refill = B.hGet h 16384
    result0 <- parseWith refill json =<< refill
    r0 <- case result0 of
            Done _ r -> return r
            _        -> fail $ "failed to read " ++ show arg
    start <- getCurrentTime
    let loop !n r
            | n >= count = return ()
            | otherwise = {-# SCC "loop" #-} do
          rnf (encode r) `seq` loop (n+1) r
    loop 0 r0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show delta
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
