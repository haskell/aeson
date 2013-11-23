{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.DeepSeq
import Control.Monad
import Text.JSON
import Data.Time.Clock
import System.Environment (getArgs)

instance NFData JSValue where
    rnf JSNull = ()
    rnf (JSBool b) = rnf b
    rnf (JSRational b r) = rnf b `seq` rnf r `seq` ()
    rnf (JSString s) = rnf (fromJSString s)
    rnf (JSArray vs) = rnf vs
    rnf (JSObject kvs) = rnf (fromJSObject kvs)

main :: IO ()
main = do
  (cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          s <- readFile arg
          case decodeStrict s of
            Ok (_::JSValue) -> loop (good+1) 0
            _ -> loop 0 (bad+1)
    (good, _) <- loop 0 0
    end <- getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show (diffUTCTime end start)
