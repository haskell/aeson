-- This benchmark is used to measure heap usage
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Stream
import System.Environment (getArgs)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy as BL

newtype Count = Count { getCount :: Int }
  deriving Show

instance FromJSON Count where
    parseJSON = withObject "Laureates" $ \obj -> do
        v <- obj .: "laureates"
        pure $ Count $ length (v :: Vector Value)

countParseJSON :: BL.ByteString -> Either String Count
countParseJSON = eitherDecode

-- | A bit manual ATM
countTokenStream :: BL.ByteString -> Either String Count
countTokenStream s = case tokenStream s of
    TkObjectOpen : TkKey "laureates" :  TkArrayOpen : rest -> go 0 rest
    _ -> Left "invalid input"
  where
    go !acc [TkArrayClose, TkObjectClose] = Right (Count acc)
    go !acc ts = do
        ts' <- skip ts
        go (acc + 1) ts'

-- We count brackets, but cheat a bit
skip :: TokenStream -> Either String TokenStream
skip = go (0 :: Int)
  where
    go _ [] = Left "Unexpected end-of-input"
    go !acc (t : ts) = case t of
        TkNull        -> done acc ts
        TkTrue        -> done acc ts
        TkFalse       -> done acc ts
        TkText _      -> done acc ts
        TkNumber _    -> done acc ts
        TkKey _       -> go acc ts
        TkObjectOpen  -> go (acc + 1) ts
        TkArrayOpen   -> go (acc + 1) ts
        TkObjectClose -> done (acc - 1) ts
        TkArrayClose  -> done (acc - 1) ts
        TkError err   -> Left err

    done !n ts | n <= 0    = Right ts
               | otherwise = go n ts

main :: IO ()
main = do
    args <- getArgs
    contents <- BL.readFile "json-data/laureate.json"

    counter <- case args of
        ["parseJSON"] -> pure countParseJSON
        ["tokenStream"] ->  pure countTokenStream
        _ -> fail "specify variant: parseJSON or tokenStream"

    c <- either fail (pure . getCount) $ counter contents
    print c
