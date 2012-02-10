{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as BL

data Color = Red
           | White
           | Black
    deriving (Show)

$(deriveJSON id ''Color)

data Mix = Nullary
         | Unary Int
         | Normal String String
    deriving (Show)

$(deriveJSON id ''Mix)

main :: IO ()
main = do
  print ( decode "[\"Red\", \"Black\", \"White\"]" :: Maybe [Color] )
  BL.putStrLn (encode [Red, Black, White])

  print ( decode "[\"Nullary\"]" :: Maybe [Mix] )
  print ( decode "{\"Unary\": 1}" :: Maybe Mix )
  print ( decode "{\"Normal\": [\"hello\", \"world\"]}" :: Maybe Mix )
  BL.putStrLn (encode [Nullary, Unary 1, Normal "hello" "world"])
