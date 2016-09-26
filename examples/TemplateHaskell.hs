-- We can use Template Haskell (TH) to generate instances of the
-- FromJSON and ToJSON classes automatically.  This is the fastest way
-- to add JSON support for a type.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

-- This splice will derive instances of ToJSON and FromJSON for us.

$(deriveJSON defaultOptions ''Coord)

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord { x = 123.4, y = 20 }
  BL.putStrLn (encode reply)
