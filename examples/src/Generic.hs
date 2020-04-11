-- This example is basically the same as in Simplest.hs, only it uses
-- GHC's builtin generics instead of explicit instances of ToJSON and
-- FromJSON.

-- We enable the DeriveGeneric language extension so that GHC can
-- automatically derive the Generic class for us.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude.Compat

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.
data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Generic)

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.

instance FromJSON Coord
instance ToJSON Coord

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord { x = 123.4, y = 20 }
  BL.putStrLn (encode reply)
