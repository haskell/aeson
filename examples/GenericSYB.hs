-- This example is basically the same as in Simplest.hs, only it uses
-- SYB generics instead of explicit instances of ToJSON and FromJSON.

-- This mechanism is much slower than the newer generics mechanism
-- demonstrated in Generic.hs, but it works on versions of GHC older
-- than 7.2.

-- We enable the DeriveDataTypeable language extension so that GHC can
-- automatically derive the Typeable and Data classes for us.

{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Data (Typeable, Data)
import Data.Aeson.Generic (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Typeable and Data classes.

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Typeable, Data)

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord 123.4 20
  BL.putStrLn (encode reply)
