
-- module Possible where
-- This example is basically the same as in Simplest.hs, only it uses
-- GHC's builtin generics instead of explicit instances of ToJSON and
-- FromJSON.

-- This example only works with GHC 7.2 or newer, as it uses the
-- datatype-generic programming machinery introduced in 7.2.

-- We enable the DeriveGeneric language extension so that GHC can
-- automatically derive the Generic class for us.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


import Data.Possible
import Data.Aeson
--  (FromJSON, ToJSON, decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as BL
-- import Data.Map
-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.

data Coord = Coord { x :: Double, y :: Double
                   , z :: Possible Double}
             deriving Show

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.

$(deriveJSON defaultOptions ''Coord)

-- instance FromJSON Coord
-- instance ToJSON Coord

blen :: ToJSON a => a -> IO ()
blen a = do
  BL.putStrLn . encode $ a
  print $ toJSON a

pde = print . (\a -> decode a :: Maybe Coord)
main :: IO ()
main = do
  pde "{\"x\":3.0,\"y\":-1.0,\"z\":1}"
  blen $ Coord 3 (-1) (HaveData 1)

  pde "{\"x\":3.0,\"y\":-1.0,\"z\":null}"
  blen $ Coord 3 (-2) HaveNull

  pde "{\"x\":3.0,\"y\":-1.0}"
  blen $ Coord 3 (-1) MissingData

  blen $ object $  ["1" .= Omitted, "2" .= Null ]

  blen $ object ["a" .= Null , "b" .= Omitted, "c" .= Number 1 ]




