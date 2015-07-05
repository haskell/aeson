{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Coord where
  toJSON (Coord xV yV) = object [ "x" .= xV,
                                  "y" .= yV ]

  toEncoding Coord{..} = pairs $
    "x" .= x <>
    "y" .= y

-- A FromJSON instance allows us to decode a value from JSON.  This
-- should match the format used by the ToJSON instance.

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _          = empty

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord 123.4 20
  BL.putStrLn (encode reply)
