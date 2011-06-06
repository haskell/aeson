{-# LANGUAGE OverloadedStrings #-} 

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

main ::IO ()
main = do 
  print $ parseFromString "{\"x\":3.0,\"y\":-1.0}"
  let reply = Coord 123.4 20
  putStrLn $ BSL.unpack (encode reply)

data Coord = Coord { x :: Double, y :: Double } deriving (Show)

instance ToJSON Coord where
  toJSON (Coord xV yV) = object ["x" .= xV, "y" .= yV]

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                          v .: "x" <*>
                          v .: "y"
  parseJSON _          = mzero

parseFromString :: String -> Maybe Coord
parseFromString s = 
  let bs = BS.pack s
  in case parse json bs of
       Done _rest res -> T.parseMaybe parseJSON res
       _              -> Nothing
