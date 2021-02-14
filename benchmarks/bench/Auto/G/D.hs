{-# LANGUAGE DeriveGeneric #-}

module Auto.G.D where

import Control.DeepSeq
import Data.Aeson
import GHC.Generics (Generic)
import Options

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  }
           deriving (Show, Eq, Generic)

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record a b y)  = a `deepseq` b `deepseq` rnf y

instance ToJSON a => ToJSON (D a) where
    toJSON = genericToJSON opts
    toEncoding = genericToEncoding opts

instance FromJSON a => FromJSON (D a) where
    parseJSON = genericParseJSON opts

type T = D (D (D ()))

d :: T
d = Record
    { testOne = 1234.56789
    , testTwo = True
    , testThree = Product "Hello World!" 'a'
                    Record
                    { testOne   = 9876.54321
                    , testTwo   = False
                    , testThree = Product "Yeehaa!!!" '\n' Nullary
                    }
    }
