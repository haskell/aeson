{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Main where

--------------------------------------------------------------------------------

import Criterion.Main

import Control.DeepSeq (NFData, rnf, deepseq)

import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Aeson.Types
import Data.Aeson.TH (mkToJSON, mkParseJSON)
import qualified Data.Aeson.Generic as G (fromJSON, toJSON) 

--------------------------------------------------------------------------------

-- Taken from the documentation of Data.Aeson.TH:
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Eq, Generic, Data, Typeable)

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record d b y)  = d `deepseq` b `deepseq` rnf y

--------------------------------------------------------------------------------

type T = D (D (D ()))

d :: T
d = Record 
    { testOne = 1234.56789
    , testTwo = True
    , testThree = Product "Hello World!" 'a' $
                    Record 
                    { testOne   = 9876.54321
                    , testTwo   = False
                    , testThree = Product "Yeehaa!!!" '\n' Nullary
                    }
    }

--------------------------------------------------------------------------------

instance ToJSON   a => ToJSON   (D a)
instance FromJSON a => FromJSON (D a)

--------------------------------------------------------------------------------

thToJSON :: ToJSON a => D a -> Value
thToJSON = $(mkToJSON id ''D)

thParseJSON :: FromJSON a => Value -> Parser (D a) 
thParseJSON = $(mkParseJSON id ''D)

thFromJSON :: FromJSON a => Value -> Result (D a)
thFromJSON = parse thParseJSON

--------------------------------------------------------------------------------

type FJ = Value -> Result T

main :: IO ()
main = 
    let v = thToJSON d
    in d `deepseq` v `deepseq` defaultMain 
         [ group "toJSON"   (nf thToJSON d)
                            (nf G.toJSON d)
                            (nf toJSON   d)
         , group "fromJSON" (nf (thFromJSON :: FJ) v)
                            (nf (G.fromJSON :: FJ) v)
                            (nf (fromJSON   :: FJ) v)
         ]

group n th syb gen = bgroup n [ bench "th"      th
                              , bench "syb"     syb
                              , bench "generic" gen
                              ]
