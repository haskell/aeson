{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Auto.T.D where

import Control.DeepSeq
import Data.Aeson.TH
import Options

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  }
           deriving (Show, Eq)

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record a b y)  = a `deepseq` b `deepseq` rnf y

deriveJSON opts ''D

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
