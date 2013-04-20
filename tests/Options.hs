module Options where

import Data.Aeson.Types
import Data.Char

optsDefault :: Options
optsDefault = defaultOptions
              { fieldNameModifier       = map toLower
              , constructorNameModifier = map toLower
              }

opts2ElemArray :: Options
opts2ElemArray = optsDefault
                 { nullaryToString = False
                 , sumEncoding     = TwoElemArray
                 }

optsObjectWithType :: Options
optsObjectWithType = optsDefault
                     { nullaryToString = False }

optsObjectWithSingleField :: Options
optsObjectWithSingleField = optsDefault
                            { nullaryToString = False
                            , sumEncoding     = ObjectWithSingleField
                            }
