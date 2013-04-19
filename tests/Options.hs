module Options where

import Data.Aeson.Types
import Data.Char

optsDefault :: Options
optsDefault = defaultOptions{ fieldNameModifier       = map toLower
                            , constructorNameModifier = map toLower
                            , sumEncoding             = TwoElemArray
                            }

opts2ElemArray :: Options
opts2ElemArray = optsDefault{ nullaryToString = False }

optsObjectWithType :: Options
optsObjectWithType = optsDefault
                     { nullaryToString = False
                     , sumEncoding     = defaultObjectWithType
                     }

optsObjectWithSingleField :: Options
optsObjectWithSingleField = optsDefault
                            { nullaryToString = False
                            , sumEncoding     = ObjectWithSingleField
                            }
