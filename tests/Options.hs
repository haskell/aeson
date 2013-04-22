module Options where

import Data.Aeson.Types
import Data.Char

optsDefault :: Options
optsDefault = defaultOptions
              { fieldLabelModifier     = map toLower
              , constructorTagModifier = map toLower
              }

opts2ElemArray :: Options
opts2ElemArray = optsDefault
                 { allNullaryToStringTag = False
                 , sumEncoding     = TwoElemArray
                 }

optsTaggedObject :: Options
optsTaggedObject = optsDefault
                   { allNullaryToStringTag = False }

optsObjectWithSingleField :: Options
optsObjectWithSingleField = optsDefault
                            { allNullaryToStringTag = False
                            , sumEncoding           = ObjectWithSingleField
                            }
