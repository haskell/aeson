module Options where

import Prelude ()
import Prelude.Compat

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

optsUnwrapUnaryRecords :: Options
optsUnwrapUnaryRecords = optsDefault
                         { unwrapUnaryRecords = True
                         }

optsTaggedObject :: Options
optsTaggedObject = optsDefault
                   { allNullaryToStringTag = False }

optsObjectWithSingleField :: Options
optsObjectWithSingleField = optsDefault
                            { allNullaryToStringTag = False
                            , sumEncoding           = ObjectWithSingleField
                            }

optsOmitNothingFields :: Options
optsOmitNothingFields = optsDefault
                        { omitNothingFields = True
                        }

optsUntaggedValue :: Options
optsUntaggedValue = optsDefault
    { sumEncoding = UntaggedValue
    }
