module Data.Aeson.Decoding.Internal where

import Data.Aeson.Decoding.Tokens

-- | Helper type-class for constructing errors.
class    AsError t        where tkErr :: e -> t k e
instance AsError Tokens   where tkErr = TkErr
instance AsError TkArray  where tkErr = TkArrayErr
instance AsError TkRecord where tkErr = TkRecordErr

negateNumber :: Number -> Number
negateNumber (NumInteger n)    = NumInteger (negate n)
negateNumber (NumDecimal n)    = NumDecimal (negate n)
negateNumber (NumScientific n) = NumScientific (negate n)
