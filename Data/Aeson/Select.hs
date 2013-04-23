{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- (C) 2013 Bas van Dijk <v.dijk.bas@gmail.dom>
module Data.Aeson.Select
    ( -- * Selectors
      (:->)
    , runSelect
    , select

     -- * Converting selectors
    , from, to

     -- * Value selectors
    , obj, array, text, number, bool

     -- * Object selectors
    , field
    , path

      -- * Number selectors
    , integer, double
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Aeson.Types
import Data.Attoparsec.Char8 (Number(..))
import Data.Text (Text)
import Prelude hiding ((.))


--------------------------------------------------------------------------------
-- Select
--------------------------------------------------------------------------------

-- | Select @b@ from @a@.
newtype a :-> b = Select {unSelect :: Kleisli Parser a b}
    deriving ( Category
             , Arrow
             , ArrowZero
             , ArrowPlus
             , ArrowChoice
             , ArrowApply
             )

runSelect :: (a :-> b) -> (a -> Parser b)
runSelect = runKleisli . unSelect
{-# INLINE runSelect #-}

select :: (a -> Parser b) -> (a :-> b)
select = Select . Kleisli
{-# INLINE select #-}


--------------------------------------------------------------------------------
-- Converting selectors
--------------------------------------------------------------------------------

-- | Parse the selected 'Value'.
from :: FromJSON a => Value :-> a
from = select parseJSON

-- | Convert the selected value into a JSON 'Value'.
to :: ToJSON a => a :-> Value
to = select $ pure . toJSON


--------------------------------------------------------------------------------
-- Value selectors
--------------------------------------------------------------------------------

-- | Select an 'Object' from a 'Value'.
obj :: Value :-> Object
obj = select $ withObject "Object" pure
{-# INLINE obj #-}

-- | Select an 'Array' from a 'Value'.
array :: Value :-> Array
array = select $ withArray "Array" pure
{-# INLINE array #-}

-- | Select a 'Text' from a 'Value'.
text :: Value :-> Text
text = select $ withText "Text" pure
{-# INLINE text #-}

-- | Select a 'Number' from a 'Value'.
number :: Value :-> Number
number = select $ withNumber "Number" pure
{-# INLINE number #-}

-- | Select a 'Bool' from a 'Value'.
bool :: Value :-> Bool
bool = select $ withBool "Bool" pure
{-# INLINE bool #-}


--------------------------------------------------------------------------------
-- Object selectors
--------------------------------------------------------------------------------

-- | Select the named field from the 'Object'.
field :: Text -> Object :-> Value
field label = select (.: label)
{-# INLINE field #-}

-- | Select a path from nested @'Object's@.
path :: [Text] -> Object :-> Value
path [] = select $ \_ -> fail "attempted to traverse an Object without a path"
path [key]      = field key
path (key:keys) = field key >>> obj >>> path keys
{-# INLINE path #-}


--------------------------------------------------------------------------------
-- Number selectors
--------------------------------------------------------------------------------

-- | Select an 'Integer' from a 'Number'.
integer :: Number :-> Integer
integer = select $ \num -> case num of
                             I i -> pure i
                             D _ -> mismatch "Integer" "Double"
{-# INLINE integer #-}

-- | Select a 'Double' from a 'Number'.
double :: Number :-> Double
double = select $ \num -> case num of
                             D d -> pure d
                             I _ -> mismatch "Double" "Integer"
{-# INLINE double #-}


--------------------------------------------------------------------------------
-- Failures
--------------------------------------------------------------------------------

mismatch :: String -> String -> Parser a
mismatch expected name = fail $ "when expecting a " ++ expected ++
                                ", encountered " ++ name ++ " instead"
