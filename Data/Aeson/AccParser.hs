{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Aeson.AccParser
  (
    AccParser (AccParser, getParser)
  , accSequence
  , accTraverse
  , (<*>+)
  ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Types.Internal (Parser (..), runParser)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty

newtype AccParser a = AccParser { getParser :: Parser a }
    deriving Functor

instance Applicative AccParser where
    pure = AccParser . pure
    f <*> a = AccParser (getParser f <*>+ getParser a)

-- | A variant of 'Control.Applicative.liftA2' that lazily accumulates errors
-- from both subparsers.
liftP2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftP2 f pa pb = Parser $ \path kf ks ->
  runParser pa path
    (\(e :| es) -> kf (e :| es ++ runParser pb path NonEmpty.toList (const [])))
    (\a -> runParser pb path kf (\b -> ks (f a b)))
{-# INLINE liftP2 #-}

accSequence :: Traversable t => t (Parser a) -> Parser (t a)
accSequence = accTraverse id

accTraverse :: Traversable t => (a -> Parser b) -> t a -> Parser (t b)
accTraverse f s = getParser $ traverse' (AccParser . f) s

-- Making sure we are using Applicative AccParser
traverse' :: Traversable t => (a -> AccParser b) -> t a -> AccParser (t b)
traverse' = traverse

infixl 4 <*>+

-- | A variant of ('<*>') that lazily accumulates errors from both subparsers.
(<*>+) :: Parser (a -> b) -> Parser a -> Parser b
(<*>+) = liftP2 id
{-# INLINE (<*>+) #-}
