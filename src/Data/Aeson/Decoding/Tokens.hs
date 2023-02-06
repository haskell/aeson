{-# LANGUAGE DeriveTraversable #-}
-- | Token definitions.
module Data.Aeson.Decoding.Tokens (
    -- * Types
    Tokens (..),
    Lit (..),
    Number (..),
    TkArray (..),
    TkRecord (..),
) where

import           Data.Aeson.Key            (Key)
import           Data.Bifoldable.Compat    (Bifoldable (..))
import           Data.Bifunctor.Compat     (Bifunctor (..))
import           Data.Bitraversable.Compat (Bitraversable (..), bifoldMapDefault, bimapDefault)
import           Data.Scientific           (Scientific)
import           Data.Text                 (Text)

-- | A well-formed JSON token stream.
--
-- /Note/: 'Lit' exists to make 'Tokens' have only 6 constructors.
-- This may or may not have impact on performance.
--
-- @since 2.1.2.0
--
data Tokens k e
    = TkLit !Lit k
    | TkText !Text k
    | TkNumber !Number k
    | TkArrayOpen (TkArray k e)
    | TkRecordOpen (TkRecord k e)
    | TkErr e
  deriving (Eq, Show)

-- | Literals. @null@, @true@, @false@.
data Lit = LitNull | LitTrue | LitFalse
  deriving (Eq, Show)

-- | Numbers
--
-- We preserve whether the number was integral, decimal or in scientific form.
data Number
    = NumInteger !Integer  -- ^ e.g. @123@
    | NumDecimal !Scientific  -- ^ e.g. @123.456@
    | NumScientific !Scientific -- ^ e.g. @123e456@, @123e-456@ or @123.456E-967@
  deriving (Eq, Show)

-- | Array tokens.
data TkArray k e
    = TkItem (Tokens (TkArray k e) e)
    | TkArrayEnd k
    | TkArrayErr e
  deriving (Eq, Show)

-- | Record tokens.
data TkRecord k e
    = TkPair !Key (Tokens (TkRecord k e) e)
    | TkRecordEnd k
    | TkRecordErr e
  deriving (Eq, Show)

instance Functor (Tokens k) where fmap = second
instance Functor (TkArray k) where fmap = second
instance Functor (TkRecord k) where fmap = second

instance Foldable (Tokens k) where foldMap = bifoldMap (const mempty)
instance Foldable (TkArray k) where foldMap = bifoldMap (const mempty)
instance Foldable (TkRecord k) where foldMap = bifoldMap (const mempty)

instance Traversable (Tokens k) where traverse = bitraverse pure
instance Traversable (TkArray k) where traverse = bitraverse pure
instance Traversable (TkRecord k) where traverse = bitraverse pure

instance Bifunctor Tokens where bimap = bimapDefault
instance Bifunctor TkArray where bimap = bimapDefault
instance Bifunctor TkRecord where bimap = bimapDefault

instance Bifoldable Tokens where bifoldMap = bifoldMapDefault
instance Bifoldable TkArray where bifoldMap = bifoldMapDefault
instance Bifoldable TkRecord where bifoldMap = bifoldMapDefault

instance Bitraversable Tokens where
    bitraverse f _ (TkLit l k)       = TkLit l <$> f k
    bitraverse f _ (TkText t k)      = TkText t <$> f k
    bitraverse f _ (TkNumber n k)    = TkNumber n <$> f k
    bitraverse f g (TkArrayOpen ts)  = TkArrayOpen <$> bitraverse f g ts
    bitraverse f g (TkRecordOpen ts) = TkRecordOpen <$> bitraverse f g ts
    bitraverse _ g (TkErr e)         = TkErr <$> g e

instance Bitraversable TkArray where
    bitraverse f g (TkItem ts)    = TkItem <$> bitraverse (bitraverse f g) g ts
    bitraverse f _ (TkArrayEnd k) = TkArrayEnd <$> f k
    bitraverse _ g (TkArrayErr e) = TkArrayErr <$> g e

instance Bitraversable TkRecord where
    bitraverse f g (TkPair k ts)   = TkPair k <$> bitraverse (bitraverse f g) g ts
    bitraverse f _ (TkRecordEnd k) = TkRecordEnd <$> f k
    bitraverse _ g (TkRecordErr e) = TkRecordErr <$> g e
