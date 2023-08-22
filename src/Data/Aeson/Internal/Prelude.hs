{-# LANGUAGE CPP #-}

-- Prelude additions for @aeson@, reducing the amount of imports per module.
module Data.Aeson.Internal.Prelude (
    module X,
) where

import Control.Applicative as X (Alternative (..), Const(..), liftA2)
import Control.Monad as X (void, when, unless, liftM2)
import Control.Monad.Fix as X (MonadFix (..))
import Data.Coerce as X (Coercible, coerce)
import Data.Data as X (Data)
import Data.Foldable as X (foldl')
import Data.Function as X (fix)
import Data.Functor as X (($>))
import Data.Int as X (Int8, Int16, Int32, Int64)
import Data.Maybe as X (fromMaybe, catMaybes, mapMaybe)
import Data.Proxy as X (Proxy(..))
import Data.Scientific as X (Scientific)
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (IsString(..))
import Data.Text as X (Text)
import Data.Time as X (UTCTime)
import Data.Typeable as X (Typeable)
import Data.Vector as X (Vector)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word8, Word16, Word32, Word64)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)

#if !MIN_VERSION_base(4,17,0)
import GHC.Generics.Generically as X (Generically (..), Generically1 (..))
#endif
