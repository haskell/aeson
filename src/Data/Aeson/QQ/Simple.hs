-- | Like @<https://hackage.haskell.org/package/aeson-qq/docs/Data-Aeson-QQ.html Data.Aeson.QQ>@ but without interpolation.
module Data.Aeson.QQ.Simple (aesonQQ) where

import           Data.Aeson
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))

-- | Converts a string representation of a JSON value into 'Data.Aeson.Value' at compile-time.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
--
-- import Data.Aeson (Value)
-- import Data.Aeson.QQ.Simple
--
-- joe :: 'Value'
-- joe = [aesonQQ|{ "name": \"Joe\", "age": 12 }|]
-- @
aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter
    { quoteExp  = aesonExp
    , quotePat  = const $ error "No quotePat defined for jsonQQ"
    , quoteType = const $ error "No quoteType defined for jsonQQ"
    , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
    }

aesonExp :: String -> ExpQ
aesonExp txt =
  case eitherDecodeStrict $ TE.encodeUtf8 $ T.pack txt of
    Left err  -> error $ "Error in aesonExp: " ++ show err
    Right val -> lift (val :: Value)
