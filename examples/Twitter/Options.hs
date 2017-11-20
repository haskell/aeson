module Twitter.Options (module Twitter.Options) where

import Data.Aeson

twitterOptions :: Options
twitterOptions = defaultOptions
    { fieldLabelModifier = \x -> case x of
        "id_" -> "id"
        _     -> x
    }
