{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Control.DeepSeq
import Criterion.Main
import Data.Aeson
-- import Data.Aeson.Types (Parser)
import Data.Hashable
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

value :: Int -> HM.HashMap T.Text T.Text
value n = HM.fromList $ map f [1..n]
  where
    f m = let t = T.pack (show m) in (t, t)

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Hashable b => Hashable (Tagged a b) where
    hashWithSalt salt (Tagged a) = hashWithSalt salt a

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

newtype T1 = T1 T.Text
  deriving (Eq, Ord)

instance NFData T1 where
    rnf (T1 t) = rnf t
instance Hashable T1 where
    hashWithSalt salt (T1 t) = hashWithSalt salt t
instance FromJSON T1 where
    parseJSON = withText "T1" $ pure . T1
instance FromJSONKey T1 where
    fromJSONKey = FromJSONKeyText T1

-------------------------------------------------------------------------------
-- Coerce
-------------------------------------------------------------------------------

-- | TODO: same as T1 for now
newtype T2 = T2 T.Text
  deriving (Eq, Ord)

instance NFData T2 where
    rnf (T2 t) = rnf t
instance Hashable T2 where
    hashWithSalt salt (T2 t) = hashWithSalt salt t
instance FromJSON T2 where
    parseJSON = withText "T2" $ pure . T2
instance FromJSONKey T2 where
    fromJSONKey = FromJSONKeyText T2

-------------------------------------------------------------------------------
-- TextParser
-------------------------------------------------------------------------------

newtype T3 = T3 T.Text
  deriving (Eq, Ord)

instance NFData T3 where
    rnf (T3 t) = rnf t
instance Hashable T3 where
    hashWithSalt salt (T3 t) = hashWithSalt salt t
instance FromJSON T3 where
    parseJSON = withText "T3" $ pure . T3
instance FromJSONKey T3 where
    fromJSONKey = FromJSONKeyTextParser (pure . T3)

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

encodedValue10 :: LBS.ByteString
encodedValue10 = encode $ value 10

encodedValue100 :: LBS.ByteString
encodedValue100 = encode $ value 100

encodedValue1000 :: LBS.ByteString
encodedValue1000 = encode $ value 1000

encodedValue10000 :: LBS.ByteString
encodedValue10000 = encode $ value 10000

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

decodeHM
    :: (FromJSONKey k, Eq k, Hashable k)
    => Proxy k -> LBS.ByteString -> Maybe (HM.HashMap k T.Text)
decodeHM _ = decode

decodeMap
    :: (FromJSONKey k, Ord k)
    => Proxy k -> LBS.ByteString -> Maybe (M.Map k T.Text)
decodeMap _ = decode

proxyText :: Proxy T.Text
proxyText = Proxy

proxyT1 :: Proxy T1
proxyT1 = Proxy

proxyT2 :: Proxy T2
proxyT2 = Proxy

proxyT3 :: Proxy T3
proxyT3 = Proxy

proxyTagged :: Proxy a -> Proxy (Tagged () a)
proxyTagged _ = Proxy

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchDecodeHM
    :: String
    -> LBS.ByteString
    -> Benchmark
benchDecodeHM name val =
    bgroup name
        [  bench "Text"            $ nf (decodeHM proxyText) val
        ,  bench "Identity"        $ nf (decodeHM proxyT1)   val
        ,  bench "Coerce"          $ nf (decodeHM proxyT2)   val
        ,  bench "Parser"          $ nf (decodeHM proxyT3)   val
        ,  bench "Tagged Text"     $ nf (decodeHM $ proxyTagged proxyText) val
        ,  bench "Tagged Identity" $ nf (decodeHM $ proxyTagged proxyT1)   val
        ,  bench "Tagged Coerce"   $ nf (decodeHM $ proxyTagged proxyT2)   val
        ,  bench "Tagged Parser"   $ nf (decodeHM $ proxyTagged proxyT3)   val
        ]

benchDecodeMap
    :: String
    -> LBS.ByteString
    -> Benchmark
benchDecodeMap name val =
    bgroup name
        [  bench "Text"     $ nf (decodeMap proxyText) val
        ,  bench "Identity" $ nf (decodeMap proxyT1)   val
        ,  bench "Coerce"   $ nf (decodeMap proxyT2)   val
        ,  bench "Parser"   $ nf (decodeMap proxyT3)   val
        ]

main :: IO ()
main = do
    defaultMain
        [ bgroup "encode"
            [ bgroup "HashMap"
                [ benchDecodeHM "10"    encodedValue10
                , benchDecodeHM "100"   encodedValue100
                , benchDecodeHM "1000"  encodedValue1000
                , benchDecodeHM "10000" encodedValue10000
                ]
            , bgroup "Map"
                [ benchDecodeMap "10"    encodedValue10
                , benchDecodeMap "100"   encodedValue100
                , benchDecodeMap "1000"  encodedValue1000
                , benchDecodeMap "10000" encodedValue10000
                ]
            ]
        ]
