{-# LANGUAGE BangPatterns, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DataKinds #-}

import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

value :: Int -> HM.HashMap T.Text T.Text
value n = HM.fromList $ map f [1..n]
  where
    f m = let t = T.pack (show m) in (t, t)

newtype T1 = T1 T.Text
  deriving (Eq, Ord)

instance NFData T1 where
  rnf (T1 t) = rnf t
instance Hashable T1 where
  hashWithSalt salt (T1 t) = hashWithSalt salt t
instance FromJSONKey T1 'JSONKeyIdentity where
    fromJSONKey _ = T1

newtype T2 = T2 T.Text
  deriving (Eq, Ord)

instance NFData T2 where
  rnf (T2 t) = rnf t
instance Hashable T2 where
  hashWithSalt salt (T2 t) = hashWithSalt salt t
instance FromJSONKey T2 'JSONKeyCoerce where
    fromJSONKey _ = ()

newtype T3 = T3 T.Text
  deriving (Eq, Ord)

instance NFData T3 where
  rnf (T3 t) = rnf t
instance Hashable T3 where
  hashWithSalt salt (T3 t) = hashWithSalt salt t
instance FromJSONKey T3 'JSONKeyTextParser where
    fromJSONKey _ = return . T3

encodedValue10 :: LBS.ByteString
encodedValue10 = encode $ value 10

encodedValue100 :: LBS.ByteString
encodedValue100 = encode $ value 100

encodedValue1000 :: LBS.ByteString
encodedValue1000 = encode $ value 1000

encodedValue10000 :: LBS.ByteString
encodedValue10000 = encode $ value 10000

main :: IO ()
main = do
  defaultMain
    [ bgroup "HashMap"
      [ bgroup "10"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T.Text T.Text)) encodedValue10
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T1 T.Text)) encodedValue10
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T2 T.Text)) encodedValue10
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T3 T.Text)) encodedValue10
        ]
      , bgroup "100"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T.Text T.Text)) encodedValue100
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T1 T.Text)) encodedValue100
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T2 T.Text)) encodedValue100
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T3 T.Text)) encodedValue100
        ]
      , bgroup "1000"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T.Text T.Text)) encodedValue1000
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T1 T.Text)) encodedValue1000
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T2 T.Text)) encodedValue1000
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T3 T.Text)) encodedValue1000
        ]
      , bgroup "10000"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T.Text T.Text)) encodedValue10000
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T1 T.Text)) encodedValue10000
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T2 T.Text)) encodedValue10000
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (HM.HashMap T3 T.Text)) encodedValue10000
        ]
      ]
    , bgroup "Map"
      [ bgroup "10"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T.Text T.Text)) encodedValue10
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T1 T.Text)) encodedValue10
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T2 T.Text)) encodedValue10
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T3 T.Text)) encodedValue10
        ]
      , bgroup "100"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T.Text T.Text)) encodedValue100
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T1 T.Text)) encodedValue100
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T2 T.Text)) encodedValue100
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T3 T.Text)) encodedValue100
        ]
      , bgroup "1000"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T.Text T.Text)) encodedValue1000
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T1 T.Text)) encodedValue1000
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T2 T.Text)) encodedValue1000
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T3 T.Text)) encodedValue1000
        ]
      , bgroup "10000"
        [  bench "Text" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T.Text T.Text)) encodedValue10000
        ,  bench "Identity" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T1 T.Text)) encodedValue10000
        ,  bench "Coerce" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T2 T.Text)) encodedValue10000
        ,  bench "Parser" $ nf (decode :: LBS.ByteString -> Maybe (M.Map T3 T.Text)) encodedValue10000
        ]
      ]
    ]
