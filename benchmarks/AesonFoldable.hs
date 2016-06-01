{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Data.Aeson
import Data.Foldable (toList)

import qualified Data.Sequence as S
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

newtype L f = L { getL :: f Int }

instance Foldable f => ToJSON (L f) where
    toJSON = error "do not use this"
    toEncoding = toEncoding . toList . getL

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

newtype F f = F { getF :: f Int }

instance Foldable f => ToJSON (F f) where
    toJSON = error "do not use this"
    toEncoding = foldable . getF

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

valueList :: [Int]
valueList = [1..1000]

valueSeq :: S.Seq Int
valueSeq = S.fromList valueList

valueVector :: V.Vector Int
valueVector = V.fromList valueList

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchEncode
    :: ToJSON a
    => String
    -> a
    -> Benchmark
benchEncode name val
    = bench name $ nf encode val

main :: IO ()
main =  defaultMain
    [ bgroup "encode"
        [ bgroup "List"
            [ benchEncode "-"     valueList
            , benchEncode "L" $ L valueList
            , benchEncode "F" $ F valueList
            ]
        , bgroup "Seq"
            [ benchEncode "-"     valueSeq
            , benchEncode "L" $ L valueSeq
            , benchEncode "F" $ F valueSeq
            ]
        , bgroup "Vector"
            [ benchEncode "-"     valueVector
            , benchEncode "L" $ L valueVector
            , benchEncode "F" $ F valueVector
            ]
        ]
    ]
