{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonFoldable (benchmark) where

import Prelude.Compat
import Bench

import Data.Foldable (toList)
import qualified Data.Aeson as A
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

newtype L f = L { getL :: f Int }

instance Foldable f => A.ToJSON (L f) where
    toJSON = error "do not use this"
    toEncoding = A.toEncoding . toList . getL

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

newtype F f = F { getF :: f Int }

instance Foldable f => A.ToJSON (F f) where
    toJSON = error "do not use this"
    toEncoding = A.foldable . getF

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

valueList :: [Int]
valueList = [1..1000]

valueSeq :: S.Seq Int
valueSeq = S.fromList valueList

valueVector :: V.Vector Int
valueVector = V.fromList valueList

valueUVector :: U.Vector Int
valueUVector = U.fromList valueList

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchEncode
    :: A.ToJSON a
    => String
    -> a
    -> Benchmark
benchEncode name val
    = bench ("A " ++ name) $ nf A.encode val

benchmark :: Benchmark
benchmark = bgroup "foldable"
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
        , bgroup "Vector.Unboxed"
            [ benchEncode "-"     valueUVector
            ]
        ]
    ]
