{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Criterion.Main

import Prelude ()
import Prelude.Compat

import Data.Foldable (toList)
import qualified "aeson" Data.Aeson as A
import qualified "aeson-benchmarks" Data.Aeson as B
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

newtype L f = L { getL :: f Int }

instance Foldable f => B.ToJSON (L f) where
    toJSON = error "do not use this"
    toEncoding = B.toEncoding . toList . getL

instance Foldable f => A.ToJSON (L f) where
    toJSON = error "do not use this"
    toEncoding = A.toEncoding . toList . getL

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

newtype F f = F { getF :: f Int }

instance Foldable f => B.ToJSON (F f) where
    toJSON = error "do not use this"
    toEncoding = B.foldable . getF

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

benchEncodeA
    :: A.ToJSON a
    => String
    -> a
    -> Benchmark
benchEncodeA name val
    = bench ("A " ++ name) $ nf A.encode val

benchEncodeB
    :: B.ToJSON a
    => String
    -> a
    -> Benchmark
benchEncodeB name val
    = bench ("B " ++ name) $ nf B.encode val

main :: IO ()
main =  defaultMain
    [ bgroup "encode"
        [ bgroup "List"
            [ benchEncodeB "-"     valueList
            , benchEncodeB "L" $ L valueList
            , benchEncodeB "F" $ F valueList
            , benchEncodeA "-"     valueList
            , benchEncodeA "L" $ L valueList
            , benchEncodeA "F" $ F valueList
            ]
        , bgroup "Seq"
            [ benchEncodeB "-"     valueSeq
            , benchEncodeB "L" $ L valueSeq
            , benchEncodeB "F" $ F valueSeq
            , benchEncodeA "-"     valueSeq
            , benchEncodeA "L" $ L valueSeq
            , benchEncodeA "F" $ F valueSeq
            ]
        , bgroup "Vector"
            [ benchEncodeB "-"     valueVector
            , benchEncodeB "L" $ L valueVector
            , benchEncodeB "F" $ F valueVector
            , benchEncodeA "-"     valueVector
            , benchEncodeA "L" $ L valueVector
            , benchEncodeA "F" $ F valueVector
            ]
        , bgroup "Vector.Unboxed"
            [ benchEncodeB "-"     valueUVector
            , benchEncodeA "-"     valueUVector
            ]
        ]
    ]
