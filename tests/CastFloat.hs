{-# LANGUAGE CPP #-}
module CastFloat (
    castDoubleToWord64,
    castWord64ToDouble,
    castFloatTests,
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (===))

import Types (UniformWord64 (..))

#if MIN_VERSION_base(4,11,0)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
#else

import Data.Word (Word64)
import Foreign.Storable (Storable (peek, poke))
import Foreign.Ptr (castPtr)
import Foreign.Marshal (alloca)
import System.IO.Unsafe (unsafeDupablePerformIO)

castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 = reinterpretCast

castWord64ToDouble :: Word64 -> Double
castWord64ToDouble = reinterpretCast

reinterpretCast :: (Storable a, Storable b) => a -> b
reinterpretCast x = unsafeDupablePerformIO $ alloca $ \ptr -> do
    poke ptr x
    peek (castPtr ptr)

#endif

castFloatTests :: TestTree
castFloatTests = testGroup "castDoubleToWord64"
    [ testCase "5e-324" $ castDoubleToWord64 5e-324 @?= 1
    , testProperty "roundtrip1" $ \d       -> castWord64ToDouble (castDoubleToWord64 d) === d
    , testProperty "roundtrip2" $ \(U64 w) -> castDoubleToWord64 (castWord64ToDouble w) === w
    ]
