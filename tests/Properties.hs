import Data.Aeson.Encode
import Data.Aeson.Types
import Data.Attoparsec.Number
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.ByteString.Lazy.Char8 as L

encodeDouble d  = encode (Number (D d)) == L.pack (show d)
encodeInteger i = encode (Number (I i)) == L.pack (show i)

main = defaultMain tests

tests = [
  testGroup "encode" [
      testProperty "encodeDouble" encodeDouble
    , testProperty "encodeInteger" encodeInteger
    ]
  ]
