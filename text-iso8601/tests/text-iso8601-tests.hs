{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.Functor.Classes       (liftEq)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (Builder, toLazyText)
import           Data.Time.LocalTime.Compat (TimeZone (..), ZonedTime (..))
import           Data.Typeable              (Typeable, typeRep)
import           Test.QuickCheck            (Arbitrary, counterexample,
                                             property)
import           Test.QuickCheck.Instances  ()
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertEqual, assertFailure, testCase)
import           Test.Tasty.QuickCheck      (testProperty)

import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Time.FromText         as T
import qualified Data.Time.ToText           as T

main :: IO ()
main = defaultMain $ testGroup "text-iso8601"
    [ testGroup "roundtrip"
        [ roundtrip (==) T.buildDay       T.parseDay
        , roundtrip (==) T.buildLocalTime T.parseLocalTime
        , roundtrip eqTZ T.buildTimeZone  T.parseTimeZone
        , roundtrip (==) T.buildUTCTime   T.parseUTCTime
        , roundtrip eqZT T.buildZonedTime T.parseZonedTime
        , roundtrip (==) T.buildTimeOfDay T.parseTimeOfDay
        , roundtrip (==) T.buildYear      T.parseYear
        , roundtrip (==) T.buildMonth     T.parseMonth
        , roundtrip (==) T.buildQuarter   T.parseQuarter
        , roundtrip (==) T.buildQuarterOfYear T.parseQuarterOfYear
        ]

    , testGroup "accepts"
        -- we accept space instead of T
        -- RFC3339 has a note suggesting allowing this.
        [ accepts T.parseUTCTime "2023-06-09 02:35:33Z"

        -- 60 second is always accepted
        , accepts T.parseUTCTime "2023-06-09T02:35:60Z"

        -- examples from RFC3339
        , accepts T.parseUTCTime "1985-04-12T23:20:50.52Z"
        , accepts T.parseUTCTime "1996-12-19T16:39:57-08:00"
        , accepts T.parseUTCTime "1990-12-31T23:59:60Z"
        , accepts T.parseUTCTime "1990-12-31T15:59:60-08:00"
        , accepts T.parseUTCTime "1937-01-01T12:00:27.87+00:20"

        -- we accept time without seconds
        , accepts T.parseUTCTime "1937-01-01 12:00Z"
        , accepts T.parseLocalTime "1937-01-01 12:00"

        -- ISO8601 allows various offsets, while RFC3339 only +-HH:MM
        , accepts T.parseUTCTime "1990-12-31T15:59:60-0800" -- no colon
        , accepts T.parseUTCTime "1990-12-31T15:59:60-08"   -- just hour

        -- accepts +23:59
        , accepts T.parseUTCTime "1937-01-01T12:00:00+23:59"
        , accepts T.parseUTCTime "1937-01-01T12:00:00-23:59"

        -- accepts 24:00:00
        , accepts T.parseUTCTime "1990-12-31T24:00:00Z"
        ]

    , testGroup "rejected"
        -- https://github.com/haskell/aeson/issues/1033
        [ rejects T.parseUTCTime "2023-06-09T02:35:33 Z" "Unexpected ' ', expecting timezone: Z, +HH:MM or -HH:MM"

        -- Y2K years
        , rejects T.parseDay "99-12-12" "expected year with at least 4 digits"

        -- we don't accept lowercase T or Z
        -- RFC3339 says we MAY limit, i.e. requiring they should be uppercase.
        , rejects T.parseUTCTime "2023-06-09T02:35:33z" "Unexpected 'z', expecting timezone: Z, +HH:MM or -HH:MM"
        , rejects T.parseUTCTime "2023-06-09t02:35:33Z" "Unexpected 't', expecting a day separator, T or space"

        -- accepts +23:59, but not 24 or 60
        , rejects T.parseUTCTime "1937-01-01T12:00:00+24:59" "Invalid TimeZone:(24,59)"
        , rejects T.parseUTCTime "1937-01-01T12:00:00-23:60" "Invalid TimeZone:(23,60)"

        -- rejects 24:xx:xx except 24:00:00
        , rejects T.parseUTCTime "1990-12-31T24:00:01Z" "Invalid time of day:(24,0,1.000000000000)"
        , rejects T.parseUTCTime "1990-12-31T24:00:60Z" "Invalid time of day:(24,0,60.000000000000)"
        , rejects T.parseUTCTime "1990-12-31T24:01:00Z" "Invalid time of day:(24,1,0.000000000000)"

        -- Reject long years
        , rejects T.parseUTCTime "1234567890123456-01-01T01:01Z" "expected year with at most 15 digits"
        ]
    ]

eqTZ :: TimeZone -> TimeZone -> Bool
eqTZ a b = timeZoneMinutes a == timeZoneMinutes b

eqZT :: ZonedTime -> ZonedTime -> Bool
eqZT (ZonedTime lt tz) (ZonedTime lt' tz') =
    lt == lt' && eqTZ tz tz'

roundtrip
    :: forall a. (Typeable a, Arbitrary a, Show a)
    => (a -> a -> Bool) -> (a -> Builder) -> (Text -> Either String a) -> TestTree
roundtrip eq build parse = testProperty (show (typeRep (Proxy :: Proxy a))) $ \x ->
    let lt = toLazyText (build x)
        y  = parse (LT.toStrict lt)
    in counterexample (LT.unpack lt) $
       counterexample (show y) $
       property (liftEq eq y (Right x))

rejects :: forall a. (Typeable a, Show a) => (Text -> Either String a) -> String -> String -> TestTree
rejects parse inp expected = testCase (show (typeRep (Proxy :: Proxy a)) ++ " rejects " ++ show inp) $ do
    case parse (T.pack inp) of
        Left actual -> assertEqual "Error message mismatch" actual expected
        Right a -> assertFailure $ "Unexpectedly accepted: " ++ show a

accepts :: forall a. (Typeable a, Show a) => (Text -> Either String a) -> String -> TestTree
accepts parse inp = testCase (show (typeRep (Proxy :: Proxy a)) ++ " accepts " ++ show inp) $ do
    case parse (T.pack inp) of
        Left err -> assertFailure $ "Unexpectedly rejected: " ++ err
        Right _  -> return ()
