{-# LANGUAGE OverloadedStrings #-}
module UnitTests.UTCTime (utcTimeTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, testCaseSteps, Assertion, assertEqual, assertFailure)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, ZonedTime)
import Data.Time.Format.Compat (parseTimeM, defaultTimeLocale)

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import Data.Aeson

-- Test decoding various UTC time formats
utcTimeGood :: Assertion
utcTimeGood = do
  let ts1 = "2015-01-01T12:13:00.00Z"
  let ts2 = "2015-01-01T12:13:00Z"
  -- 'T' between date and time is not required, can be space
  let ts3 = "2015-01-03 12:13:00.00Z"
  let ts4 = "2015-01-03 12:13:00.125Z"
  t1 <- parseWithAeson ts1
  t2 <- parseWithAeson ts2
  t3 <- parseWithAeson ts3
  t4 <- parseWithAeson ts4
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts1) t1
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" ts2) t2
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts3) t3
  assertEqual "utctime" (parseWithRead "%F %T%QZ" ts4) t4
  -- Time zones.  Both +HHMM and +HH:MM are allowed for timezone
  -- offset, and MM may be omitted.
  let ts5 = "2015-01-01T12:30:00.00+00"
  let ts6 = "2015-01-01T12:30:00.00+01:15"
  let ts7 = "2015-01-01T12:30:00.00-02"
  let ts8 = "2015-01-01T22:00:00.00-03"
  let ts9 = "2015-01-01T22:00:00.00-04:30"
  t5 <- parseWithAeson ts5
  t6 <- parseWithAeson ts6
  t7 <- parseWithAeson ts7
  t8 <- parseWithAeson ts8
  t9 <- parseWithAeson ts9
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T12:30:00.00Z") t5
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T11:15:00.00Z") t6
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t7
  -- ts8 wraps around to the next day in UTC
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T01:00:00Z") t8
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-02T02:30:00Z") t9

  -- Seconds in Time can be omitted
  let ts10 = "2015-01-03T12:13Z"
  let ts11 = "2015-01-03 12:13Z"
  let ts12 = "2015-01-01T12:30-02"
  t10 <- parseWithAeson ts10
  t11 <- parseWithAeson ts11
  t12 <- parseWithAeson ts12
  assertEqual "utctime" (parseWithRead "%FT%H:%MZ" ts10) t10
  assertEqual "utctime" (parseWithRead "%F %H:%MZ" ts11) t11
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-01-01T14:30:00Z") t12

  -- leap seconds are included correctly
  let ts13 = "2015-08-23T23:59:60.128+00"
  t13 <- parseWithAeson ts13
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-08-23T23:59:60.128Z") t13
  let ts14 = "2015-08-23T23:59:60.999999999999+00"
  t14 <- parseWithAeson ts14
  assertEqual "utctime" (parseWithRead "%FT%T%QZ" "2015-08-23T23:59:60.999999999999Z") t14

  where
    parseWithRead :: String -> LT.Text -> UTCTime
    parseWithRead f s =
      fromMaybe (error "parseTime input malformed") . parseTimeM True defaultTimeLocale f . LT.unpack $ s

    parseWithAeson :: LT.Text -> IO UTCTime
    parseWithAeson s = either fail return . eitherDecode . LT.encodeUtf8 $ LT.concat ["\"", s, "\""]

-- Test that a few non-timezone qualified timestamp formats get
-- rejected if decoding to UTCTime.
utcTimeBad :: (String -> IO ()) -> Assertion
utcTimeBad info = do
  verifyFailParse "2000-01-01T12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01 12:13:00" -- missing Zulu time not allowed (some TZ required)
  verifyFailParse "2000-01-01"          -- date only not OK
  verifyFailParse "2000-01-01Z"         -- date only not OK
  verifyFailParse "2015-01-01T12:30:00.00+00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-01T12:30:00.00+00:00Z" -- no Zulu if offset given
  verifyFailParse "2015-01-03 12:13:00.Z" -- decimal at the end but no digits
  verifyFailParse "2015-01-03 12:13.000Z" -- decimal at the end, but no seconds
  verifyFailParse "2015-01-03 23:59:61Z"  -- exceeds allowed seconds per day
  verifyFailParse "2015-01-03 12:13:00 Z" -- space before Zulu
  verifyFailParse "2015-01-03 12:13:00 +00:00" -- space before offset
  where
    verifyFailParse :: LT.Text -> Assertion
    verifyFailParse s = do
      info (LT.unpack s)
      let bs = LT.encodeUtf8 $ LT.concat ["\"", s, "\""]
      let decU = decode bs :: Maybe UTCTime
      let decZ = decode bs :: Maybe ZonedTime
      assertIsNothing "verify failure UTCTime"   decU
      assertIsNothing "verify failure ZonedTime"   decZ

assertIsNothing :: Show a => String -> Maybe a -> Assertion
assertIsNothing _    Nothing = return ()
assertIsNothing err (Just a) = assertFailure $ err ++ " " ++ show a

utcTimeTests :: TestTree
utcTimeTests = testGroup "utctime" [
      testCase "good" utcTimeGood
    , testCaseSteps "bad"  utcTimeBad
    ]
