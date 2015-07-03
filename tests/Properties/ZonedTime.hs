{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Properties.ZonedTime (zonedTimeToJSON) where

import Data.Aeson.Internal (IResult(..), formatError, ifromJSON)
import Data.Aeson.Types
import Data.Time
import Instances ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((===), Property)
import Types

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale, dateTimeFmt)
#endif

zonedTimeToJSON :: Test
zonedTimeToJSON = testGroup "ZonedTime" [
    test "%FT%T%QZ" clearTimeZone  -- javascript new Date().toISOString()
  , test "%F %T%Q%z" id            -- postgres
  , test "%F %T%Q %Z" id           -- time's Show format
  , test "%FT%T%Q%z" id
  , test "%Y-%mT%T%Q" (clearTimeZone . clearDay)
  , test "%Y-%mT%R" (clearTimeZone . clearDay . clearSeconds)
  , test "%Y-%mT%T" (clearTimeZone . clearDay)
  , test "%Y-%mT%T%QZ" (clearTimeZone . clearDay)
  , test "%Y-%mT%T%Q%z" clearDay
  , test "%YT%T%Q" (clearTimeZone . clearMonth . clearDay)
  , test "%YT%R" (clearTimeZone . clearMonth . clearDay . clearSeconds)
  , test "%YT%T" (clearTimeZone . clearMonth . clearDay)
  , test "%YT%T%QZ" (clearTimeZone . clearMonth . clearDay)
  , test "%YT%T%Q%z" (clearMonth . clearDay)
  , test "%FT%T%Q" clearTimeZone
  , test "%FT%R" (clearTimeZone . clearSeconds)
  , test "%FT%T" clearTimeZone
  , test (dateTimeFmt defaultTimeLocale) id
  ]
  where
    toJSON' :: String -> ZonedTime -> Value
    toJSON' format = toJSON . formatTime defaultTimeLocale format

    test :: String -> (ZonedTime -> ZonedTime) -> Test
    test format f = testProperty format go
      where
        go :: ZonedTime -> Property
        go t = case ifromJSON (toJSON' format t) of
                 IError path msg -> failure "fromJSON" (formatError path msg) t
                 ISuccess t'     -> t' === f t

clearTimeZone :: ZonedTime -> ZonedTime
clearTimeZone t = t { zonedTimeZone = TimeZone 0 False "" }

clearMonth :: ZonedTime -> ZonedTime
clearMonth zt = zt { zonedTimeToLocalTime = g (zonedTimeToLocalTime zt) }
  where
    g lt = lt { localDay = h (localDay lt) }

    h :: Day -> Day
    h = maybe (error "clearMonth") id . parseTime defaultTimeLocale "%Y-%m-%d" .
        formatTime defaultTimeLocale "%Y-01-%d"

clearDay :: ZonedTime -> ZonedTime
clearDay zt = zt { zonedTimeToLocalTime = g (zonedTimeToLocalTime zt) }
  where
    g lt = lt { localDay = h (localDay lt) }

    h :: Day -> Day
    h = maybe (error "clearDay") id . parseTime defaultTimeLocale "%Y-%m-%d" .
        formatTime defaultTimeLocale "%Y-%m-01"

clearSeconds :: ZonedTime -> ZonedTime
clearSeconds zt = zt { zonedTimeToLocalTime = g (zonedTimeToLocalTime zt) }
  where
    g lt = lt { localTimeOfDay = h (localTimeOfDay lt) }

    h :: TimeOfDay -> TimeOfDay
    h = maybe (error "clearSeconds") id .
        parseTime defaultTimeLocale "%H:%M:%S" .
        formatTime defaultTimeLocale "%H:%M:00"
