{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Properties.ZonedTime (zonedTimeToJSON) where

import Data.Aeson.Types
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, (===), property)
import Data.Time
import Instances ()
import System.IO
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale, dateTimeFmt)
#endif

zonedTimeToJSON :: ZonedTime -> Bool
zonedTimeToJSON t = and $
    toFromJSON' "%FT%T%QZ" t (clearTimeZone t) :  -- (javascript new Date().toISOString())
    toFromJSON' "%F %T%Q%z" t t :   -- (postgres)
    toFromJSON' "%F %T%Q %Z" t t :   -- (time's Show format)
    toFromJSON' "%FT%T%Q%z" t t :
    toFromJSON' "%Y-%mT%T%Q" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%R" t (clearTimeZone . clearDay . clearSeconds $ t) :
    toFromJSON' "%Y-%mT%T" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%T%QZ" t (clearTimeZone . clearDay $ t) :
    toFromJSON' "%Y-%mT%T%Q%z" t (clearDay t) :
    toFromJSON' "%YT%T%Q" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%R" t (clearTimeZone . clearMonth . clearDay . clearSeconds $ t) :
    toFromJSON' "%YT%T" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%T%QZ" t (clearTimeZone . clearMonth . clearDay $ t) :
    toFromJSON' "%YT%T%Q%z" t (clearMonth . clearDay $ t) :
    toFromJSON' "%FT%T%Q" t (clearTimeZone t) :
    toFromJSON' "%FT%R" t (clearTimeZone . clearSeconds $ t) :
    toFromJSON' "%FT%T" t (clearTimeZone t) :
    toFromJSON' (dateTimeFmt defaultTimeLocale) t t :
    []
  where
    toJSON' :: String -> ZonedTime -> Value
    toJSON' format = toJSON . formatTime defaultTimeLocale format

    toFromJSON' :: String -> ZonedTime -> ZonedTime -> Bool
    toFromJSON' format t_ t_' = case fromJSON . toJSON' format $ t_ of
                                  Error msg -> error' msg
                                  Success t_'' -> if t_'' == t_' then True else error' (show t_'')
      where
        error' :: String -> Bool
        error' msg = unsafePerformIO (hPutStrLn stderr . ("zonedTimeToJSON: " ++) $ show
                                       (format, t_, toJSON' format t_, t_', msg))
            `seq` False

clearTimeZone :: ZonedTime -> ZonedTime
clearTimeZone t = t { zonedTimeZone = TimeZone 0 False "" }

clearMonth :: ZonedTime -> ZonedTime
clearMonth = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localDay = h $ localDay lt }

    h :: Day -> Day
    h = maybe (error "clearMonth") id . parseTime defaultTimeLocale "%Y-%m-%d" . formatTime defaultTimeLocale "%Y-01-%d"

clearDay :: ZonedTime -> ZonedTime
clearDay = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localDay = h $ localDay lt }

    h :: Day -> Day
    h = maybe (error "clearDay") id . parseTime defaultTimeLocale "%Y-%m-%d" . formatTime defaultTimeLocale "%Y-%m-01"

clearSeconds :: ZonedTime -> ZonedTime
clearSeconds = f
  where
    f zt = zt { zonedTimeToLocalTime = g $ zonedTimeToLocalTime zt }
    g lt = lt { localTimeOfDay = h $ localTimeOfDay lt }

    h :: TimeOfDay -> TimeOfDay
    h = maybe (error "clearSeconds") id . parseTime defaultTimeLocale "%H:%M:%S" . formatTime defaultTimeLocale "%H:%M:00"
