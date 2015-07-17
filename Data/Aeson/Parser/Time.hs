{-# LANGUAGE CPP #-}

module Data.Aeson.Parser.Time
    (
      day
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    ) where

import Control.Applicative ((<$>), (<|>), empty)
import Control.Monad (when, void)
import qualified Data.Aeson.Types.Internal as Aeson
import Data.Attoparsec.Text as A
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import qualified Data.Time.LocalTime as Local
import qualified Data.Text as T
import qualified Data.Text.Read as T

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale, dateTimeFmt)
#else
import System.Locale (defaultTimeLocale, dateTimeFmt)
#endif

day :: A.Parser Day
day = do
  y <- decimal <* char '-'
  m <- decimal <* char '-'
  d <- decimal
  maybe (fail "invalid date") return (fromGregorianValid y m d)

timeOfDay :: A.Parser Local.TimeOfDay
timeOfDay = do
  h <- decimal <* char ':'
  m <- decimal <* char ':'
  s <- (fromRational . toRational) <$> double
  maybe (fail "invalid time") return (Local.makeTimeOfDayValid h m s)

timeZone :: A.Parser Local.TimeZone
timeZone = do
  skipSpace
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Local.utc
    else do
      let dec f s = case f T.decimal s of
                        Right (n, r) | T.null r -> return n
                        _                       -> fail "invalid time zone"
      h <- dec T.signed =<< T.cons ch <$> A.take 2
      c <- peekChar'
      when (c == ':') $ void anyChar
      m <- dec id =<< A.take 2
      let off = h * 60 + m
      if off < -720 || off > 840 || m > 59
        then fail "invalid time zone offset"
        else return (Local.minutesToTimeZone off)

utcTime :: A.Parser UTCTime
utcTime = do
  let daySep = satisfy (\c -> c == 'T' || c == ' ')
  lt <- Local.LocalTime <$> day <* daySep <*> timeOfDay
  tz <- timeZone
  return (Local.localTimeToUTC tz lt)

zonedTime :: Text -> Aeson.Parser Local.ZonedTime
zonedTime t =
      tryFormats alternateFormats
      <|> fail "could not parse ECMA-262 ISO-8601 date"
      where
        tryFormat f =
          case parseTime defaultTimeLocale f (T.unpack t) of
            Just d -> pure d
            Nothing -> empty
        tryFormats = foldr1 (<|>) . map tryFormat
        alternateFormats =
            "%FT%T%QZ" :  -- (javascript new Date().toISOString())
            "%F %T%Q%z" :   -- (postgres)
            "%F %T%Q %Z" :   -- (time's Show format)
            "%FT%T%Q%z" :
            "%Y-%mT%T%Q" :
            "%Y-%mT%R" :
            "%Y-%mT%T" :
            "%Y-%mT%T%QZ" :
            "%Y-%mT%T%Q%z" :
            "%YT%T%Q" :
            "%YT%R" :
            "%YT%T" :
            "%YT%T%QZ" :
            "%YT%T%Q%z" :
            "%FT%T%Q" :
            "%FT%R" :
            "%FT%T" :
            dateTimeFmt defaultTimeLocale :
            []
