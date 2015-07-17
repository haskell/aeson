module Data.Aeson.Parser.Time
    (
      day
    , timeOfDay
    , timeZone
    , utcTime
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when, void)
import Data.Attoparsec.Text as A
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.LocalTime as Local
import qualified Data.Text as T
import qualified Data.Text.Read as T

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
