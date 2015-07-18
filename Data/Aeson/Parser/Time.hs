{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aeson.Parser.Time
    (
      run
    , day
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    ) where

import Control.Applicative ((<$>), (<|>), empty)
import Control.Monad (when, void)
import Data.Attoparsec.Text as A
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson.Types.Internal as Aeson
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Local

run :: Parser a -> Text -> Aeson.Parser a
run p t = case A.parseOnly (p <* endOfInput) t of
              Left err -> fail $ "could not parse date: " ++ err
              Right r  -> return r

day :: Parser Day
day = do
  y <- decimal <* char '-'
  m <- twoDigits <* char '-'
  d <- twoDigits
  maybe (fail "invalid date") return (fromGregorianValid y m d)

c2d :: Char -> Int
c2d c = ord c .&. 15

twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  return $! c2d a * 10 + c2d b

timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits <* char ':'
  m <- twoDigits <* char ':'
  s <- pico
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

pico :: Parser Pico
pico = do
  w <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      (t,f :: Int64) <- anyChar *> match decimal
      return $! fromIntegral w + fromIntegral f / 10 ^ T.length t
    _ -> return $! fromIntegral w

timeZone :: Parser Local.TimeZone
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return (Local.TimeZone 0 False "")
    else do
      h0 <- twoDigits
      m <- maybeSkip ':' *> twoDigits
      let h | ch == '-' = negate h0
            | otherwise = h0
          off           = h * 60 + m
      if off < -720 || off > 840 || m > 59
        then fail "invalid time zone offset"
        else return $! Local.minutesToTimeZone off

localTime :: Parser Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == 'T' || c == ' ')

utcTime :: Parser UTCTime
utcTime = do
  lt <- localTime
  tz <- timeZone
  return $! Local.localTimeToUTC tz lt

zonedTime :: Parser Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> timeZone
