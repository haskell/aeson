{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

-- |
-- Module:      Data.Aeson.Parser.Time
-- Copyright:   (c) 2015 Bryan O'Sullivan
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Parsers for parsing dates and times.

module Data.Aeson.Parser.Time
    (
      run
    , day
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    , localTime
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (when, void)
import Data.Attoparsec.Text as A
import Data.Bits ((.&.))
import Data.Char (isDigit, ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorianValid)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Aeson.Types.Internal as Aeson
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Local

#if MIN_VERSION_base(4,7,0)

import Data.Fixed (Fixed(MkFixed))

mkPico :: Integer -> Pico
mkPico = MkFixed

#else

import Unsafe.Coerce

mkPico :: Integer -> Pico
mkPico = unsafeCoerce

#endif

-- | Run an attoparsec parser as an aeson parser.
run :: Parser a -> Text -> Aeson.Parser a
run p t = case A.parseOnly (p <* endOfInput) t of
            Left err -> fail $ "could not parse date: " ++ err
            Right r  -> return r

-- | Parse a date of the form @YYYY-MM-DD@.
day :: Parser Day
day = do
  y <- decimal <* char '-'
  m <- twoDigits <* char '-'
  d <- twoDigits
  maybe (fail "invalid date") return (fromGregorianValid y m d)

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

-- | Parse a time of the form @HH:MM:SS[.SSS]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits <* char ':'
  m <- twoDigits <* char ':'
  s <- seconds
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos real t
    _ -> return $! fromIntegral real
 where
  parsePicos a0 t = mkPico (fromIntegral (t' * 10^n))
    where T n t'  = T.foldl' step (T 12 (fromIntegral a0)) t
          step ma@(T m a) c
              | m <= 0    = ma
              | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h0 <- twoDigits
      m <- maybeSkip ':' *> twoDigits
      let h | ch == '-' = negate h0
            | otherwise = h0
      case h * 60 + m of
        off | off == 0 ->
              return Nothing
            | off < -720 || off > 840 || m > 59 ->
              fail "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM:SS@.
-- The space may be replaced with a @T@.  The number of seconds may be
-- followed by a fractional component.
localTime :: Parser Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == 'T' || c == ' ')

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: Parser UTCTime
utcTime = do
  lt@(Local.LocalTime d t) <- localTime
  mtz <- timeZone
  case mtz of
    Nothing -> let !tt = Local.timeOfDayToTime t
               in return (UTCTime d tt)
    Just tz -> return $! Local.localTimeToUTC tz lt

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM:SS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- are minutes.
zonedTime :: Parser Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""
