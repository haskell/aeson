{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}
-- |
--
-- The [RFC3339 grammar](https://datatracker.ietf.org/doc/html/rfc3339#section-5.6) is below
--
-- @
-- date-fullyear   = 4DIGIT
-- date-month      = 2DIGIT  ; 01-12
-- date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on month/year
-- time-hour       = 2DIGIT  ; 00-23
-- time-minute     = 2DIGIT  ; 00-59
-- time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second rules
-- time-secfrac    = "." 1*DIGIT
-- time-numoffset  = ("+" / "-") time-hour ":" time-minute
-- time-offset     = \"Z" / time-numoffset
--
-- partial-time    = time-hour ":" time-minute ":" time-second [time-secfrac]
-- full-date       = date-fullyear "-" date-month "-" date-mday
-- full-time       = partial-time time-offset
--
-- date-time       = full-date \"T" full-time
-- @
--
-- The parsers are a bit more lenient:
--
-- * We also accept space instead of @T@ date-time separator. (Allowed by RFC3339, forbidden by ISO8601)
--
-- * Seconds are optional (allowed by ISO8601)
--
-- * numerical timezone offset can be just @("+" / "-") time-hour@ or without a colon: @("+" / "-") time-hour time-minute@ (allowed by ISO8601).
--   However we require colons in between hours, minutes and seconds in the time (@partial-time@) production, and dashes in @full-date@ production.
--
-- * We allow over 4 digits in the year part (and that is a reason to require dashes).
--
-- * We allow @-00:00@ time offset. (Allowed by RFC3339, forbidden by ISO8601)
--
-- * We always allow time with 60 seconds, we don't consult any leap second database.
--
module Data.Time.FromText (
    parseDay,
    parseLocalTime,
    parseTimeOfDay,
    parseTimeZone,
    parseUTCTime,
    parseZonedTime,
    parseYear,
    parseMonth,
    parseQuarter,
    parseQuarterOfYear,
) where

import           Data.Bits                         ((.&.))
import           Data.Char                         (ord, chr)
import           Data.Fixed                        (Fixed (..), Pico)
import           Data.Integer.Conversion           (textToInteger)
import           Data.Text.Array                   (Array)
import           Data.Text.Internal                (Text (..))
import           GHC.Exts                          (inline)

import           Data.Time.Calendar                (Day, fromGregorianValid)
import           Data.Time.Calendar.Compat         (Year)
import           Data.Time.Calendar.Month.Compat   (Month, fromYearMonthValid)
import           Data.Time.Calendar.Quarter.Compat (Quarter, QuarterOfYear (..),
                                                    fromYearQuarter)
import           Data.Time.Clock                   (UTCTime (..))

import qualified Data.Text                         as T
import qualified Data.Text.Array                   as A
import qualified Data.Time.LocalTime               as Local

-- The parsing functions here are written in continuation passing style
-- with everything marked INLINE and continuation called with GHC.Exts.inline
-- to try to enforce that whole CPS-business goes away (with slight code-duplication).
--
-- Using staging would be a nicer way to enforce what we want here,
-- but that would require TemplateHaskell.

-------------------------------------------------------------------------------
-- Public functions
-------------------------------------------------------------------------------

-- | Parse a date of the form @[+-]YYYY-MM-DD@.
--
-- The year must contain at least 4 digits, to avoid the Y2K problem:
-- a two-digit year @YY@ may mean @YY@, @19YY@, or @20YY@, and we make it
-- an error to prevent the ambiguity.
-- Years from @0000@ to @0999@ must thus be zero-padded.
-- The year may have more than 4 digits.
--
parseDay :: Text -> Either String Day
parseDay = parseDay_ expectingEOF

-- | Parse a month of the form @[+-]YYYY-MM@.
--
-- See also 'parseDay' for details about the year format.
parseMonth :: Text -> Either String Month
parseMonth = parseMonth_ $ \y m t ->
    case fromYearMonthValid y m of
        Nothing     -> Left $ "invalid month:" ++ show (y, m)
        Just !month -> expectingEOF month t

-- | Parse a year @[+-]YYYY@, with at least 4 digits. Can include a sign.
--
-- See also 'parseDay' for details about the year format.
--
-- Note: 'Year' is a type synonym for 'Integer'.
parseYear :: Text -> Either String Year
parseYear = parseYear_ Right $ \_ c _ -> unexpectedChar c "end-of-input"

-- | Parse a quarter of the form @[+-]YYYY-QN@.
--
-- See also 'parseDay' for details about the year format.
--
parseQuarter :: Text -> Either String Quarter
parseQuarter = parseQuarter_ $ \y q t ->
    let !quarter = fromYearQuarter y q in expectingEOF quarter t

-- | Parse a quarter of year of the form @QN@ or @qN@.
parseQuarterOfYear :: Text -> Either String QuarterOfYear
parseQuarterOfYear = parseQuarterOfYear_ expectingEOF

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
parseTimeOfDay :: Text -> Either String Local.TimeOfDay
parseTimeOfDay = parseTimeOfDay_ kontEOF $ \_ _ _ c _ -> unexpectedChar c "end-of-input" where
    kontEOF h m s = makeTimeOfDay h m s Right

-- | Parse a time zone.
--
-- The accepted formats are @Z@, @+HH@, @+HHMM@, or @+HH:MM@. (@+@ can be @-@).
--
-- Accepts @-23:59..23:59@ range, i.e. @HH < 24@ and @MM < 59@.
-- (This is consistent with grammar, and with what Python, Clojure, joda-time do).
--
parseTimeZone :: Text -> Either String Local.TimeZone
parseTimeZone = parseTimeZone_ Right

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@.  The number of seconds is optional
-- and may be followed by a fractional component.
parseLocalTime :: Text -> Either String Local.LocalTime
parseLocalTime = parseLocalTime_ Right $ \_ c _ -> unexpectedChar c "end-of-input"

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
parseUTCTime :: Text -> Either String UTCTime
parseUTCTime = parseUTCTime_ Right

-- | Parse a date with time zone info. Acceptable formats:
--
-- @
-- YYYY-MM-DD HH:MMZ
-- YYYY-MM-DD HH:MM:SSZ
-- YYYY-MM-DD HH:MM:SS.SSSZ
-- @
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
parseZonedTime :: Text -> Either String Local.ZonedTime
parseZonedTime = parseZonedTime_ Right

-------------------------------------------------------------------------------
-- Uncons
-------------------------------------------------------------------------------

-- As all characters in the time format are ASCII
-- we can use slightly more efficient (or at least smaller) uncons.

{-# INLINE unconsAscii_ #-}
unconsAscii_
    :: Array -> Int -> Int
    -> Either String r                         -- ^ EOF continuation
    -> (Char -> Int -> Int -> Either String r) -- ^ character continuation
    -> Either String r
unconsAscii_ arr off len kontEOF kontC
    | len <= 0  = inline kontEOF
    | c < 0x80  = inline kontC (chr (fromIntegral c)) (off + 1) (len - 1)
    | otherwise = Left "Non-ASCII character"
  where
    c = A.unsafeIndex arr off

{-# INLINE unconsAscii #-}
unconsAscii :: Either String r -> (Char -> Text -> Either String r) -> Text -> Either String r
unconsAscii kontEOF kontC (Text arr off len) =
    unconsAscii_ arr off len kontEOF $ \c off' len' ->
    inline kontC c (Text arr off' len')

-------------------------------------------------------------------------------
-- Expecting errors
-------------------------------------------------------------------------------

expectingEOF :: r -> Text -> Either String r
expectingEOF = expectingEOF_ Right
{-# INLINE expectingEOF #-}

expectingEOF_ :: (a -> Either String r) -> a -> Text -> Either String r
expectingEOF_ kont a t = case T.uncons t of
    Nothing     -> inline kont a
    Just (c, _) -> unexpectedChar c "end-of-input"
{-# INLINE expectingEOF_ #-}

unexpectedEOF :: String -> Either String r
unexpectedEOF expected = Left $ "Unexpected end-of-input, expecting " ++ expected

unexpectedChar :: Char -> String -> Either String r
unexpectedChar c expected = Left $ "Unexpected '" ++ c : "', expecting " ++ expected

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE fromChar #-}
fromChar :: Char -> Int
fromChar c = ord c .&. 0xf

{-# INLINE twoDigits #-}
twoDigits
    :: (Int -> Text -> Either String r)
    -> Text
    -> Either String r
twoDigits kont =
    unconsAscii (unexpectedEOF "a digit") $ \c1 -> if
        | '0' <= c1, c1 <= '9' -> unconsAscii (unexpectedEOF "a digit") $ \c2 -> if
            | '0' <= c2, c2 <= '9' -> inline kont (fromChar c1 * 10 + fromChar c2)
            | otherwise -> \_ -> unexpectedChar c2 "a digit"
        | otherwise -> \_ -> unexpectedChar c1 "a digit"

{-# INLINE munchDigits #-}
munchDigits
    :: (Text -> Either String r)
    -> (Text -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
munchDigits kontEOF kontC (Text arr off len) =
    munchDigits_ kontEOF kontC arr off off len

{-# INLINE munchDigits_ #-}
munchDigits_
    :: (Text -> Either String r)
    -> (Text -> Char -> Text -> Either String r)
    -> Array
    -> Int
    -> Int
    -> Int
    -> Either String r
munchDigits_ kontEOF kontC arr = loop where
    loop off0 off len = unconsAscii_ arr off len (inline kontEOF (Text arr off0 (off - off0))) $ \c off' len' -> if
        | '0' <= c, c <= '9' -> loop off0 off' len'
        | otherwise          -> inline kontC (Text arr off0 (off - off0)) c (Text arr off' len')

utcTimeZone :: Local.TimeZone
utcTimeZone = Local.TimeZone 0 False ""

-------------------------------------------------------------------------------
-- Implementation: Dates
-------------------------------------------------------------------------------

-- parse year: @[+-]YYYY@.
-- Two continuations as we look at the following character.
{-# INLINE parseYear_ #-}
parseYear_
    :: forall r. (Year -> Either String r)
    -> (Year -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
parseYear_ kontEOF kontC (Text arr offS lenS) = start offS lenS where
    start :: Int -> Int -> Either String r
    start !off !len = unconsAscii_ arr off len
        (unexpectedEOF "-, +, or a digit") $ \c off' len' -> case c of
            '-' -> loop negate off' off' len'
            '+' -> loop id     off' off' len'
            _
                | '0' <= c, c <= '9' -> loop id    off  off' len'
                | otherwise          -> Left $ "Unexpected '" ++ show c ++ ", expecting -, +, or a digit"

    loop :: (Integer -> Integer) -> Int -> Int -> Int -> Either String r
    loop !posNeg !off0 !off !len = unconsAscii_ arr off len (finishEOF posNeg off0 off) $ \c off' len' -> if
        | '0' <= c, c <= '9' -> loop posNeg off0 off' len'
        | otherwise          -> finishC posNeg c off0 off off' len'

    finishEOF :: (Integer -> Integer) -> Int -> Int -> Either String r
    finishEOF !posNeg !off0 !off
        | len0 >= 4
        = year `seq` kontEOF year

        | otherwise
        = Left "expected year with at least 4 digits"
      where
        len0 = off - off0
        year = posNeg (textToInteger (Text arr off0 len0))
    {-# INLINE finishEOF #-}

    finishC :: (Integer -> Integer) -> Char -> Int -> Int -> Int -> Int-> Either String r
    finishC !posNeg c !off0 !off !off' !len'
        | len0 >= 4
        = year `seq` kontC year c (Text arr off' len')

        | otherwise
        = Left "expected year with at least 4 digits"
      where
        len0 = off - off0
        year = posNeg (textToInteger (Text arr off0 len0))
    {-# INLINE finishC #-}

{-# INLINE parseYear__ #-}
-- parse year and the following dash: @[+-]YYYY-@
parseYear__
    :: forall r. (Year -> Text -> Either String r)
    -> Text
    -> Either String r
parseYear__ kont =
    parseYear_ (\_ -> unexpectedEOF "a dash after a year part") $ \ !y c t ->
    if c == '-'
    then kont y t
    else unexpectedChar c "a dash after a year part"

-- parse month: @[-+]YYYY-MM@
{-# INLINE parseMonth_ #-}
parseMonth_
    :: forall r. (Year -> Int -> Text -> Either String r)
    -> Text
    -> Either String r
parseMonth_ kont =
    parseYear__ $ \ !y ->
    twoDigits $ \ !m ->
    kont y m

-- parse day: @[-+]YYYY-MM-DD@
{-# INLINE parseDay_ #-}
parseDay_
    :: forall r. (Day -> Text -> Either String r)
    -> Text
    -> Either String r
parseDay_ kont =
    parseMonth_ $ \y m ->
    skipDash $
    twoDigits $ \d ->
    case fromGregorianValid y m d of
        Nothing   -> \_ -> Left $ "invalid day:" ++ show (y, m, d)
        Just !day -> inline kont day

-- parse quarter: @[+-]YYYY-QN@
{-# INLINE parseQuarter_ #-}
parseQuarter_
    :: forall r. (Year -> QuarterOfYear -> Text -> Either String r)
    -> Text
    -> Either String r
parseQuarter_ kont =
    parseYear__ $ \y ->
    parseQuarterOfYear_ $ \q ->
    inline kont y q

{-# INLINE parseQuarterOfYear_ #-}
parseQuarterOfYear_
    :: forall r. (QuarterOfYear -> Text -> Either String r)
    -> Text
    -> Either String r
parseQuarterOfYear_ kont =
    unconsAscii (unexpectedEOF "QuarterOfYear") $ \c -> if
        | 'Q' == c || 'q' == c -> unconsAscii (unexpectedEOF "Quarter digit") $ \c' -> case c' of
            '1' -> inline kont Q1
            '2' -> inline kont Q2
            '3' -> inline kont Q3
            '4' -> inline kont Q4
            _   -> \_ -> unexpectedChar c' "QuarterOfYear digit"

        | otherwise -> \_ -> unexpectedChar c "QuarterOfYear"

{-# INLINE skipDash #-}
skipDash
    :: forall r. (Text -> Either String r)
    -> Text
    -> Either String r
skipDash kont = unconsAscii (unexpectedEOF "a dash, -") $ \c ->
    if c == '-'
    then inline kont
    else \_ -> unexpectedChar c "a dash, -"

-------------------------------------------------------------------------------
-- Implementation: Time
-------------------------------------------------------------------------------

-- Parse time of day : @HH:MM[:SS[.SSS]]@
{-# INLINE parseTimeOfDay_ #-}
parseTimeOfDay_
    :: (Int -> Int -> Pico -> Either String r)
    -> (Int -> Int -> Pico -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
parseTimeOfDay_ kontEOF kontC =
    twoDigits $ \h ->
    skipColon $
    twoDigits $ \m -> unconsAscii (inline kontEOF h m 0) $ \ c ->
        if c == ':'
        then parseSeconds_ (inline kontEOF h m) (inline kontC h m)
        else inline kontC h m 0 c

{-# INLINE parseTimeOfDay__ #-}
parseTimeOfDay__
    :: (Local.TimeOfDay -> Either String r)
    -> (Local.TimeOfDay -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
parseTimeOfDay__ kontEOF kontC = parseTimeOfDay_
    (\h m s -> makeTimeOfDay h m s kontEOF)
    (\h m s c t -> makeTimeOfDay h m s $ \l -> inline kontC l c t)

{-# INLINE makeTimeOfDay #-}
makeTimeOfDay :: Int -> Int -> Pico -> (Local.TimeOfDay -> Either String r) -> Either String r
makeTimeOfDay h m s kont =
    if h < 24 && m < 60 && s < 61
    then inline kont (Local.TimeOfDay h m s)
    else Left $ "Invalid time of day:" ++ show (h,m,s)

-- Parse seconds: @SS.SSS@.
--
{-# INLINE parseSeconds_ #-}
parseSeconds_
    :: (Pico -> Either String r)
    -> (Pico -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
parseSeconds_ kontEOF kontC =
    twoDigits $ \real ->
    unconsAscii (inline kontEOF (fromIntegral real)) $ \c ->
    if c == '.'
    then munchDigits (\i -> makeSeconds real kontEOF i) (\i c' t -> makeSeconds real (\j -> inline kontC j c' t) i)
    else kontC (MkFixed $ toInteger real * pico) c

{-# INLINE makeSeconds #-}
makeSeconds :: Int -> (Pico -> Either String r) -> Text -> Either String r
makeSeconds real kont t@(Text _arr _off len)
    | len == 0
    = Left "Expecting at least one decimal after a dot"

    | len > 12
    = Left "Unexpectedly over twelve decimals"

    | otherwise
    = inline kont (MkFixed (toInteger real * pico + textToInteger t * 10 ^ expo))
  where
    expo = 12 - len

{-# INLINE parseTimeZone_ #-}
parseTimeZone_
    :: (Local.TimeZone -> Either String r)
    -> Text
    -> Either String r
parseTimeZone_ kont =
    inline unconsAscii (unexpectedEOF "timezone: Z, +HH:MM or -HH:MM") $ \c t ->
    parseTimeZone__ () (\_ -> inline kont) c t

pico :: Integer
pico = 1000000000000 -- 12 zeros

{-# INLINE parseTimeZone__ #-}
parseTimeZone__
    :: a -- "extra bit of state"
    -> (a -> Local.TimeZone -> Either String r)
    -> Char
    -> Text
    -> Either String r
parseTimeZone__ x kont c t0 = case c of
    '-' -> hhmm x negate t0
    '+' -> hhmm x id     t0
    'Z' -> expectingEOF_ (inline kont x) utcTimeZone t0
    _   -> unexpectedChar c "timezone: Z, +HH:MM or -HH:MM"
  where
    hhmm y posNeg =
        twoDigits $ \hh ->
        unconsAscii (withResult posNeg hh 0 (kont y)) $ \c1 -> case c1 of
            ':' ->
                twoDigits $ \mm ->
                expectingEOF_ (\mm' -> withResult posNeg hh mm' (kont y)) mm

            _ | '0' <= c1, c1 <= '9' ->
                unconsAscii (unexpectedEOF "a digit") $ \c2 ->
                    if '0' <= c2 && c2 <= '9'
                    then expectingEOF_ (\mm' -> withResult posNeg hh mm' (kont y)) (fromChar c1 * 10 + fromChar c2)
                    else \_ -> unexpectedChar c2 "a digit"

            _   -> \_ -> unexpectedChar c1 "colon or a digit"

    withResult :: (Int -> Int) -> Int -> Int -> (Local.TimeZone -> Either String b) -> Either String b
    withResult posNeg hh mm kontR =
        -- we accept hours <24 and minutes <60
        -- this is how grammar implies, and also how python, joda-time
        -- and clojure #inst literals seem to work.
        -- Java's java.time seems to restrict to -18..18: https://docs.oracle.com/javase/8/docs/api/java/time/ZoneOffset.html
        -- but that seems more arbitrary.
        if hh < 24 && mm < 60
        then kontR (Local.minutesToTimeZone (posNeg (hh * 60 + mm)))
        else Left $ "Invalid TimeZone:" ++ show (hh, mm)

{-# INLINE parseLocalTime_ #-}
parseLocalTime_
    :: (Local.LocalTime -> Either String r)
    -> (Local.LocalTime -> Char -> Text -> Either String r)
    -> Text
    -> Either String r
parseLocalTime_ kontEOF kontC =
    parseDay_ $ \d ->
    skipDaySeparator $
    parseTimeOfDay__
        (\l -> inline kontEOF (Local.LocalTime d l))
        (\l c t -> inline kontC (Local.LocalTime d l) c t)

{-# INLINE parseUTCTime_ #-}
parseUTCTime_
    :: (UTCTime -> Either String r)
    -> Text
    -> Either String r
parseUTCTime_ kont = parseZonedTime_ $ \zt -> inline kont (Local.zonedTimeToUTC zt)

{-# INLINE parseZonedTime_ #-}
parseZonedTime_
    :: (Local.ZonedTime -> Either String r)
    -> Text
    -> Either String r
parseZonedTime_ kont =
    parseLocalTime_ (\_ -> unexpectedEOF "timezone") $ \lt c ->
    parseZT kont lt c

{-# INLINE parseZT #-}
parseZT
    :: (Local.ZonedTime -> Either String r)
    -> Local.LocalTime
    -> Char -> Text -> Either String r
parseZT kont lt = parseTimeZone__ lt $ \lt' tz -> inline kont (Local.ZonedTime lt' tz)

{-# INLINE skipColon #-}
skipColon
    :: (Text -> Either String r)
    -> Text
    -> Either String r
skipColon kont = unconsAscii (unexpectedEOF "a colon, :") $ \c ->
    if c == ':'
    then inline kont
    else \_ -> unexpectedChar c "a colon, :"

{-# INLINE skipDaySeparator #-}
skipDaySeparator
    :: (Text -> Either String r)
    -> Text
    -> Either String r
skipDaySeparator kont = unconsAscii (unexpectedEOF "a day separator, T or space") $ \c ->
    if c == 'T' || c == ' '
    then inline kont
    else \_ -> unexpectedChar c "a day separator, T or space"
