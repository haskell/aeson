{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -O2 #-}
-- | Parser from strict 'Text' to 'Tokens'.
module Data.Aeson.Decoding.Text (
    textToTokens,
) where

import           Data.Char                            (chr)
import           Data.Integer.Conversion              (textToInteger)
import           Data.Text.Internal                   (Text (..))

import qualified Data.Aeson.Key                       as Key
import qualified Data.Scientific                      as Sci
import qualified Data.Text                            as T
import qualified Data.Text.Array                      as A

import           Data.Aeson.Decoding.Internal
import           Data.Aeson.Decoding.Tokens
import           Data.Aeson.Internal.Prelude
import           Data.Aeson.Internal.UnescapeFromText (unescapeFromText)

#if MIN_VERSION_text(2,0,0)
import qualified Data.Word8.Patterns as W
#else
import qualified Data.Word16.Patterns as W
#endif

#if MIN_VERSION_text(2,0,0)
type Point = Word8
#else
type Point = Word16
#endif


-- | Lex (and parse) strict 'ByteString' into 'Tokens' stream.
--
-- @since 2.2.1.0
--
textToTokens :: Text -> Tokens Text String
textToTokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT (skipSpace -> bs) k = case unconsPoint bs of
        Nothing         -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k

    tokenCase
        :: Point              -- head
        -> Text               -- tail
        -> Text               -- whole input, needed for number parsing
        -> (Text -> k)        -- continuation
        -> Tokens k String
    tokenCase W.LEFT_CURLY   !bs !_   k       = TkRecordOpen (goR bs k)
    tokenCase W.LEFT_SQUARE   bs  _   k       = TkArrayOpen (goA bs k)
    tokenCase W.DOUBLE_QUOTE  bs  _   k       = scanStringLiteral (\t bs' -> TkText t (k bs')) tkErr bs
    tokenCase W.HYPHEN        bs  _   k       = scanNumberLiteral (\n bs' -> TkNumber (negateNumber n) (k bs')) tkErr bs
    tokenCase w                _   wbs k
        | W.DIGIT_0 <= w, w <= W.DIGIT_9      = scanNumberLiteral (\n bs' -> TkNumber n (k bs')) tkErr wbs
    tokenCase W.LOWER_N       bs  _   k
        | Just bs1 <- stripPrefix "ull" 3 bs  = TkLit LitNull (k bs1)
    tokenCase W.LOWER_T       bs  _   k
        | Just bs1 <- stripPrefix "rue" 3 bs  = TkLit LitTrue (k bs1)
    tokenCase W.LOWER_F       bs  _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)
    tokenCase _          _   wbs _            = tkErr $ "Unexpected " ++ showBeginning wbs ++ ", expecting JSON value"
    -- Array
    goA :: Parser TkArray k
    goA (skipSpace -> bs) k = case unconsPoint bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (W.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k

    goA1 :: Parser TkArray k
    goA1 (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                      -> tkErrEOF ", or ]"
        Just (W.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (W.COMMA, !bs1)        -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _                            -> tkErrBS bs ", or ]"

    -- Record
    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    -- after record pair, expecting ," or }
    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W.COMMA, !bs1) -> case unconsPoint (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    -- key of record (after double quote)
    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    -- after key of a record, expecting :
    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case T.uncons bs of
        Nothing          -> tkErrEOF ":"
        Just (':', !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _           -> tkErrBS bs ":"

stripPrefix :: Text -> Int -> Text -> Maybe Text
stripPrefix pfx _ bs = T.stripPrefix pfx bs
{-# INLINE stripPrefix #-}

type Parser tk k = Text -> (Text -> k) -> tk k String

showBeginning :: Text -> String
showBeginning = show . T.take 30

-- | Strip leading (ASCII) space
skipSpace :: Text -> Text
skipSpace = T.dropWhile $ \w -> w == '\x20' || w == '\x0a' || w == '\x0d' || w == '\x09'
{-# INLINE skipSpace #-}

tkErrEOF :: AsError t => String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected
{-# INLINE tkErrEOF #-}

tkErrBS :: AsError t => Text -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
{-# INLINE tkErrBS #-}

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

scanStringLiteral
    :: forall r. (Text -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    -- the length is counted in bytes.
    go :: Int -> Text -> r
    go !n !bs = case unconsPoint bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeTakePoints n bs0) (unsafeDropPoints (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            -- we don't need to check for @>= 0x80@ chars, as text is valid unicode.
            | otherwise  -> go (n + 1) bs'

    -- in goEsc and goSlash we don't need to check for control characters as unescapeText verifies that.
    goEsc :: Int -> Text -> r
    goEsc !n !bs = case unconsPoint bs of
        Nothing        -> errEnd
        Just (34, _)   -> case unescapeFromText (unsafeTakePoints n bs0) of
            Right t -> ok t (unsafeDropPoints (n + 1) bs0)
            Left e  -> err (show e)
        Just (92, bs') -> goSlash (n + 1) bs'
        Just (_,  bs') -> goEsc (n + 1) bs'

    goSlash :: Int -> Text -> r
    goSlash !n !bs = case unconsPoint bs of
        Nothing       -> errEnd
        Just (_, bs') -> goEsc (n + 1) bs'

    errEnd = err "Unexpected end-of-input while parsing string literal"
    errCC  = err "Unespected control character while parsing string literal"

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------

--
-- number   := integer fraction exponent
-- integer  := 0 | [1-9][0-9]* | -0 | -[1-9][0-9]*
-- fraction := "" | . [0-9]+
-- exponent := "" | E sign [0-9]+ | e sign [0-9]+
-- sign     := "" | - | +
--
-- This scanner doesn't recognize the leading minus sign, we recognize only integer := 0 | [1-9][0-9]*,
-- as the minus sign is recognized by outer scanner already.
--
scanNumberLiteral
    :: forall r. (Number -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanNumberLiteral kont err bs0 = state_start bs0 where
    state_start :: Text -> r
    state_start !bs = case unconsPoint bs of
        Nothing                                   -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 < w8, w8 <= W.DIGIT_9     -> state_i1 1 bs'
            | W.DIGIT_0 == w8                     -> state_after0 bs'
            | otherwise                           -> errUnx w8

    state_after0 :: Text -> r
    state_after0 !bs = case unconsPoint bs of
        Nothing                                   -> kont (NumInteger 0) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> err "Number literal with leading zero"
            | W.PERIOD == w8                      -> go_dec 0 bs'
            | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci 0 0 bs'
            | otherwise                           -> kont (NumInteger 0) bs

    state_i1 :: Int -> Text -> r
    state_i1 !n !bs = case unconsPoint bs of
        Nothing                                   -> kont (NumInteger int) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_i1 (n + 1) bs'
            | W.PERIOD == w8                      -> go_dec int bs'
            | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci int 0 bs'
            | otherwise                           -> kont (NumInteger int) bs
      where
        int = textToInteger (unsafeTakePoints n bs0)

    go_dec :: Integer -> Text -> r
    go_dec !int !bs1 = case unconsPoint bs1 of
        Nothing                                   -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_dec 1 bs'
            | otherwise                           -> errUnx w8
      where
        state_dec :: Int -> Text -> r
        state_dec !n !bs = case unconsPoint bs of
            Nothing                                   -> kont (NumDecimal dec) bs
            Just (w8, bs')
                | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_dec (n + 1) bs'
                | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci coef (negate n) bs'
                | otherwise                           -> kont (NumDecimal dec) bs
          where
            frac = textToInteger (unsafeTakePoints n bs1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> Text -> r
    go_sci !coef !exp10 !bs2 = case unconsPoint bs2 of
        Nothing                                           -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9            -> go_sci_pos coef exp10 bs2 1 bs'
            | W.PLUS == w8 -> case unconsPoint bs' of
                Nothing                                   -> errEnd
                Just (w8', bs'')
                    | W.DIGIT_0 <= w8', w8' <= W.DIGIT_9  -> go_sci_pos coef exp10 bs' 1 bs''
                    | otherwise                           -> errUnx w8'
            | W.HYPHEN == w8 -> case unconsPoint bs' of
                Nothing                                   -> errEnd
                Just (w8', bs'')
                    | W.DIGIT_0 <= w8', w8' <= W.DIGIT_9  -> go_sci_neg coef exp10 bs' 1 bs''
                    | otherwise                           -> errUnx w8'
            | otherwise                                   -> errUnx w8

    go_sci_pos :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_pos !coef !exp10 !bs2 !n !bs = case unconsPoint bs of
        Nothing                                 -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9  -> go_sci_pos coef exp10 bs2 (n + 1) bs'
            | otherwise                         -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (textToInteger (unsafeTakePoints n bs2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_neg !coef !exp10 !bs2 !n !bs = case unconsPoint bs of
        Nothing                                 -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9  -> go_sci_neg coef exp10 bs2 (n + 1) bs'
            | otherwise                         -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (textToInteger (unsafeTakePoints n bs2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd    = err "Unexpected end-of-input while parsing number literal"
    errUnx w8 = err $ "Unexpected " ++ show (chr (fromIntegral w8)) ++ " while parsing number literal"

-------------------------------------------------------------------------------
-- Unsafe primitives
-------------------------------------------------------------------------------

{-# INLINE unconsPoint #-}
-- Uncons a primitive unit of storage from text.
-- The left-over 'Text' value may be invalid.
unconsPoint :: Text -> Maybe (Point, Text)
unconsPoint (Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just (w8, Text arr (off + 1) (len - 1))
  where
    w8 = A.unsafeIndex arr off

unsafeTakePoints :: Int -> Text -> Text
unsafeTakePoints n (Text arr off _len) = Text arr off n
{-# INLINE unsafeTakePoints #-}

unsafeDropPoints :: Int -> Text -> Text
unsafeDropPoints n (Text arr off len) = Text arr (off + n) (len - n)
{-# INLINE unsafeDropPoints #-}
