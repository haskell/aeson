{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -O2 #-}
-- | Parser from strict 'ByteString' to 'Tokens'.
module Data.Aeson.Decoding.ByteString (
    bsToTokens,
) where

import           Data.ByteString              (ByteString)
import           Data.Char                    (chr)
import           Data.Integer.Conversion      (byteStringToInteger)
import           Data.Text                    (Text)
import           Data.Word                    (Word8)

import qualified Data.Aeson.Key               as Key
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BS.Unsafe
import qualified Data.Scientific              as Sci

import           Data.Aeson.Decoding.Internal
import           Data.Aeson.Decoding.Tokens
import           Data.Aeson.Internal.Text     (unsafeDecodeASCII)
import           Data.Aeson.Internal.Unescape (unescapeText)
import           Data.Aeson.Internal.Word8

-- | Lex (and parse) strict 'ByteString' into 'Tokens' stream.
--
-- @since 2.1.2.0
--
bsToTokens :: ByteString -> Tokens ByteString String
bsToTokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT (skipSpace -> bs) k = case BS.uncons bs of
        Nothing         -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k

    tokenCase
        :: Word8              -- head
        -> ByteString         -- tail
        -> ByteString         -- whole input, needed for number parsing
        -> (ByteString -> k)  -- continuation
        -> Tokens k String
    tokenCase W8_OPEN_CURLY   !bs !_   k      = TkRecordOpen (goR bs k)
    tokenCase W8_OPEN_SQUARE   bs  _   k      = TkArrayOpen (goA bs k)
    tokenCase W8_DOUBLE_QUOTE  bs  _   k      = scanStringLiteral (\t bs' -> TkText t (k bs')) tkErr bs
    tokenCase W8_MINUS         bs  _   k      = scanNumberLiteral (\n bs' -> TkNumber (negateNumber n) (k bs')) tkErr bs
    tokenCase w                _   wbs k
        | W8_0 <= w, w <= W8_9                = scanNumberLiteral (\n bs' -> TkNumber n (k bs')) tkErr wbs
    tokenCase W8_n             bs  _   k
        | Just bs1 <- stripPrefix "ull" 3 bs  = TkLit LitNull (k bs1)
    tokenCase W8_t             bs  _   k
        | Just bs1 <- stripPrefix "rue" 3 bs  = TkLit LitTrue (k bs1)
    tokenCase W8_f             bs  _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)
    tokenCase _                _   wbs _      = tkErr $ "Unexpected " ++ showBeginning wbs ++ ", expecting JSON value"

    -- Array
    goA :: Parser TkArray k
    goA (skipSpace -> bs) k = case BS.uncons bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (W8_CLOSE_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k

    goA1 :: Parser TkArray k
    goA1 (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                      -> tkErrEOF ", or ]"
        Just (W8_CLOSE_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (W8_COMMA, !bs1)        -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _                            -> tkErrBS bs ", or ]"

    -- Record
    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W8_DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W8_CLOSE_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    -- after record pair, expecting ," or }
    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W8_COMMA, !bs1) -> case BS.uncons (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W8_DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W8_CLOSE_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    -- key of record (after double quote)
    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    -- after key of a record, expecting :
    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case BS.uncons bs of
        Nothing               -> tkErrEOF ":"
        Just (W8_COLON, !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _                -> tkErrBS bs ":"

stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
stripPrefix pfx n bs | BS.isPrefixOf pfx bs = Just (BS.Unsafe.unsafeDrop n bs)
                     | otherwise            = Nothing
{-# INLINE stripPrefix #-}

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . BS.take 30

-- | Strip leading (ASCII) space
skipSpace :: ByteString -> ByteString
skipSpace = BS.dropWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected
{-# INLINE tkErrEOF #-}

tkErrBS :: AsError t => ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
{-# INLINE tkErrBS #-}

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

scanStringLiteral
    :: forall r. (Text -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    go :: Int -> ByteString -> r
    go !n !bs = case BS.uncons bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeDecodeASCII (BS.Unsafe.unsafeTake n bs0)) (BS.Unsafe.unsafeDrop (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            | w8 >= 0x80 -> goEsc (n + 1) bs'
            | otherwise  -> go (n + 1) bs'

    -- in goEsc and goSlash we don't need to check for control characters as unescapeText verifies that.
    goEsc :: Int -> ByteString -> r
    goEsc !n !bs = case BS.uncons bs of
        Nothing        -> errEnd
        Just (34, _)   -> case unescapeText (BS.Unsafe.unsafeTake n bs0) of
            Right t -> ok t (BS.drop (n + 1) bs0)
            Left e  -> err (show e)
        Just (92, bs') -> goSlash (n + 1) bs'
        Just (_,  bs') -> goEsc (n + 1) bs'

    goSlash :: Int -> ByteString -> r
    goSlash !n !bs = case BS.uncons bs of
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
    :: forall r. (Number -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanNumberLiteral kont err bs0 = state_start bs0 where
    state_start :: ByteString -> r
    state_start !bs = case BS.uncons bs of
        Nothing                      -> errEnd
        Just (w8, bs')
            | W8_0 < w8, w8 <= W8_9  -> state_i1 1 bs'
            | W8_0 == w8             -> state_after0 bs'
            | otherwise              -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"

    state_after0 :: ByteString -> r
    state_after0 !bs = case BS.uncons bs of
        Nothing                         -> kont (NumInteger 0) bs
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9    -> err "Number literal with leading zero"
            | W8_DOT == w8              -> go_dec 0 bs'
            | W8_e == w8 || W8_E == w8  -> go_sci 0 0 bs'
            | otherwise                 -> kont (NumInteger 0) bs

    state_i1 :: Int -> ByteString -> r
    state_i1 !n !bs = case BS.uncons bs of
        Nothing                         -> kont (NumInteger int) bs
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9    -> state_i1 (n + 1) bs'
            | W8_DOT == w8              -> go_dec int bs'
            | W8_e == w8 || W8_E == w8  -> go_sci int 0 bs'
            | otherwise                 -> kont (NumInteger int) bs
      where
        int = byteStringToInteger (BS.Unsafe.unsafeTake n bs0)

    go_dec :: Integer -> ByteString -> r
    go_dec !int !bs1 = case BS.uncons bs1 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9  -> state_dec 1 bs'
            | otherwise               -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"
      where
        state_dec :: Int -> ByteString -> r
        state_dec !n !bs = case BS.uncons bs of
            Nothing                         -> kont (NumDecimal dec) bs
            Just (w8, bs')
                | W8_0 <= w8, w8 <= W8_9    -> state_dec (n + 1) bs'
                | W8_e == w8 || W8_E == w8  -> go_sci coef (negate n) bs'
                | otherwise                 -> kont (NumDecimal dec) bs
          where
            frac = byteStringToInteger (BS.Unsafe.unsafeTake n bs1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> ByteString -> r
    go_sci !coef !exp10 !bs2 = case BS.uncons bs2 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9  -> go_sci_pos coef exp10 bs2 1 bs'
            | W8_PLUS == w8           -> case BS.uncons bs' of
                Nothing               -> errEnd
                Just (w8', bs'')
                    | W8_0 <= w8', w8' <= W8_9  -> go_sci_pos coef exp10 bs' 1 bs''
                    | otherwise       ->  errUnx w8'
            | W8_MINUS == w8          -> case BS.uncons bs' of
                Nothing               -> errEnd
                Just (w8', bs'')
                    | W8_0 <= w8', w8' <= W8_9  -> go_sci_neg coef exp10 bs' 1 bs''
                    | otherwise       ->  errUnx w8'
            | otherwise               -> errUnx w8

    go_sci_pos :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_pos !coef !exp10 !bs2 !n !bs = case BS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9  -> go_sci_pos coef exp10 bs2 (n + 1) bs'
            | otherwise               -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (BS.Unsafe.unsafeTake n bs2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_neg !coef !exp10 !bs2 !n !bs = case BS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8_0 <= w8, w8 <= W8_9  -> go_sci_neg coef exp10 bs2 (n + 1) bs'
            | otherwise               -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (BS.Unsafe.unsafeTake n bs2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd    = err "Unexpected end-of-input while parsing number literal"
    errUnx w8 = err $ "Unexpected " ++ show (chr (fromIntegral w8)) ++ " while parsing number literal"
