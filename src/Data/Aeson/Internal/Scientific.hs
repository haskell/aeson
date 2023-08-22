{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Aeson.Internal.Scientific (
    scanScientific,
) where

import           Data.Integer.Conversion (textToInteger)
import           Data.Scientific         (Scientific)
import           Data.Text               (Text)

import qualified Data.Scientific         as Sci
import qualified Data.Text               as T

-- | Parse 'Scientific' number from 'Text'.
--
-- This is different from how JSON numbers are parsed: arbitrary leading zeroes are accepted.
--
scanScientific
    :: forall r. (Scientific -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanScientific kont err input0 = case T.uncons input0 of
    Nothing -> errEnd
    Just (c, text')
        | c == '+'  -> scanScientific' kont err text'
        | c == '-'  -> scanScientific' (\sci -> kont (negate sci)) err text'
        | otherwise -> scanScientific' kont err input0
  where
    errEnd   = err "Unexpected end-of-input while parsing number literal"

scanScientific'
    :: forall r. (Scientific -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanScientific' kont err input0 = state_start input0 where
    state_start :: Text -> r
    state_start !text = case T.uncons text of
        Nothing                      -> errEnd
        Just (c, text')
            | '0' <= c, c <= '9'     -> state_i 1 text'
            | otherwise              -> err $ "Unexpected " ++ show c ++ " while parsing number literal"

    state_i :: Int -> Text -> r
    state_i !n !text = case T.uncons text of
        Nothing                      -> kont (fromInteger int) text
        Just (c, text')
            | '0' <= c, c <= '9'     -> state_i (n + 1) text'
            | '.' == c               -> go_dec int text'
            | 'e' == c || 'E' == c   -> go_sci int 0 text'
            | otherwise              -> kont (fromInteger int) text
      where
        int = textToInteger (T.take n input0)

    go_dec :: Integer -> Text -> r
    go_dec !int !text1 = case T.uncons text1 of
        Nothing                       -> errEnd
        Just (c, text')
            | '0' <= c, c <= '9'      -> state_dec 1 text'
            | otherwise               -> err $ "Unexpected " ++ show c ++ " while parsing number literal"
      where
        state_dec :: Int -> Text -> r
        state_dec !n !text = case T.uncons text of
            Nothing                      -> kont dec text
            Just (c, text')
                | '0' <= c, c <= '9'     -> state_dec (n + 1) text'
                | 'e' == c || 'E' == c   -> go_sci coef (negate n) text'
                | otherwise              -> kont dec text
          where
            frac = textToInteger (T.take n text1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> Text -> r
    go_sci !coef !exp10 !text2 = case T.uncons text2 of
        Nothing                       -> errEnd
        Just (c, text')
            | '0' <= c, c <= '9'      -> go_sci_pos coef exp10 text2 1 text'
            | '+' == c                -> case T.uncons text' of
                Nothing               -> errEnd
                Just (c', text'')
                    | '0' <= c', c' <= '9'  -> go_sci_pos coef exp10 text' 1 text''
                    | otherwise       -> errUnx c'
            | '-' == c                -> case T.uncons text' of
                Nothing               -> errEnd
                Just (c', text'')
                    | '0' <= c', c' <= '9'  -> go_sci_neg coef exp10 text' 1 text''
                    | otherwise       -> errUnx c'
            | otherwise               -> errUnx c

    go_sci_pos :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_pos !coef !exp10 !text2 !n !text = case T.uncons text of
        Nothing                       -> kont sci text
        Just (c, text')
            | '0' <= c, c <= '9'      -> go_sci_pos coef exp10 text2 (n + 1) text'
            | otherwise               -> kont sci text
      where
        exp10' = fromInteger (textToInteger (T.take n text2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_neg !coef !exp10 !text2 !n !text = case T.uncons text of
        Nothing                       -> kont sci text
        Just (c, text')
            | '0' <= c, c <= '9'  -> go_sci_neg coef exp10 text2 (n + 1) text'
            | otherwise               -> kont sci text
      where
        exp10' = fromInteger (textToInteger (T.take n text2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd   = err "Unexpected end-of-input while parsing number literal"
    errUnx c = err $ "Unexpected " ++ show c ++ " while parsing number literal"
