{-# LANGUAGE BangPatterns, OverloadedStrings, RankNTypes #-}
module Data.Aeson.Stream (
    Token (..),
    TokenStream,
    tokenStream,
    decodeWith,
    eitherDecodeWith,
    decodeValue,
    parseValue,
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Data.Aeson.Parser.Internal (jstring_, scientific)
import Data.Aeson.Types.Internal (IResult(..), JSONPath, Result(..), Value(..))
import Data.Scientific (Scientific)
import Data.Text (Text)

import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Vector as V

-- import Debug.Trace

-------------------------------------------------------------------------------
-- Token
-------------------------------------------------------------------------------

data Token
    = TkNull
    | TkTrue
    | TkFalse
    | TkArrayOpen
    | TkArrayClose
    | TkObjectOpen
    | TkObjectClose
    | TkKey !Text     -- ^ object keys use separate token
    | TkText !Text
    | TkNumber !Scientific
    | TkError String  -- ^ error
  deriving (Eq, Show)

instance NFData Token where
    rnf TkNull        = ()
    rnf TkTrue        = ()
    rnf TkFalse       = ()
    rnf TkArrayOpen   = ()
    rnf TkArrayClose  = ()
    rnf TkObjectOpen  = ()
    rnf TkObjectClose = ()
    rnf (TkKey t)     = rnf t
    rnf (TkText t)    = rnf t
    rnf (TkNumber s)  = rnf s
    rnf (TkError err) = rnf err

type TokenStream = [Token]

-------------------------------------------------------------------------------
-- Token recognize
-------------------------------------------------------------------------------

newtype TokenParser = TokenParser
    { runTokenParser :: [TokenParser] -> BSL.ByteString -> [Token] }

-- | Parse input into a token stream.
tokenStream :: BSL.ByteString -> [Token]
tokenStream = runTokenParser value [] . skipSpace
  where
    end :: TokenParser
    end = TokenParser $ \ !_ !bs ->
        if BSL.null bs
        then []
        else [TkError $ "Expecting end-of-input, got " ++ BSL8.unpack (BSL.take 30 bs)]

    value :: TokenParser
    value = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting JSON value, got end-of-input"]
        Just (!w, !bs') -> valueCase ks w bs bs'

    {-# INLINE valueCase #-}
    valueCase !ks !w !bs !bs' = case w of
            123 {- { -} -> TkObjectOpen : runTokenParser record0 (recordK : ks) (skipSpace bs')
            91  {- [ -} -> TkArrayOpen : runTokenParser array0 (arrayK : ks) (skipSpace bs')
            34  {- " -} -> case A.parse jstring_ bs' of
                A.Fail _ _ err -> [TkError err]
                A.Done bs'' t  -> TkText t : pop ks bs''
            116 {- t -} | "rue" `BSL.isPrefixOf` bs' ->
                TkTrue : pop ks (BSL.drop 3 bs')
            102 {- f -} | "alse" `BSL.isPrefixOf` bs' ->
                TkFalse : pop ks (BSL.drop 4 bs')
            110 {- n -} | "ull" `BSL.isPrefixOf` bs' ->
                TkNull : pop ks (BSL.drop 3 bs')
            _ | 48 <= w && w <= 75 || w == 45 -> case A.parse scientific bs of
                A.Fail _ _ err -> [TkError err]
                A.Done bs'' s  -> TkNumber s : pop ks bs''
            _ -> [TkError $ "Expecting JSON value, got " ++ BSL8.unpack (BSL.take 30 bs)]

    record0 :: TokenParser
    record0 = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting record key, got end-of-input"]
        Just (!w, !bs') -> case w of
            34 {- " -}  -> case A.parse jstring_ bs' of
                A.Fail _ _ err -> [TkError err]
                A.Done bs'' t  -> TkKey t : runTokenParser recordV ks (skipSpace bs'')
            125 {- } -} -> TkObjectClose : pop (tail ks) bs'
            _ -> [TkError $ "Expecting record key or '}', got " ++ BSL8.unpack (BSL.take 30 bs)]

    record1 :: TokenParser
    record1 = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting record key, got end-of-input"]
        Just (!w, !bs') -> case w of
            34 {- " -}  -> case A.parse jstring_ bs' of
                A.Fail _ _ err -> [TkError err]
                A.Done bs'' t  -> TkKey t : runTokenParser recordV ks (skipSpace bs'')
            _ -> [TkError $ "Expecting record key, got " ++ BSL8.unpack (BSL.take 30 bs)]

    recordV :: TokenParser
    recordV = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting ':', got end-of-input"]
        Just (!w, !bs') -> case w of
            58 {- : -} -> runTokenParser value ks (skipSpace bs')
            _ -> [TkError $ "Expecting ':', got " ++ BSL8.unpack (BSL.take 30 bs)]

    recordK  :: TokenParser
    recordK = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting ',' or '}', got end-of-input"]
        Just (!w, !bs') -> case w of
            44  {- , -} -> runTokenParser record1 (recordK : ks) (skipSpace bs')
            125 {- } -} -> TkObjectClose : pop ks bs'
            _ -> [TkError $ "Expecting ',' or '}', got " ++ BSL8.unpack (BSL.take 30 bs)]

    array0 :: TokenParser
    array0 = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting JSON value or ']', got end-of-input"]
        Just (!w, !bs') -> case w of
            93 {- ] -} -> TkArrayClose : pop (tail ks) bs'
            _          -> valueCase ks w bs bs'

    arrayK  :: TokenParser
    arrayK = TokenParser $ \ !ks !bs -> case BSL.uncons bs of
        Nothing -> [TkError "Expecting ',' or ']', got end-of-input"]
        Just (!w, !bs') -> case w of
            44 {- , -} -> runTokenParser value (arrayK : ks) (skipSpace bs')
            93 {- ] -} -> TkArrayClose : pop ks bs'
            _ -> [TkError $ "Expecting ',' or ']', got " ++ BSL8.unpack (BSL.take 30 bs)]

    pop :: [TokenParser] -> BSL.ByteString -> [Token]
    pop [] bs     = runTokenParser end [] (skipSpace bs)
    pop (k:ks) bs = runTokenParser k ks (skipSpace bs)

-- , 44
-- : 58
-- " 34
-- n 110
-- t 116
-- f 102
-- - 45
-- 0 48
-- 9 57

skipSpace :: BSL.ByteString -> BSL.ByteString
skipSpace bs = case BSL.uncons bs of
    Just (w, bs') | w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
        -> skipSpace bs'
    _ -> bs
{-# INLINE skipSpace #-}

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

parseValue :: TokenStream -> Either String Value
parseValue stream = runP value stream end
  where
    end x []      = Right x
    end _ (t : _) = Left $ "Unconsumed input: " ++ show t

    token :: P Token
    token = P $ \ts k -> case ts of
        []        -> Left "Unexpected end-of-input"
        (t : ts') -> k t ts'

    peekToken :: P Token
    peekToken = P $ \ts k -> case ts of
        []      -> Left "Unexpected end-of-input"
        (t : _) -> k t ts

    value :: P Value
    value = do
        t <- token
        case t of
            TkObjectOpen -> fmap Aeson.object record
            TkArrayOpen  -> fmap mkArray array
            TkText x     -> pure (String x)
            TkNumber x   -> pure (Number x)
            TkNull       -> pure Null
            TkTrue       -> pure (Bool True)
            TkFalse      -> pure (Bool False)
            _ -> fail $ "Expecting JSON value, got token " ++ show t

    record :: P [Aeson.Pair]
    record = do
        t <- token
        case t of
            TkKey k -> do
                v <- value
                fmap (k Aeson..= v :) record
            TkObjectClose -> pure []
            _ -> fail $ "Expecting record key or '}', got token " ++ show t

    array :: P [Value]
    array = do
        t <- peekToken
        case t of
            TkArrayClose -> token >> pure []
            _ -> (:) <$> value <*> array

    mkArray :: [Value] -> Value
    mkArray = Array . V.fromList

decodeValue :: BSL.ByteString -> Either String Value
decodeValue = parseValue . tokenStream

decodeWith :: unused -> (Value -> Result a) -> BSL.ByteString -> Maybe a
decodeWith _ to s = case decodeValue s of
    Left _  -> Nothing
    Right v -> case to v of
        Success a -> Just a
        _         -> Nothing
{-# INLINE decodeWith #-}

eitherDecodeWith :: unused -> (Value -> IResult a) -> BSL.ByteString -> Either (JSONPath, String) a
eitherDecodeWith _ to s = case decodeValue s of
    Left err -> Left ([], err)
    Right v -> case to v of
        ISuccess a      -> Right a
        IError path msg -> Left (path, msg)
{-# INLINE eitherDecodeWith #-}

-------------------------------------------------------------------------------
-- LL(1)
-------------------------------------------------------------------------------

-- | LL(1) Parser to make TokenStream -> Value transform.
newtype P a = P
    { runP :: forall b. TokenStream
    -> (a -> TokenStream -> Either String b)
    -> Either String b
    }

instance Functor P where
    fmap f (P p) = P $ \ts k -> p ts (k . f)

instance Applicative P where
    pure x = P $ \ts k -> k x ts
    (<*>) = ap

instance Monad P where
    return = pure
    P p >>= f = P $ \ts k -> p ts $ \a ts' -> runP (f a) ts' k

    fail err = P $ \_ _ -> Left err
