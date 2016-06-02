#!/usr/bin/env stack
{- stack --resolver=lts-5.9 runghc
      --package mtl
      --package dlist
-}

import Control.Monad (forM_)
import Control.Monad.State.Strict
import Control.Monad.Writer (MonadWriter (..))
import Data.Char (toUpper)
import Data.Foldable (fold, traverse_)
import Data.List (intercalate)
import Data.Semigroup ((<>))

import qualified Data.DList as DList

variables :: [String]
variables = map (:[]) $ ['a'..'z']

t :: [String] -> State (DList.DList String) ()
t xs = traverse_ f xs *> f "\n"
  where
    f :: String -> State (DList.DList String) ()
    f x = state (\s -> ((), mappend s $ DList.singleton x))

commaSep :: [String] -> String
commaSep vs = "(" <> intercalate ", " vs <> ")"

tuple :: Int -> String
tuple n = fold $ flip execState DList.empty $ do
    -- ToJSON2
    t ["instance ", toJsonContext2, "ToJSON2 (", tupleConstr, " ", vs2, ") where" ]
    t ["    liftToJSON2 ", toPrev, " _ ", toLast, " _ ", vs', " = Array $ V.create $ do" ]
    t ["        mv <- VM.unsafeNew ", show n ]
    forM_ (zip [0..] vs) $ \(i, v) ->
        let to = case i of
                      _ | i == n - 1 -> toLast
                        | i == n - 2 -> toPrev
                        | otherwise  -> "toJSON"
        in t [ "        VM.unsafeWrite mv ", show i, " (", to, " ", v, ")" ]
    t ["        return mv" ]
    t ["    {-# INLINE liftToJSON2 #-}" ]
    t []

    t ["    liftToEncoding2 ", toPrev, " _ ", toLast, " _ ", vs', " = tuple $" ]
    forM_ (zip [0..] vs) $ \(i, v) -> t . ("        " :) $ case i of

        _ | i == n - 1 -> [ "fromEncoding (", toLast, " ", v, ")" ]
        _ | i == n - 2 -> [ "fromEncoding (", toPrev, " ", v, ") >*<" ]
        _ | otherwise  -> [ "builder ", v, " >*<" ]
    t ["    {-# INLINE liftToEncoding2 #-}" ]
    t []

    -- ToJSON1
    t ["instance ", toJsonContext1, "ToJSON1 (", tupleConstr, " ", vs1, ") where" ]
    t ["    liftToJSON = liftToJSON2 toJSON toJSONList" ]
    t ["    {-# INLINE liftToJSON #-}" ]
    t ["    liftToEncoding = liftToEncoding2 toEncoding toEncodingList" ]
    t ["    {-# INLINE liftToEncoding #-}" ]
    t []

    -- ToJSON
    t [ "instance ", toJsonContext, " => ToJSON ", vs', " where" ]
    t [ "    toJSON = toJSON2" ]
    t [ "    {-# INLINE toJSON #-}" ]
    t [ "    toEncoding = toEncoding2" ]
    t [ "    {-# INLINE toEncoding #-}" ]
    t []

    -- FromJSON2
    t ["instance ", fromJsonContext2, "FromJSON2 (", tupleConstr, " ", vs2, ") where" ]
    t ["    liftParseJSON2 ", pPrev, " _ ", pLast, " _ = withArray \"", vs', "\" $ \\t -> " ]
    t ["        let n = V.length t" ]
    t ["        in if n == ", show n ]
    t ["            then ", tupleConstr ]
    forM_ (zip [0..] vs) $ \(i, v) -> do
        let op = if i == 0 then "<$>" else "<*>"
        let p = case i of
                      _ | i == n - 1 -> pLast
                        | i == n - 2 -> pPrev
                        | otherwise  -> "parseJSON"
        t ["                ", op, " parseJSONElemAtIndex ", p, " ", show i, " t" ]
    t ["            else fail $ \"cannot unpack array of length \" ++ show n ++ \" into a tuple of length ", show n, "\"" ]
    t ["    {-# INLINE liftParseJSON2 #-}" ]
    t []

    -- FromJSON1
    t ["instance ", fromJsonContext1, "FromJSON1 (", tupleConstr, " ", vs1, ") where" ]
    t ["    liftParseJSON = liftParseJSON2 parseJSON parseJSONList" ]
    t ["    {-# INLINE liftParseJSON #-}" ]
    t []

    -- FeomJSON
    t [ "instance ", fromJsonContext, " => FromJSON ", vs', " where" ]
    t [ "    parseJSON = parseJSON2" ]
    t [ "    {-# INLINE parseJSON #-}" ]
    t []

    t []
  where
    vs = take n variables
    vs' = commaSep vs
    toJsonContext = commaSep $ map ("ToJSON " <>) vs
    fromJsonContext = commaSep $ map ("FromJSON " <>) vs

    toJsonContext2
        | n <= 2 = ""
        | otherwise = (commaSep $ map ("ToJSON " <>) $ take (n - 2) vs) <> " => "
    fromJsonContext2
        | n <= 2 = ""
        | otherwise = (commaSep $ map ("FromJSON " <>) $ take (n - 2) vs) <> " => "
    vs2 = intercalate " " $ take (n - 2) vs 

    toJsonContext1 = (commaSep $ map ("ToJSON " <>) $ take (n - 1) vs) <> " => "
    fromJsonContext1 = (commaSep $ map ("FromJSON " <>) $ take (n - 1) vs) <> " => "
    vs1 = intercalate " " $ take (n - 1) vs 

    toLast = "to" <> map toUpper (vs !! (n - 1))
    toPrev = "to" <> map toUpper (vs !! (n - 2))
    pLast = "p" <> map toUpper (vs !! (n - 1))
    pPrev = "p" <> map toUpper (vs !! (n - 2))

    tupleConstr = "(" <> intercalate "," (replicate n "") <> ")"

main :: IO ()
main = forM_ [2..15] $ putStr . tuple
