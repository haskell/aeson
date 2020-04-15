{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

import           Prelude.Compat

import           Control.Applicative (many)
import           Control.Monad       (ap)
import           Data.Foldable       (for_, foldl')
import           Data.List           (intercalate)
import           Data.Map            (Map)
import           Data.Traversable    (for)
import           Numeric             (showFFloat, showGFloat)
import           System.FilePath     (dropExtension, takeFileName)

import qualified Data.Map.Strict     as Map
import qualified Options.Applicative as O

import           CsvParse
import           Types

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optRunNames :: [RunName]
    , optOutput   :: FilePath
    , optRunPaths :: [FilePath]
    }

options :: O.Parser Options
options = Options
    <$> many (O.strOption $ O.short 'l' <> O.long "label" <> O.help "label")
    <*> O.strOption (O.short 'o' <> O.long "output" <> O.metavar "FILE" <> O.help "output file name" <> O.value "-")
    <*> many (O.strArgument $ O.metavar "FILE" <> O.help "CSV file name")

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

flipFiniteMap :: (Ord a, Ord b) => Map a (Map b v) -> Map b (Map a v)
flipFiniteMap abv = Map.unionsWith Map.union
    [ Map.singleton b $ Map.singleton a v
    | (a, bv) <- Map.toList abv
    , (b, v)  <- Map.toList bv
    ]

-------------------------------------------------------------------------------
-- List Builder
-------------------------------------------------------------------------------

newtype ListBuilder x a = LB { unLB :: forall r. (([x] -> [x]) -> a -> r) -> r }

instance Functor (ListBuilder x) where
    fmap f (LB k) = LB $ k $ \endo a k' -> k' endo (f a)

instance Applicative (ListBuilder x) where
    pure x = LB $ \f -> f id x
    (<*>)  = ap

instance Monad (ListBuilder x) where
    return = pure

    m >>= k =
        LB $ \r ->
        unLB m $ \endo1 a ->
        unLB (k a) $ \endo2 b ->
        r (endo1 . endo2) b

buildList :: ListBuilder x () -> [x]
buildList (LB f) = f $ \endo _ -> endo []

item :: x -> ListBuilder x ()
item x = LB $ \f -> f (x :) ()

-------------------------------------------------------------------------------
-- Make table
-------------------------------------------------------------------------------

makeTable
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Map BenchName (Map RunName Stats)
    -> [[String]]
makeTable fname names results = buildList $ do
    -- header
    item $ buildList $ do
        item "Benchmark"
        item (getRunName fname)
        for_ names $ \name -> do
            item (getRunName name)
            item ""

    -- rows
    for_ (Map.toList results) $ \(bn, mp) ->
        for_ (Map.lookup fname mp) $ \fstats -> item $ buildList $ do
            -- benchmark and first value
            item (getBenchName bn)
            let fmean = statsMean fstats
            item $ showD fmean

            -- rest benchmarks
            for_ names $ \name -> case Map.lookup name mp of
                Nothing    -> do
                    item ""
                    item ""
                Just stats -> do
                    let mean = statsMean stats
                    item $ showD mean
                    item $ showP fmean mean
  where
    showD :: Double -> String
    showD d = showGFloat (Just 2) d ""

    showP :: Double -> Double -> String
    showP orig curr
        | curr > orig  = '+' : showFFloat (Just 2) (100 * (curr - orig) / orig) "%"
        | otherwise    = '-' : showFFloat (Just 2) (100 * (orig - curr) / orig) "%"

-- https://oleg.fi/gists/posts/2019-04-28-tabular.html
--
-- unfortunately this doesn't allow colspans
tabular :: [[String]] -> String
tabular zs = unlines rs where
    (cs, ws, rs) = foldr go (0, repeat 0, []) zs

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go x (c, w, ys) =
        (max c (length x), zipWith max w (map length x ++ repeat 0),
         unwords' (take cs (zipWith' x ws)) : ys)

    fr s n = replicate (n - length s) ' ' ++ s
    fl s n = s ++ replicate (n - length s) ' '

    zipWith' :: [String] -> [Int] -> [String]
    zipWith' (s:ss) (n:ns) = fl s n : zipWith fr ss ns
    zipWith' _      _      = []

    -- two spaces instead one
    unwords' = intercalate "  "

-------------------------------------------------------------------------------
-- Geometric mean
-------------------------------------------------------------------------------

gmean :: Traversable f => f Stats -> Double
gmean = post . foldl' f (A 1.0 0 0) . fmap statsMean where
    f (A acc es n) d = A c (es + e) (n + 1) where
        (c, e) = split (acc * d)

    split :: Double -> (Double, Int)
    split d  = (d / 2 ^^ e, e) where e = exponent d

    post :: A -> Double
    post (A acc es n) = acc ** (1 / fromIntegral n)
                      * 2 ** (fromIntegral es / fromIntegral n)

-- | @A x e n@ is @n@ elements which product is @x * 2 ^^ e@
data A = A !Double !Int !Int

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- O.execParser $ O.info (O.helper <*> options) mempty

    let runs :: [(RunName, FilePath)]
        runs = zipWith f (map Just optRunNames ++ repeat Nothing) optRunPaths
          where
            f (Just n) fp = (n, fp)
            f Nothing  fp = (RunName $ dropExtension $ takeFileName fp, fp)

    let names' :: [RunName]
        names' = map fst runs

    case names' of
        [] -> return () -- nothing to do
        fname : names -> do
            results0 <- fmap Map.fromList $ for runs $ \(name, fp) ->
                (,) name . Map.fromList  <$> readResults fp

            let results1 :: Map RunName (Map BenchName Stats)
                results1 = fmap addGMean results0 where
                    -- zzz will make the line appear last.
                    addGMean m = Map.insert (BenchName "zzz-geom-mean") (Stats (gmean m) 0 0 0 0 0) m

            let results :: Map BenchName (Map RunName Stats)
                results = flipFiniteMap results1

            let table :: [[String]]
                table = makeTable fname names results

            putStrLn $ tabular table
