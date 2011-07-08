{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

{-|
Module:      Data.Aeson.TH
License:     Apache
Stability:   experimental
Portability: portable

Functions to mechanically derive 'ToJSON' and 'FromJSON' instances. Note that
you need to enable the @TemplateHaskell@ language extension in order to use this
module.

An example shows how instances are generated for arbitrary data types. First we
define a data type:


> data D a = Nullary
>          | Unary Int
>          | Product String Char a
>          | Record { testOne   :: Double
>                   , testTwo   :: Bool
>                   , testThree :: D a
>                   } deriving Eq


Next we derive the necessary instances. Note that we make use of the feature to
rewrite record fields. In this case we drop the first 4 characters of every
field name.


>>> $(deriveJSON (drop 4) ''D)


This will result in the following (simplified) code to be spliced in your program:

> import Control.Applicative
> import Control.Monad
> import Data.Aeson
> import Data.Aeson.TH
> import qualified Data.Map    as M
> import qualified Data.Text   as T
> import qualified Data.Vector as V
>
> instance ToJSON a => ToJSON (D a) where
>     toJSON =
>       \value ->
>         case value of
>           Nullary ->
>               object [T.pack "Nullary" .= toJSON ([] :: [()])]
>           Unary arg1 ->
>               object [T.pack "Unary" .= toJSON arg1]
>           Product arg1 arg2 arg3 ->
>               object [ T.pack "Product"
>                        .= toJSON [ toJSON arg1
>                                  , toJSON arg2
>                                  , toJSON arg3
>                                  ]
>                      ]
>           Record arg1 arg2 arg3 ->
>               object [ T.pack "Record"
>                        .= object [ T.pack "One"   .= arg1
>                                  , T.pack "Two"   .= arg2
>                                  , T.pack "Three" .= arg3
>                                  ]
>                      ]
>
> instance FromJSON a => FromJSON (D a) where
>     parseJSON =
>       \value ->
>         case value of
>           Object obj ->
>             case M.toList obj of
>               [(conKey, conVal)] ->
>                   case conKey of
>                     _ | (conKey == T.pack "Nullary") ->
>                           case conVal of
>                             Array arr | V.null arr -> pure Nullary
>                             _ -> mzero
>                       | (conKey == T.pack "Unary") ->
>                           case conVal of
>                             arg -> Unary <$> parseJSON arg
>                       | (conKey == T.pack "Product") ->
>                           case conVal of
>                             Array arr | V.length arr == 3 ->
>                               Product <$> parseJSON (arr V.! 0)
>                                       <*> parseJSON (arr V.! 1)
>                                       <*> parseJSON (arr V.! 2)
>                             _ -> mzero
>                       | (conKey == T.pack "Record") ->
>                           case conVal of
>                             Object obj ->
>                               Record <$> (obj .: T.pack "One")
>                                      <*> (obj .: T.pack "Two")
>                                      <*> (obj .: T.pack "Three")
>                             _ -> mzero
>                      | otherwise -> mzero
>               _ -> mzero
>           _ -> mzero

Now we can use the newly created instances.

> d :: D Int
> d = Record { testOne = 3.14159
>            , testTwo = True
>            , testThree = Product "test" 'A' 123
>            }

>>> fromJSON (toJSON d) == Success d
> True

-}

module Data.Aeson.TH
    ( deriveJSON

    , deriveToJSON
    , deriveFromJSON

    , mkToJSON
    , mkParseJSON
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from aeson:
import Data.Aeson ( toJSON, object, (.=), (.:)
                  , ToJSON, toJSON
                  , FromJSON, parseJSON
                  )
import Data.Aeson.Types ( Value(..) )
-- from base:
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad       ( return, mapM, mzero, liftM2 )
import Data.Bool           ( otherwise )
import Data.Char           ( String )
import Data.Eq             ( (==) )
import Data.Function       ( ($), (.), id )
import Data.Functor        ( fmap )
import Data.List           ( (++), foldl', map, zip, genericLength )
import Prelude             ( (-), Integer, error )
import Text.Show           ( show )
-- from containers:
import qualified Data.Map as M ( toList )
-- from template-haskell:
import Language.Haskell.TH
-- from text:
import qualified Data.Text as T ( pack )
-- from vector:
import qualified Data.Vector as V ( (!), null, length )



--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

deriveJSON :: (String -> String) -> Name -> Q [Dec]
deriveJSON withField name =
    liftM2 (++)
           (deriveToJSON   withField name)
           (deriveFromJSON withField name)


--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

{-
TODO: Don't constrain type variables that are not used in any constructor.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary.
-}
deriveToJSON :: (String -> String) -> Name -> Q [Dec]
deriveToJSON withField name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''ToJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToJSON withField cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''ToJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

mkToJSON :: (String -> String) -> Name -> Q Exp
mkToJSON withField name = withType name (\_ cons -> consToJSON withField cons)

consToJSON :: (String -> String) -> [Con] -> Q Exp
consToJSON _ [] = error $ "Data.Aeson.TH.consToJSON: "
                          ++ "Not a single constructor given!"
consToJSON withField [con] = do
    value <- newName "value"
    lam1E (varP value)
          $ caseE (varE value)
                  [encodeArgs id withField con]
consToJSON withField cons = do
    value <- newName "value"
    lam1E (varP value)
          $ caseE (varE value)
                  [ encodeArgs (wrap $ getConName con) withField con
                  | con <- cons
                  ]
  where
    wrap :: Name -> Q Exp -> Q Exp
    wrap name exp =
        let fieldName = [e|T.pack|] `appE` litE (stringL $ nameBase name)
        in [e|object|] `appE` listE [ infixApp fieldName
                                               [e|(.=)|]
                                               exp
                                    ]

encodeArgs :: (Q Exp -> Q Exp) -> (String -> String) -> Con -> Q Match
-- Nullary constructors.
encodeArgs withExp _ (NormalC conName []) =
    match (conP conName [])
          (normalB $ withExp [e|toJSON ([] :: [()])|])
          []
-- Polyadic constructors with special case for unary constructors.
encodeArgs withExp _ (NormalC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let js = case [[e|toJSON|] `appE` varE arg | arg <- args] of
               -- Single argument is directly converted.
               [e] -> e
               -- Multiple arguments are converted to a JSON array.
               es  -> [e|toJSON|] `appE` listE es
    match (conP conName $ map varP args)
          (normalB $ withExp js)
          []
-- Records.
encodeArgs withExp withField (RecC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let js = [ infixApp ([e|T.pack|] `appE` fieldNameExp withField field)
                        [e|(.=)|]
                        (varE arg)
             | (arg, (field, _, _)) <- zip args ts
             ]
    match (conP conName $ map varP args)
          (normalB $ withExp $ [e|object|] `appE` listE js)
          []
-- Infix constructors.
encodeArgs withExp _ (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ withExp
          $ [e|toJSON|] `appE` listE [ [e|toJSON|] `appE` varE a
                                     | a <- [al,ar]
                                     ]
          )
          []
-- Existentially quantified constructors.
encodeArgs withExp withField (ForallC _ _ con) =
    encodeArgs withExp withField con


--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

deriveFromJSON :: (String -> String) -> Name -> Q [Dec]
deriveFromJSON withField name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''FromJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'parseJSON
                         [ clause []
                                  (normalB $ consFromJSON withField cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''FromJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

mkParseJSON :: (String -> String) -> Name -> Q Exp
mkParseJSON withField name =
    withType name (\_ cons -> consFromJSON withField cons)

consFromJSON :: (String -> String) -> [Con] -> Q Exp
consFromJSON _ [] = error $ "Data.Aeson.TH.consFromJSON: "
                            ++ "Not a single constructor given!"
consFromJSON withField [con] = do
  value <- newName "value"
  lam1E (varP value)
        $ caseE (varE value)
                (parseArgs withField con)
consFromJSON withField cons = do
  value  <- newName "value"
  obj    <- newName "obj"
  conKey <- newName "conKey"
  conVal <- newName "conVal"

  let -- Convert the Data.Map inside the Object to a list and pattern match
      -- against it. It must contain a single element otherwise the parse will
      -- fail.
      caseLst = caseE ([e|M.toList|] `appE` varE obj)
                      [ match (listP [tupP [varP conKey, varP conVal]])
                              (normalB caseKey)
                              []
                      , errorMatch
                      ]
      caseKey = caseE (varE conKey)
                      [match wildP (guardedB guards) []]
      guards = [ do g <- normalG $ infixApp (varE conKey)
                                            [|(==)|]
                                            ( [|T.pack|]
                                              `appE` conNameExp con
                                            )
                    e <- caseE (varE conVal)
                               (parseArgs withField con)
                    return (g, e)
               | con <- cons
               ]
               ++
               [liftM2 (,) (normalG [e|otherwise|]) [e|mzero|]]

  lam1E (varP value)
        $ caseE (varE value)
                [ match (conP 'Object [varP obj])
                        (normalB caseLst)
                        []
                , errorMatch
                ]

parseArgs :: (String -> String) -> Con -> [Q Match]
-- Nullary constructors.
parseArgs _ (NormalC conName []) =
    [ do arr <- newName "arr"
         g <- normalG $ [|V.null|] `appE` varE arr
         e <- [e|pure|] `appE` conE conName
         match (conP 'Array [varP arr])
               (guardedB [return (g, e)])
               []
    , errorMatch
    ]
-- Unary constructors.
parseArgs _ (NormalC conName [_]) =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [e|(<$>)|]
                                    ([e|parseJSON|] `appE` varE arg)
               )
               []
    ]

-- Polyadic constuctors.
parseArgs _ (NormalC conName ts) = parseProduct conName $ genericLength ts
-- Records.
parseArgs withField (RecC conName ts) =
    [ do obj <- newName "obj"
         -- List of: "obj .: "<FIELD>""
         let x:xs = [ infixApp (varE obj)
                               [|(.:)|]
                               ( [e|T.pack|]
                                 `appE`
                                 fieldNameExp withField field
                               )
                    | (field, _, _) <- ts
                    ]
         match (conP 'Object [varP obj])
               ( normalB $ foldl' (\a b -> infixApp a [|(<*>)|] b)
                                  (infixApp (conE conName) [|(<$>)|] x)
                                  xs
               )
               []
    , errorMatch
    ]
-- Infix constructors.
parseArgs _ (InfixC _ conName _) = parseProduct conName 2
-- Existentially quantified constructors.
parseArgs withField (ForallC _ _ con) = parseArgs withField con

parseProduct :: Name -> Integer -> [Q Match]
parseProduct conName numArgs =
    [ do arr <- newName "arr"
         g <- normalG $ infixApp ([|V.length|] `appE` varE arr)
                                 [|(==)|]
                                 (litE $ integerL numArgs)
         -- List of: "parseJSON (arr V.! <IX>)"
         let x:xs = [ [|parseJSON|]
                      `appE`
                      infixApp (varE arr)
                               [|(V.!)|]
                               (litE $ integerL ix)
                    | ix <- [0 .. numArgs - 1]
                    ]
         e <- foldl' (\a b -> infixApp a [|(<*>)|] b)
                     (infixApp (conE conName) [|(<$>)|] x)
                     xs
         match (conP 'Array [varP arr])
               (guardedB [return (g, e)])
               []
    , errorMatch
    ]

-- "_ -> mzero"
errorMatch :: Q Match
errorMatch = match wildP (normalB [|mzero|]) []


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

withType :: Name -> ([TyVarBndr] -> [Con] -> Q a) -> Q a
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f tvbs cons
          NewtypeD _ _ tvbs con  _ -> f tvbs [con]
          other -> error $ "Data.Aeson.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "Data.Aeson.TH.withType: I need the name of a type."

getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

fieldNameExp :: (String -> String) -> Name -> Q Exp
fieldNameExp f = litE . stringL . f . nameBase

conNameExp :: Con -> Q Exp
conNameExp = litE . stringL . nameBase . getConName
