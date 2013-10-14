{-# LANGUAGE CPP, FlexibleInstances, IncoherentInstances, NamedFieldPuns,
    NoImplicitPrelude, OverlappingInstances, TemplateHaskell,
    UndecidableInstances #-}

{-|
Module:      Data.Aeson.TH
Copyright:   (c) 2011, 2012 Bryan O'Sullivan
             (c) 2011 MailRank, Inc.
License:     Apache
Stability:   experimental
Portability: portable

Functions to mechanically derive 'ToJSON' and 'FromJSON' instances. Note that
you need to enable the @TemplateHaskell@ language extension in order to use this
module.

An example shows how instances are generated for arbitrary data types. First we
define a data type:

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances. Note that we make use of the
feature to change record field names. In this case we drop the first 4
characters of every field name. We also modify constructor names by
lower-casing them:

@
$('deriveJSON' 'defaultOptions'{'fieldLabelModifier' = 'drop' 4, 'constructorTagModifier' = map toLower} ''D)
@

Now we can use the newly created instances.

@
d :: D 'Int'
d = Record { testOne = 3.14159
           , testTwo = 'True'
           , testThree = Product \"test\" \'A\' 123
           }
@

>>> fromJSON (toJSON d) == Success d
> True

Please note that you can derive instances for tuples using the following syntax:

@
-- FromJSON and ToJSON instances for 4-tuples.
$('deriveJSON' 'defaultOptions' ''(,,,))
@

-}

module Data.Aeson.TH
    ( -- * Encoding configuration
      Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject

     -- * FromJSON and ToJSON derivation
    , deriveJSON

    , deriveToJSON
    , deriveFromJSON

    , mkToJSON
    , mkParseJSON
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from aeson:
import Data.Aeson ( toJSON, Object, object, (.=), (.:), (.:?)
                  , ToJSON, toJSON
                  , FromJSON, parseJSON
                  )
import Data.Aeson.Types ( Value(..), Parser
                        , Options(..)
                        , SumEncoding(..)
                        , defaultOptions
                        , defaultTaggedObject
                        )
-- from base:
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad       ( return, mapM, liftM2, fail )
import Data.Bool           ( Bool(False, True), otherwise, (&&) )
import Data.Eq             ( (==) )
import Data.Function       ( ($), (.) )
import Data.Functor        ( fmap )
import Data.Int            ( Int )
import Data.Either         ( Either(Left, Right) )
import Data.List           ( (++), foldl, foldl', intercalate
                           , length, map, zip, genericLength, all, partition
                           )
import Data.Maybe          ( Maybe(Nothing, Just), catMaybes )
import Prelude             ( String, (-), Integer, fromIntegral, error )
import Text.Printf         ( printf )
import Text.Show           ( show )
#if __GLASGOW_HASKELL__ < 700
import Control.Monad       ( (>>=), (>>) )
import Prelude             ( fromInteger )
#endif
-- from unordered-containers:
import qualified Data.HashMap.Strict as H ( lookup, toList )
-- from template-haskell:
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )
-- from text:
import qualified Data.Text as T ( Text, pack, unpack )
-- from vector:
import qualified Data.Vector as V ( unsafeIndex, null, length, create, fromList )
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )


--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- | Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON' and 'deriveFromJSON'.
deriveJSON :: Options
           -- ^ Encoding options.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveJSON opts name =
    liftM2 (++)
           (deriveToJSON   opts name)
           (deriveFromJSON opts name)


--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

{-
TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary and perhaps undesirable.
-}

-- | Generates a 'ToJSON' instance declaration for the given data type.
deriveToJSON :: Options
             -- ^ Encoding options.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveToJSON opts name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''ToJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToJSON opts cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''ToJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

-- | Generates a lambda expression which encodes the given data type as JSON.
mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON opts name = withType name (\_ cons -> consToJSON opts cons)

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates code
-- to generate the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consToJSON :: Options
           -- ^ Encoding options.
           -> [Con]
           -- ^ Constructors for which to generate JSON generating code.
           -> Q Exp

consToJSON _ [] = error $ "Data.Aeson.TH.consToJSON: "
                          ++ "Not a single constructor given!"

-- A single constructor is directly encoded. The constructor itself may be
-- forgotten.
consToJSON opts [con] = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) [encodeArgs opts False con]

consToJSON opts cons = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) matches
  where
    matches
        | allNullaryToStringTag opts && all isNullary cons =
              [ match (conP conName []) (normalB $ conStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise = [encodeArgs opts True con | con <- cons]

conStr :: Options -> Name -> Q Exp
conStr opts = appE [|String|] . conTxt opts

conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . conStringE opts

conStringE :: Options -> Name -> Q Exp
conStringE opts = stringE . constructorTagModifier opts . nameBase

-- | If constructor is nullary.
isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

encodeSum :: Options -> Bool -> Name -> Q Exp -> Q Exp
encodeSum opts multiCons conName exp
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
              [|Array|] `appE` ([|V.fromList|] `appE` listE [conStr opts conName, exp])
          TaggedObject{tagFieldName, contentsFieldName} ->
              [|object|] `appE` listE
                [ infixApp [|T.pack tagFieldName|]     [|(.=)|] (conStr opts conName)
                , infixApp [|T.pack contentsFieldName|] [|(.=)|] exp
                ]
          ObjectWithSingleField ->
              [|object|] `appE` listE
                [ infixApp (conTxt opts conName) [|(.=)|] exp
                ]

    | otherwise = exp

-- | Generates code to generate the JSON encoding of a single constructor.
encodeArgs :: Options -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
encodeArgs  opts multiCons (NormalC conName []) =
    match (conP conName [])
          (normalB (encodeSum opts multiCons conName [e|toJSON ([] :: [()])|]))
          []

-- Polyadic constructors with special case for unary constructors.
encodeArgs opts multiCons (NormalC conName ts) = do
    let len = length ts
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    js <- case [[|toJSON|] `appE` varE arg | arg <- args] of
            -- Single argument is directly converted.
            [e] -> return e
            -- Multiple arguments are converted to a JSON array.
            es  -> do
              mv <- newName "mv"
              let newMV = bindS (varP mv)
                                ([|VM.unsafeNew|] `appE`
                                  litE (integerL $ fromIntegral len))
                  stmts = [ noBindS $
                              [|VM.unsafeWrite|] `appE`
                                (varE mv) `appE`
                                  litE (integerL ix) `appE`
                                    e
                          | (ix, e) <- zip [(0::Integer)..] es
                          ]
                  ret = noBindS $ [|return|] `appE` varE mv
              return $ [|Array|] `appE`
                         (varE 'V.create `appE`
                           doE (newMV:stmts++[ret]))
    match (conP conName $ map varP args)
          (normalB $ encodeSum opts multiCons conName js)
          []

-- Records.
encodeArgs opts multiCons (RecC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let exp = [|object|] `appE` pairs

        pairs | omitNothingFields opts = infixApp maybeFields
                                                  [|(++)|]
                                                  restFields
              | otherwise = listE $ map toPair argCons

        argCons = zip args ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE $ map toPair rest

        (maybes, rest) = partition isMaybe argCons

        isMaybe (_, (_, _, AppT (ConT t) _)) = t == ''Maybe
        isMaybe _ = False

        maybeToPair (arg, (field, _, _)) =
            infixApp (infixE (Just $ toFieldName field)
                             [|(.=)|]
                             Nothing)
                     [|(<$>)|]
                     (varE arg)

        toPair (arg, (field, _, _)) =
            infixApp (toFieldName field)
                     [|(.=)|]
                     (varE arg)

        toFieldName field = [|T.pack|] `appE` fieldLabelExp opts field

    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons
            then case sumEncoding opts of
                   TwoElemArray -> [|toJSON|] `appE` tupE [conStr opts conName, exp]
                   TaggedObject{tagFieldName} ->
                       [|object|] `appE`
                         -- TODO: Maybe throw an error in case
                         -- tagFieldName overwrites a field in pairs.
                         infixApp (infixApp [|T.pack tagFieldName|]
                                            [|(.=)|]
                                            (conStr opts conName))
                                  [|(:)|]
                                  pairs
                   ObjectWithSingleField ->
                       [|object|] `appE` listE
                         [ infixApp (conTxt opts conName) [|(.=)|] exp ]
            else exp
          ) []

-- Infix constructors.
encodeArgs opts multiCons (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ encodeSum opts multiCons conName
          $ [|toJSON|] `appE` listE [ [|toJSON|] `appE` varE a
                                    | a <- [al,ar]
                                    ]
          )
          []
-- Existentially quantified constructors.
encodeArgs opts multiCons (ForallC _ _ con) =
    encodeArgs opts multiCons con


--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Generates a 'FromJSON' instance declaration for the given data type.
deriveFromJSON :: Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate a 'FromJSON' instance
               -- declaration.
               -> Q [Dec]
deriveFromJSON opts name =
    withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons =
        instanceD (return $ map (\t -> ClassP ''FromJSON [VarT t]) typeNames)
                  (classType `appT` instanceType)
                  [ funD 'parseJSON
                         [ clause []
                                  (normalB $ consFromJSON name opts cons)
                                  []
                         ]
                  ]
      where
        classType = conT ''FromJSON
        typeNames = map tvbName tvbs
        instanceType = foldl' appT (conT name) $ map varT typeNames

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type.
mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON opts name =
    withType name (\_ cons -> consFromJSON name opts cons)

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: Name
             -- ^ Name of the type to which the constructors belong.
             -> Options
             -- ^ Encoding options
             -> [Con]
             -- ^ Constructors for which to generate JSON parsing code.
             -> Q Exp

consFromJSON _ _ [] = error $ "Data.Aeson.TH.consFromJSON: "
                              ++ "Not a single constructor given!"

consFromJSON tName opts [con] = do
  value <- newName "value"
  lam1E (varP value) (parseArgs tName opts con (Right value))

consFromJSON tName opts cons = do
  value <- newName "value"
  lam1E (varP value) $ caseE (varE value) $
    if allNullaryToStringTag opts && all isNullary cons
    then allNullaryMatches
    else mixedMatches

  where
    allNullaryMatches =
      [ do txt <- newName "txt"
           match (conP 'String [varP txt])
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           ([|T.pack|] `appE`
                                              conStringE opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  | con <- cons
                  , let conName = getConName con
                  ]
                  ++
                  [ liftM2 (,)
                      (normalG [|otherwise|])
                      ( [|noMatchFail|]
                        `appE` (litE $ stringL $ show tName)
                        `appE` ([|T.unpack|] `appE` varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` (litE $ stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches =
        case sumEncoding opts of
          TaggedObject {tagFieldName, contentsFieldName} ->
            parseObject $ parseTaggedObject tagFieldName contentsFieldName
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'Array [varP arr])
                       (guardedB $
                        [ liftM2 (,) (normalG $ infixApp ([|V.length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     (([|not2ElemArray|]
                                       `appE` (litE $ stringL $ show tName)
                                       `appE` ([|V.length|] `appE` varE arr)))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` (litE $ stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]

    parseObject f =
        [ do obj <- newName "obj"
             match (conP 'Object [varP obj]) (normalB $ f obj) []
        , do other <- newName "other"
             match (varP other)
                   ( normalB
                     $ [|noObjectFail|]
                         `appE` (litE $ stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey)
                  (infixApp (varE obj)
                            [|(.:)|]
                            ([|T.pack|] `appE` stringE typFieldName))
          , noBindS $ parseContents conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject
          ]

    parse2ElemArray arr = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      let letIx n ix =
              valD (varP n)
                   (normalB ([|V.unsafeIndex|] `appE`
                               varE arr `appE`
                               litE (integerL ix)))
                   []
      letE [ letIx conKey 0
           , letIx conVal 1
           ]
           (caseE (varE conKey)
                  [ do txt <- newName "txt"
                       match (conP 'String [varP txt])
                             (normalB $ parseContents txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` (litE $ stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField obj = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      caseE ([e|H.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents conKey (Right conVal) 'conNotFoundFailObjectSingleField)
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` (litE $ stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents conKey contents errorFun =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     ([|T.pack|] `appE`
                                                        conNameExp opts con)
                             e <- parseArgs tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` (litE $ stringL $ show tName)
                                   `appE` listE (map ( litE
                                                     . stringL
                                                     . constructorTagModifier opts
                                                     . nameBase
                                                     . getConName
                                                     ) cons
                                                )
                                   `appE` ([|T.unpack|] `appE` varE conKey)
                                 )
                        ]
                      )
                      []
              ]

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches tName conName =
    [ do arr <- newName "arr"
         match (conP 'Array [varP arr])
               (guardedB $
                [ liftM2 (,) (normalG $ [|V.null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty Array")
                                (infixApp (litE $ stringL $ "Array of length ")
                                          [|(++)|]
                                          ([|show . V.length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "Array"
    ]

parseUnaryMatches :: Name -> [Q Match]
parseUnaryMatches conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    ([|parseJSON|] `appE` varE arg)
               )
               []
    ]

parseRecord :: Options -> Name -> Name -> [VarStrictType] -> Name -> ExpQ
parseRecord opts tName conName ts obj =
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      x:xs = [ [|lookupField|]
               `appE` (litE $ stringL $ show tName)
               `appE` (litE $ stringL $ constructorTagModifier opts $ nameBase conName)
               `appE` (varE obj)
               `appE` ( [|T.pack|] `appE` fieldLabelExp opts field
                      )
             | (field, _, _) <- ts
             ]

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ infixApp (varE obj)
                                    [|(.:)|]
                                    ([|T.pack|] `appE`
                                       (litE $ stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]

-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> Con -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
parseArgs tName _ (NormalC conName []) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseNullaryMatches tName conName
parseArgs tName _ (NormalC conName []) (Right valName) =
  caseE (varE valName) $ parseNullaryMatches tName conName

-- Unary constructors.
parseArgs _ _ (NormalC conName [_]) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseUnaryMatches conName
parseArgs _ _ (NormalC conName [_]) (Right valName) =
  caseE (varE valName) $ parseUnaryMatches conName

-- Polyadic constructors.
parseArgs tName _ (NormalC conName ts) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName $ genericLength ts
parseArgs tName _ (NormalC conName ts) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName $ genericLength ts

-- Records.
parseArgs tName opts (RecC conName ts) (Left (_, obj)) =
    parseRecord opts tName conName ts obj
parseArgs tName opts (RecC conName ts) (Right valName) = do
  obj <- newName "recObj"
  caseE (varE valName)
    [ match (conP 'Object [varP obj]) (normalB $ parseRecord opts tName conName ts obj) []
    , matchFailed tName conName "Object"
    ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
parseArgs tName _ (InfixC _ conName _) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName 2
parseArgs tName _ (InfixC _ conName _) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName 2

-- Existentially quantified constructors. We ignore the quantifiers
-- and proceed with the contained constructor.
parseArgs tName opts (ForallC _ _ con) contents =
    parseArgs tName opts con contents

-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
         let x:xs = [ [|parseJSON|]
                      `appE`
                      infixApp (varE arr)
                               [|V.unsafeIndex|]
                               (litE $ integerL ix)
                    | ix <- [0 .. numArgs - 1]
                    ]
         match (conP 'Array [varP arr])
               (normalB $ condE ( infixApp ([|V.length|] `appE` varE arr)
                                           [|(==)|]
                                           (litE $ integerL numArgs)
                                )
                                ( foldl' (\a b -> infixApp a [|(<*>)|] b)
                                         (infixApp (conE conName) [|(<$>)|] x)
                                         xs
                                )
                                ( parseTypeMismatch tName conName
                                    (litE $ stringL $ "Array of length " ++ show numArgs)
                                    ( infixApp (litE $ stringL $ "Array of length ")
                                               [|(++)|]
                                               ([|show . V.length|] `appE` varE arr)
                                    )
                                )
               )
               []
    , matchFailed tName conName "Array"
    ]


--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName
                      (litE $ stringL expected)
                      ([|valueConName|] `appE` varE other)
        )
        []

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

class (FromJSON a) => LookupField a where
    lookupField :: String -> String -> Object -> T.Text -> Parser a

instance (FromJSON a) => LookupField a where
    lookupField tName rec obj key =
        case H.lookup key obj of
          Nothing -> unknownFieldFail tName rec (T.unpack key)
          Just v  -> parseJSON v

instance (FromJSON a) => LookupField (Maybe a) where
    lookupField _ _ = (.:?)

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

noArrayFail :: String -> String -> Parser fail
noArrayFail t o = fail $ printf "When parsing %s expected Array but got %s." t o

noObjectFail :: String -> String -> Parser fail
noObjectFail t o = fail $ printf "When parsing %s expected Object but got %s." t o

firstElemNoStringFail :: String -> String -> Parser fail
firstElemNoStringFail t o = fail $ printf "When parsing %s expected an Array of 2 elements where the first element is a String but got %s at the first element." t o

wrongPairCountFail :: String -> String -> Parser fail
wrongPairCountFail t n =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair but got %s pairs."
                  t n

noStringFail :: String -> String -> Parser fail
noStringFail t o = fail $ printf "When parsing %s expected String but got %s." t o

noMatchFail :: String -> String -> Parser fail
noMatchFail t o =
    fail $ printf "When parsing %s expected a String with the tag of a constructor but got %s." t o

not2ElemArray :: String -> Int -> Parser fail
not2ElemArray t i = fail $ printf "When parsing %s expected an Array of 2 elements but got %i elements" t i

conNotFoundFail2ElemArray :: String -> [String] -> String -> Parser fail
conNotFoundFail2ElemArray t cs o =
    fail $ printf "When parsing %s expected a 2-element Array with a tag and contents element where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailObjectSingleField :: String -> [String] -> String -> Parser fail
conNotFoundFailObjectSingleField t cs o =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailTaggedObject :: String -> [String] -> String -> Parser fail
conNotFoundFailTaggedObject t cs o =
    fail $ printf "When parsing %s expected an Object with a tag field where the value is one of [%s], but got %s."
                  t (intercalate ", " cs) o

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' tName conName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([TyVarBndr] -> [Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
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

-- | Extracts the name from a constructor.
getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

-- | Extracts the name from a type variable binder.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

-- | Makes a string literal expression from a constructor's name.
conNameExp :: Options -> Con -> Q Exp
conNameExp opts = litE
                . stringL
                . constructorTagModifier opts
                . nameBase
                . getConName

-- | Creates a string literal expression from a record field label.
fieldLabelExp :: Options -- ^ Encoding options
              -> Name
              -> Q Exp
fieldLabelExp opts = litE . stringL . fieldLabelModifier opts . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"
