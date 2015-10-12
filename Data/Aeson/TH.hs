{-# LANGUAGE CPP, FlexibleInstances, NamedFieldPuns,
    NoImplicitPrelude, TemplateHaskell,
    UndecidableInstances #-}

#include "overlapping-compat.h"

{-|
Module:      Data.Aeson.TH
Copyright:   (c) 2011-2015 Bryan O'Sullivan
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

This also works for data family instances, but instead of passing in the data
family name (with double quotes), we pass in a data family instance
constructor (with a single quote):

@
data family DF a
data instance DF Int = DF1 Int
                     | DF2 Int Int
                     deriving Eq

$('deriveJSON' 'defaultOptions' 'DF1)
-- Alternatively, one could pass 'DF2 instead
@

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
    , mkToEncoding
    , mkParseJSON
    ) where

import Control.Applicative ( pure, (<$>), (<*>) )
import Data.Aeson ( toJSON, Object, (.=), (.:), (.:?)
                  , ToJSON, toEncoding, toJSON
                  , FromJSON, parseJSON
                  )
import Data.Aeson.Types ( Value(..), Parser
                        , Options(..)
                        , SumEncoding(..)
                        , defaultOptions
                        , defaultTaggedObject
                        )
import Data.Aeson.Types.Internal (Encoding(..))
import Control.Monad       ( join, liftM2, return, mapM, fail )
import Data.Bool           ( Bool(False, True), otherwise, (&&), not )
import Data.Either         ( Either(Left, Right) )
import Data.Eq             ( (==) )
import Data.Function       ( ($), (.), flip )
import Data.Functor        ( fmap )
import Data.Int            ( Int )
import Data.List           ( (++), all, any, filter, find, foldl, foldl'
                           , genericLength , intercalate , intersperse, length, map
                           , partition, zip
                           )
import Data.Maybe          ( Maybe(Nothing, Just), catMaybes )
import Data.Monoid         ( (<>), mconcat )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )
import Prelude             ( String, (-), Integer, error, foldr1, fromIntegral
                           , snd, uncurry
                           )
#if MIN_VERSION_template_haskell(2,8,0) && __GLASGOW_HASKELL__ < 710
import Prelude             ( drop )
#endif
import Text.Printf         ( printf )
import Text.Show           ( show )
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Builder as E
import qualified Data.Aeson.Encode.Functions as E
import qualified Data.HashMap.Strict as H ( lookup, toList )
#if MIN_VERSION_template_haskell(2,8,0) && __GLASGOW_HASKELL__ < 710
import qualified Data.Set as Set ( Set, empty, singleton, size, union, unions )
#endif
import qualified Data.Text as T ( Text, pack, unpack )
import qualified Data.Vector as V ( unsafeIndex, null, length, create, fromList )
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )


--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- | Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type or data family instance constructor.
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

-- | Generates a 'ToJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON :: Options
             -- ^ Encoding options.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveToJSON opts name =
    withType name $ \name' tvbs cons mbTys -> fmap (:[]) $ fromCons name' tvbs cons mbTys
  where
    fromCons :: Name -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Dec
    fromCons name' tvbs cons mbTys =
        instanceD instanceCxt
                  instanceType
                  [ funD 'toJSON
                         [ clause []
                                  (normalB $ consToValue opts cons)
                                  []
                         ]
                  , funD 'toEncoding
                         [ clause []
                                  (normalB $ consToEncoding opts cons)
                                  []
                         ]
                  ]
      where
        (instanceCxt, instanceType) =
            buildTypeInstance name' ''ToJSON tvbs mbTys

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value'.
mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON opts name = withType name (\_ _ cons _ -> consToValue opts cons)

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string.
mkToEncoding :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkToEncoding opts name = withType name (\_ _ cons _ -> consToEncoding opts cons)

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates
-- code to generate a 'Value' of a number of constructors. All constructors
-- must be from the same type.
consToValue :: Options
           -- ^ Encoding options.
           -> [Con]
           -- ^ Constructors for which to generate JSON generating code.
           -> Q Exp

consToValue _ [] = error $ "Data.Aeson.TH.consToValue: "
                          ++ "Not a single constructor given!"

-- A single constructor is directly encoded. The constructor itself may be
-- forgotten.
consToValue opts [con] = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) [argsToValue opts False con]

consToValue opts cons = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) matches
  where
    matches
        | allNullaryToStringTag opts && all isNullary cons =
              [ match (conP conName []) (normalB $ conStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise = [argsToValue opts True con | con <- cons]

conStr :: Options -> Name -> Q Exp
conStr opts = appE [|String|] . conTxt opts

conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . conStringE opts

conStringE :: Options -> Name -> Q Exp
conStringE opts = stringE . constructorTagModifier opts . nameBase

-- | Helper function used by both 'deriveToJSON' and 'mkToEncoding'. Generates
-- code to write out a value for a number of constructors. All constructors
-- must be from the same type.
consToEncoding :: Options
                  -- ^ Encoding options.
               -> [Con]
               -- ^ Constructors for which to generate JSON generating code.
               -> Q Exp

consToEncoding _ [] = error $ "Data.Aeson.TH.consToEncoding: "
                      ++ "Not a single constructor given!"

-- A single constructor is directly encoded. The constructor itself may be
-- forgotten.
consToEncoding opts [con] = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) [argsToEncoding opts False con]

-- Encode just the name of the constructor of a sum type iff all the
-- constructors are nullary.
consToEncoding opts cons = do
    value <- newName "value"
    lam1E (varP value) $ caseE (varE value) matches
  where
    matches
        | allNullaryToStringTag opts && all isNullary cons =
              [ match (conP conName [])
                (normalB $ [|Encoding|] `appE` encStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise = [argsToEncoding opts True con | con <- cons]

encStr :: Options -> Name -> Q Exp
encStr opts = appE [|E.text|] . conTxt opts

-- | If constructor is nullary.
isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

sumToValue :: Options -> Bool -> Name -> Q Exp -> Q Exp
sumToValue opts multiCons conName exp
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
              [|Array|] `appE` ([|V.fromList|] `appE` listE [conStr opts conName, exp])
          TaggedObject{tagFieldName, contentsFieldName} ->
              [|A.object|] `appE` listE
                [ infixApp [|T.pack tagFieldName|]     [|(.=)|] (conStr opts conName)
                , infixApp [|T.pack contentsFieldName|] [|(.=)|] exp
                ]
          ObjectWithSingleField ->
              [|A.object|] `appE` listE
                [ infixApp (conTxt opts conName) [|(.=)|] exp
                ]

    | otherwise = exp

-- | Generates code to generate the JSON encoding of a single constructor.
argsToValue :: Options -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
argsToValue  opts multiCons (NormalC conName []) =
    match (conP conName [])
          (normalB (sumToValue opts multiCons conName [e|toJSON ([] :: [()])|]))
          []

-- Polyadic constructors with special case for unary constructors.
argsToValue opts multiCons (NormalC conName ts) = do
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
          (normalB $ sumToValue opts multiCons conName js)
          []

-- Records.
argsToValue opts multiCons (RecC conName ts) = case (unwrapUnaryRecords opts, not multiCons, ts) of
  (True,True,[(_,st,ty)]) -> argsToValue opts multiCons (NormalC conName [(st,ty)])
  _ -> do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let exp = [|A.object|] `appE` pairs

        pairs | omitNothingFields opts = infixApp maybeFields
                                                  [|(++)|]
                                                  restFields
              | otherwise = listE $ map toPair argCons

        argCons = zip args ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE $ map toPair rest

        (maybes, rest) = partition isMaybe argCons

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
                       [|A.object|] `appE`
                         -- TODO: Maybe throw an error in case
                         -- tagFieldName overwrites a field in pairs.
                         infixApp (infixApp [|T.pack tagFieldName|]
                                            [|(.=)|]
                                            (conStr opts conName))
                                  [|(:)|]
                                  pairs
                   ObjectWithSingleField ->
                       [|A.object|] `appE` listE
                         [ infixApp (conTxt opts conName) [|(.=)|] exp ]
            else exp
          ) []

-- Infix constructors.
argsToValue opts multiCons (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ sumToValue opts multiCons conName
          $ [|toJSON|] `appE` listE [ [|toJSON|] `appE` varE a
                                    | a <- [al,ar]
                                    ]
          )
          []
-- Existentially quantified constructors.
argsToValue opts multiCons (ForallC _ _ con) =
    argsToValue opts multiCons con

isMaybe :: (a, (b, c, Type)) -> Bool
isMaybe (_, (_, _, AppT (ConT t) _)) = t == ''Maybe
isMaybe _                            = False

(<^>) :: ExpQ -> ExpQ -> ExpQ
(<^>) a b = infixApp a [|(<>)|] b
infixr 6 <^>

(<:>) :: ExpQ -> ExpQ -> ExpQ
(<:>) a b = a <^> [|E.char7 ':'|] <^> b
infixr 5 <:>

(<%>) :: ExpQ -> ExpQ -> ExpQ
(<%>) a b = a <^> [|E.char7 ','|] <^> b
infixr 4 <%>

array :: ExpQ -> ExpQ
array exp = [|Encoding|] `appE` ([|E.char7 '['|] <^> exp <^> [|E.char7 ']'|])

object :: ExpQ -> ExpQ
object exp = [|Encoding|] `appE` ([|E.char7 '{'|] <^> exp <^> [|E.char7 '}'|])

sumToEncoding :: Options -> Bool -> Name -> Q Exp -> Q Exp
sumToEncoding opts multiCons conName exp
    | multiCons =
        let fexp = [|fromEncoding|] `appE` exp in
        case sumEncoding opts of
          TwoElemArray ->
            array (encStr opts conName <%> fexp)
          TaggedObject{tagFieldName, contentsFieldName} ->
            object $
            ([|E.text (T.pack tagFieldName)|] <:> encStr opts conName) <%>
            ([|E.text (T.pack contentsFieldName)|] <:> fexp)
          ObjectWithSingleField ->
            object (encStr opts conName <:> fexp)

    | otherwise = exp

-- | Generates code to generate the JSON encoding of a single constructor.
argsToEncoding :: Options -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
argsToEncoding  opts multiCons (NormalC conName []) =
    match (conP conName [])
          (normalB (sumToEncoding opts multiCons conName [e|toEncoding ([] :: [()])|]))
          []

-- Polyadic constructors with special case for unary constructors.
argsToEncoding opts multiCons (NormalC conName ts) = do
    let len = length ts
    args <- mapM newName ["arg" ++ show n | n <- [1..len]]
    js <- case args of
            -- Single argument is directly converted.
            [e] -> return ([|toEncoding|] `appE` varE e)
            -- Multiple arguments are converted to a JSON array.
            es  ->
              return (array (foldr1 (<%>) [[|E.builder|] `appE` varE x | x <- es]))
    match (conP conName $ map varP args)
          (normalB $ sumToEncoding opts multiCons conName js)
          []

-- Records.
argsToEncoding opts multiCons (RecC conName ts) = case (unwrapUnaryRecords opts, not multiCons, ts) of
  (True,True,[(_,st,ty)]) -> argsToEncoding opts multiCons (NormalC conName [(st,ty)])
  _ -> do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]

    let exp = object objBody

        objBody = [|mconcat|] `appE`
                  ([|intersperse (E.char7 ',')|] `appE` pairs)
        pairs | omitNothingFields opts = infixApp maybeFields
                                                  [|(<>)|]
                                                  restFields
              | otherwise = listE (map toPair argCons)

        argCons = zip args ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE (map toPair rest)

        (maybes, rest) = partition isMaybe argCons

        maybeToPair (arg, (field, _, _)) =
            infixApp
              (infixApp
                (infixE
                  (Just $ toFieldName field <^> [|E.char7 ':'|])
                  [|(<>)|]
                  Nothing)
                [|(.)|]
                [|E.builder|])
              [|(<$>)|]
              (varE arg)

        toPair (arg, (field, _, _)) =
          toFieldName field <:> [|E.builder|] `appE` varE arg

        toFieldName field = [|E.text|] `appE`
                            ([|T.pack|] `appE` fieldLabelExp opts field)

    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons
            then case sumEncoding opts of
                   TwoElemArray -> array $
                     encStr opts conName <%> [|fromEncoding|] `appE` exp
                   TaggedObject{tagFieldName} -> object $
                     ([|E.text (T.pack tagFieldName)|] <:>
                      encStr opts conName) <%>
                     objBody
                   ObjectWithSingleField -> object $
                     encStr opts conName <:> [|fromEncoding|] `appE` exp
            else exp
          ) []

-- Infix constructors.
argsToEncoding opts multiCons (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ sumToEncoding opts multiCons conName
          $ [|toEncoding|] `appE` listE [ [|toJSON|] `appE` varE a
                                        | a <- [al,ar]
                                        ]
          )
          []
-- Existentially quantified constructors.
argsToEncoding opts multiCons (ForallC _ _ con) =
    argsToEncoding opts multiCons con


--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Generates a 'FromJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON :: Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate a 'FromJSON' instance
               -- declaration.
               -> Q [Dec]
deriveFromJSON opts name =
    withType name $ \name' tvbs cons mbTys -> fmap (:[]) $ fromCons name' tvbs cons mbTys
  where
    fromCons :: Name -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Dec
    fromCons name' tvbs cons mbTys =
        instanceD instanceCxt
                  instanceType
                  [ funD 'parseJSON
                         [ clause []
                                  (normalB $ consFromJSON name' opts cons)
                                  []
                         ]
                  ]
      where
        (instanceCxt, instanceType) =
            buildTypeInstance name' ''FromJSON tvbs mbTys

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor.
mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON opts name =
    withType name (\name' _ cons _ -> consFromJSON name' opts cons)

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
parseArgs tName opts (RecC conName ts) (Right valName) = case (unwrapUnaryRecords opts,ts) of
  (True,[(_,st,ty)])-> parseArgs tName opts (NormalC conName [(st,ty)]) (Right valName)
  _ -> do
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

instance OVERLAPPABLE_ (FromJSON a) => LookupField a where
    lookupField tName rec obj key =
        case H.lookup key obj of
          Nothing -> unknownFieldFail tName rec (T.unpack key)
          Just v  -> parseJSON v

instance (FromJSON a) => LookupField (Maybe a) where
    lookupField _ _ obj key = join <$> obj .:? key

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
parseTypeMismatch' conName tName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Boilerplate for top level splices.
--
-- The given 'Name' must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.

-- Any other value will result in an exception.
withType :: Name
         -> (Name -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the datatype/data family 'Name', type variable binders and
         -- constructors extracted from the given 'Name'. If the 'Name' is
         -- from a data family instance constructor, it will also have its
         -- instantiated types; otherwise, it will be 'Nothing'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ tvbs cons _ -> f name tvbs cons Nothing
          NewtypeD _ _ tvbs con  _ -> f name tvbs [con] Nothing
          other -> error $ ns ++ "Unsupported type: " ++ show other
#if MIN_VERSION_template_haskell(2,11,0)
      DataConI _ _ parentName   -> do
#else
      DataConI _ _ parentName _ -> do
#endif
        parentInfo <- reify parentName
        case parentInfo of
#if MIN_VERSION_template_haskell(2,11,0)
          FamilyI (DataFamilyD _ tvbs _) decs ->
#else
          FamilyI (FamilyD DataFam _ tvbs _) decs ->
#endif
            let instDec = flip find decs $ \dec -> case dec of
                  DataInstD    _ _ _ cons _ -> any ((name ==) . getConName) cons
                  NewtypeInstD _ _ _ con  _ -> name == getConName con
                  _ -> error $ ns ++ "Must be a data or newtype instance."
             in case instDec of
                  Just (DataInstD    _ _ instTys cons _)
                    -> f parentName tvbs cons $ Just instTys
                  Just (NewtypeInstD _ _ instTys con  _)
                    -> f parentName tvbs [con] $ Just instTys
                  _ -> error $ ns ++
                    "Could not find data or newtype instance constructor."
          _ -> error $ ns ++ "Data constructor " ++ show name ++
            " is not from a data family instance constructor."
#if MIN_VERSION_template_haskell(2,11,0)
      FamilyI DataFamilyD{} _ ->
#else
      FamilyI (FamilyD DataFam _ _ _) _ ->
#endif
        error $ ns ++
          "Cannot use a data family name. Use a data family instance constructor instead."
      _ -> error $ ns ++ "I need the name of a plain data type constructor, "
                      ++ "or a data family instance constructor."
  where
    ns :: String
    ns = "Data.Aeson.TH.withType: "

-- | Infer the context and instance head needed for a FromJSON or ToJSON instance.
buildTypeInstance :: Name
                  -- ^ The type constructor or data family name
                  -> Name
                  -- ^ The typeclass name ('ToJSON' or 'FromJSON')
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> Maybe [Type]
                  -- ^ 'Just' the types used to instantiate a data family instance,
                  -- or 'Nothing' if it's a plain data type
                  -> (Q Cxt, Q Type)
                  -- ^ The resulting 'Cxt' and 'Type' to use in a class instance
-- Plain data type/newtype case
buildTypeInstance tyConName constraint tvbs Nothing =
    (applyCon constraint typeNames, conT constraint `appT` instanceType)
  where
    typeNames :: [Name]
    typeNames = map tvbName tvbs

    instanceType :: Q Type
    instanceType = applyTyCon tyConName $ map varT typeNames
-- Data family instance case
buildTypeInstance dataFamName constraint tvbs (Just instTysAndKinds) =
    (applyCon constraint lhsTvbNames, conT constraint `appT` instanceType)
  where
    -- We need to make sure that type variables in the instance head which have
    -- constraints aren't poly-kinded, e.g.,
    --
    -- @
    -- instance ToJSON a => ToJSON (Foo (a :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Q Type
    instanceType = applyTyCon dataFamName $ map (return . unSigT) rhsTypes

    -- We need to mindful of an old GHC bug which causes kind variables appear in
    -- @instTysAndKinds@ (as the name suggests) if (1) @PolyKinds@ is enabled, and
    -- (2) either GHC 7.6 or 7.8 is being used (for more info, see
    -- https://ghc.haskell.org/trac/ghc/ticket/9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@
    instTypes :: [Type]
    instTypes =
#if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
        instTysAndKinds
#else
        drop (Set.size . Set.unions $ map (distinctKindVars . tvbKind) tvbs)
             instTysAndKinds
#endif

    lhsTvbNames :: [Name]
    lhsTvbNames = map (tvbName . uncurry replaceTyVarName)
                . filter (isTyVar . snd)
                $ zip tvbs rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of this bug:
    -- https://ghc.haskell.org/trac/ghc/ticket/9692). To work around this, we borrow
    -- some type variables from the data family instance declaration.
    rhsTypes :: [Type]
    rhsTypes =
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
            instTypes ++ map tvbToType
                             (drop (length instTypes)
                                   tvbs)
#else
            instTypes
#endif

#if MIN_VERSION_template_haskell(2,8,0) && __GLASGOW_HASKELL__ < 710
distinctKindVars :: Kind -> Set.Set Name
distinctKindVars (AppT k1 k2) = distinctKindVars k1 `Set.union` distinctKindVars k2
distinctKindVars (SigT k _)   = distinctKindVars k
distinctKindVars (VarT k)     = Set.singleton k
distinctKindVars _            = Set.empty

-- | Extracts the kind from a type variable binder.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _  ) = starK
tvbKind (KindedTV _ k) = k
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k
#endif

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

-- | Replace the Name of a TyVarBndr with one from a Type (if the Type has a Name).
replaceTyVarName :: TyVarBndr -> Type -> TyVarBndr
replaceTyVarName tvb            (SigT t _) = replaceTyVarName tvb t
replaceTyVarName (PlainTV  _)   (VarT n)   = PlainTV  n
replaceTyVarName (KindedTV _ k) (VarT n)   = KindedTV n k
replaceTyVarName tvb            _          = tvb

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Q Type] -> Q Type
applyTyCon = foldl' appT . conT

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

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

applyCon :: Name -> [Name] -> Q [Pred]
applyCon con typeNames = return (map apply typeNames)
  where apply t =
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif
