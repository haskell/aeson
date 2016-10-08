{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

#include "incoherent-compat.h"
#include "overlapping-compat.h"

{-|
Module:      Data.Aeson.TH
Copyright:   (c) 2011-2016 Bryan O'Sullivan
             (c) 2011 MailRank, Inc.
License:     BSD3
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
    (
      -- * Encoding configuration
      Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

     -- * FromJSON and ToJSON derivation
    , deriveJSON
    , deriveJSON1
    , deriveJSON2

    , deriveToJSON
    , deriveToJSON1
    , deriveToJSON2
    , deriveFromJSON
    , deriveFromJSON1
    , deriveFromJSON2

    , mkToJSON
    , mkLiftToJSON
    , mkLiftToJSON2
    , mkToEncoding
    , mkLiftToEncoding
    , mkLiftToEncoding2
    , mkParseJSON
    , mkLiftParseJSON
    , mkLiftParseJSON2
    ) where

import Prelude ()
import Prelude.Compat hiding (exp)

import Control.Applicative ((<|>))
import Data.Aeson (Object, (.=), (.:), FromJSON(..), FromJSON1(..), FromJSON2(..), ToJSON(..), ToJSON1(..), ToJSON2(..))
import Data.Aeson.Types (Options(..), Parser, SumEncoding(..), Value(..), defaultOptions, defaultTaggedObject)
import Data.Aeson.Types.Internal ((<?>), Pair, JSONPathElement(Key))
import Data.Aeson.Types.FromJSON (parseOptionalFieldWith)
import Control.Monad (liftM2, unless, when)
import Data.Foldable (foldr')
#if MIN_VERSION_template_haskell(2,8,0) && !MIN_VERSION_template_haskell(2,10,0)
import Data.List (nub)
#endif
import Data.List (find, foldl', genericLength , intercalate , intersperse, partition, union)
import Data.List.NonEmpty ((<|), NonEmpty((:|)))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
#if MIN_VERSION_template_haskell(2,8,0)
import Language.Haskell.TH hiding (Arity)
#else
import Language.Haskell.TH
#endif
import Language.Haskell.TH.Syntax (VarStrictType)
#if MIN_VERSION_template_haskell(2,7,0) && !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Lib (starK)
#endif
#if MIN_VERSION_template_haskell(2,8,0) && !(MIN_VERSION_template_haskell(2,10,0))
import Language.Haskell.TH.Syntax (mkNameG_tc)
#endif
import Text.Printf (printf)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Foldable as F (all)
import qualified Data.HashMap.Strict as H (lookup, toList)
import qualified Data.List.NonEmpty as NE (drop, length, reverse, splitAt)
import qualified Data.Map as M (fromList, findWithDefault, keys, lookup , singleton, size)
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Vector as V (unsafeIndex, null, length, create, fromList)
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)


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
deriveJSON = deriveJSONBoth deriveToJSON deriveFromJSON

-- | Generates both 'ToJSON1' and 'FromJSON1' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON1' and 'deriveFromJSON1'.
deriveJSON1 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON1' and 'FromJSON1'
            -- instances.
            -> Q [Dec]
deriveJSON1 = deriveJSONBoth deriveToJSON1 deriveFromJSON1

-- | Generates both 'ToJSON2' and 'FromJSON2' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON2' and 'deriveFromJSON2'.
deriveJSON2 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON2' and 'FromJSON2'
            -- instances.
            -> Q [Dec]
deriveJSON2 = deriveJSONBoth deriveToJSON2 deriveFromJSON2

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
deriveToJSON = deriveToJSONCommon toJSONClass

-- | Generates a 'ToJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON1 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON1' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON1 = deriveToJSONCommon toJSON1Class

-- | Generates a 'ToJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON2 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON2' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON2 = deriveToJSONCommon toJSON2Class

deriveToJSONCommon :: JSONClass
                   -- ^ The ToJSON variant being derived.
                   -> Options
                   -- ^ Encoding options.
                   -> Name
                   -- ^ Name of the type for which to generate an instance.
                   -> Q [Dec]
deriveToJSONCommon = deriveJSONClass [ (ToJSON,     \jc _ -> consToValue    jc)
                                     , (ToEncoding, \jc _ -> consToEncoding jc)
                                     ]

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value'.
mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON = mkToJSONCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToJSON :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkLiftToJSON = mkToJSONCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- functions on occurrences of the last two type parameters.
mkLiftToJSON2 :: Options -- ^ Encoding options.
              -> Name -- ^ Name of the type to encode.
              -> Q Exp
mkLiftToJSON2 = mkToJSONCommon toJSON2Class

mkToJSONCommon :: JSONClass -- ^ Which class's method is being derived.
               -> Options -- ^ Encoding options.
               -> Name -- ^ Name of the encoded type.
               -> Q Exp
mkToJSONCommon = mkFunCommon (\jc _ -> consToValue jc)

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string.
mkToEncoding :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkToEncoding = mkToEncodingCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToEncoding :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the type to encode.
                 -> Q Exp
mkLiftToEncoding = mkToEncodingCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- functions on occurrences of the last two type parameters.
mkLiftToEncoding2 :: Options -- ^ Encoding options.
                  -> Name -- ^ Name of the type to encode.
                  -> Q Exp
mkLiftToEncoding2 = mkToEncodingCommon toJSON2Class

mkToEncodingCommon :: JSONClass -- ^ Which class's method is being derived.
                   -> Options -- ^ Encoding options.
                   -> Name -- ^ Name of the encoded type.
                   -> Q Exp
mkToEncodingCommon = mkFunCommon (\jc _ -> consToEncoding jc)

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates
-- code to generate a 'Value' of a number of constructors. All constructors
-- must be from the same type.
consToValue :: JSONClass
            -- ^ The ToJSON variant being derived.
            -> Options
            -- ^ Encoding options.
            -> [Con]
            -- ^ Constructors for which to generate JSON generating code.
            -> Q Exp

consToValue _ _ [] = error $ "Data.Aeson.TH.consToValue: "
                            ++ "Not a single constructor given!"

consToValue jc opts cons = do
    value <- newName "value"
    tjs   <- newNameList "_tj"  $ arityInt jc
    tjls  <- newNameList "_tjl" $ arityInt jc
    let zippedTJs      = zip tjs tjls
        interleavedTJs = interleave tjs tjls
    lamE (map varP $ interleavedTJs ++ [value]) $
        caseE (varE value) (matches zippedTJs)
  where
    matches tjs = case cons of
      -- A single constructor is directly encoded. The constructor itself may be
      -- forgotten.
      [con] -> [argsToValue jc tjs opts False con]
      _ | allNullaryToStringTag opts && all isNullary cons ->
              [ match (conP conName []) (normalB $ conStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise -> [argsToValue jc tjs opts True con | con <- cons]

conStr :: Options -> Name -> Q Exp
conStr opts = appE [|String|] . conTxt opts

conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . conStringE opts

conStringE :: Options -> Name -> Q Exp
conStringE opts = stringE . constructorTagModifier opts . nameBase

-- | Helper function used by both 'deriveToJSON' and 'mkToEncoding'. Generates
-- code to write out a value for a number of constructors. All constructors
-- must be from the same type.
consToEncoding :: JSONClass
               -- ^ The ToJSON variant being derived.
               -> Options
               -- ^ Encoding options.
               -> [Con]
               -- ^ Constructors for which to generate JSON generating code.
               -> Q Exp

consToEncoding _ _ [] = error $ "Data.Aeson.TH.consToEncoding: "
                        ++ "Not a single constructor given!"

consToEncoding jc opts cons = do
    value <- newName "value"
    tes   <- newNameList "_te"  $ arityInt jc
    tels  <- newNameList "_tel" $ arityInt jc
    let zippedTEs      = zip tes tels
        interleavedTEs = interleave tes tels
    lamE (map varP $ interleavedTEs ++ [value]) $
        caseE (varE value) (matches zippedTEs)
  where
    matches tes = case cons of
      -- A single constructor is directly encoded. The constructor itself may be
      -- forgotten.
      [con] -> [argsToEncoding jc tes opts False con]
      -- Encode just the name of the constructor of a sum type iff all the
      -- constructors are nullary.
      _ | allNullaryToStringTag opts && all isNullary cons ->
              [ match (conP conName [])
                (normalB $ encStr opts conName) []
              | con <- cons
              , let conName = getConName con
              ]
        | otherwise -> [argsToEncoding jc tes opts True con | con <- cons]

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
          UntaggedValue -> exp
    | otherwise = exp

nullarySumToValue :: Options -> Bool -> Name -> Q Exp
nullarySumToValue opts multiCons conName =
    case sumEncoding opts of
      TaggedObject{tagFieldName} ->
          [|A.object|] `appE` listE
            [ infixApp [|T.pack tagFieldName|] [|(.=)|] (conStr opts conName)
            ]
      UntaggedValue -> conStr opts conName
      _ -> sumToValue opts multiCons conName [e|toJSON ([] :: [()])|]

-- | Generates code to generate the JSON encoding of a single constructor.
argsToValue :: JSONClass -> [(Name, Name)] -> Options -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
argsToValue jc tjs opts multiCons (NormalC conName []) = do
    ([], _) <- reifyConTys jc tjs conName
    match (conP conName [])
          (normalB (nullarySumToValue opts multiCons conName))
          []

-- Polyadic constructors with special case for unary constructors.
argsToValue jc tjs opts multiCons (NormalC conName ts) = do
    (argTys, tvMap) <- reifyConTys jc tjs conName
    let len = length ts
    args <- newNameList "arg" len
    js <- case [ dispatchToJSON jc conName tvMap argTy
                   `appE` varE arg
               | (arg, argTy) <- zip args argTys
               ] of
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
                                varE mv `appE`
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
argsToValue jc tjs opts multiCons (RecC conName ts) = case (unwrapUnaryRecords opts, not multiCons, ts) of
  (True,True,[(_,st,ty)]) -> argsToValue jc tjs opts multiCons (NormalC conName [(st,ty)])
  _ -> do
    (argTys, tvMap) <- reifyConTys jc tjs conName
    args <- newNameList "arg" $ length ts
    let exp = [|A.object|] `appE` pairs

        pairs | omitNothingFields opts = infixApp maybeFields
                                                  [|(++)|]
                                                  restFields
              | otherwise = listE $ map toPair argCons

        argCons = zip3 args argTys ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE $ map toPair rest

        (maybes, rest) = partition isMaybe argCons

        maybeToPair (arg, argTy, (field, _, _)) =
            infixApp ([|keyValuePairWith|]
                        `appE` dispatchToJSON jc conName tvMap argTy
                        `appE` toFieldName field)
                     [|(<$>)|]
                     (varE arg)

        toPair (arg, argTy, (field, _, _)) =
            [|keyValuePairWith|]
              `appE` dispatchToJSON jc conName tvMap argTy
              `appE` toFieldName field
              `appE` varE arg

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
                   UntaggedValue -> exp
            else exp
          ) []

-- Infix constructors.
argsToValue jc tjs opts multiCons (InfixC _ conName _) = do
    ([alTy, arTy], tvMap) <- reifyConTys jc tjs conName
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ sumToValue opts multiCons conName
          $ [|toJSON|] `appE` listE [ dispatchToJSON jc conName tvMap aTy
                                        `appE` varE a
                                    | (a, aTy) <- [(al,alTy), (ar,arTy)]
                                    ]
          )
          []
-- Existentially quantified constructors.
argsToValue jc tjs opts multiCons (ForallC _ _ con) =
    argsToValue jc tjs opts multiCons con

#if MIN_VERSION_template_haskell(2,11,0)
-- GADTs.
argsToValue jc tjs opts multiCons (GadtC conNames ts _) =
    argsToValue jc tjs opts multiCons $ NormalC (head conNames) ts

argsToValue jc tjs opts multiCons (RecGadtC conNames ts _) =
    argsToValue jc tjs opts multiCons $ RecC (head conNames) ts
#endif

isMaybe :: (a, b, (c, d, Type)) -> Bool
isMaybe (_, _, (_, _, AppT (ConT t) _)) = t == ''Maybe
isMaybe _                               = False

(<^>) :: ExpQ -> ExpQ -> ExpQ
(<^>) a b = infixApp a [|(E.><)|] b
infixr 6 <^>

(<:>) :: ExpQ -> ExpQ -> ExpQ
(<:>) a b = a <^> [|E.colon|] <^> b
infixr 5 <:>

(<%>) :: ExpQ -> ExpQ -> ExpQ
(<%>) a b = a <^> [|E.comma|] <^> b
infixr 4 <%>

array :: ExpQ -> ExpQ
array exp = [|E.wrapArray|] `appE` exp

object :: ExpQ -> ExpQ
object exp = [|E.wrapObject|] `appE` exp

sumToEncoding :: Options -> Bool -> Name -> Q Exp -> Q Exp
sumToEncoding opts multiCons conName exp
    | multiCons =
        let fexp = exp in
        case sumEncoding opts of
          TwoElemArray ->
            array (encStr opts conName <%> fexp)
          TaggedObject{tagFieldName, contentsFieldName} ->
            object $
            ([|E.text (T.pack tagFieldName)|] <:> encStr opts conName) <%>
            ([|E.text (T.pack contentsFieldName)|] <:> fexp)
          ObjectWithSingleField ->
            object (encStr opts conName <:> fexp)
          UntaggedValue -> exp
    | otherwise = exp

nullarySumToEncoding :: Options -> Bool -> Name -> Q Exp
nullarySumToEncoding opts multiCons conName =
    case sumEncoding opts of
      TaggedObject{tagFieldName} ->
          object $
            [|E.text (T.pack tagFieldName)|] <:> encStr opts conName
      UntaggedValue -> encStr opts conName
      _ -> sumToEncoding opts multiCons conName [e|toEncoding ([] :: [()])|]

-- | Generates code to generate the JSON encoding of a single constructor.
argsToEncoding :: JSONClass -> [(Name, Name)] -> Options -> Bool -> Con -> Q Match
-- Nullary constructors. Generates code that explicitly matches against the
-- constructor even though it doesn't contain data. This is useful to prevent
-- type errors.
argsToEncoding jc tes opts multiCons (NormalC conName []) = do
    ([], _) <- reifyConTys jc tes conName
    match (conP conName [])
          (normalB (nullarySumToEncoding opts multiCons conName))
          []

-- Polyadic constructors with special case for unary constructors.
argsToEncoding jc tes opts multiCons (NormalC conName ts) = do
    (argTys, tvMap) <- reifyConTys jc tes conName
    let len = length ts
    args <- newNameList "arg" len
    js <- case zip args argTys of
            -- Single argument is directly converted.
            [(e,eTy)] -> return (dispatchToEncoding jc conName tvMap eTy
                                   `appE` varE e)
            -- Multiple arguments are converted to a JSON array.
            es  ->
              return (array (foldr1 (<%>) [ dispatchToEncoding jc conName tvMap xTy
                                              `appE` varE x
                                          | (x,xTy) <- es
                                          ]))
    match (conP conName $ map varP args)
          (normalB $ sumToEncoding opts multiCons conName js)
          []

-- Records.
argsToEncoding jc tes opts multiCons (RecC conName ts) = case (unwrapUnaryRecords opts, not multiCons, ts) of
  (True,True,[(_,st,ty)]) -> argsToEncoding jc tes opts multiCons (NormalC conName [(st,ty)])
  _ -> do
    args <- newNameList "arg" $ length ts
    (argTys, tvMap) <- reifyConTys jc tes conName
    let exp = object objBody

        objBody = [|E.econcat|] `appE`
                  ([|intersperse E.comma|] `appE` pairs)
        pairs | omitNothingFields opts = infixApp maybeFields
                                                  [|(++)|]
                                                  restFields
              | otherwise = listE (map toPair argCons)

        argCons = zip3 args argTys ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE (map toPair rest)

        (maybes, rest) = partition isMaybe argCons

        maybeToPair (arg, argTy, (field, _, _)) =
            infixApp
              (infixApp
                (infixE
                  (Just $ toFieldName field <^> [|E.colon|])
                  [|(E.><)|]
                  Nothing)
                [|(.)|]
                (dispatchToEncoding jc conName tvMap argTy))
              [|(<$>)|]
              (varE arg)

        toPair (arg, argTy, (field, _, _)) =
          toFieldName field
            <:> dispatchToEncoding jc conName tvMap argTy
                  `appE` varE arg

        toFieldName field = [|E.text|] `appE`
                            ([|T.pack|] `appE` fieldLabelExp opts field)

    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons
            then case sumEncoding opts of
                   TwoElemArray -> array $
                     encStr opts conName <%>  exp
                   TaggedObject{tagFieldName} -> object $
                     ([|E.text (T.pack tagFieldName)|] <:>
                      encStr opts conName) <%>
                     objBody
                   ObjectWithSingleField -> object $
                     encStr opts conName <:> exp
                   UntaggedValue -> exp
            else exp
          ) []

-- Infix constructors.
argsToEncoding jc tes opts multiCons (InfixC _ conName _) = do
    al <- newName "argL"
    ar <- newName "argR"
    ([alTy,arTy], tvMap) <- reifyConTys jc tes conName
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ sumToEncoding opts multiCons conName
          $ array (foldr1 (<%>) [ dispatchToEncoding jc conName tvMap aTy
                                    `appE` varE a
                                | (a,aTy) <- [(al,alTy), (ar,arTy)]
                                ])
          )
          []
-- Existentially quantified constructors.
argsToEncoding jc tes opts multiCons (ForallC _ _ con) =
    argsToEncoding jc tes opts multiCons con

#if MIN_VERSION_template_haskell(2,11,0)
-- GADTs.
argsToEncoding jc tes opts multiCons (GadtC conNames ts _) =
    argsToEncoding jc tes opts multiCons $ NormalC (head conNames) ts

argsToEncoding jc tes opts multiCons (RecGadtC conNames ts _) =
    argsToEncoding jc tes opts multiCons $ RecC (head conNames) ts
#endif

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
deriveFromJSON = deriveFromJSONCommon fromJSONClass

-- | Generates a 'FromJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON1 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON1' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON1 = deriveFromJSONCommon fromJSON1Class

-- | Generates a 'FromJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON2 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON3' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON2 = deriveFromJSONCommon fromJSON2Class

deriveFromJSONCommon :: JSONClass
                     -- ^ The FromJSON variant being derived.
                     -> Options
                     -- ^ Encoding options.
                     -> Name
                     -- ^ Name of the type for which to generate an instance.
                     -- declaration.
                     -> Q [Dec]
deriveFromJSONCommon = deriveJSONClass [(ParseJSON, consFromJSON)]

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor.
mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON = mkParseJSONCommon fromJSONClass

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- function on occurrences of the last type parameter.
mkLiftParseJSON :: Options -- ^ Encoding options.
                -> Name -- ^ Name of the encoded type.
                -> Q Exp
mkLiftParseJSON = mkParseJSONCommon fromJSON1Class

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- functions on occurrences of the last two type parameters.
mkLiftParseJSON2 :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the encoded type.
                 -> Q Exp
mkLiftParseJSON2 = mkParseJSONCommon fromJSON2Class

mkParseJSONCommon :: JSONClass -- ^ Which class's method is being derived.
                  -> Options -- ^ Encoding options.
                  -> Name -- ^ Name of the encoded type.
                  -> Q Exp
mkParseJSONCommon = mkFunCommon consFromJSON

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: JSONClass
             -- ^ The FromJSON variant being derived.
             -> Name
             -- ^ Name of the type to which the constructors belong.
             -> Options
             -- ^ Encoding options
             -> [Con]
             -- ^ Constructors for which to generate JSON parsing code.
             -> Q Exp

consFromJSON _ _ _ [] = error $ "Data.Aeson.TH.consFromJSON: "
                              ++ "Not a single constructor given!"

consFromJSON jc tName opts cons = do
  value <- newName "value"
  pjs   <- newNameList "_pj"  $ arityInt jc
  pjls  <- newNameList "_pjl" $ arityInt jc
  let zippedPJs      = zip pjs pjls
      interleavedPJs = interleave pjs pjls
  lamE (map varP $ interleavedPJs ++ [value]) $ lamExpr value zippedPJs

  where
    lamExpr value pjs = case cons of
      [con] -> parseArgs jc pjs tName opts con (Right value)
      _ | sumEncoding opts == UntaggedValue
            -> parseUntaggedValue pjs cons value
        | otherwise
            -> caseE (varE value) $
                   if allNullaryToStringTag opts && all isNullary cons
                   then allNullaryMatches
                   else mixedMatches pjs

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
                        `appE` litE (stringL $ show tName)
                        `appE` ([|T.unpack|] `appE` varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` litE (stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches pjs =
        case sumEncoding opts of
          TaggedObject {tagFieldName, contentsFieldName} ->
            parseObject $ parseTaggedObject pjs tagFieldName contentsFieldName
          UntaggedValue -> error "UntaggedValue: Should be handled already"
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField pjs
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'Array [varP arr])
                       (guardedB
                        [ liftM2 (,) (normalG $ infixApp ([|V.length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray pjs arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     ([|not2ElemArray|]
                                       `appE` litE (stringL $ show tName)
                                       `appE` ([|V.length|] `appE` varE arr))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` litE (stringL $ show tName)
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
                         `appE` litE (stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject pjs typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey)
                  (infixApp (varE obj)
                            [|(.:)|]
                            ([|T.pack|] `appE` stringE typFieldName))
          , noBindS $ parseContents pjs conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject
          ]

    parseUntaggedValue pjs cons' conVal =
        foldr1 (\e e' -> infixApp e [|(<|>)|] e')
               (map (\x -> parseValue pjs x conVal) cons')

    parseValue _pjs (NormalC conName []) conVal = do
      str <- newName "str"
      caseE (varE conVal)
        [ match (conP 'String [varP str])
                (guardedB
                  [ liftM2 (,) (normalG $ infixApp (varE str) [|(==)|] ([|T.pack|] `appE` conStringE opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  ]
                )
                []
        , matchFailed tName conName "String"
        ]
    parseValue pjs con conVal =
      parseArgs jc pjs tName opts con (Right conVal)


    parse2ElemArray pjs arr = do
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
                             (normalB $ parseContents pjs
                                                      txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` litE (stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField pjs obj = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      caseE ([e|H.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents pjs conKey (Right conVal) 'conNotFoundFailObjectSingleField)
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` litE (stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents pjs conKey contents errorFun =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     ([|T.pack|] `appE`
                                                        conNameExp opts con)
                             e <- parseArgs jc pjs tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` litE (stringL $ show tName)
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
               (guardedB
                [ liftM2 (,) (normalG $ [|V.null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty Array")
                                (infixApp (litE $ stringL "Array of length ")
                                          [|(++)|]
                                          ([|show . V.length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "Array"
    ]

parseUnaryMatches :: JSONClass -> TyVarMap -> Type -> Name -> [Q Match]
parseUnaryMatches jc tvMap argTy conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    (dispatchParseJSON jc conName tvMap argTy
                                      `appE` varE arg)
               )
               []
    ]

parseRecord :: JSONClass
            -> TyVarMap
            -> [Type]
            -> Options
            -> Name
            -> Name
            -> [VarStrictType]
            -> Name
            -> ExpQ
parseRecord jc tvMap argTys opts tName conName ts obj =
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      x:xs = [ [|lookupField|]
               `appE` dispatchParseJSON jc conName tvMap argTy
               `appE` litE (stringL $ show tName)
               `appE` litE (stringL $ constructorTagModifier opts $ nameBase conName)
               `appE` varE obj
               `appE` ( [|T.pack|] `appE` fieldLabelExp opts field
                      )
             | ((field, _, _), argTy) <- zip ts argTys
             ]

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ infixApp (varE obj)
                                    [|(.:)|]
                                    ([|T.pack|] `appE`
                                       litE (stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]

matchCases :: Either (String, Name) Name -> [MatchQ] -> Q Exp
matchCases (Left (valFieldName, obj)) = getValField obj valFieldName
matchCases (Right valName)            = caseE (varE valName)

-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: JSONClass -- ^ The FromJSON variant being derived.
          -> [(Name, Name)] -- ^ The names of the encoding/decoding function arguments.
          -> Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> Con -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
parseArgs jc pjs _ _ (NormalC conName []) (Left _) = do
  ([], _) <- reifyConTys jc pjs conName
  [|pure|] `appE` conE conName
parseArgs jc pjs tName _ (NormalC conName []) (Right valName) = do
  ([], _) <- reifyConTys jc pjs conName
  caseE (varE valName) $ parseNullaryMatches tName conName

-- Unary constructors.
parseArgs jc pjs _ _ (NormalC conName [_]) contents = do
  ([argTy], tvMap) <- reifyConTys jc pjs conName
  matchCases contents $ parseUnaryMatches jc tvMap argTy conName

-- Polyadic constructors.
parseArgs jc pjs tName _ (NormalC conName ts) contents = do
    (argTys, tvMap) <- reifyConTys jc pjs conName
    let len = genericLength ts
    matchCases contents $ parseProduct jc tvMap argTys tName conName len

-- Records.
parseArgs jc pjs tName opts (RecC conName ts) (Left (_, obj)) = do
    (argTys, tvMap) <- reifyConTys jc pjs conName
    parseRecord jc tvMap argTys opts tName conName ts obj
parseArgs jc pjs tName opts (RecC conName ts) (Right valName) = case (unwrapUnaryRecords opts,ts) of
  (True,[(_,st,ty)])-> parseArgs jc pjs tName opts (NormalC conName [(st,ty)]) (Right valName)
  _ -> do
    obj <- newName "recObj"
    (argTys, tvMap) <- reifyConTys jc pjs conName
    caseE (varE valName)
      [ match (conP 'Object [varP obj]) (normalB $
          parseRecord jc tvMap argTys opts tName conName ts obj) []
      , matchFailed tName conName "Object"
      ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
parseArgs jc pjs tName _ (InfixC _ conName _) contents = do
    (argTys, tvMap) <- reifyConTys jc pjs conName
    matchCases contents $ parseProduct jc tvMap argTys tName conName 2

-- Existentially quantified constructors. We ignore the quantifiers
-- and proceed with the contained constructor.
parseArgs jc pjs tName opts (ForallC _ _ con) contents =
    parseArgs jc pjs tName opts con contents

#if MIN_VERSION_template_haskell(2,11,0)
-- GADTs. We ignore the refined return type and proceed as if it were a
-- NormalC or RecC.
parseArgs jc pjs tName opts (GadtC conNames ts _) contents =
    parseArgs jc pjs tName opts (NormalC (head conNames) ts) contents

parseArgs jc pjs tName opts (RecGadtC conNames ts _) contents =
    parseArgs jc pjs tName opts (RecC (head conNames) ts) contents
#endif

-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: JSONClass -- ^ The FromJSON variant being derived.
             -> TyVarMap -- ^ Maps the last type variables to their decoding
                         --   function arguments.
             -> [Type] -- ^ The argument types of the constructor.
             -> Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct jc tvMap argTys tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
         let x:xs = [ dispatchParseJSON jc conName tvMap argTy
                      `appE`
                      infixApp (varE arr)
                               [|V.unsafeIndex|]
                               (litE $ integerL ix)
                    | (argTy, ix) <- zip argTys [0 .. numArgs - 1]
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
                                    ( infixApp (litE $ stringL "Array of length ")
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

class LookupField a where
    lookupField :: (Value -> Parser a) -> String -> String
                -> Object -> T.Text -> Parser a

instance OVERLAPPABLE_ LookupField a where
    lookupField = lookupFieldWith

instance INCOHERENT_ LookupField (Maybe a) where
    lookupField pj _ _ = parseOptionalFieldWith pj

lookupFieldWith :: (Value -> Parser a) -> String -> String
                -> Object -> T.Text -> Parser a
lookupFieldWith pj tName rec obj key =
    case H.lookup key obj of
      Nothing -> unknownFieldFail tName rec (T.unpack key)
      Just v  -> pj v <?> Key key

keyValuePairWith :: (v -> Value) -> T.Text -> v -> Pair
keyValuePairWith tj name value = (name, tj value)

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
-- Shared ToJSON and FromJSON code
--------------------------------------------------------------------------------

-- | Functionality common to 'deriveJSON', 'deriveJSON1', and 'deriveJSON2'.
deriveJSONBoth :: (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'ToJSON'.
               -> (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'FromJSON'.
               -> Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
               -- instances.
               -> Q [Dec]
deriveJSONBoth dtj dfj opts name =
    liftM2 (++) (dtj opts name) (dfj opts name)

-- | Functionality common to @deriveToJSON(1)(2)@ and @deriveFromJSON(1)(2)@.
deriveJSONClass :: [(JSONFun, JSONClass -> Name -> Options -> [Con] -> Q Exp)]
                -- ^ The class methods and the functions which derive them.
                -> JSONClass
                -- ^ The class for which to generate an instance.
                -> Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a class instance
                -- declaration.
                -> Q [Dec]
deriveJSONClass consFuns jc opts name =
    withType name $ \name' ctxt tvbs cons mbTys ->
        (:[]) <$> fromCons name' ctxt tvbs cons mbTys
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Dec
    fromCons name' ctxt tvbs cons mbTys = do
      (instanceCxt, instanceType)
        <- buildTypeInstance name' jc ctxt tvbs mbTys
      instanceD (return instanceCxt)
                (return instanceType)
                (methodDecs name' cons)

    methodDecs :: Name -> [Con] -> [Q Dec]
    methodDecs name' cons = flip map consFuns $ \(jf, jfMaker) ->
      funD (jsonFunValName jf (arity jc))
           [ clause []
                    (normalB $ jfMaker jc name' opts cons)
                    []
           ]

mkFunCommon :: (JSONClass -> Name -> Options -> [Con] -> Q Exp)
            -- ^ The function which derives the expression.
            -> JSONClass
            -- ^ Which class's method is being derived.
            -> Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the encoded type.
            -> Q Exp
mkFunCommon consFun jc opts name = withType name fromCons
  where
    fromCons :: Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q Exp
    fromCons name' ctxt tvbs cons mbTys = do
        -- We force buildTypeInstance here since it performs some checks for whether
        -- or not the provided datatype's kind matches the derived method's
        -- typeclass, and produces errors if it can't.
        !_ <- buildTypeInstance name' jc ctxt tvbs mbTys
        consFun jc name' opts cons

dispatchFunByType :: JSONClass
                  -> JSONFun
                  -> Name
                  -> TyVarMap
                  -> Bool -- True if we are using the function argument that works
                          -- on lists (e.g., [a] -> Value). False is we are using
                          -- the function argument that works on single values
                          -- (e.g., a -> Value).
                  -> Type
                  -> Q Exp
dispatchFunByType _ jf _ tvMap list (VarT tyName) =
    varE $ case M.lookup tyName tvMap of
                Just (tfjExp, tfjlExp) -> if list then tfjlExp else tfjExp
                Nothing                -> jsonFunValOrListName list jf Arity0
dispatchFunByType jc jf conName tvMap list (SigT ty _) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list (ForallT _ _ ty) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arityInt jc) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = M.keys tvMap

    itf <- isTyFamily tyCon
    if any (`mentionsName` tyVarNames) lhsArgs
          || itf && any (`mentionsName` tyVarNames) tyArgs
       then outOfPlaceTyVarError jc conName
       else if any (`mentionsName` tyVarNames) rhsArgs
            then appsE $ varE (jsonFunValOrListName list jf $ toEnum numLastArgs)
                         : zipWith (dispatchFunByType jc jf conName tvMap)
                                   (cycle [False,True])
                                   (interleave rhsArgs rhsArgs)
            else varE $ jsonFunValOrListName list jf Arity0

dispatchToJSON, dispatchToEncoding, dispatchParseJSON
  :: JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchToJSON     jc n tvMap = dispatchFunByType jc ToJSON     n tvMap False
dispatchToEncoding jc n tvMap = dispatchFunByType jc ToEncoding n tvMap False
dispatchParseJSON  jc n tvMap = dispatchFunByType jc ParseJSON  n tvMap False

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
         -> (Name -> Cxt -> [TyVarBndr] -> [Con] -> Maybe [Type] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the datatype/data family 'Name', datatype context, type
         -- variable binders and constructors extracted from the given
         -- 'Name'. If the 'Name' is from a data family instance
         -- constructor, it will also have its instantiated types;
         -- otherwise, it will be 'Nothing'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
#if MIN_VERSION_template_haskell(2,11,0)
          DataD    ctxt _ tvbs _ cons _ -> f name ctxt tvbs cons Nothing
          NewtypeD ctxt _ tvbs _ con  _ -> f name ctxt tvbs [con] Nothing
#else
          DataD    ctxt _ tvbs   cons _ -> f name ctxt tvbs cons Nothing
          NewtypeD ctxt _ tvbs   con  _ -> f name ctxt tvbs [con] Nothing
#endif
          other -> fail $ ns ++ "Unsupported type: " ++ show other
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
#if MIN_VERSION_template_haskell(2,11,0)
                  DataInstD    _ _ _ _ cons _ -> any ((name ==) . getConName) cons
                  NewtypeInstD _ _ _ _ con  _ -> name == getConName con
#else
                  DataInstD    _ _ _   cons _ -> any ((name ==) . getConName) cons
                  NewtypeInstD _ _ _   con  _ -> name == getConName con
#endif
                  _ -> error $ ns ++ "Must be a data or newtype instance."
             in case instDec of
#if MIN_VERSION_template_haskell(2,11,0)
                  Just (DataInstD    ctxt _ instTys _ cons _) -> f parentName ctxt tvbs cons $ Just instTys
                  Just (NewtypeInstD ctxt _ instTys _ con  _) -> f parentName ctxt tvbs [con] $ Just instTys
#else
                  Just (DataInstD    ctxt _ instTys   cons _) -> f parentName ctxt tvbs cons $ Just instTys
                  Just (NewtypeInstD ctxt _ instTys   con  _) -> f parentName ctxt tvbs [con] $ Just instTys
#endif
                  _ -> fail $ ns ++
                    "Could not find data or newtype instance constructor."
          _ -> fail $ ns ++ "Data constructor " ++ show name ++
            " is not from a data family instance constructor."
#if MIN_VERSION_template_haskell(2,11,0)
      FamilyI DataFamilyD{} _ ->
#else
      FamilyI (FamilyD DataFam _ _ _) _ ->
#endif
        fail $ ns ++
          "Cannot use a data family name. Use a data family instance constructor instead."
      _ -> fail $ ns ++ "I need the name of a plain data type constructor, "
                      ++ "or a data family instance constructor."
  where
    ns :: String
    ns = "Data.Aeson.TH.withType: "

-- | Infer the context and instance head needed for a FromJSON or ToJSON instance.
buildTypeInstance :: Name
                  -- ^ The type constructor or data family name
                  -> JSONClass
                  -- ^ The typeclass to derive
                  -> Cxt
                  -- ^ The datatype context
                  -> [TyVarBndr]
                  -- ^ The type variables from the data type/data family declaration
                  -> Maybe [Type]
                  -- ^ 'Just' the types used to instantiate a data family instance,
                  -- or 'Nothing' if it's a plain data type
                  -> Q (Cxt, Type)
                  -- ^ The resulting 'Cxt' and 'Type' to use in a class instance
-- Plain data type/newtype case
buildTypeInstance tyConName jc dataCxt tvbs Nothing =
    let varTys :: [Type]
        varTys = map tvbToType tvbs
    in buildTypeInstanceFromTys tyConName jc dataCxt varTys False
-- Data family instance case
--
-- The CPP is present to work around a couple of annoying old GHC bugs.
-- See Note [Polykinded data families in Template Haskell]
buildTypeInstance dataFamName jc dataCxt tvbs (Just instTysAndKinds) = do
#if !(MIN_VERSION_template_haskell(2,8,0)) || MIN_VERSION_template_haskell(2,10,0)
    let instTys :: [Type]
        instTys = zipWith stealKindForType tvbs instTysAndKinds
#else
    let kindVarNames :: [Name]
        kindVarNames = nub $ concatMap (tyVarNamesOfType . tvbKind) tvbs

        numKindVars :: Int
        numKindVars = length kindVarNames

        givenKinds, givenKinds' :: [Kind]
        givenTys                :: [Type]
        (givenKinds, givenTys) = splitAt numKindVars instTysAndKinds
        givenKinds' = map sanitizeStars givenKinds

        -- A GHC 7.6-specific bug requires us to replace all occurrences of
        -- (ConT GHC.Prim.*) with StarT, or else Template Haskell will reject it.
        -- Luckily, (ConT GHC.Prim.*) only seems to occur in this one spot.
        sanitizeStars :: Kind -> Kind
        sanitizeStars = go
          where
            go :: Kind -> Kind
            go (AppT t1 t2)                 = AppT (go t1) (go t2)
            go (SigT t k)                   = SigT (go t) (go k)
            go (ConT n) | n == starKindName = StarT
            go t                            = t

            -- It's quite awkward to import * from GHC.Prim, so we'll just
            -- hack our way around it.
            starKindName :: Name
            starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"

    -- If we run this code with GHC 7.8, we might have to generate extra type
    -- variables to compensate for any type variables that Template Haskell
    -- eta-reduced away.
    -- See Note [Polykinded data families in Template Haskell]
    xTypeNames <- newNameList "tExtra" (length tvbs - length givenTys)

    let xTys   :: [Type]
        xTys = map VarT xTypeNames
        -- ^ Because these type variables were eta-reduced away, we can only
        --   determine their kind by using stealKindForType. Therefore, we mark
        --   them as VarT to ensure they will be given an explicit kind annotation
        --   (and so the kind inference machinery has the right information).

        substNamesWithKinds :: [(Name, Kind)] -> Type -> Type
        substNamesWithKinds nks t = foldr' (uncurry substNameWithKind) t nks

        -- The types from the data family instance might not have explicit kind
        -- annotations, which the kind machinery needs to work correctly. To
        -- compensate, we use stealKindForType to explicitly annotate any
        -- types without kind annotations.
        instTys :: [Type]
        instTys = map (substNamesWithKinds (zip kindVarNames givenKinds'))
                  -- Note that due to a GHC 7.8-specific bug
                  -- (see Note [Polykinded data families in Template Haskell]),
                  -- there may be more kind variable names than there are kinds
                  -- to substitute. But this is OK! If a kind is eta-reduced, it
                  -- means that is was not instantiated to something more specific,
                  -- so we need not substitute it. Using stealKindForType will
                  -- grab the correct kind.
                $ zipWith stealKindForType tvbs (givenTys ++ xTys)
#endif
    buildTypeInstanceFromTys dataFamName jc dataCxt instTys True

-- For the given Types, generate an instance context and head.
buildTypeInstanceFromTys :: Name
                         -- ^ The type constructor or data family name
                         -> JSONClass
                         -- ^ The typeclass to derive
                         -> Cxt
                         -- ^ The datatype context
                         -> [Type]
                         -- ^ The types to instantiate the instance with
                         -> Bool
                         -- ^ True if it's a data family, False otherwise
                         -> Q (Cxt, Type)
buildTypeInstanceFromTys tyConName jc dataCxt varTysOrig isDataFamily = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM expandSyn varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - arityInt jc

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || elem NotKindStar droppedStarKindStati) $
      derivingKindError jc tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = concatMap tyVarNamesOfType droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError jc tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint jc) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (union droppedKindVarNames kvNames'))
            $ take remainingLength varTysOrig

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ jsonClassName jc)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: JSONClass -> Type -> (Maybe Pred, [Name])
deriveConstraint jc t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = (Just (applyCon (jcConstraint Arity0) tName), [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | jcArity >= Arity1
              -> (Just (applyCon (jcConstraint Arity1) tName), ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | jcArity == Arity2
                   -> (Just (applyCon (jcConstraint Arity2) tName), ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

    jcArity :: Arity
    jcArity = arity jc

    jcConstraint :: Arity -> Name
    jcConstraint = jsonClassName . JSONClass (direction jc)

{-
Note [Polykinded data families in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to come up with the correct instance context and head for an instance, e.g.,

  instance C a => C (Data a) where ...

We need to know the exact types and kinds used to instantiate the instance. For
plain old datatypes, this is simple: every type must be a type variable, and
Template Haskell reliably tells us the type variables and their kinds.

Doing the same for data families proves to be much harder for three reasons:

1. On any version of Template Haskell, it may not tell you what an instantiated
   type's kind is. For instance, in the following data family instance:

     data family Fam (f :: * -> *) (a :: *)
     data instance Fam f a

   Then if we use TH's reify function, it would tell us the TyVarBndrs of the
   data family declaration are:

     [KindedTV f (AppT (AppT ArrowT StarT) StarT),KindedTV a StarT]

   and the instantiated types of the data family instance are:

     [VarT f1,VarT a1]

   We can't just pass [VarT f1,VarT a1] to buildTypeInstanceFromTys, since we
   have no way of knowing their kinds. Luckily, the TyVarBndrs tell us what the
   kind is in case an instantiated type isn't a SigT, so we use the stealKindForType
   function to ensure all of the instantiated types are SigTs before passing them
   to buildTypeInstanceFromTys.
2. On GHC 7.6 and 7.8, a bug is present in which Template Haskell lists all of
   the specified kinds of a data family instance efore any of the instantiated
   types. Fortunately, this is easy to deal with: you simply count the number of
   distinct kind variables in the data family declaration, take that many elements
   from the front of the  Types list of the data family instance, substitute the
   kind variables with their respective instantiated kinds (which you took earlier),
   and proceed as normal.
3. On GHC 7.8, an even uglier bug is present (GHC Trac #9692) in which Template
   Haskell might not even list all of the Types of a data family instance, since
   they are eta-reduced away! And yes, kinds can be eta-reduced too.

   The simplest workaround is to count how many instantiated types are missing from
   the list and generate extra type variables to use in their place. Luckily, we
   needn't worry much if its kind was eta-reduced away, since using stealKindForType
   will get it back.

Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since Template Haskell doesn't always
have the best track record with reifying kind signatures), then GHC will flat-out
reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the mk- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)   If there's a type parameter n of kind *, generate a ToJSON n/FromJSON n
         constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a ToJSON1 n/FromJSON1 n constraint, and if
         k1/k2 are kind variables, then substitute k1/k2 with * elsewhere in the
         types. We must consider the case where they are kind variables because
         you might have a scenario like this:

           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))

         Which would have a derived ToJSON1 instance of:

           instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a ToJSON2 n/FromJSON2 n constraint
         and perform kind substitution as in the other cases.
-}

-- Determines the types of a constructor's arguments as well as the last type
-- parameters (mapped to their encoding/decoding functions), expanding through
-- any type synonyms.
--
-- The type parameters are determined on a constructor-by-constructor basis since
-- they may be refined to be particular types in a GADT.
reifyConTys :: JSONClass
            -> [(Name, Name)]
            -> Name
            -> Q ([Type], TyVarMap)
reifyConTys jc tpjs conName = do
    info  <- reify conName
    (ctxt, uncTy) <- case info of
        DataConI _ ty _
#if !(MIN_VERSION_template_haskell(2,11,0))
                _
#endif
                -> fmap uncurryTy (expandSyn ty)
        _ -> error "Must be a data constructor"
    let (argTys, [resTy]) = NE.splitAt (NE.length uncTy - 1) uncTy
        unapResTy = unapplyTy resTy
        -- If one of the last type variables is refined to a particular type
        -- (i.e., not truly polymorphic), we mark it with Nothing and filter
        -- it out later, since we only apply encoding/decoding functions to
        -- arguments of a type that it (1) one of the last type variables,
        -- and (2) of a truly polymorphic type.
        jArity = arityInt jc
        mbTvNames = map varTToNameMaybe $
                        NE.drop (NE.length unapResTy - jArity) unapResTy
        -- We use M.fromList to ensure that if there are any duplicate type
        -- variables (as can happen in a GADT), the rightmost type variable gets
        -- associated with the show function.
        --
        -- See Note [Matching functions with GADT type variables]
        tvMap = M.fromList
                    . catMaybes -- Drop refined types
                    $ zipWith (\mbTvName tpj ->
                                  fmap (\tvName -> (tvName, tpj)) mbTvName)
                              mbTvNames tpjs
    if (any (`predMentionsName` M.keys tvMap) ctxt
         || M.size tvMap < jArity)
         && not (allowExQuant jc)
       then existentialContextError conName
       else return (argTys, tvMap)

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving ToJSON2, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which encoding functions should be applied to which arguments of BothCon?
We have a choice, since both the function of type (a -> Value) and of type
(b -> Value) can be applied to either argument. In such a scenario, the
second encoding function takes precedence over the first encoding function, so the
derived ToJSON2 instance would be something like:

  instance ToJSON2 Both where
    liftToJSON2 tj1 tj2 p (BothCon x1 x2) = Array $ create $ do
      mv <- unsafeNew 2
      unsafeWrite mv 0 (tj1 x1)
      unsafeWrite mv 1 (tj2 x2)
      return mv

This is not an arbitrary choice, as this definition ensures that
liftToJSON2 toJSON = liftToJSON for a derived ToJSON1 instance for
Both.
-}

-- A mapping of type variable Names to their encoding/decoding function Names.
-- For example, in a ToJSON2 declaration, a TyVarMap might look like
--
-- { a ~> (tj1, tjl1)
-- , b ~> (tj2, tjl2) }
--
-- where a and b are the last two type variables of the datatype, tj1 and tjl1 are
-- the function arguments of types (a -> Value) and ([a] -> Value), and tj2 and tjl2
-- are the function arguments of types (b -> Value) and ([b] -> Value).
type TyVarMap = Map Name (Name, Name)

-- | If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
stealKindForType _   t        = t

-- | Extracts the kind from a type variable binder.
tvbKind :: TyVarBndr -> Kind
#if MIN_VERSION_template_haskell(2,8,0)
tvbKind (PlainTV  _  ) = StarT
#else
tvbKind (PlainTV  _  ) = StarK
#endif
tvbKind (KindedTV _ k) = k

tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix len = mapM newName [prefix ++ show n | n <- [1..len]]

-- Gets all of the type/kind variable names mentioned somewhere in a Type.
tyVarNamesOfType :: Type -> [Name]
tyVarNamesOfType = go
  where
    go :: Type -> [Name]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           ++ go _k
#endif
    go (VarT n)     = [n]
    go _            = []

-- | Gets all of the type/kind variable names mentioned somewhere in a Kind.
tyVarNamesOfKind :: Kind -> [Name]
#if MIN_VERSION_template_haskell(2,8,0)
tyVarNamesOfKind = tyVarNamesOfType
#else
tyVarNamesOfKind _ = [] -- There are no kind variables
#endif

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (NE.length uk - 1 == kindArrows) && F.all isStarOrVar uk
        then Just (concatMap tyVarNamesOfKind uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe (VarT n)   = Just n
varTToNameMaybe (SigT t _) = varTToNameMaybe t
varTToNameMaybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToNameMaybe

-- | Extracts the name from a constructor.
getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con
#if MIN_VERSION_template_haskell(2,11,0)
getConName (GadtC    names _ _) = head names
getConName (RecGadtC names _ _) = head names
#endif

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#else
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, NonEmpty Type)
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1 <| tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], t :| [])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = snd . uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1 <| uncurryKind k2
uncurryKind k              = k :| []
#endif

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k !0 = k
#if MIN_VERSION_template_haskell(2,8,0)
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)
#else
    go k !n = go (ArrowK StarK k) (n - 1)
#endif

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

applyCon :: Name -> Name -> Pred
applyCon con t =
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = ForallT tvs ctx <$> expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t k)          = do t' <- expandSyn t
                                   k' <- expandSynKind k
                                   return (SigT t' k')
expandSyn t                   = return t

expandSynKind :: Kind -> Q Kind
#if MIN_VERSION_template_haskell(2,8,0)
expandSynKind = expandSyn
#else
expandSynKind = return -- There are no kind synonyms to deal with
#endif

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do
    t2' <- expandSyn t2
    expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl' AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                subs = mkSubst tvs ts'
                rhs' = substType subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type TypeSubst = Map Name Type
type KindSubst = Map Name Kind

mkSubst :: [TyVarBndr] -> [Type] -> TypeSubst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in M.fromList $ zip vs' ts

substType :: TypeSubst -> Type -> Type
substType subs (ForallT v c t) = ForallT v c $ substType subs t
substType subs t@(VarT n)      = M.findWithDefault t n subs
substType subs (AppT t1 t2)    = AppT (substType subs t1) (substType subs t2)
substType subs (SigT t k)      = SigT (substType subs t)
#if MIN_VERSION_template_haskell(2,8,0)
                                      (substType subs k)
#else
                                      k
#endif
substType _ t                  = t

substKind :: KindSubst -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
substKind = substType
#else
substKind _ t = t -- There are no kind variables!
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = substKind (M.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: JSONClass -> Name -> Q a
derivingKindError jc tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind "
  . showString (pprint . createKindChain $ arityInt jc)
  $ ""
  where
    className :: String
    className = nameBase $ jsonClassName jc

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: JSONClass -> Name -> a
outOfPlaceTyVarError jc conName = error
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must only use its last "
    . shows n
    . showString " type variable(s) within the last "
    . shows n
    . showString " argument(s) of a data type"
    $ ""
  where
    n :: Int
    n = arityInt jc

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> a
existentialContextError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of the arity of the ToJSON/FromJSON typeclass being derived.
data Arity = Arity0 | Arity1 | Arity2
  deriving (Enum, Eq, Ord)

-- | Whether ToJSON(1)(2) or FromJSON(1)(2) is being derived.
data Direction = To | From

-- | A representation of which typeclass method is being spliced in.
data JSONFun = ToJSON | ToEncoding | ParseJSON

-- | A representation of which typeclass is being derived.
data JSONClass = JSONClass { direction :: Direction, arity :: Arity }

toJSONClass, toJSON1Class, toJSON2Class,
    fromJSONClass, fromJSON1Class, fromJSON2Class :: JSONClass
toJSONClass    = JSONClass To   Arity0
toJSON1Class   = JSONClass To   Arity1
toJSON2Class   = JSONClass To   Arity2
fromJSONClass  = JSONClass From Arity0
fromJSON1Class = JSONClass From Arity1
fromJSON2Class = JSONClass From Arity2

jsonClassName :: JSONClass -> Name
jsonClassName (JSONClass To   Arity0) = ''ToJSON
jsonClassName (JSONClass To   Arity1) = ''ToJSON1
jsonClassName (JSONClass To   Arity2) = ''ToJSON2
jsonClassName (JSONClass From Arity0) = ''FromJSON
jsonClassName (JSONClass From Arity1) = ''FromJSON1
jsonClassName (JSONClass From Arity2) = ''FromJSON2

jsonFunValName :: JSONFun -> Arity -> Name
jsonFunValName ToJSON     Arity0 = 'toJSON
jsonFunValName ToJSON     Arity1 = 'liftToJSON
jsonFunValName ToJSON     Arity2 = 'liftToJSON2
jsonFunValName ToEncoding Arity0 = 'toEncoding
jsonFunValName ToEncoding Arity1 = 'liftToEncoding
jsonFunValName ToEncoding Arity2 = 'liftToEncoding2
jsonFunValName ParseJSON  Arity0 = 'parseJSON
jsonFunValName ParseJSON  Arity1 = 'liftParseJSON
jsonFunValName ParseJSON  Arity2 = 'liftParseJSON2

jsonFunListName :: JSONFun -> Arity -> Name
jsonFunListName ToJSON     Arity0 = 'toJSONList
jsonFunListName ToJSON     Arity1 = 'liftToJSONList
jsonFunListName ToJSON     Arity2 = 'liftToJSONList2
jsonFunListName ToEncoding Arity0 = 'toEncodingList
jsonFunListName ToEncoding Arity1 = 'liftToEncodingList
jsonFunListName ToEncoding Arity2 = 'liftToEncodingList2
jsonFunListName ParseJSON  Arity0 = 'parseJSONList
jsonFunListName ParseJSON  Arity1 = 'liftParseJSONList
jsonFunListName ParseJSON  Arity2 = 'liftParseJSONList2

jsonFunValOrListName :: Bool -- e.g., toJSONList if True, toJSON if False
                     -> JSONFun -> Arity -> Name
jsonFunValOrListName False = jsonFunValName
jsonFunValOrListName True  = jsonFunListName

arityInt :: JSONClass -> Int
arityInt = fromEnum . arity

allowExQuant :: JSONClass -> Bool
allowExQuant (JSONClass To _) = True
allowExQuant _                = False

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
                     _               -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName
