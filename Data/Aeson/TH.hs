{-# LANGUAGE CPP
           , NoImplicitPrelude
           , TemplateHaskell
           , NamedFieldPuns
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
  #-}

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

Next we derive the necessary instances. Note that we make use of the feature to
change record field names. In this case we drop the first 4 characters of every
field name.

@
$('deriveJSON' 'defaultOptions'{'fieldNameModifier' = 'drop' 4} ''D)
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
    ( Options(..), SumEncoding(..), defaultOptions

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
import Data.Aeson.Types ( Value(..), Parser )
-- from base:
import Control.Applicative ( pure, (<$>), (<*>) )
import Control.Monad       ( return, mapM, liftM2, fail )
import Data.Bool           ( Bool(False, True), otherwise, (&&) )
import Data.Eq             ( (==) )
import Data.Function       ( ($), (.), id, const )
import Data.Functor        ( fmap )
import Data.Int            ( Int )
import Data.Either         ( Either(Left, Right), either )
import Data.List           ( (++), foldl, foldl', intercalate
                           , length, map, zip, genericLength, all
                           )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Prelude             ( String, (-), Integer, fromIntegral, error )
import Text.Printf         ( printf )
import Text.Show           ( show )
#if __GLASGOW_HASKELL__ < 700
import Control.Monad       ( (>>=) )
import Prelude             ( fromInteger )
#endif
-- from unordered-containers:
import qualified Data.HashMap.Strict as H ( lookup )
-- from template-haskell:
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )
-- from text:
import qualified Data.Text as T ( Text, pack, unpack )
-- from vector:
import qualified Data.Vector as V ( unsafeIndex, null, length, create, fromList )
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Options that specify how to encode your datatype to JSON.
data Options = Options
    { fieldNameModifier :: String -> String
      -- ^ Function applied to field names.
      -- Handy for removing common record prefixes for example.
    , nullaryToString   :: Bool
      -- ^ If 'True' the constructors of a datatypes, with all nullary
      -- constructors, will be encoded to a string with the
      -- constructor name. If 'False' the encoding will always follow
      -- the `sumEncoding`.
    , sumEncoding       :: SumEncoding
      -- ^ Specifies how to encode constructors of a sum datatype.
    }

-- | Specifies how to encode constructors of a sum datatype.
data SumEncoding =
    TwoElemArray -- ^ A constructor will be encoded to a 2-element
                 -- array where the first element is the name of the
                 -- constructor and the second element the content of
                 -- the constructor.
  | ObjectWithType { typeFieldName  :: String
                   , valueFieldName :: String
                   }
    -- ^ A constructor will be encoded to an object with a field
    -- 'typeFieldName' which specifies the constructor name. If the
    -- constructor is not a record the constructor content will be
    -- stored under the 'valueFieldName' field.

-- | Default encoding options which specify to not modify field names,
-- encode the constructors of a datatype with all nullary constructors
-- to just strings with the name of the constructor and use a
-- 2-element array for other sum datatypes.
defaultOptions :: Options
defaultOptions = Options
                 { fieldNameModifier = id
                 , nullaryToString   = True
                 , sumEncoding       = TwoElemArray
                 }

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
    -- Constructors of a datatype with all nullary constructors are encoded to
    -- just a string with the constructor name:
    matches | nullaryToString opts && all isNullary cons =
      [ match (conP conName []) (normalB $ conStr conName) []
      | con <- cons
      , let conName = getConName con
      ]
      -- Constructors of a datatype having some constructors with arity > 0 are
      -- encoded to a 2-element array where the first element is a string with
      -- the constructor name and the second element is the encoded argument or
      -- arguments of the constructor.
      | otherwise = [ encodeArgs opts True con
                    | con <- cons
                    ]

conStr :: Name -> Q Exp
conStr = appE [|String|] . appE [|T.pack|] . stringE . nameBase

-- | If constructor is nullary.
isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

encodeSum :: Options -> Bool -> Name -> Q Exp -> Q Exp
encodeSum opts multiCons conName exp
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
              [|Array|] `appE` ([|V.fromList|] `appE` listE [conStr conName, exp])
          ObjectWithType{typeFieldName, valueFieldName} ->
              [|object|] `appE` listE
                [ infixApp [|T.pack typeFieldName|]  [|(.=)|] (conStr conName)
                , infixApp [|T.pack valueFieldName|] [|(.=)|] exp
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
    let js = [ infixApp ([|T.pack|] `appE` fieldNameExp opts field)
                        [|(.=)|]
                        (varE arg)
             | (arg, (field, _, _)) <- zip args ts
             ]
        exp = [|object|] `appE` listE js
    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons
            then case sumEncoding opts of
                   TwoElemArray -> [|toJSON|] `appE` tupE [conStr conName, exp]
                   ObjectWithType{typeFieldName} ->
                       [|object|] `appE` listE
                         ( infixApp [|T.pack typeFieldName|] [|(.=)|]
                                    (conStr conName)
                         : js
                         )
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
    if nullaryToString opts && all isNullary cons
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
                                              stringE (nameBase conName)))
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
          ObjectWithType {typeFieldName, valueFieldName} ->
            [ do obj <- newName "obj"
                 match (conP 'Object [varP obj])
                       (normalB $ parseObject typeFieldName valueFieldName obj)
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noObjectFail|]
                             `appE` (litE $ stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]
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

    parseObject typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey)
                  (infixApp (varE obj)
                            [|(.:)|]
                            ([|T.pack|] `appE` stringE typFieldName))
          , noBindS $ parseContents conKey (Left (valFieldName, obj))
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
           (parseContents conKey (Right conVal))

    parseContents conKey contents =
        caseE (varE conKey)
              [ do txt <- newName "txt"
                   match (conP 'String [varP txt])
                         (guardedB $
                          [ liftM2 (,) (normalG $
                                          infixApp (varE txt)
                                                   [|(==)|]
                                                   ([|T.pack|] `appE`
                                                     conNameExp con))
                                       (parseArgs tName opts con contents)
                          | con <- cons
                          ]
                          ++
                          [ liftM2 (,)
                              (normalG [|otherwise|])
                              ( [|conNotFoundFail|]
                                `appE` (litE $ stringL $ show tName)
                                `appE` listE (map ( litE
                                                  . stringL
                                                  . nameBase
                                                  . getConName
                                                  )
                                                  cons
                                             )
                                `appE` ([|T.unpack|] `appE` varE txt)
                              )
                          ]
                         )
                         []
              , do other <- newName "other"
                   match (varP other)
                         ( normalB $
                           (either (const [|typeNotString|])
                                   (const [|firstElemNotString|])
                                   contents)
                             `appE` (litE $ stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
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
               `appE` (litE $ stringL $ nameBase conName)
               `appE` (varE obj)
               `appE` ( [|T.pack|] `appE` fieldNameExp opts field
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

noStringFail :: String -> String -> Parser fail
noStringFail t o = fail $ printf "When parsing %s expected String but got %s." t o

noMatchFail :: String -> String -> Parser fail
noMatchFail t o =
    fail $ printf "When parsing %s expected a String with the name of a constructor but got %s." t o

not2ElemArray :: String -> Int -> Parser fail
not2ElemArray t i = fail $ printf "When parsing %s expected an Array of 2-elements but got %i elements"
                                   t i
typeNotString :: String -> String -> Parser fail
typeNotString t o = fail $ printf "When parsing %s expected an Object where the type field is a String with the name of a constructor but got %s." t o

firstElemNotString :: String -> String -> Parser fail
firstElemNotString t o = fail $ printf "When parsing %s expected an Array where the first element is a String with the name of a constructor but got %s." t o

conNotFoundFail :: String -> [String] -> String -> Parser fail
conNotFoundFail t cs o =
    fail $ printf "When parsing %s expected a 2-element Array with a name and value element where the name is one of [%s], but got %s."
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
conNameExp :: Con -> Q Exp
conNameExp = litE . stringL . nameBase . getConName

-- | Creates a string literal expression from a record field name.
fieldNameExp :: Options -- ^ Encoding options
             -> Name
             -> Q Exp
fieldNameExp opts = litE . stringL . fieldNameModifier opts . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"
