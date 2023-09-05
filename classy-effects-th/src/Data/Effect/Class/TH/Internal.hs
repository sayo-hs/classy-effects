{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Class.TH.Internal where

import Control.Monad (forM, replicateM, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate)
import Data.Maybe (isNothing, mapMaybe)

import Language.Haskell.TH.Lib (
    appT,
    conT,
    patSynSigD,
    sigT,
    varT,
 )
import Language.Haskell.TH.Syntax (
    Con,
    Cxt,
    Dec (ClassD, SigD),
    Info (ClassI),
    Kind,
    Name,
    Q,
    Quote (newName),
    TyVarBndr (KindedTV, PlainTV),
    Type (
        AppKindT,
        AppT,
        ArrowT,
        ConT,
        ForallT,
        ImplicitParamT,
        InfixT,
        ParensT,
        PromotedT,
        SigT,
        StarT,
        UInfixT,
        VarT
    ),
    nameBase,
    reify,
 )

import Control.Effect.Class (LiftIns (LiftIns))
import Control.Lens ((%~), (^?), _head, _last)
import Control.Monad.Writer (Any (Any), runWriterT, tell)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Effect.Class.TH.HFunctor.Internal (DataInfo (DataInfo), infoToDataD, tyVarName)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra (dropEnd)
import Language.Haskell.TH (
    Bang (Bang),
    Con (GadtC),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    arrowT,
    conP,
    implBidir,
    mkName,
    patSynD,
    pragCompleteD,
    prefixPatSyn,
    tySynD,
    varP,
 )

-- | Generate /instruction/ and /signature/ data types from an effect class, from 'EffectInfo'.
generateEffectDataByEffInfo ::
    -- | An effect order of an effect data type to generate.
    EffectOrder ->
    -- | A name of an effect data type to generate.
    Name ->
    EffectInfo ->
    Q (DataInfo (), Dec)
generateEffectDataByEffInfo order effDataName info = do
    effDataInfo <- do
        let pvs = effParamVars info

        additionalTypeParams <- do
            a <- do
                a <- newName "a"
                pure $ KindedTV a () StarT

            pure case order of
                FirstOrder -> [a]
                HigherOrder -> [unkindTyVar $ effMonad info, a]

        cons <- do
            (errorMethods, cons) <- do
                consWithMethodInfo <- do
                    effData <- do
                        let paramTypes = fmap (tyVarType . unkindTyVar) pvs
                        foldl appT (conT effDataName) paramTypes

                    forM (effMethods info) \method ->
                        (methodName method,)
                            <$> interfaceToCon info effData method

                pure . partitionEithers $
                    consWithMethodInfo <&> \(methodName, (methodOrder, con)) ->
                        if methodOrder == order
                            then Right con
                            else Left (methodOrder, nameBase methodName)

            unless (null errorMethods) $
                fail $
                    "Unexpected order of effect methods: "
                        <> intercalate
                            ", "
                            ( errorMethods <&> \(methodOrder, name) ->
                                name <> " [" <> [fst $ effectOrderSymbol methodOrder] <> "]"
                            )

            pure cons

        pure $ DataInfo [] effDataName (pvs ++ additionalTypeParams) cons []

    pure (effDataInfo, infoToDataD effDataInfo)

-- | Convert an effect method interface to a constructor of the effect data type.
interfaceToCon ::
    EffectInfo ->
    Type ->
    MethodInterface ->
    Q (EffectOrder, Con)
interfaceToCon info effData MethodInterface{..} = do
    (methodOrder,) <$> do
        effData' <- case methodOrder of
            FirstOrder -> pure effData
            HigherOrder -> pure effData `appT` (unkindType <$> tyVarType (effMonad info))
        pure $
            GadtC
                [renameMethodToCon methodName]
                (methodParamTypes & map (Bang NoSourceUnpackedness NoSourceStrictness,))
                (AppT effData' methodReturnType)

{- |
Decompose an effect method interface type to get the effect order, the list of argument types, and
the return type.
-}
analyzeMethodInterface :: TyVarBndr () -> Type -> Q (EffectOrder, [Type], Type)
analyzeMethodInterface m interface = do
    ((resultType, paramTypes), Any isHigherOrderMethod) <- runWriterT $ go interface
    pure (bool FirstOrder HigherOrder isHigherOrderMethod, paramTypes, resultType)
  where
    go = \case
        ArrowT `AppT` l `AppT` r -> do
            when (tyVarName m `occurs` l) $ tell $ Any True
            fmap (l :) <$> go r
        ForallT _ _ u -> go u
        VarT n `AppT` a | n == tyVarName m -> pure (a, [])
        other -> fail $ "Expected a pure type of the form 'm a', but encountered: " ++ show other

-- | Convert a lower-camel-cased method name to an upper-camel-cased constructor name.
renameMethodToCon :: Name -> Name
renameMethodToCon = mkName . (_head %~ toUpper) . nameBase

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

-- | Is the order of effect higher-order?
isHigherOrder :: EffectOrder -> Bool
isHigherOrder = \case
    FirstOrder -> False
    HigherOrder -> True

{- |
The default naming convention of effect data types.

Add an @I@ or @S@ symbol indicating the order of the effect to the end of the effect class name.

If the name of the effect class ends in @F@ or @H@, depending on its order, replace @F@ or @H@ with
@I@ or @S@.
-}
defaultEffectDataNamer :: EffectOrder -> String -> String
defaultEffectDataNamer order clsName =
    effNameBase ++ [dataOrderSym]
  where
    (clsOrderSym, dataOrderSym) = effectOrderSymbol order
    effNameBase =
        if clsName ^? _last == Just clsOrderSym
            then dropEnd 1 clsName
            else clsName

-- | Symbol letters representing the order of the effect.
effectOrderSymbol :: EffectOrder -> (Char, Char)
effectOrderSymbol = \case
    FirstOrder -> ('F', 'I')
    HigherOrder -> ('H', 'S')

-- ** Generating Synonyms about LiftIns

{- |
Generate the pattern synonyms for instruction constructors:

    @pattern BazS ... = LiftIns (Baz ...)@
-}
generateLiftInsPatternSynonyms :: Name -> EffectInfo -> Q [Dec]
generateLiftInsPatternSynonyms dataName info = do
    patSyns <-
        forM (effMethods info) \MethodInterface{..} -> do
            let conName = renameMethodToCon methodName
                newConName = mkName $ nameBase conName ++ "S"
            args <- replicateM (length methodParamTypes) (newName "x")
            a <- varT . mkName . show <$> newName "a"
            (newConName,)
                <$> sequence
                    [ patSynSigD
                        newConName
                        -- For some reason, if I don't write constraints in this form, the type is
                        -- not inferred properly (why?).
                        [t|
                            () =>
                            ($a ~ $(pure methodReturnType)) =>
                            $( foldr
                                (\l r -> arrowT `appT` pure l `appT` r)
                                [t|
                                    $(liftInsType dataName $ tyVarName <$> effParamVars info)
                                        $(varT $ tyVarName $ effMonad info)
                                        $a
                                    |]
                                methodParamTypes
                             )
                            |]
                    , patSynD
                        newConName
                        (prefixPatSyn args)
                        implBidir
                        (conP 'LiftIns [conP conName $ varP <$> args])
                    ]

    (concatMap snd patSyns ++)
        <$> sequence [pragCompleteD (fst <$> patSyns) Nothing]

{- |
Generate the type synonym for an instruction datatype:

    @type (FoobarS ...) = LiftIns (FoobarI ...)@
-}
generateLiftInsTypeSynonym :: EffectInfo -> Name -> Q Dec
generateLiftInsTypeSynonym info dataName = do
    nameS <- mkName <$> renameI2S (nameBase dataName)
    tySynD
        nameS
        (pvs <&> (`PlainTV` ()))
        (liftInsType dataName pvs)
  where
    pvs = tyVarName <$> effParamVars info

renameI2S :: String -> Q String
renameI2S name = dropEndI name <&> (++ "S")

dropEndI :: String -> Q String
dropEndI name =
    if name ^? _last == Just 'I'
        then pure $ dropEnd 1 name
        else fail $ "The name doesn't end in 'I': \"" <> name <> "\"."

liftInsType :: Name -> [Name] -> Q Type
liftInsType dataName pvs =
    conT ''LiftIns `appT` foldl appT (conT dataName) (varT <$> pvs)

applyEffPVs :: Name -> [Name] -> Q Type
applyEffPVs effClsName = foldl appT (conT effClsName) . fmap varT

-- ** Reification of Effect Class

-- | Information about effect type classes.
data EffectInfo = EffectInfo
    { effCxts :: [Type]
    , effName :: Name
    , effParamVars :: [TyVarBndr ()]
    , effMonad :: TyVarBndr ()
    , effMethods :: [MethodInterface]
    }

effParamVar :: (Name, Maybe Kind) -> TyVarBndr ()
effParamVar (n, k) = case k of
    Just k' -> KindedTV n () k'
    Nothing -> PlainTV n ()

data MethodInterface = MethodInterface
    { methodName :: Name
    , methodOrder :: EffectOrder
    , methodParamTypes :: [Type]
    , methodReturnType :: Type
    }

-- | Given a type class name, extracts infos about an effect.
reifyEffectInfo :: Name -> Q EffectInfo
reifyEffectInfo className = do
    info <- reify className
    case info of
        ClassI (ClassD cxts name tyVars _funDeps decs) _ -> do
            (paramVars, monad) <-
                case tyVars of
                    [] ->
                        fail $
                            "The specified effect type class `"
                                ++ nameBase name
                                ++ "' has no monad type variable. "
                                ++ "It is expected to be the last type variable."
                    vs -> pure (init vs, last vs)

            EffectInfo cxts name paramVars monad
                <$> sequence
                    [ do
                        (order, paramTypes, retType) <- analyzeMethodInterface monad t
                        pure $ MethodInterface n order paramTypes retType
                    | SigD n t <- decs
                    ]
        other ->
            fail $
                "The specified name `"
                    ++ nameBase className
                    ++ "' is not a type class, but the following instead: "
                    ++ show other

-- | Constructs the type of an effect, i.e. the type class without its monad parameter.
effectType :: EffectInfo -> Q Type
effectType info =
    foldl
        appT
        (conT $ effName info)
        (fmap tyVarType (effParamVars info))

partitionSuperEffects :: EffectInfo -> (Cxt, [Type])
partitionSuperEffects info =
    ( filter (isNothing . extract) cxts
    , mapMaybe extract (effCxts info)
    )
  where
    cxts = effCxts info
    m = tyVarName (effMonad info)
    extract = \case
        ForallT _ _ t -> extract t
        SigT t _ -> extract t
        ParensT t -> extract t
        t `AppT` VarT n | n == m -> Just t
        InfixT t _ (VarT n) | n == m -> Just t
        UInfixT t _ (VarT n) | n == m -> Just t
        AppKindT t _ -> extract t
        ImplicitParamT _ t -> extract t
        _ -> Nothing

{- |
Extracts the super classes of an effect which have the kind of effects. As an example, for the
following effect ...

@class (State s m, Monad m) => MyEffect s m where ...@

... this would pure [State s, Monad].
-}
superEffects :: EffectInfo -> [Type]
superEffects = snd . partitionSuperEffects

{- |
Like superEffects, but ignores super classes from base (i.e., Applicative, Functor, Monad, MonadIO).
-}
superEffectsWithoutBase :: EffectInfo -> [Type]
superEffectsWithoutBase =
    filter (not . isBase) . superEffects
  where
    isBase = \case
        ConT n -> n `elem` [''Applicative, ''Functor, ''Monad, ''MonadIO]
        _ -> False

effectParamCxt :: EffectInfo -> Cxt
effectParamCxt = fst . partitionSuperEffects

-- ** Utility functions

-- | Construct a namer from a conversion function of string.
pureNamer :: (String -> String) -> Name -> Q Name
pureNamer f = pure . mkName . f . nameBase

-- | Throws away all kind information from a type.
unkindType :: Type -> Type
unkindType = \case
    ForallT vs ps t -> ForallT (fmap unkindTyVar vs) (fmap unkindType ps) (unkindType t)
    AppT l r -> AppT (unkindType l) (unkindType r)
    SigT t _ -> t
    InfixT l n r -> InfixT (unkindType l) n (unkindType r)
    UInfixT l n r -> UInfixT (unkindType l) n (unkindType r)
    ParensT t -> ParensT (unkindType t)
    AppKindT t _ -> unkindType t
    ImplicitParamT s t -> ImplicitParamT s (unkindType t)
    other -> other

-- | Throws away the kind information of a type variable.
unkindTyVar :: TyVarBndr a -> TyVarBndr a
unkindTyVar (KindedTV n s _) = PlainTV n s
unkindTyVar unkinded = unkinded

-- | Converts a type variable to a type.
tyVarType :: TyVarBndr a -> Q Type
tyVarType (PlainTV n _) = varT n
tyVarType (KindedTV n _ k) = sigT (varT n) k

tyVarKind :: TyVarBndr a -> Q Type
tyVarKind (KindedTV _ _ k) = pure k
tyVarKind (PlainTV _ _) = fail "The type variable has no kind."

-- | Counts the parameters of a type.
paramCount :: Type -> Int
paramCount = \case
    ArrowT `AppT` _ `AppT` r -> 1 + paramCount r
    ForallT _ _ t -> paramCount t
    _ -> 0

-- | Checks if a name m appears somewhere in a type.
occurs :: Name -> Type -> Bool
occurs m = \case
    ForallT _ _ t -> m `occurs` t
    AppT l r -> m `occurs` l || m `occurs` r
    SigT t _ -> m `occurs` t
    VarT n -> n == m
    ConT n -> n == m
    PromotedT n -> n == m
    InfixT l n r -> n == m || m `occurs` l || m `occurs` r
    UInfixT l n r -> n == m || m `occurs` l || m `occurs` r
    ParensT t -> m `occurs` t
    AppKindT t _ -> m `occurs` t
    ImplicitParamT _ t -> m `occurs` t
    _ -> False