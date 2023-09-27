{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Machinery.TH.Internal where

import Control.Effect.Class (LiftIns)
import Control.Effect.Class.Machinery.HFunctor ((:+:))
import Control.Effect.Class.Machinery.TH.Send (deriveEffectSend)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Effect.Class.TH.HFunctor.Internal (deriveHFunctor, tyVarName)
import Data.Effect.Class.TH.Internal (
    EffectInfo (EffectInfo, effName, effParamVars),
    EffectOrder (FirstOrder, HigherOrder),
    applyEffPVs,
    defaultEffectDataNamer,
    generateEffectDataByEffInfo,
    generateLiftInsPatternSynonyms,
    generateLiftInsTypeSynonym,
 )
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind qualified as K
import Language.Haskell.TH (
    Dec,
    Name,
    Q,
    TyVarBndr (PlainTV),
    appT,
    classD,
    kindedTV,
    mkName,
    nameBase,
    tySynD,
    varT,
 )

generateEffect :: EffectOrder -> EffectInfo -> Q [Dec]
generateEffect order info =
    generateEffectWith
        order
        (mkName $ defaultEffectDataNamer order $ nameBase $ effName info)
        info

generateEffectWith :: EffectOrder -> Name -> EffectInfo -> Q [Dec]
generateEffectWith order effDataName info =
    execWriterT do
        (effDataInfo, effData) <- generateEffectDataByEffInfo order effDataName info & lift
        tell [effData]

        case order of
            FirstOrder -> do
                generateLiftInsPatternSynonyms effDataName info & lift >>= tell
                [generateLiftInsTypeSynonym info effDataName] & lift . sequence >>= tell
            HigherOrder ->
                deriveHFunctor effDataInfo & lift >>= tell

        [deriveEffectSend info $ Just (order, effDataName)] & lift . sequence >>= tell

        pure info

{- |
Generate the order-unified empty effect class:

    @class (FoobarF ... f, FoobarH ... f) => Foobar ... f@

, and derive an instance of the effect that handles via 'Control.Effect.Class.SendIns'/
'Control.Effect.Class.SendSig' instances.
-}
generateOrderUnifiedEffectClass :: EffectInfo -> EffectInfo -> [Name] -> Name -> Q [Dec]
generateOrderUnifiedEffectClass infoF infoH pvs unifiedClsName = do
    fKind <- [t|K.Type -> K.Type|]
    let f = mkName "f"
        fKinded = f `kindedTV` fKind

    cxt <-
        sequence
            [ applyEffPVs (effName infoF) pvs `appT` varT f
            , applyEffPVs (effName infoH) pvs `appT` varT f
            ]
    let pvs' = pvs <&> (`PlainTV` ())

    sequence
        [ classD
            (pure cxt)
            unifiedClsName
            (pvs' ++ [fKinded])
            []
            []
        , deriveEffectSend
            ( EffectInfo
                cxt
                unifiedClsName
                pvs'
                fKinded
                []
            )
            Nothing
        ]

{- |
Generate the order-unified effect data type synonym:

    @type Foobar ... = FoobarS ... :+: LiftIns (FoobarI ...)@
-}
generateOrderUnifiedEffDataTySyn :: Name -> Name -> [Name] -> Name -> Q Dec
generateOrderUnifiedEffDataTySyn dataI dataS pvs tySynName = do
    tySynD
        tySynName
        ((`PlainTV` ()) <$> pvs)
        [t|$(applyEffPVs dataS pvs) :+: LiftIns $(applyEffPVs dataI pvs)|]

unifyEffTypeParams :: EffectInfo -> EffectInfo -> Q [Name]
unifyEffTypeParams infoF infoH = do
    let pvF = nameBase . tyVarName <$> effParamVars infoF
        pvH = nameBase . tyVarName <$> effParamVars infoH

    when (pvF /= pvH) $
        fail $
            "The type parameter lists for the first and higher-order effect classes do not match:\n"
                <> (nameBase (effName infoF) <> ": " <> show pvF <> "\n")
                <> (nameBase (effName infoH) <> ": " <> show pvH <> "\n")

    pure $ mkName <$> pvH
