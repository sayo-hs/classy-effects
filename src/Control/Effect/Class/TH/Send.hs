{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of the effect that handles
via 'Send'/'SendF' type classes.
-}
module Control.Effect.Class.TH.Send where

import Control.Effect.Class (Send, SendF, SendVia (SendVia), runSendVia, send, sendF)
import Control.Effect.Class.HFunctor (hfmap)
import Control.Effect.Class.TH.HFunctor (tyVarName)
import Control.Monad (forM, replicateM)
import Data.Effect.Class.TH (
    EffectInfo,
    EffectOrder (FirstOrder, HigherOrder),
    MethodInterface (MethodInterface, methodName),
    effMethods,
    effMonad,
    effName,
    effParamVars,
    effectParamCxt,
    methodOrder,
    methodParamTypes,
    reifyEffectInfo,
    renameMethodToCon,
    superEffects,
    tyVarType,
    unkindTyVar,
 )
import Data.Maybe (maybeToList)
import Language.Haskell.TH (
    Dec (InstanceD),
    Inline (Inline),
    Name,
    Phases (AllPhases),
    Q,
    RuleMatch (FunLike),
    Type (ConT),
    appE,
    appT,
    clause,
    conE,
    conT,
    funD,
    newName,
    normalB,
    pragInlD,
    varE,
    varP,
    varT,
 )

-- | Derive an instance of the effect that handles via 'Send'/'SendF' type classes.
makeEffectSend ::
    -- | The class name of the effect.
    Name ->
    -- | The name of /instruction/ datatype corresponding to the effect.
    Maybe Name ->
    -- | The name of /signature/ datatype corresponding to the effect.
    Maybe Name ->
    Q Dec
makeEffectSend effClsName effDataNameF effDataNameH = do
    info <- reifyEffectInfo effClsName
    deriveEffectSend info effDataNameF effDataNameH

{- |
Derive an instance of the effect that handles via 'Send'/'SendF' type classes, from 'EffectInfo'.
-}
deriveEffectSend ::
    -- | The reified information of the effect class.
    EffectInfo ->
    -- | The name of /instruction/ datatype corresponding to the effect.
    Maybe Name ->
    -- | The name of /signature/ datatype corresponding to the effect.
    Maybe Name ->
    Q Dec
deriveEffectSend info effDataNameF effDataNameH = do
    let f = varT $ tyVarName $ effMonad info

    let pvs = effParamVars info
        paramTypes = fmap (tyVarType . unkindTyVar) pvs

        carrier = [t|SendVia $f|]

    sendCxt <- do
        let effSendClsAndDataNames =
                flip
                    foldMap
                    [(effDataNameF, ''SendF), (effDataNameH, ''Send)]
                    \(effDataName, sendCls) ->
                        maybeToList $ (conT sendCls,) <$> effDataName

        forM effSendClsAndDataNames \(sendCls, effDataName) -> do
            let effData = foldl appT (conT effDataName) paramTypes
            [t|$sendCls $effData $f|]

    let effParamCxt = effectParamCxt info
    superEffCxt <- forM (superEffects info) ((`appT` carrier) . pure)

    inst <- do
        let eff = pure $ ConT $ effName info
        [t|$(foldl appT eff paramTypes) $carrier|]

    decs <- do
        let methods =
                [ (sig, renameMethodToCon methodName)
                | sig@MethodInterface{methodName} <- effMethods info
                ]
        mapM (uncurry effectMethodDec) methods

    return $ InstanceD Nothing (sendCxt ++ superEffCxt ++ effParamCxt) inst (concat decs)

-- | Generate a method implementation of the effect that handles via 'Send'/'SendF' type classes.
effectMethodDec ::
    -- | The interface of the effect method.
    MethodInterface ->
    -- | The name of effect data constructor corresponding to the method.
    Name ->
    Q [Dec]
effectMethodDec MethodInterface{..} conName = do
    methodParams <- replicateM (length methodParamTypes) (newName "x")

    let ins = foldl appE (conE conName) (varE <$> methodParams)
        sendMethod = case methodOrder of
            FirstOrder -> [|sendF|]
            HigherOrder -> [|send . hfmap runSendVia|]
        body = [|SendVia $ $sendMethod $ins|]

    funDef <- funD methodName [clause (fmap varP methodParams) (normalB body) []]
    funInline <- pragInlD methodName Inline FunLike AllPhases

    return [funDef, funInline]
