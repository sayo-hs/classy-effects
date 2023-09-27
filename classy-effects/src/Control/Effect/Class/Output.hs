{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the t`Output` effect, comes
from [@Polysemy.Output@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Output.html)
in the @polysemy@ package.
-}
module Control.Effect.Class.Output where

import Control.Effect.Class.Machinery.Context (
    Context,
    ContextType (FunctorialContext),
    ContextTypeOf,
    ExtensibleFunctorialContext (applyContextEx, injectContext),
    FunctorialContext (applyContext),
 )

class Output o f where
    output :: o -> f ()

makeEffectF ''Output

data OutputCtx

type instance ContextTypeOf OutputCtx = 'FunctorialContext

instance (Output o m, Monad m) => FunctorialContext OutputCtx ((,) o) m where
    applyContext m = do
        (o, a) <- m
        output o
        pure a
    {-# INLINE applyContext #-}

instance (Output o m, Monad m) => ExtensibleFunctorialContext OutputCtx (,) m o a (m ()) where
    injectContext = output
    {-# INLINE injectContext #-}

    applyContextEx m = do
        (output_, a) <- m
        output_
        pure a
    {-# INLINE applyContextEx #-}

type OUT a = Context OutputCtx a
