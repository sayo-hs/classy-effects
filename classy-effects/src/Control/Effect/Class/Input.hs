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

This module provides the t`Input` effect, comes
from [@Polysemy.Input@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Input.html)
in the @polysemy@ package.
-}
module Control.Effect.Class.Input where

import Control.Effect.Class.Machinery.Context (
    Context,
    ContextType (ImplicitContext),
    ContextTypeOf,
    ImplicitContext (fetchContext),
 )

class Input i (f :: Type -> Type) where
    input :: f i

makeEffectF ''Input

data InputCtx

type instance ContextTypeOf InputCtx = 'ImplicitContext

instance (Input i m, Monad m) => ImplicitContext InputCtx i m where
    fetchContext = (=<< input)
    {-# INLINE fetchContext #-}

type IN a = Context InputCtx a
