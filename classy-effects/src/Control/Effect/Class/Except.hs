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
-}
module Control.Effect.Class.Except where

import Control.Effect.Class.Machinery.Context (
    Context,
    ContextType (FunctorialContext),
    ContextTypeOf,
    ExtensibleFunctorialContext (applyContextEx, injectContext),
    FunctorialContext (applyContext),
 )

class Throw e (f :: Type -> Type) where
    throw :: e -> f a

class Catch e f where
    catch :: f a -> (e -> f a) -> f a

makeEffect "Except" ''Throw ''Catch

data ExceptCtx

type instance ContextTypeOf ExceptCtx = 'FunctorialContext

instance (Throw e m, Monad m) => FunctorialContext ExceptCtx (Either e) m where
    applyContext = (>>= either throw pure)
    {-# INLINE applyContext #-}

instance (Throw e m, Monad m) => ExtensibleFunctorialContext ExceptCtx Either m e a (m a) where
    injectContext = throw
    {-# INLINE injectContext #-}

    applyContextEx = (>>= either id pure)
    {-# INLINE applyContextEx #-}

type EXC a = Context ExceptCtx a
