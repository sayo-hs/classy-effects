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
module Control.Effect.Class.State where

import Control.Effect.Class.Machinery.Context (
    Context,
    ContextType (FunctorialContext, ImplicitContext),
    ContextTypeOf,
    ExtensibleFunctorialContext (applyContextEx, injectContext),
    FunctorialContext (applyContext),
    ImplicitContext (fetchContext),
 )

class State s f where
    get :: f s
    put :: s -> f ()

makeEffectF ''State

gets :: (State s f, Functor f) => (s -> a) -> f a
gets f = f <$> get

modify :: (State s m, Monad m) => (s -> s) -> m ()
modify f = put . f =<< get

data PutCtx
type instance ContextTypeOf PutCtx = 'FunctorialContext
instance (State s m, Monad m) => FunctorialContext PutCtx ((,) s) m where
    applyContext m = do
        (s, a) <- m
        put s
        pure a
    {-# INLINE applyContext #-}
instance (State s m, Monad m) => ExtensibleFunctorialContext PutCtx (,) m s a (m ()) where
    injectContext = put
    {-# INLINE injectContext #-}

    applyContextEx m = do
        (put_, a) <- m
        put_
        pure a
    {-# INLINE applyContextEx #-}
type PUT a = Context PutCtx a

data GetCtx
type instance ContextTypeOf GetCtx = 'ImplicitContext
instance (State s m, Monad m) => ImplicitContext GetCtx s m where
    fetchContext = (=<< get)
    {-# INLINE fetchContext #-}
type GET a = Context GetCtx a
