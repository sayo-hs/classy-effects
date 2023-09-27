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
module Control.Effect.Class.Writer where

import Control.Effect.Class.Machinery.Context (
    Context,
    ContextType (FunctorialContext),
    ContextTypeOf,
    ExtensibleFunctorialContext (applyContextEx, injectContext),
    FunctorialContext (applyContext),
 )

class Monoid w => Tell w f where
    tell :: w -> f ()

class Monoid w => WriterH w f where
    listen :: f a -> f (a, w)
    censor :: (w -> w) -> f a -> f a

makeEffect "Writer" ''Tell ''WriterH

data WriteCtx

type instance ContextTypeOf WriteCtx = 'FunctorialContext

instance (Writer w m, Monad m) => FunctorialContext WriteCtx ((,) w) m where
    applyContext m = do
        (w, a) <- m
        tell w
        pure a
    {-# INLINE applyContext #-}

instance (Writer w m, Monad m) => ExtensibleFunctorialContext WriteCtx (,) m w a (m ()) where
    injectContext = tell
    {-# INLINE injectContext #-}

    applyContextEx m = do
        (tell_, a) <- m
        tell_
        pure a
    {-# INLINE applyContextEx #-}

type WRITE a = Context WriteCtx a
