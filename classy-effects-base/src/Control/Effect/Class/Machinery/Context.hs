{-# LANGUAGE AllowAmbiguousTypes #-}

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
module Control.Effect.Class.Machinery.Context where

import Data.Kind (Type)

-- | Retrieve input (/implicit context/) @i@ under the carrier @f@.
class ImplicitContext cTag i (f :: Type -> Type) where
    -- | Retrieve input @i@ under the carrier @f@.
    fetchContext :: (i -> f a) -> f a

-- | Reflect the /functorial context/ @ctx@ into the carrier @f@.
class FunctorialContext cTag ctx (f :: Type -> Type) where
    -- | Reflect the /functorial context/ @ctx@ into the carrier @f@.
    applyContext :: f (ctx a) -> f a

{- |
Reflect the /functorial context/ @ctx@ into the carrier @f@, using the Boehm-Berarducci encoding for
extensibility.
-}
class
    FunctorialContext cTag (ctx r) f =>
    ExtensibleFunctorialContext cTag ctx (f :: Type -> Type) a o r
        | cTag ctx f a -> r
    where
    -- | Inject the output value @o@ into the extensible final type @r@.
    injectContext :: o -> r

    -- | Reflect the /functorial context/ @ctx@ into the carrier @f@.
    applyContextEx :: f (ctx r a) -> f a

-- | A data kind representing the type of context.
data ContextType = ImplicitContext | FunctorialContext

-- | Obtain the context type from the context type tag.
type family ContextTypeOf (cTag :: k) :: ContextType

{- |
A marker for specifying the /context/ part within the effect interface. Used in automatic derivation
via Template Haskell to determine that the specified part is the /context/.
-}
type Context cTag a = a
