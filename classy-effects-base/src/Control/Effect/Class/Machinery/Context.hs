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

Enable localized effect handling.

For example, for an interface like @a -> f (Either e b)@, expand @Either e@ as a @Throw@ effect to
@f@, automatically generating @Throw e f => a -> f b@ from @a -> f (Either e b)@.
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

In general, for an effect class @E@, a functorial context @ctx@, and an @applyContext@ that
translates @ctx@ into any carrier under the @E@ constraint, when every @E@-based effect
@f :: A -> f B@ issued in @applyContext@ has an interface @A -> f B@ that is /divisible/ with
respect to the effect class parameter @o@, such a functorial context is considered extensible.

Here, by /divisible/, we mean that when @o@ is composed as a direct sum @o=o_1+o_2+...+o_n@, a
variant of @f@, denoted @f_i@, where any occurrence of @o@ in the interface of @f@ has been replaced
by any @o_i@, can be derived from @f@ (a function exists that maps @f@ to @f_i@). This is
reminiscent of projection operations, and divisibility likely equates to the interface being of the
form @o -> ...@.
In this case, by combining the @...@ parts for all effects @f@ issued in @applyContext@ as a direct
product, it is expected to constitute the extensible final type @r@.
For instance, the forms of effects like @throw@ or @put@ are straightforward to grasp. However, a
slightly more complex example would be the @These@ monad (@Chronicle@ effect class). Since
@applyContext@ can issue two effects, @dictate@ and @confess@, @r@ would be a pair of their @...@
parts, resulting in @(m a, m ())@, where @m@ is the carrier. For more details, refer to
@Control.Effect.Class.Chronicle@ in the @classy-effects@ package.
-}
class
    FunctorialContext cTag (ctx o) f =>
    ExtensibleFunctorialContext cTag ctx (f :: Type -> Type) o a r
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
