# Status (2024) : **This `classy-effects` project was abandoned due to the discovery of fundamental difficulties.**
The reasons are as follows: https://github.com/orgs/sayo-hs/projects/4?pane=issue&itemId=48547880

**As an alternative, consider using `sayo-hs/data-effects`, a GADTs-based common effect representation foundation:** https://github.com/sayo-hs/data-effects

# classy-effects

[![Hackage](https://img.shields.io/hackage/v/classy-effects.svg?logo=haskell&label=classy-effects)](https://hackage.haskell.org/package/classy-effects)
[![Hackage](https://img.shields.io/hackage/v/classy-effects-th.svg?logo=haskell&label=classy-effects-th)](https://hackage.haskell.org/package/classy-effects-th)
[![Hackage](https://img.shields.io/hackage/v/classy-effects-base.svg?logo=haskell&label=classy-effects-base)](https://hackage.haskell.org/package/classy-effects-base)

This repository is for **CEPs** and its associated Haskell library.

CEPs (Classy-Effects Protocols) are a collection of protocols for expressing and defining effects in Haskell, aimed at the fist objective in the [Sayo Project](https://github.com/sayo-hs):

> Exploring ways to improve interoperability among the myriad Haskell effect system libraries

For more details, please refer to [CEPs/README.md](CEPs/README.md).

## Background
In Haskell, there are numerous libraries to realize an effect system. The issue here is interoperability between these libraries. For instance, the [Reader effect in `fused-effects`](https://hackage.haskell.org/package/fused-effects-1.1.2.2/docs/Control-Effect-Reader.html#t:Reader) and the [Reader effect in `polysemy`](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Reader.html) have completely identical definitions. If we could extract and define effects purely as interfaces, eliminating this redundancy in effect definitions, then an effectful program could be handled by both effect system library A and effect system library B. The specific mechanisms for handling effects vary from one library to another, and unifying them isn't the goal. We're specifically looking to unify the overlapping and common definitions of effects as interfaces.

Based on this idea, CEPs (classy-effects protocols) were developed. CEPs propose a convention (protocol) for unified effect definitions in Haskell. It's expected that effects defined in accordance with CEPs will be usable across various effect systems and backends.

## Encoding of Effect
First, for the representation of effects, there is the GADTs encoding typical in Extensible Effects and the type-class encoding typical in the monad transformer approach. In principle, if the type class representing the effect is decided, GADTs can be automatically generated, and the reverse is probably also possible. There's a kind of symmetry here. However, type classes have the advantage of expressing inclusion relationships of effects naturally by specifying superclasses. From this perspective, CEPs primarily adopt type class representation for effects. Moreover, a mechanism has been implemented to derive GADTs from type classes[^1], and from GADTs and the type classes that cast them to monads ([`SendIns`](https://hackage.haskell.org/package/classy-effects-base-0.1.0.0/docs/Control-Effect-Class.html#t:SendIns), [`SendSig`](https://hackage.haskell.org/package/classy-effects-base-0.1.0.0/docs/Control-Effect-Class.html#t:SendSig)), to automatically derive instances of the effect's type class ([`classy-effects-th`](https://hackage.haskell.org/package/classy-effects-th)). This allows compliance with CEPs automatically by just writing the type class that represents the effect.

[^1]: In the future, we might also consider implementing the derivation of type classes from GADTs.

## Examples
Examples of using classy-effects combined with the Heftia effect handler backend can be found [here](https://github.com/sayo-hs/heftia/blob/master/docs/examples/01%20First-order.md).

## Your contributions are welcome!
Please see [CONTRIBUTING.md](CONTRIBUTING.md).

## Credits
Parts of this project have been adapted or inspired by the following resources:

* **[Hefty Algebras -- The Artifact](https://github.com/heft-lang/POPL2023)**
    * **Copyright** (c) 2023 Casper Bach Poulsen and Cas van der Rest
    * **License**: MIT
    * **Modifications**: The inspiration for the idea of Heftia. Code was used in the definition of [`LiftIns`](https://github.com/sayo-hs/classy-effects/blob/5b6ccb1f2bcfef804692bc13996e060bd0739475/classy-effects-base/src/Control/Effect/Class.hs#L49).

* **[effet](https://github.com/typedbyte/effet)**
    * **Copyright** (c) 2020 Michael Szvetits
    * **License**: BSD-3-Clause
    * **Modifications**: Used TemplateHaskell code to handle the effect type class.

* **[compdata](https://github.com/pa-ba/compdata)**
    * **Copyright** (c) 2010--2011 Patrick Bahr, Tom Hvitved
    * **License**: BSD-3-Clause
    * **Modifications**: Used TemplateHaskell code to derive instances of the `HFunctor` type class.
