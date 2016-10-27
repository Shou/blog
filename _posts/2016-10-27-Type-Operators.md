---
layout: post
title: You're up and running!
---

This package contains type-level combinators that hopefully feel familiar based
on value-level operators. It exists on Hackage and Stackage under
`type-operators`.

```haskell
import Control.Type.Operator
```

Type-level operators are just what you'd expect: operators at the type-level.
This means you can use `data`, `newtype`, `type`, `type family`, and so on, to
define type constructors in the shape of operators. A 'common' example is the
function application operator `$`, which can be defined in various ways, and as
expected allows you to rid excessive parentheses and sequentialize your type
signatures.

```
type (f $ a) = f a
type ($) f = f
type family ($) f a where
    ($) f a = f a

a :: Either String $ Maybe Int
```

## History

Type-level operators have a long history in GHC. They originally were prefixed
with `:` and older literature on the subject will have that present. [The
proposal dates back eleven years](https://prime.haskell.org/wiki/InfixTypeConstructors),
and talks about how operators were treated like variables in previous versions
of GHC.
[Servant](http://www.arow.info/blog/posts/2015-07-10-servant-intro.html) for
example uses the old type operator format in datatype definitions.
[With GHC 7.6 that all changed](https://ghc.haskell.org/trac/ghc/ticket/1930):

> From the 7.6.1 release notes: "The behavior of the TypeOperator extension has changed: previously, only type operators starting with ":" were considered type constructors, and other operators were treated as type variables. Now type operators are always constructors. "

So now we can't use type operators as variables anymore, just as constructors.

Type-level operators, in their current form, were talked about [briefly during
the 2014 Christmas calendar "24 Days of GHC Extensions"](https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html),
but otherwise they don't seem to have had much presence.

In GHC 7.10 and earlier, type-level operators are wonky in contexts without
parentheses which is unfortunate because type-level combinators are often
meant to strip parentheses.

-- Find trac post

```haskell
 >> Proxy :: [Show, Num] <+> [a, b] => Proxy (a -> b)

 <interactive>:3:33: parse error on input `=>''`

 >> Proxy :: ([Show, Num] <+> [a, b]) => Proxy (a -> b)
```

Luckily with GHC 8.0 this is fixed and parentheses are no longer necessary.

## Why use them?

You'd want to use type-level combinators for the same reasons you'd want them
at the value-level: they're syntactic conveniences. They often strip parentheses
away, or shave charcters off.

## Inspiration

## Other notes

It could potentially also be possible to make `=>` into a type-level operator
that takes a `Constraint` kind on the LHS. Just making a type-synonym for `=>`
doesn't work the way you might expect, rather this constrains it *outside* the
context and hence introduces RankNTypes.

```haskell
type (==>) (c :: Constraint) (a :: *) = c => a
```

