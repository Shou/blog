---
layout: post
title: Fun with type-level combinators
---

How did you find your way here??? This document is work-in-progress.

## A brief historical overview

Type-level operators have a long, but subdued history in GHC. They were originally prefixed with `:` and older literature on the subject will have that present. [Servant](http://www.arow.info/blog/posts/2015-07-10-servant-intro.html) for example uses the old type operator format in datatype definitions. [The original proposal dates back eleven years](https://prime.haskell.org/wiki/InfixTypeConstructors), and also mentions how operators were treated like variables in previous versions of GHC. [With GHC 7.6 that all changed](https://ghc.haskell.org/trac/ghc/ticket/1930):

> From the 7.6.1 release notes: "The behavior of the TypeOperator extension has changed: previously, only type operators starting with ":" were considered type constructors, and other operators were treated as type variables. Now type operators are always constructors. "

So nowadays we can't use type operators as variables anymore, and [some people were unhappy about that change](http://haskell.1045720.n5.nabble.com/Type-operators-in-GHC-td5154978.html) because it meant, e.g. that this was no longer possible:

```haskell
 >> :type a :: (Arrow (~>)) => a ~> b

<interactive>:1:13: error:
    Not in scope: type constructor or class `~>'
```

But on the other hand behaviour is more consistent and familiar as they took on behaviour from the value-level.

## So how do I use them?





In more recent changes: with GHC 7.10 and earlier, type-level operators are wonky in contexts without parentheses which is unfortunate because type-level combinators are often meant to strip parentheses.

```haskell
 >> a :: [Show, Num] <+> [a, b] => a -> b

 <interactive>:3:33: parse error on input `=>''`

 >> a :: ([Show, Num] <+> [a, b]) => a -> b
```

With GHC 8.0 this behaviour was rectified and parentheses are no longer necessary, allowing the first example.

-- Introduction to -XConstraintKinds and its effects



Type-level operators, in their current form, were talked about [briefly during the 2014 Christmas calendar "24 Days of GHC Extensions"](https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html), but otherwise they don't seem to have had much presence. I think that's unfortunate because I believe people *want* type-level combinators based on how much they're used at value-level, may just not be aware of it.

## Type-level combinators

Type-level operators are just what they say on the tin, and this means you can use `data`, `newtype`, `type`, `type family`, and so on, to define type constructors in the shape of operators. A 'common' example is the function application operator `$`, which can be defined in various ways, and as expected allows you to rid excessive parentheses and sequentialize your type signatures.

```haskell
type (f $ a) = f a
type ($) f = f
type family ($) f a where
    ($) f a = f a

a :: Either String $ Maybe Int
```

## Why use them?

You'd want to use type-level combinators for the same reasons you'd want them at the value-level: they're syntactic conveniences. They often strip parentheses away, or shave charcters off.

## Inspiration

I've tried to take as much from already existing operators as possible so that the library should be familiar to any users who've dealt with value-level combinators in `base`. Applicative patterns such as `<$>` and `<*>` are for example possible, but without the applicative-ness of it.

```haskell
type (<*>) f a = f a
infixl 4 <*>

a :: Either <*> SomeException ^> IO () <*> Maybe Int
=
a :: Either (SomeException -> IO ()) (Maybe Int)
```

## Other notes

It could potentially also be possible to make `=>` into a type-level operator that takes a `Constraint` kind on the LHS. Just making a type-synonym for `=>` doesn't work the way you might expect, rather this constrains it *outside* the context and hence introduces RankNTypes.

```haskell
type (==>) (c :: Constraint) (a :: *) = c => a
```

An interesting change in behaviour between type-synonyms and type-families is how their types end up looking after compilation.

```haskell
type (a * b) = (a, b)

 >> :t undefined :: a * b
 undefined :: a * b :: a * b

type family (a * b) where
    a * b = (a, b)

 >> :t undefined :: a * b
 undefined :: a * b :: (a, b)
```

Type synonyms don't show the underlying definition of the type as you'd expect but type families do. This may make operator combinators more amicable to the novice as they can easily expose the underlying definition without having to look up Hackage documentation. Just whip out GHCi and go nuts.


## Partially applied type constructors

One of the big conveniences functional programming has at the value-level is partial application. Remember the `<*>` type operator from earlier? That uses partial application to its advantage.

```haskell
 >> :t undefined :: Either <*> a <*> b
undefined :: Either <*> a <*> b :: Either a b
```

`Either` is a a type defined using the `data` keyword, allowing partial application. We can also partially apply classes.

```haskell

class C a b where
    c :: a -> b -> a

test :: '[C a] <+> '[a, b] => a -> b -> a
```

However, what if we want to change the order of `C`'s arguments? At the value-level we have the trusty `flip` function. We can define it at the type-level.

```haskell
type Flip f a b = f b a

 >> :t undefined :: '[Flip C b] <+> '[a,b] => a -> b -> a

<interactive>:1:14: error:
    * The type synonym `Flip' should have 3 arguments, but has been given 2
    * In an expression type signature:
        '[Flip C b] <+> '[a, b] => a -> b -> a
      In the expression:
          undefined :: '[Flip C b] <+> '[a, b] => a -> b -> a
```

Unfortunately, as you can probably tell, it's not very useful because we can't partially apply it — we can't partially apply type synonyms. The full power of partial application you're familiar with at the value-level is not possible at the type-level. Even simple cases don't work.

```haskell
type E a b = Either a b

 >> :t undefined :: E <*> a <*> b
<interactive>:1:14: error:
    * The type synonym `E' should have 2 arguments, but has been given none
    * In an expression type signature: (E <*> a) <*> b
      In the expression: undefined :: (E <*> a) <*> b`
```

In some cases we are lucky and can define type synonyms partially applied, which does make it work. This simply isn't always possible, as is the case with Flip.

```
type E = Either
undefined :: E <*> a <*> b :: E a b

```

Why?

