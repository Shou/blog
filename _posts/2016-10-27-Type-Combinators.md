---
layout: post
title: Through type-level combinators
---

## TODO

- Type-level operators behaviour in `class`, `type family`, `GADT`, et al.
- Use :kind! to inspect types rather than `undefined :: blah`


Type-level operator are helpful syntactic combinators, and much of the work incorporated into the [type-operators](https://www.stackage.org/package/type-operators) package is shown here.

## A brief historical overview

Type-level operators have a long, but subdued history in GHC. They were originally prefixed with `:` and older literature on the subject will have that present. [Servant](http://www.arow.info/blog/posts/2015-07-10-servant-intro.html) for example uses the old type operator format in datatype definitions. [The original proposal dates back eleven years](https://prime.haskell.org/wiki/InfixTypeConstructors), and also mentions how operators were treated like variables in previous versions of GHC. [With GHC 7.6 that all changed](https://ghc.haskell.org/trac/ghc/ticket/1930):

> From the 7.6.1 release notes: "The behavior of the TypeOperator extension has changed: previously, only type operators starting with ":" were considered type constructors, and other operators were treated as type variables. Now type operators are always constructors. "

So nowadays we are unable to use type operators as variables anymore, and [some people were unhappy about that change](http://haskell.1045720.n5.nabble.com/Type-operators-in-GHC-td5154978.html) because it meant, e.g. that this was no longer possible:

```haskell
 >> :type a :: (Arrow (~>)) => a ~> b

<interactive>:1:13: error:
    Not in scope: type constructor or class `~>'
```

But on the other hand behaviour is more consistent and familiar as they took on behaviour from the value-level.

Type-level operators, in their current form, were talked about [briefly during the 2014 Christmas calendar "24 Days of GHC Extensions"](https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html), but otherwise they don't seem to have had much presence.

## Getting to the meat of it

Type-level operators are just what they say on the tin, and this means you can use `data`, `newtype`, `type`, `type family`, `class`, and so on, to define type constructors in the shape of operators. A 'common' example is the function application operator `$`, which can be defined in various ways, and as expected allows you to rid excessive parentheses and 'sequentialize' your type signatures.

While we're at it, let's get a little opinionated and 'fix' `$` by specifying its fixity with a hyphen. We'll see why shortly...

```haskell
type (f $- a) = f a
type ($-) f = f
type family ($-) f a where
    ($-) f a = f a

a :: Either String $- Maybe Int
```

This is your average function application operator, except now living at the type-level. With some added type-level caveats, it works pretty much the same as.

Now, there's also the helpful *argument* application operator. This is identical to the function application operator but its fixity is flipped. This happens to allow us to apply arbitrary amounts of arguments. This pattern may be familiar if you've used Applicative's `<*>`.

```haskell
type (-$) f a = f a
infixl 4 -$

a :: Either -$ SomeException ^> IO () -$ Maybe Int
=
a :: Either (SomeException -> IO ()) (Maybe Int)
```

The unmentioned operator here, `^>` is a tightly binding `->`, its fixity and precedence is `infixr ^> 8` like the `^` operator familiar from mathematics. It allows removal of parentheses to denote function types as arguments, seen above.

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

This operator is the tuple constructor -- some languages use this constructor instead of parentheses and commas, such as Isabelle. It can be seen as the product of two types in type theory, or set theory's Cartesian product operator where the logic behind it is that types are sets of values. One that accepts arbitrary size tuples up to 9 is also possible with type families.


```haskell
type family (f1 * f2) where
    (*) (a, b, c, d, e, f, g, h) i = (a, b, c, d, e, f, g, h, i)
    (*) (a, b, c, d, e, f, g)    h = (a, b, c, d, e, f, g, h)
    (*) (a, b, c, d, e, f)       g = (a, b, c, d, e, f, g)
    (*) (a, b, c, d, e)          f = (a, b, c, d, e, f)
    (*) (a, b, c, d)             e = (a, b, c, d, e)
    (*) (a, b, c)                d = (a, b, c, d)
    (*) (a, b)                   c = (a, b, c)
    (*) a                        b = (a, b)
infixl 9 *
```

The downside to this is that it consumes tuples on the left, even if it's intended to be a tuple. I'm sadly unaware of any way around that issue while retaining the simplicity.

We can also go a little crazy and define Applicative and Monadic constrained function type operators, with `-XRankNTypes` enabled.

```haskell
type (a *> b) = forall m. Applicative m => m a -> m b

type (a >> b) = forall m. Monad m => m a -> m b

idM :: a >> a
idM m = m >>= return . id
```

Or a little unconventional and compress function types with a sequence type family. This requires `-XUndecidableInstances` and `-XDataKinds`, which loosen the type family instance restrictions, and enable type-level literals including type-level lists.

```haskell
type family IfEmpty xs a b where
    IfEmpty '[] a b = a
    IfEmpty (x ': xs) a b = b

type family S ts where
    S (a ': b ': rest) = IfEmpty rest (a -> b) (a -> b -> S rest)
    S '[a] = a

 >> :t undefined :: S [a,b,c,d]
undefined :: S [a,b,c,d] :: a -> b -> c -> d
```

`S` is a recursive function that just applies `->` between every element until the end of the list is hit. A single element is just the element itself, and there is no instance for empty lists.

Going further with constraints and type-level lists, we can leverage the `-XConstraintKinds` language extension to make some pretty handy operators. This language extension gives us the `Constraint` kind allowing us to say a type variable has `c :: Constraint`, or even `f :: * -> Constraint`. 

```haskell
import Data.Kind (Constraint)

type family (+>) (c :: k -> Constraint) (as :: [k]) where
    (+>) c '[] = (() :: Constraint)
    (+>) c (h ': t) = (c h, (+>) c t)
infixl 9 +>

type family (<+>) (ca :: [k -> Constraint]) (cb :: [k]) where
    (<+>) ca '[] = (() :: Constraint)
    (<+>) '[] cb = (() :: Constraint)
    (<+>) (cha ': cta) cbs = (cha +> cbs, cta <+> cbs)
infixl 9 <+>

test :: [Read, Show, Monoid] <+> [a,b] => a -> b
test a = let b = read (show a) in b <> b

 >> :t test
test :: (Monoid b, Monoid a, Show b, Show a, Read b, Read a) => a -> b
```

Every constraint in the list is applied to every variable in the second list. The expanded type reveals how useful this is syntactically if you're dealing with many constraints.

Do note with GHC 7.10 and earlier, type-level operators are wonky in contexts without parentheses which is unfortunate because type-level combinators are often meant to strip parentheses. With GHC 8.0 this behaviour was rectified and parentheses are no longer necessary, allowing the first example.

```haskell
 >> a :: [Show, Num] <+> [a, b] => a -> b

 <interactive>:3:33: parse error on input `=>''`

 >> a :: ([Show, Num] <+> [a, b]) => a -> b
```

## Partially applied type constructors

One of the big conveniences functional programming has at the value-level is partial application. Remember the `-$` type operator from earlier? That uses partial application to its advantage.

```haskell
 >> :t undefined :: Either -$ a -$ b
undefined :: Either -$ a -$ b :: Either a b
```

`Either` is a type defined using the `data` keyword, allowing partial application. We can also partially apply classes.

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

In some cases we are lucky and can define type synonyms partially applied, which does make it work, but this isn't always possible as is the case with Flip above.

```haskell
type E = Either
undefined :: E <*> a <*> b :: E a b

type ($) f = f

type family Map f ts where
    Map f (a ': rest) = f a ': Map f rest
    Map f '[] = '[]

 >> :t undefined :: S (Map (($) Maybe) [a,b,c])
undefined :: S (Map (($) Maybe) [a,b,c])
          :: Maybe a -> Maybe b -> Maybe c
```

`-XLiberalTypeSynonyms` seems to lax the rules. An example from the GHC test suite:

```haskell
type Thing m = m ()

type Const a b = a

test :: Thing (Const Int) -> Thing (Const Int)
test = test
```

