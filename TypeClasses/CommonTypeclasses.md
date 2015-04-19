# Common Typeclasses

These Typeclasses shown here are for PureScript.
Haskell has all these too, but you need to search for them on Hackage.

## Semigroup

The `(<>)` combines elements from the semigroup, and must be associative.

### Laws

- `(a <> b) <> c = a <> (b <> c)`

```haskell
class Semigroup a where
    (<>) :: forall a. a -> a -> a
```

## Monoid

A monoid describes how to accumulate a result with the type `m`

### Laws

- `mempty <> a = a`
- `a <> mempty = a`
- `(a <> b) <> c = a <> (b <> c)` (from the semigroup)

```haskell
class (Semigroup m) <= Monoid m where
    mempty :: m
```

## Functor

A functor has a `map` function `(<$>)`.

It's also called `lift`, because it lifts a function into the functor:
`a -> b` becomes `f a -> f b`.

Other names:

- Scala: `map`
- Elm: `map` or `(<~)` (was `lift`)
- Haskell: `fmap`

### Laws

- `fmap id = id`
- `fmap (g . h) = (fmap g) . (fmap h)`

```haskell
class Functor f where
    (<$>) :: forall a b. (a -> b) -> f a -> f b
```

## Apply

Apply works well together with a functor. You use the functor for the first
argument and the apply for the rest.

Example: `lift3 f x y z = f <$> x <*> y <*> z`

Other names:

- Haskell: `ap`
- Elm: `(~)`

```haskell
class (Functor f) <= Apply f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
```

## Applicative

```haskell
class (Apply f) <= Applicative f where
    pure :: forall a. a -> f a
```

### Laws

- `pure id <*> v = v`
- `pure f <*> pure x = pure (f x)`
- `u <*> pure y = pure ($ y) <*> u`
  `u <*> pure y = pure (\f -> f y) <*> u`

  The order in which we evaluate the function and its argument doesn't matter.

- `u <*> (v <*> w) = pure (.) <*> u <*> v <*> w`

## Traversable

```haskell
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b f. (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequence :: forall a f. (Applicative f) => t (f a) -> f (t a)
```

## Bind

```haskell
class (Apply m) <= Bind m where
    (>>=) :: forall a b. m a -> (a -> m b) -> m b
```

## Semigroupoid

```haskell
class Semigroupoid a where
    (<<<) :: forall b c d. a c d -> a b c -> a b d
```

## Category

```haskell
class (Semigroupoid a) <= Category a where
    id :: forall t. a t t
```

## Monad

```haskell
class (Applicative m, Bind m) <= Monad m where
```
