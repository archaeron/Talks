# Typeclasses (Purescript)

## Einführung

```haskell
class Show a where
  show :: a -> String

instance Show Boolean where
  show true = "true"
  show false = "false"
```

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

data Ordering = LT | EQ | GT
class Eq a => Ord a where
  compare :: a -> a -> Ordering
```

## Theorie

### Semigroup (Halbgruppe)

The `(<>)` operator is for 'concatenation', and must be associative:
`(a <> b) <> c = a <> (b <> c)`

```haskell
class Semigroup a where
  (<>) :: forall a. a -> a -> a
```

### Monoid

A monoid describes how to accumulate a result with the type `m`

```haskell
class (Semigroup m) <= Monoid m where
  mempty :: m
```

### Functor

A functor has a `map` function `(<$>)`.

It's also called `lift`, because it lifts a function into the functor:
`a -> b` becomes `f a -> f b`.

Other names:

- Scala: `map`
- Elm: `map` or `(<~)` (was `lift`)
- Haskell: `fmap`

```haskell
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b
```

### Apply

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

### Applicative

```haskell
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

### Traversable

```haskell
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b f. (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequence :: forall a f. (Applicative f) => t (f a) -> f (t a)
```

### Bind

```haskell
class (Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
```

### Semigroupoid

```haskell
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d
```

### Category

```haskell
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

### Monad

```haskell
class (Applicative m, Bind m) <= Monad m where
```

(von https://hackage.haskell.org/package/semigroupoids)
```
Foldable ----> Traversable <--- Functor ------> Alt ---------> Plus           Semigroupoid
|               |                 |                              |                  |
v               v                 v                              v                  v
Foldable1 ---> Traversable1     Apply --------> Applicative -> Alternative      Category
                                  |               |              |                  |
                                  v               v              v                  v
                                  Bind ---------> Monad -------> MonadPlus        Arrow
```
