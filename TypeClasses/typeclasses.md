# Typeclasses

---

## Beispiel Division

---

### Simpel

```haskell
data Term = Con Int | Div Term Term

eval           :: Term -> Int
eval (Con a)   =  a
eval (Div t u) =  eval t / eval u
```

---

```haskell
eval           :: Term -> Int
eval (Con a)   =  a
eval (Div t u) =  eval t / eval u

answer         :: Term
answer         =  Div (Div (Con 1972) (Con 2)) (Con 23)

eval answer
-- Div (Div (Con 1972) (Con 2)) (Con 23)
-- Div (Div 1972 2) 23
-- Div (1972 / 2) 23
-- (1972 / 2) / 23
-- 986 / 23
-- 42
```

---

### Counter

```haskell
type State = Int
type M a = State -> (a, State)

eval :: Term -> M Int
eval (Con a) x = (a, x)
eval (Div t u) x =
    let (a, y) = eval t x in
    let (b, z) = eval u y in
    (a / b, z + 1)
```

---

### Exceptions

```haskell
type Exception = String
data M a = Raise Exception | Return a

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) =
    case eval t of
        Raise e -> Raise e
        Return a ->
            case eval u of
                Raise e -> Raise e
                Return b ->
                    if b == 0
                        then Raise "divide by zero"
                        else Return (a / b)
```

### Output

```haskell
type Output = String
type M a = (Output, a)

eval :: Term -> M Int
eval (Con a) = (line (Con a) a, a)
eval (Div t u) =
    let (x, a) = eval t in
    let (y, b) = eval u in
    (x ++ y ++ line (Div t u) (a / b), a / b)

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") = " ++ show a ++ "\n"
```

---

# Typeclasses

---

## Einführung

```haskell
class Show a where
    show :: a -> String

instance Show Boolean where
    show true = "true"
    show false = "false"
```

---

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

data Ordering = LT | EQ | GT

class Eq a => Ord a where
    compare :: a -> a -> Ordering
```

---

## Monads

---

### Einführung

```haskell
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

---

```haskell
eval :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> return (a / b)
```

-- eval (Div t u) = ((eval t) >>= (\a -> ((eval u) >>= (\b -> (return (a / b)))))














---

## Semigroup (Halbgruppe)

The `(<>)` operator is for 'concatenation', and must be associative:
`(a <> b) <> c = a <> (b <> c)`

```haskell
class Semigroup a where
    (<>) :: forall a. a -> a -> a
```

---

## Monoid

A monoid describes how to accumulate a result with the type `m`

```haskell
class (Semigroup m) <= Monoid m where
    mempty :: m
```

---

## Functor

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

### Laws

- `fmap id = id`
- `fmap (g . h) = (fmap g) . (fmap h)`

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

## Applicative validation (Haskell)

```haskell
> import Control.Applicative
>
> data Address = Address { street :: String, city :: String, canton :: String }
>
> liftA3 Address (Just "Gutestr. 45") (Just "Zürich") (Just "Zürich")
< Just (Address {street = "Gutestr. 45", city = "Zürich", canton = "Zürich"})
> liftA3 Address (Just "a") (Just "b") Nothing
< Nothing
```

```haskell
> :t (<*>)
< (<*>) :: Applicative f => f (a -> b) -> f a -> f b
> :t (<$>)
< (<$>) :: Functor f => (a -> b) -> f a -> f b
> lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
> lift3 f x y z = f <$> x <*> y <*> z
```

```haskell
> Address <$> (Just "Gutestr. 45") <*> (Just "Zürich") <*> (Just "Zürich")
< Just (Address {street = "Gutestr. 45", city = "Zürich", canton = "Zürich"})
```

```haskell
(<?>) Nothing err = Left err
(<?>) (Just a) _ = Right a

fullNameEither first middle last =
    fullName <$> (first <?> "First name was missing")
        <*> (middle <?> "Middle name was missing")
        <*> (last <?> "Last name was missing")
```

---

# Sources

* Philip Wadler
  - [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html)

---

# Helps

* Miran Lipovaca
  - [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
* Erik Meijer
  - [Introduction to Functional Programming](https://www.edx.org/course/introduction-functional-programming-delftx-fp101x)
* Stephen Diehl
  - [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/#applicatives)
* Graham Hutton
 -- [Programming in Haskell](http://www.cs.nott.ac.uk/~gmh/book.html)
* Bartosz Milewski
  - [Fun With Categories](https://github.com/LambdaCon/2015/blob/master/Opening%20keynote%20-%20Fun%20with%20categories/slides/Fun%20with%20categories.pdf)
