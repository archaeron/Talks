# Typeclasses

---

## Introduction

```haskell
class Show a where
    show :: a -> String

instance Show Boolean where
    show true = "true"
    show false = "false"
```

---

Let's use the Show typeclass

```haskell
describeValue :: Show a => a -> String
describeValue a = "The values is: " ++ show a
```

---

## Eq and Ord typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

data Ordering = LT | EQ | GT

class Eq a => Ord a where
    compare :: a -> a -> Ordering
```

---

## Functor

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

### Laws

- `fmap id = id`
- `fmap (g . f) = (fmap g) . (fmap f)`

---

Let's use the Functor typeclass

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

data List a = Cons a (List a) | Nil deriving Show

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```

```haskell
fmap (+1) Nil -- Nil
fmap (+1) $ Cons 4 Nil -- Cons 5 Nil
fmap (+1) $ Cons 4 $ Cons 10 Nil -- Cons 5 (Cons 11 Nil)
```

---

check the first law: `fmap id = id`

```haskell
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```

Check:

```haskell
-- check with Nil
fmap id Nil = Nil

-- check with `Cons x xs`
fmap id (Cons x xs)
    = Cons (id x) (fmap id xs)
    = Cons x (fmap id xs)
```

---

check the second law:

`fmap (g . f) = (fmap g) . (fmap f)`

```haskell
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```

Check:

```haskell
-- check with Nil
fmap (g . f) Nil = Nil
(fmap g) . (fmap f) $ Nil = (fmap g) Nil = Nil

-- check with `Cons x xs`
fmap (g . f) (Cons x xs)
    = Cons (g . f $ x) (fmap (g . f) xs)

(fmap g) . (fmap f) $ (Cons x xs)
    = (fmap g) (Cons (f x) (fmap f xs))
    = Cons (g . f $ x) (fmap (g . f) xs)
```

---

## Applicative

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

### Laws

- `pure id <*> a = a`
- `pure f <*> pure a = pure (f a)`
- `a <*> pure b = pure ($ a) <*> b`

---

## Applicative Functor

```haskell
fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

---

## Applicative validation

```haskell
fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

```haskell
data Address = Address
    { street :: String
    , city :: String
    }
-- :t Address
-- Address :: String -> String -> String -> Address
```
```haskell
Address
    <$> (Just "Technikumstrasse 9")
    <*> (Just "Winterthur")
-- Just (Address
--     { street = "Gutestr. 45"
--     , city = "Zürich" })
Address (Just "a") (Just "b") Nothing
-- Nothing
```

---

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

### Exceptions

```haskell
type Exception = String
data MException a = Raise Exception | Return a

eval :: Term -> MException Int
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

---

Let's make a monad for the exception

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

---

```haskell
type Exception = String
data MException a = Raise Exception | Return a

instance Monad MException where
    return = Return
    (Raise e)  >>= _ = Raise e
    (Return a) >>= f = f a
```

---

```haskell
-- Monad with the bind (>>=) function
evalException2 :: Term -> MException Int
evalException2 (Con a) = return a
evalException2 (Div t u) =
	(evalException2 t) >>=
		(\a -> evalException2 u >>=
			(\b ->
			 	if b == 0
					then Raise "divide by zero"
					else return (a `div` b)))
```

---

```haskell
-- Monad with do-notation
evalException3 :: Term -> MException Int
evalException3 (Con a) = return a
evalException3 (Div t u) =
	do
		a <- evalException3 t
		b <- evalException3 u
		if b == 0
		then Raise "divide by zero"
		else Return $ a `div` b
```

---

```haskell
-- using `Either` instead of our own Monad
evalExceptionEither :: Term -> Either String Int
evalExceptionEither (Con a) = return a
evalExceptionEither (Div t u) =
	do
		a <- evalExceptionEither t
		b <- evalExceptionEither u
		if b == 0
		then Left "divide by zero"
		else Right $ a `div` b
```

---

# Sources

* Philip Wadler
  - [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html)
* Phil Freeman
  - [PureScript by Example](https://leanpub.com/purescript/read)

---

# Helps

* Miran Lipovaca
  - [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
* Erik Meijer
  - [Introduction to Functional Programming](https://www.edx.org/course/introduction-functional-programming-delftx-fp101x)
* Stephen Diehl
  - [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/#applicatives)
* Graham Hutton
  - [Programming in Haskell](http://www.cs.nott.ac.uk/~gmh/book.html)
* Bartosz Milewski
  - [Fun With Categories](https://github.com/LambdaCon/2015/blob/master/Opening%20keynote%20-%20Fun%20with%20categories/slides/Fun%20with%20categories.pdf)
* Various
  - [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
