# Haskell Dojo

***

## Haskell Einführung

---

```haskell
add      :: Int -> Int -> Int
add x y  =  x + y
```

---

```haskell
add      :: Int -> Int -> Int
add x y  =  x + y

add42    :: Int -> Int
add42    =  add 42
```

---

```haskell
numbers  :: [Int]
numbers  =  [1, 2, 3, 4, 5]
```

---

```haskell
numbers  :: [Int]
numbers  =  [1, 2, 3, 4, 5]

map        :: (a -> b) -> [a] -> [b]
add42      :: Int -> Int

```

---

```haskell
numbers  :: [Int]
numbers  =  [1, 2, 3, 4, 5]

map        :: (a -> b) -> [a] -> [b]
add42      :: Int -> Int

map add42  :: [Int] -> [Int]

numbers2   =  map add42 numbers

```

---


```haskell
data Boolean = True | False
type Exception = String

```

***

## Beispiel Division

### Simpel

```haskell
data Term = Con Int | Div Term Term

eval           :: Term -> Int
eval (Con a)   =  a
eval (Div t u) =  eval t / eval u

answer         :: Term
answer         =  Div (Div (Con 1972) (Con 2)) (Con 23)
error          :: Term
error          =  Div (Con 1) (Con 0)

```

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


```haskell
eval           :: Term -> Int
eval (Con a)   =  a
eval (Div t u) =  eval t / eval u

error          :: Term
error          =  Div (Con 1) (Con 0)

eval error
-- Div (Con 1) (Con 0)
-- Div 1 0
-- 1 / 0
-- ERROR!
```

## Mit Fehlerhandling

```haskell
type Exception = String
data M a = Raise Exception | Return a

```

```haskell
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
                    then
                        Raise "divide by zero"
                    else
                        Return (a / b)

```
                    
## Mit Divisionszähler

```haskell
type State = Int
type M a = State -> (a, State)

eval :: Term -> M Int
eval (Con a) x = (a, x)
-- eval :: Term -> State -> (a, State)
-- eval (Con a) = \x -> (a, x)
eval (Div t u) x =
    let (a, y) = eval t x in
    let (b, z) = eval u y in
    (a / b, z + 1)

```

## Monads

### Einführung

```haskell
return :: a -> M a
(>>=)  :: M a -> (a -> M b) -> M b

```

Monad: Tripel mit (`M`, `return`, `>>=`)

```haskell
m >>= \a -> n

m             :: M a
a             :: a
n             :: M b
\a -> n       :: a -> M b
m >>= \a -> n :: M b
```
                    
                  
```haskell
m >>= \a -> n
```
ist das gleiche wie:
```haskell
let a = m in n
```
oder:
```javascript
var a = m();
n;

```
in einer "unreinen" Sprache.
                    

```haskell
eval :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> return (a / b)
-- eval (Div t u) = ((eval t) >>= (\a -> ((eval u) >>= (\b -> (return (a / b)))))
```


Der neue Code ist ein wenig komplizierter, aber viel flexibler.

Um unsere zwei Beispiele von oben zu realisieren, müssen wir nur `M`, `return` und `>>=` verändern und kleine lokale Veränderungen vornehmen.

Später werden wir eine schönere Art sehen das gleiche zu schreiben.
