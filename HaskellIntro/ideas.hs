sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2..]

take 10 primes

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
