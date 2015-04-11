# Introduction to Haskell

![Repo](https://github.com/archaeron/Dojos)

---
## Assignments

```haskell
number :: Int
number = 5

greetings :: String
greetings = "Hello World"
```

---
## Functions

```haskell
add :: Int -> Int -> Int
add a b = a + b

add42 :: Int -> Int
add42 n = add 42 n
-- add42 8 == 50

add43 :: Int -> Int
add43 = add 43
-- add43 7 == 50
```

---
## Recursion

```haskell
repeat :: Int -> String -> String
repeat 1 string = string
repeat n string = string ++ (repeat (n-1) string)
-- repeat 3 "hi" == "hihihi"
```

---
## Lists

```haskell
list :: [Int]
list = [1, 2, 3, 4]

prependInt :: [Int] -> Int -> [Int]
prependInt xs x = x : xs
-- prependInt list 0 == [0, 1, 2, 3, 4]

appendInt :: [Int] -> Int -> [Int]
appendInt xs x = xs ++ [x]

reverse :: [a] -> [a]  
reverse [] = []  
reverse (x:xs) = reverse xs ++ [x]   
```


---
## Higher order functions
```haskell
map :: (a -> b) -> [a] -> [b]
-- map (\x -> x + 1) list == map (+1) list

filter :: (a -> Bool) -> [a] -> [a]
-- filter even list == [2, 4]
```

---
## Laziness
```haskell
naturals = [0..]

-- take 4 naturals == [0, 1, 2, 3]
-- zip naturals [1, 2, 3] == [(0,1),(1,2),(2,3)]
```

---
## List comprehension
```haskell
odds :: [Int]
odds = [x | x <- [0..], odd x]

carthesianProduct :: [a] -> [b] -> [(a, b)]
carthesianProduct xs ys = [(x, y) | x <- xs, y <- ys]
-- carthesianProduct [1, 2] [10, 11, 12] == [(1,10),(1,11),(1,12),(2,10),(2,11),(2,12)]

take 10 [ (i,j) | i <- [1..], j <- [1..i-1], gcd i j == 1 ]
--  [(2,1),(3,1),(3,2),(4,1),(4,3),(5,1),(5,2),(5,3),(5,4),(6,1)]

```


---
## All together now
```haskell
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2..]

-- take 10 primes == [2,3,5,7,11,13,17,19,23,29]
```


---
## Polymorphic Types

```haskell
prepend :: [a] -> a -> [a]
prepend xs x = x : xs
-- prepend list 0 == [0, 1, 2, 3, 4]
-- prepend "ello" 'H' == "Hello"

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

tuplify :: a -> b -> (a, b)
tuplify x y = (x, y)

positions :: Eq a => a -> [a] -> [Int]
```


---
## Maybe

```java
public int foo (String text){
	String parsedString = null;
	try{
		parsedString = parseString(text);
	}catch(ParseException e) {
		System.out.println("oh snap!")
	}
	int i = -1;
	if(parsedString != null) {
		i = bar(parsedString);
	}
	return i;
}

```
[Java 8 optional](http://www.oracle.com/technetwork/articles/java/java8-optional-2175753.html)


---
```haskell
foo :: String -> Maybe Int
foo text = 
	case parseString text of 
	Just parsedString -> Maybe . bar parsedString
	Nothing -> Nothing
-- Or even nicer:
foo :: String -> Maybe Int
foo text = fmap bar $ parseString text
```

---
## Define Types

Type = synonyms
Data = new constructs

```haskell
type Year = Int
type Month = Int
type Day = Int
data Date = Date Year Month Day

type Name = String

data Celebration = 
   		Birthday Name Date
   	| 	Wedding Name Name Date
```


---
## Guess the following

```haskell
Int -> Char -> String
```

[Hoogle: Int -> Char -> String](https://www.haskell.org/hoogle/?hoogle=Int+-%3E+Char+-%3E+String+-quickcheck)

---
## Currying

```haskell
add' :: (Int, Int) -> Int
add' (x,y) = x + y

add'' :: Int - >(Int -> Int)
add'' = λx -> (λy -> x + y)

add''' :: Int -> Int -> Int
add''' x y = x + y
```

---
## Outlook

    - If it compiles, it works.
    - Immutable by default
    - No null
    - Refactoring
    - Abstraction

---
## Caesar Cipher

Keep calm and curry on - pjju%hfqr%fsi%hzww~%ts