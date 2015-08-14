# Elm

---

## Values

```elm
number : Int
number = 5

greetings : String
greetings = "Hello World"
```

---

## Functions

```elm
add : Int -> Int -> Int
add a b = a + b

add42 : Int -> Int
add42 n = add 42 n
-- add42 8 == 50

add43 : Int -> Int
add43 = add 43
-- add43 7 == 50
```

---

## Conditionals

```elm
nearlyFizzBuzz n =
  if
    | n % 3 == 0 -> "Fizz"
    | n % 5 == 0 -> "Buzz"
    | otherwise -> toString n
```

---

## Tuples

```elm
person : (String, String, Int)
person = ("Hans Meier", "Zürich", 8000)
```

---

## Pattern Matching

```elm
fizzBuzz n =
  case (n % 3, n % 5) of
    (0, 0) -> "Fizz Buzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    (_, _) -> toString n
```

---

## Lists

```elm
list : List Int
list = [1, 2, 3, 4]

prepend : List a -> a -> List a
prepend xs x = x :: xs
-- prepend list 0 == [0, 1, 2, 3, 4]

append : List a -> a -> List a
append xs x = xs ++ [x]
-- What is the result of: append list 5

-- List.sort [2, 4, 1, 3] == [1, 2, 3, 4]
```

---

## Tagged Unions

```elm
type Answer = Yes | No

type Name = HasName String | NoName

-- builtin
type Maybe a = Just a | Nothing

-- builtin
type Result error value = Ok value | Error error
```

---

## Working with Tagged Unions

```elm
type Name = HasName String | NoName

printName : Name -> String
printName name =
  case name of
    HasName n -> n
    NoName -> "I don't have a name :("

printName (HasName "Frank") -- "Frank"
printName NoName -- "I don't have a name :("
```

---

## Records

```elm
type alias Player =
  { name : String
  , x : Float
  , y : Float
  , score : Int
  }

player1 : Player
player1 =
  { name = "Player 1"
  , x = 100
  , y = 100
  , score = 0
  }

player2 = Player "Player 2" 150 150 0
```

---

## Working with Records

```elm
point = { x = 100, y = 150 }

yCoord1 = point.y -- 150

yCoord2 = .y point -- 150

List.map .y [point, point, point] -- [150, 150, 150]
```

---

```elm
point = { x = 100, y = 150 }

updatedPoint = { point | x <- 150 }
-- updatedPoint == { x = 150, y = 150 }
-- point == { x = 100, y = 150 }
```

---

## Anonymous Functions

```elm
successor : number -> number
successor n = n + 1

successor : number -> number
successor = \n -> n + 1

List.map (\n -> n + 1) [1, 2, 3]
```

---

## Higher order functions

```elm
List.map : (a -> b) -> List a -> List b
-- List.map (\x -> x + 1) [1, 2, 3, 4] == [2, 3, 4, 5]

List.filter : (a -> Bool) -> List a -> List a
-- List.filter even [1, 2, 3, 4] == [2, 4]
```

---

## Maybe

```elm
type Maybe a = Just a | Nothing

justOne = Just 1
nothing = Nothing

parseNumber : String -> Maybe Int

parseNumber "hello" : Maybe Int -- Nothing
parseNumber "23" : Maybe Int -- Just 23
```

---

```elm
-- can you implement Maybe.map yourself?

-- Maybe.map : (a -> b) -> Maybe a -> Maybe b

Maybe.map (\x -> x + 1) justOne -- Just 2
Maybe.map (\x -> x + 1) nothing -- Nothing
```

---

```elm
map func maybe =
  case maybe of
    Nothing -> Nothing
    Just value -> Just (func value)
```

---

## Result

```elm
type Result error value = Ok value | Error error

-- Result.map : (a -> value) -> Result x a -> Result x value
-- Result.andThen : Result x a -> (a -> Result x b) -> Result x b
```

---

## Signal

```elm
Mouse.position : Signal (Int, Int)
main : Signal Html

-- Signal.map : (a -> result) -> Signal a -> Signal result
-- Signal.foldp : (a -> state -> state) -> state -> Signal a -> Signal state
-- Signal.filter : (a -> Bool) -> a -> Signal a -> Signal a
-- Singal.sampleOn : Signal a -> Signal b -> Signal b
```
