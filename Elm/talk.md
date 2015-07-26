# Elm

---

## Assignments

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


## Recursion

```elm
repeat : Int -> String -> String
repeat n string =
  case n of
    0 -> ""
    _ -> string ++ (repeat (n-1) string)
-- repeat 3 "hi" == "hihihi"
```

---

## Lists

```elm
list :: List Int
list = [1, 2, 3, 4]

prepend : List a -> a -> List a
prepend xs x = x :: xs
-- prepend list 0 == [0, 1, 2, 3, 4]

append : List a -> a -> List a
append xs x = xs ++ [x]
-- What is the result of: append list 5

-- List.sort [2, 4, 1, 3] == [1, 2, 3, 4]
```

## Tuples

```elm
person = (String, String, Int)
person = ("Hans Meier", "Zürich", 8000)
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

## Higher order functions

```elm
List.map : (a -> b) -> List a -> List b
-- List.map (\x -> x + 1) list == [1, 2, 3, 4, 5]

List.filter : (a -> Bool) -> List a -> List a
-- List.filter even list == [2, 4]
```

---

## Maybe

```elm
justHello = Just "hello"
nothing = Nothing
```

---
