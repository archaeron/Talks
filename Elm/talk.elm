module Talk where

-- Assignments
--------------

number : Int
number = 5

greetings : String
greetings = "Hello World"

-- Functions
------------

add : Int -> Int -> Int
add a b = a + b

add42 : Int -> Int
add42 n = add 42 n
-- add42 8 == 50

add43 : Int -> Int
add43 = add 43


-- Recursion
------------

repeat : Int -> String -> String
repeat n string =
  case n of
    0 -> ""
    _ -> string ++ (repeat (n-1) string)

-- Lists
--------

list : List Int
list = [1, 2, 3, 4]

prepend : List a -> a -> List a
prepend xs x = x :: xs

append : List a -> a -> List a
append xs x = xs ++ [x]

-- Tagged Unions
----------------

type Answer = Yes | No

type Name = HasName String | NoName

type Option a = Some a | None

-- Records
----------

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

-- Maybe
--------

justHello = Just "hello"
nothing = Nothing
