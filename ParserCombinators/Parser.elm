module Parser where

import Combine exposing (..)
import Combine.Char exposing (char, space, newline, lower, upper)
import Combine.Infix exposing ((<*), (*>), (<*>), (<$>), (<$) ,(<|>), (<?>))
import Combine.Num exposing (float)
import String
import Types exposing (..)


parser : Parser System
parser = fail [ "You need to implement a parser" ]

-- helper functions

{-| Maps over two parsers.

    type Model = Model Char Int

    model = map2 Model upper int
    parse model "N7" == Ok (Model 'N' 7)
-}
map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f a b = f <$> a <*> b

map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 f a b c = f <$> a <*> b <*> c

{-| Parse a string to an L-System
-}
parse : String -> Result.Result (List String) System
parse s =
    let
        (parseResult, _) = Combine.parse parser s
    in
        case parseResult of
            Done res ->
                Result.Ok res
            Fail e ->
                Result.Err e
