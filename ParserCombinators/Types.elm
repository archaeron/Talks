module Types where

{-| Here are defined all the types we will be using -}

import Char

type Angle = Angle Float

type Expression
    = Variable Char
    | Constant Char
    | TurnLeft
    | TurnRight
    | Push
    | Pop

type Premise = Premise (List Expression)

type Expansion = Expansion (List Expression)

type Rule = Rule Premise Expansion

type alias System =
    { angle : Angle
    , start : List Expression
    , rules : List Rule
    }

type Point = Point Float Float

type alias Line =
    { start : Point
    , end : Point
    , width : Int
    }
