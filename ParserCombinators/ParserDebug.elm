module ParserDebug where

import Types exposing (..)
import String

showAngle : Angle -> String
showAngle (Angle a) = toString a

showExpression : Expression -> String
showExpression e =
    case e of
        Variable v -> String.fromChar v
        Constant c -> String.fromChar c
        TurnLeft -> "-"
        TurnRight -> "+"
        Push -> "["
        Pop -> "]"

showExpressions : List Expression -> String
showExpressions es =
    es
    |> List.map showExpression
    |> String.concat

showPremise : Premise -> String
showPremise (Premise es) =
    showExpressions es

showExpansion : Expansion -> String
showExpansion (Expansion es) =
    showExpressions es

showRule : Rule -> String
showRule (Rule p e) =
    String.concat [showPremise p, " -> ", showExpansion e]

showRules : List Rule -> String
showRules rs =
    rs
    |> List.map showRule
    |> String.join "\n"

showSystem : System -> String
showSystem s =
    String.concat
        [ "Angle = " , showAngle s.angle, "\n"
        , "Start = ", showExpressions s.start, "\n"
        , "Rules =\n", showRules s.rules
        ]
