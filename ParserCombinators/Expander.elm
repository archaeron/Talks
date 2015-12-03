module Expander (expandSystem) where

import Types exposing
    (Expression, Expansion(..) , Rule(..), System, Premise(..))

{-| Tests every element with a predicate.
Returns Nothing if it can't find the element

    tryFind (\n -> n > 3) [1, 2, 3, 4] == Maybe 4
    tryFind (\n -> n > 4) [1, 2, 3, 4] == Nothing
-}
tryFind : (a -> Bool) -> List a -> Maybe a
tryFind predicate ys =
    case ys of
        [] ->
            Nothing
        x :: xs ->
            if predicate x then
                Just x
            else
                tryFind predicate xs

findRule : List Rule -> Expression -> Maybe Rule
findRule rules expression =
    let
        finder (Rule (Premise premise) _) =
            case List.head premise of
                Just e ->
                    e == expression
                Nothing ->
                    False
    in
        tryFind finder rules

extractExpansion : Rule -> List Expression
extractExpansion (Rule _ (Expansion e)) = e

expandRules' : List Rule -> Expression -> List Expression -> List Expression
expandRules' rules expression acc =
    let
        expansion =
            expression
            |> findRule rules
            |> Maybe.map extractExpansion
    in
        case expansion of
            Just exp ->
                exp ++ acc
            _ ->
                expression :: acc

reverseExpansion : Expansion -> Expansion
reverseExpansion (Expansion e) = Expansion (List.reverse e)

reverseRule : Rule -> Rule
reverseRule (Rule (Premise premise) expansion) =
    Rule (Premise (List.reverse premise)) (reverseExpansion expansion)

expandRules : List Expression -> List Rule -> Int -> List Expression
expandRules start rules step =
    case step of
        0 ->
            start
        _ ->
            let
                expanded =
                    start
                    |> List.foldl (expandRules' rules) []
                    |> List.reverse
            in
                expandRules expanded rules (step - 1)

expand : List Expression -> List Rule -> Int -> List Expression
expand start rules step =
    let
        reversedRules =
            List.map reverseRule rules
    in
        expandRules start reversedRules step

expandSystem : Int -> System -> List Expression
expandSystem step system =
    expand system.start system.rules step
