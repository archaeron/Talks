module Renderer where

import Color
import List
import Types exposing (..)

{-| Calculate the endpoint from a startpoint, an angle and a length -}
endpoint : Point -> Angle -> Float -> Point
endpoint (Point x y) (Angle angle) length =
    Point (x + length * cos angle) (y + length * sin angle)

mkLine : Point -> Angle -> Float -> Int -> Line
mkLine point angle length width =
    { start = point
    , end = endpoint point angle length
    , width = width
    }

type alias IterationData =
    { currentAngle : Float
    , angle : Angle
    , currentPosition : Point
    , positionStack : List (Point, Float)
    , lines : List Line
    }

constantStep : IterationData -> IterationData
constantStep data =
    let
        line = mkLine data.currentPosition (Angle data.currentAngle) 10.0 2
    in
        { data
        | currentPosition = line.end
        , lines = (line :: data.lines)
        }

pushStep : IterationData -> IterationData
pushStep data =
    { data
    | positionStack =
        (data.currentPosition, data.currentAngle) :: data.positionStack
    }

popStep : IterationData  -> IterationData
popStep data =
    let
        (oldPosition, oldAngle) =
            data.positionStack
            |> List.head
            |> Maybe.withDefault (data.currentPosition, data.currentAngle)
        newPositionStack =
            data.positionStack
            |> List.tail
            |> Maybe.withDefault []
    in
        { data
        | currentAngle = oldAngle
        , currentPosition = oldPosition
        , positionStack = newPositionStack
        }

leftStep : IterationData -> IterationData
leftStep data =
    let
        (Angle angle) = data.angle
    in
        { data
        | currentAngle = data.currentAngle - angle
        }

rightStep : IterationData -> IterationData
rightStep data =
    let
        (Angle angle) = data.angle
    in
        { data
        | currentAngle = data.currentAngle + angle
        }

iteration : Expression -> IterationData -> IterationData
iteration expression acc =
    case expression of
        Constant _ ->
            constantStep acc
        Push ->
            pushStep acc
        Pop ->
            popStep acc
        TurnLeft ->
            leftStep acc
        TurnRight ->
            rightStep acc
        Variable _ ->
            acc

render : IterationData -> List Expression -> List Line
render start expressions =
    .lines <| List.foldl iteration start expressions

expressionsToLines : System -> List Expression -> List Line
expressionsToLines system expressions =
    let
        startPoint = Point 0.0 0.0
        startAngle = 0.0
        start =
            { currentAngle = startAngle
            , angle = system.angle
            , currentPosition = startPoint
            , positionStack = []
            , lines = []
            }
    in
        render start expressions
