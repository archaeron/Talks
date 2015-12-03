module Fitter (fit) where

import Debug
import Types exposing (..)

{-| A very high number that we use as a value that is greater than every
other value.
-}
maxFloat : Float
maxFloat = 9007199254740991.0

{-| A very small number that we use as a value that is smaller than every
other value.
-}
minFloat : Float
minFloat = -9007199254740991.0

{-| Move and scale all lines so that they fit in a given rectangle. -}
fit : Int -> Int -> List Line -> List Line
fit width height lines =
    let
        outermost = outermostPoints lines
        scaling = scalingFactor width height outermost
        moving = movingFactor outermost
    in
        lines
        |> moveAndScale scaling moving

type alias Outermost =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }

type Scaling = Scaling Float Float

type Moving = Moving Float Float

{-| We use extreme values to make sure that the value will be
replaced at the first comparison -}
extremeOutermost : Outermost
extremeOutermost =
    { top = maxFloat
    , right = minFloat
    , bottom = minFloat
    , left = maxFloat
    }

{-| Check if an x-Coordinate is further on the left or on the right than the
current values. -}
leftOrRight : Float -> Outermost -> Outermost
leftOrRight pX outermost =
    if pX < outermost.left then
        { outermost
        | left = pX
        }
    else if pX > outermost.right then
        { outermost
        | right = pX
        }
    else
        outermost

{-| Check if an y-Coordinate is further up or down than the current value. -}
topOrBottom : Float -> Outermost -> Outermost
topOrBottom pY outermost =
    if pY < outermost.top then
        { outermost
        | top = pY
        }
    else if pY > outermost.bottom then
        { outermost
        | bottom = pY
        }
    else
        outermost

{-| Check if the point further out than the old outermost point. -}
comparePoint : Point -> Outermost -> Outermost
comparePoint (Point x y) outermost =
    outermost
    |> leftOrRight x
    |> topOrBottom y

{-| Check if both points in a line are further out than
the old outermost point. -}
compareLine : Line -> Outermost -> Outermost
compareLine line outermost =
    outermost
    |> comparePoint line.start
    |> comparePoint line.end

{-| Find the x- and y-Coordinates that are furthest out in a list of lines. -}
outermostPoints : List Line -> Outermost
outermostPoints =
    List.foldl compareLine extremeOutermost

{-| Calculate how much every point must be scaled to fit in a rectangle with
the help of the outermost points. -}
scalingFactor : Int -> Int -> Outermost -> Scaling
scalingFactor maxWidth maxHeight outermost =
    let
        actualWidth = outermost.right - outermost.left
        actualHeight = outermost.bottom - outermost.top
        xScaling =
            if actualWidth /= 0 then (toFloat maxWidth) / actualWidth else 1
        yScaling =
            if actualHeight /= 0 then (toFloat maxHeight) / actualHeight else 1
    in
        Scaling  xScaling yScaling

{-| Find out how much all lines have to be moved, so that they fit into
into the viewport. -}
movingFactor : Outermost -> Moving
movingFactor outermost =
    Moving (-outermost.left) (-outermost.top)

{-| Move and scale a point according to moving and scaling factors. -}
moveAndScalePoint : Scaling -> Moving -> Point -> Point
moveAndScalePoint (Scaling sx sy) (Moving mx my) (Point x y) =
    Point ((x + mx) * sx) ((y + my) * sy)

{-| Move and scale a line according to moving and scaling factors. -}
moveAndScaleLine : Scaling -> Moving -> Line -> Line
moveAndScaleLine scaling moving line =
    { line
    | start = moveAndScalePoint scaling moving line.start
    , end = moveAndScalePoint scaling moving line.end
    }

{-| Move and scale a list of lines according to moving and scaling factors. -}
moveAndScale : Scaling -> Moving -> List Line -> List Line
moveAndScale scaling moving lines =
    List.map (moveAndScaleLine scaling moving) lines
