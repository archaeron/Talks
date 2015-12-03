module Display (drawLines) where

{-| Convert lines into an SVG image.

# Converters
@docs drawLines

-}

import Types exposing (..)
import Svg
import Svg.Attributes

{-| Convert a line into an SVG-line. -}
drawLine : Line -> Svg.Svg
drawLine line =
    let
        (Point x1 y1) = line.start
        (Point x2 y2) = line.end
    in
        Svg.line
            [ Svg.Attributes.x1 (toString x1)
            , Svg.Attributes.y1 (toString y1)
            , Svg.Attributes.x2 (toString x2)
            , Svg.Attributes.y2 (toString y2)
            ]
            []

{-| Draw a list of lines into a properly sized SVG. -}
drawLines : Int -> Int -> List Line -> Svg.Svg
drawLines width height lines =
    Svg.svg
        [ Svg.Attributes.width (toString width)
        , Svg.Attributes.height (toString height)
        , Svg.Attributes.viewBox ("0 0 " ++ (toString width) ++ " " ++ (toString height))
        ]
        [
            Svg.g
                [ Svg.Attributes.stroke "#232323"
                , Svg.Attributes.strokeWidth "2"
                ]
                (List.map drawLine lines)
        ]
