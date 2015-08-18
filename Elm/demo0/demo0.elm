module Demo0 where

import Graphics.Element exposing (..)
import Text exposing (..)
import Mouse

posToString : (Int, Int) -> String
posToString (x, y) =
    "X: " ++ (toString x) ++ " Y: " ++ (toString y)

view : (Int, Int) -> Element
view pos =
    leftAligned (fromString (posToString pos))

main : Signal Element
main =
    Signal.map view Mouse.position
