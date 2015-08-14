import Graphics.Element exposing (..)
import Text exposing (..)
import Mouse

view : (Int, Int) -> Element
view (x, y) =
    flow down
        [ flow right
            [ leftAligned (fromString "X: ")
            , show x
            ]
        , flow right
            [ leftAligned (fromString "Y: ")
            , show y
            ]
        ]

main : Signal Element
main =
  Signal.map view Mouse.position
