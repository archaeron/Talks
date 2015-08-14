import Html exposing (..)
import Mouse

view : (Int, Int) -> Html
view (x, y) =
  dl []
    [ dt [] [ text "X:" ]
    , dd [] [ text <| toString x ]
    , dt [] [ text "Y:" ]
    , dd [] [ text <| toString y ]
    ]

main : Signal Html
main =
  Signal.map view Mouse.position
