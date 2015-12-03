module Input where

import Html exposing (Attribute, div, input, textarea)
import Html.Attributes exposing (style, type', value)
import Html.Events exposing (on, targetValue)
import String

-- MODEL

type alias Model =
    { input : String
    , step : Int
    }

init : Model
init =
    Model "" 0

-- UPDATE

type Action
    = ChangeInput String
    | ChangeStep Int
    | DoNothing

update : Action -> Model -> Model
update message model =
    case message of
        ChangeInput i ->
            { model | input = i }
        ChangeStep n ->
            { model | step = n }
        DoNothing ->
            model

-- VIEW

textareaStyle : Attribute
textareaStyle =
    style
        [ ("height", "200px")
        , ("width", "100%")
        ]

rangeStyle : Attribute
rangeStyle =
    style
        [ ("width", "100%")
        ]

toIntWithDefault : Int -> String -> Int
toIntWithDefault n s =
    case String.toInt s of
        Ok k -> k
        Err _ -> n

toRange : String -> Int
toRange = toIntWithDefault 0

view : Signal.Address Action -> Model -> Html.Html
view address model =
    div []
        [
            textarea
                [ on "input" targetValue (Signal.message address << ChangeInput)
                , textareaStyle
                , value model.input
                ]
                []
        ,
            input
                [ on "input" targetValue (Signal.message address << ChangeStep << toRange)
                , type' "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "20"
                , rangeStyle
                , value <| toString model.step
                ]
                []
        ]
