module System where

import Debug
import ParserDebug

import Color
import Display
import Expander
import Fitter
import Html
import Input
import Parser
import Renderer
import Types exposing (..)

type alias Model =
    { input : Input.Model
    , lines : Result (List String) (List Line)
    }

startMessage = """
You need to input a system.

Here is an example for one:

Angle = 90.0
Start = F
Rules =
F -> F+F-F-F+F
"""

imageWidth = 1000
imageHeight = 1000

init : Model
init =
    Model Input.init (Err [startMessage])

-- UPDATE

type Action
    = UpdateInput Input.Action

update : Action -> Model -> Model
update action model =
    case action of
        UpdateInput action ->
            let
                -- Get the system and the step the user has specified.
                input = Input.update action model.input
                -- Parse the specified system.
                system = Parser.parse Parser.parser input.input
                a = Debug.watch "System" (Result.map ParserDebug.showSystem system)
                -- Expand the system according to our rules.
                expanded = Result.map (Expander.expandSystem input.step) system
                --b = Debug.watch "Expanded" expanded
                -- Convert the expressions to lines that we can later draw.
                lines = Result.map2 Renderer.expressionsToLines system expanded
                -- Fit the lines into our viewport.
                fittedLines = Result.map (Fitter.fit imageWidth imageHeight) lines
            in
                Model input fittedLines

renderOutput : Model -> Html.Html
renderOutput model =
    case model.lines of
        Ok lines ->
            Display.drawLines imageWidth imageHeight lines
        Err e ->
            Html.pre
                []
                (List.map Html.text e)

renderSteps : Model -> Html.Html
renderSteps model =
    Html.div
        []
        [ Html.text "Steps:"
        , Html.text <| toString model.input.step
        ]

view : Signal.Address Action -> Model -> Html.Html
view address model =
    Html.div
        []
        [ Input.view (Signal.forwardTo address UpdateInput) model.input
        , renderSteps model
        , renderOutput model
        ]
