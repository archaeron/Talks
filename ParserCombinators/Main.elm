module Main where

import Html
import StartApp.Simple
import Task

import System exposing (init, update, view)

-- MODEL

main : Signal Html.Html
main =
    StartApp.Simple.start
        { model = init
        , update = update
        , view = view
        }
