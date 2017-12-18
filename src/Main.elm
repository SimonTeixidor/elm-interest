module Main exposing (..)

import Date
import Html
import Model exposing (Model, initialState)
import Task
import Update exposing (Msg(..), update)
import View exposing (view)


main =
    Html.program
        { init =
            ( initialState
            , Task.perform NewDate Date.now
            )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
