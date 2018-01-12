module Main exposing (..)

import Date
import Html
import Model exposing (Model, init)
import Msg exposing (Msg(..))
import Task
import Update exposing (update)
import View exposing (view)


main =
    Html.programWithFlags
        { init =
            \url ->
                ( init url
                , Task.perform NewDate Date.now
                )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
