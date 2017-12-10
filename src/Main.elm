module Main exposing (..)

import Date
import Html
import Model exposing (Model, initialState)
import Task
import Update exposing (Msg(..), update)
import View exposing (view)
import Window


main =
    Html.program
        { init =
            ( initialState
            , Cmd.batch
                [ Task.perform NewDate Date.now
                , Task.perform NewWindowSize Window.size
                ]
            )
        , view = view
        , update = update
        , subscriptions = \_ -> Window.resizes NewWindowSize
        }
