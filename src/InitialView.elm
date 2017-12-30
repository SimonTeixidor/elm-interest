module InitialView exposing (view)

import Html
import Model exposing (initialState)
import Update exposing (Msg)
import View as V


view : Html.Html Msg
view =
    V.view initialState
