module InitialView exposing (view)

import Html
import Model exposing (initialState)
import Msg exposing (Msg)
import View as V


view : Html.Html Msg
view =
    V.view initialState
