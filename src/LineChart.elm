module LineChart exposing (lineChart)

import Date exposing (toTime)
import Html
import Plot exposing (..)


lineChart : List ( Date.Date, Float ) -> Html.Html msg
lineChart data =
    let
        default =
            defaultSeriesPlotCustomizations

        margin =
            default.margin

        newMargin =
            { margin | left = 100, right = 100 }

        newCustom =
            { default | margin = newMargin }
    in
    viewSeriesCustom newCustom
        [ line (List.map (\( x, y ) -> clear (toTime x) y)) ]
        data
