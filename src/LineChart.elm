module LineChart exposing (lineChart)

import Date exposing (Month(..))
import Date.Extra.Create exposing (dateFromFields)
import Html
import Plot exposing (..)


lineChart : List ( Date.Date, Float ) -> Html.Html msg
lineChart data =
    let
        margin =
            defaultSeriesPlotCustomizations.margin

        newMargin =
            { margin | left = 100, right = 100 }

        newCustom =
            { defaultSeriesPlotCustomizations | margin = newMargin, horizontalAxis = horizontalAxis }
    in
    viewSeriesCustom newCustom
        [ line (List.map (\( x, y ) -> clear (yearFraction x) y)) ]
        data


yearFraction : Date.Date -> Float
yearFraction d =
    let
        firstOfYear y =
            dateFromFields y Jan 0 0 0 0 0

        prev =
            Date.toTime <| firstOfYear <| Date.year d

        next =
            Date.toTime <| firstOfYear <| 1 + Date.year d

        time =
            Date.toTime d

        yearFraction =
            (time - prev) / (next - prev)
    in
    yearFraction + (toFloat <| Date.year d)


yearPositions : AxisSummary -> List Float
yearPositions summary =
    let
        offset =
            (toFloat <| ceiling summary.min) - summary.min

        diff =
            summary.max - summary.min

        spacing =
            if diff < 10 then
                1
            else if diff < 20 then
                2
            else if diff < 50 then
                5
            else
                10
    in
    interval offset spacing summary


horizontalAxis =
    customAxis <|
        \summary ->
            let
                axisLine =
                    simpleLine summary
            in
            { position = closestToZero
            , axisLine = Just { axisLine | end = Maybe.withDefault 0 <| List.maximum <| yearPositions summary }
            , ticks = List.map simpleTick (yearPositions summary)
            , labels = List.map simpleLabel (yearPositions summary)
            , flipAnchor = False
            }
