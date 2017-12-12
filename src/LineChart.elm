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


spacedPositions : AxisSummary -> (Int -> Int) -> List Float
spacedPositions summary spacingF =
    let
        start =
            ceiling <| summary.min

        stop =
            ceiling <| summary.max

        diff =
            stop - start

        spacing =
            spacingF diff
    in
    List.range 0 ((stop - start) // spacing)
        |> List.map ((*) spacing)
        |> List.map ((+) start)
        |> List.map toFloat


yearPositions : AxisSummary -> List Float
yearPositions summary =
    spacedPositions summary <|
        \diff ->
            if diff < 10 then
                1
            else if diff < 20 then
                2
            else if diff < 50 then
                5
            else
                10


horizontalAxis =
    customAxis <|
        \summary ->
            let
                axisLine =
                    simpleLine summary
            in
            { position = closestToZero
            , axisLine = Just { axisLine | end = toFloat <| ceiling <| summary.max }
            , ticks = List.map simpleTick (yearPositions summary)
            , labels = List.map simpleLabel (yearPositions summary)
            , flipAnchor = False
            }


amountPositions : AxisSummary -> List Float
amountPositions summary =
    spacedPositions summary <|
        \diff ->
            let
                zeroes =
                    floor <| logBase 10 <| toFloat diff

                even =
                    10 ^ (zeroes - 1)
            in
            if diff // even < 2 then
                even // 5
            else if diff // even < 5 then
                even // 2
            else
                even


verticalAxis =
    customAxis <|
        \summary ->
            let
                axisLine =
                    simpleLine summary
            in
            { position = closestToZero
            , axisLine = Just { axisLine | end = toFloat <| ceiling <| summary.max }
            , ticks = List.map simpleTick (amountPositions summary)
            , labels = List.map simpleLabel (amountPositions summary)
            , flipAnchor = False
            }
