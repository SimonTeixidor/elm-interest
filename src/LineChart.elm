module LineChart exposing (lineChart)

import Date
import Date.Extra.Core exposing (fromTime, toTime)
import Html exposing (Html)
import Svg exposing (g, svg)
import Svg.Attributes exposing (class, d, fill, height, stroke, strokeWidth, transform, width)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Shape as Shape
import Window


padding : Int
padding =
    60


xScale : List ( Date.Date, Float ) -> Int -> ContinuousTimeScale
xScale lst w =
    let
        xVals =
            List.map Tuple.first lst

        maxVal =
            fromTime <| Maybe.withDefault 100 <| List.maximum <| List.map toTime xVals

        minVal =
            fromTime <| Maybe.withDefault 0 <| List.minimum <| List.map toTime xVals
    in
    Scale.time ( minVal, maxVal ) ( 0, toFloat <| w - 2 * padding )


yScale : List ( Date.Date, Float ) -> Int -> ContinuousScale
yScale lst h =
    let
        yVals =
            List.map Tuple.second lst

        maxVal =
            Maybe.withDefault 0 <| List.maximum yVals

        minVal =
            Maybe.withDefault 0 <| List.minimum yVals
    in
    Scale.linear ( minVal / 1.2, maxVal * 1.2 ) ( toFloat <| h - 2 * padding, 0 )


xAxis : List ( Date.Date, Float ) -> Int -> Svg.Svg msg
xAxis lst w =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, ticks = Just [ fromTime 1 ] } <| xScale lst w


yAxis : List ( Date.Date, Float ) -> Int -> Svg.Svg msg
yAxis lst h =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 10 } <| yScale lst h


lineChart : Window.Size -> List ( Date.Date, Float ) -> Html msg
lineChart size data =
    let
        w =
            size.width - padding

        h =
            min w <| floor <| toFloat size.height * 0.7

        scale ( x, y ) =
            Just ( Scale.convert (xScale data w) x, Scale.convert (yScale data h) y )

        line =
            d <| Shape.line Shape.linearCurve <| List.map scale <| data
    in
    Svg.svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis data size.width ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis data h ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
            [ Svg.path [ line, stroke "red", strokeWidth "3px", fill "none" ] [] ]
        ]
