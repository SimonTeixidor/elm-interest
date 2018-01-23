module Plot exposing (lineChart)

import Color
import Date
import Html exposing (Html)
import Internal.Axis.Intersection as Intersection
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Axis.Values as Values
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Msg exposing (Msg)


lineColour =
    Color.rgb 0 0 0


timeAxis pixels title variable =
    Axis.custom
        { title = Title.atDataMax ( 0, 0 ) title
        , variable = Just << variable
        , pixels = pixels
        , range = Range.padded 20 20
        , axisLine = AxisLine.rangeFrame
        , ticks =
            Ticks.custom <|
                \data range ->
                    let
                        smallest =
                            Coordinate.Range (min data.min range.min) (max data.max range.max)

                        rangeLong =
                            range.max - range.min

                        rangeSmall =
                            smallest.max - smallest.min

                        diff =
                            1 - (rangeLong - rangeSmall) / rangeLong

                        amount =
                            round <| diff * toFloat pixels / 150
                    in
                    List.map Tick.time <| Values.time amount smallest
        }


defaultConfig : LineChart.Config ( Date.Date, Float ) Msg
defaultConfig =
    { y = Axis.default 800 "" Tuple.second
    , x = timeAxis 800 "" (Date.toTime << Tuple.first)
    , container = Container.default "chart"
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.none
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }


lineChart : List ( Date.Date, Float ) -> Html Msg
lineChart =
    LineChart.viewCustom defaultConfig << List.singleton << LineChart.line lineColour Dots.none ""
