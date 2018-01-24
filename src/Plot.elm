port module Plot exposing (lineChart)

import AccumulatedInterest exposing (accumulatedInterest)
import Date
import Model


lineChart : Model.Model -> Cmd msg
lineChart =
    chartist << List.map (Tuple.mapFirst Date.toTime) << accumulatedInterest


port chartist : List ( Float, Float ) -> Cmd msg
