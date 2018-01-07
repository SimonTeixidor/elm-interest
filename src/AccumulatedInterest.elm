module AccumulatedInterest exposing (accumulatedInterest)

import Date
import Date.Extra.Duration as Duration
import Model exposing (CalcParams, Model)


valueAtYear : CalcParams -> Float -> Float -> Float
valueAtYear m principal years =
    let
        rate =
            m.interest / m.compoundingPerYear / 100

        contributionRate =
            let
                wiggle =
                    if m.interest == m.contributionGrowthRate then
                        0.00000001
                    else
                        0
            in
            (m.contributionGrowthRate + wiggle) / m.compoundingPerYear / 100

        compounds =
            m.compoundingPerYear * years

        compoundInterest =
            principal * (1 + rate) ^ compounds

        contribution =
            m.contribution * 12 / m.compoundingPerYear

        futureValueOfSeries =
            contribution * (((1 + rate) ^ compounds - (1 + contributionRate) ^ compounds) / (rate - contributionRate))
    in
    futureValueOfSeries + compoundInterest


last : List a -> Maybe a
last =
    List.head << List.reverse


accumulatedInterest : Model -> List ( Date.Date, Float )
accumulatedInterest model =
    let
        resolution =
            100

        years params =
            List.range 0 100
                |> List.map toFloat
                |> List.map ((*) (toFloat params.years / resolution))

        dateAfterYears start y =
            Duration.add Duration.Day (floor (y * 365)) start

        calc params principal =
            List.map2 (,)
                (years params)
                (List.map (valueAtYear params principal) <| years params)
    in
    List.map (\( year, amount ) -> ( dateAfterYears model.currentDate year, amount )) <|
        List.foldr
            (\params lst ->
                let
                    lastAmount =
                        Maybe.withDefault 0 <| last <| List.map Tuple.second lst

                    lastYear =
                        Maybe.withDefault 0 <| last <| List.map Tuple.first lst

                    newVals =
                        calc params lastAmount
                in
                lst ++ List.map (\( year, amount ) -> ( year + lastYear, amount )) newVals
            )
            (calc model.firstParam model.initialPrincipal)
            model.parameters
