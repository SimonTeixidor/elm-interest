module AccumulatedInterest exposing (accumulatedInterest)

import Date
import Date.Extra.Duration as Duration
import Model exposing (Model)


valueAtYear : Model -> Float -> Float
valueAtYear m years =
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
            m.initialPrincipal * (1 + rate) ^ compounds

        contribution =
            m.contribution * 12 / m.compoundingPerYear

        futureValueOfSeries =
            contribution * (((1 + rate) ^ compounds - (1 + contributionRate) ^ compounds) / (rate - contributionRate))
    in
    futureValueOfSeries + compoundInterest


accumulatedInterest : Model -> List ( Date.Date, Float )
accumulatedInterest model =
    let
        resolution =
            100

        years =
            List.range 0 100
                |> List.map toFloat
                |> List.map ((*) (toFloat model.years / resolution))

        dateAfterYears y =
            Duration.add Duration.Day (floor (y * 365)) model.currentDate

        interest =
            model.interest / 100
    in
    List.map2 (,)
        (List.map dateAfterYears years)
        (List.map (valueAtYear model) years)
