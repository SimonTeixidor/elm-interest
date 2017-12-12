module View exposing (view)

import AccumulatedInterest exposing (accumulatedInterest)
import FormatNumber
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Attribute, Html, br, div, input, label, option, p, select, text)
import Html.Attributes exposing (class, maxlength, placeholder, value)
import Html.Events exposing (onInput)
import LineChart exposing (lineChart)
import Model exposing (Model, initialState)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        dataPoints =
            accumulatedInterest model

        finalNumber =
            Maybe.withDefault 0 <| Maybe.map Tuple.second <| List.head <| List.reverse dataPoints

        formattedBalance =
            FormatNumber.format { frenchLocale | decimals = 2 } finalNumber
    in
    div []
        [ div [ class "row" ]
            [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
            , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "sakura.css" ] []
            , div [ class "input-box" ]
                [ label [] [ text "Yearly Interest:" ]
                , input [ placeholder <| toString initialState.interest ++ "%", onInput Interest ] []
                ]
            , div [ class "input-box" ]
                [ label [] [ text "Starting Principal:" ]
                , input [ placeholder <| toString initialState.initialPrincipal ++ " EUR", onInput Principal ] []
                ]
            , div [ class "input-box" ]
                [ label [] [ text "Monthly Contribution:" ]
                , input [ placeholder <| toString initialState.contribution ++ " EUR", onInput Contribution ] []
                ]
            , div [ class "input-box" ]
                [ label [] [ text "Contribution Growth:" ]
                , input [ placeholder <| toString initialState.contributionGrowthRate ++ " %", onInput ContributionRate ] []
                ]
            , div [ class "input-box" ]
                [ label [] [ text "Duration (years):" ]
                , input [ placeholder <| toString initialState.years, onInput Duration, maxlength 2 ] []
                ]
            , div [ class "input-box" ]
                [ label [] [ text "Compounding Frequency:" ]
                , select [ onInput CompoundPerYear ]
                    [ option [ value "1" ] [ text "Yearly" ]
                    , option [ value "6" ] [ text "Semi Anually" ]
                    , option [ value "12" ] [ text "Monthly" ]
                    , option [ value "365" ] [ text "Daily" ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ p [] [ text <| "Final balance: " ++ formattedBalance ]
            ]
        , div [ class "row" ]
            [ lineChart <| accumulatedInterest model
            ]
        ]
