module View exposing (view)

import AccumulatedInterest exposing (accumulatedInterest)
import FormatNumber
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Attribute, Html, br, button, div, h2, h3, input, label, option, p, select, text)
import Html.Attributes exposing (class, id, maxlength, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import LineChart exposing (lineChart)
import Model exposing (CalcParams, Model, initialCalcParams, initialState)
import Msg exposing (Msg(..), ParamUpdate(..))


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
            ([ h2 [] [ text "Settings" ] ]
                ++ List.map (p [] << List.singleton << text) settingsIntro
            )
        , div []
            ([ h3 []
                [ text "First:"
                ]
             ]
                ++ (List.intersperse (h3 [] [ text "And then:" ]) <|
                        List.map (calcParamsView model.showAdvanced) (model.firstParam :: model.parameters)
                   )
            )
        , div [ class "row" ]
            [ div [ onClick AddParamGroup, class "plus" ] []
            , div []
                [ label [] [ text "Show advanced parameters:" ]
                , input [ type_ "checkbox", onCheck ShowAdvanced ] []
                ]
            ]
        , div [ class "row" ]
            [ h3 [] [ text <| "Final balance: " ++ formattedBalance ]
            ]
        , div [ class "row", id "plot" ]
            [ lineChart <| dataPoints
            ]
        ]


calcParamsView : Bool -> CalcParams -> Html Msg
calcParamsView showAdvanced params =
    div [ class "row" ] <|
        -- Principal is only showed for the first form
        (if params.id == initialCalcParams.id then
            [ div [ class "input-box" ]
                [ label [] [ text "Starting Principal:" ]
                , input [ placeholder <| toString initialState.initialPrincipal ++ " EUR", onInput Principal ] []
                ]
            ]
         else
            []
        )
            -- All forms show the basic parameters
            ++ [ div [ class "input-box" ]
                    [ label [] [ text "Yearly Return:" ]
                    , if showAdvanced then
                        input [ placeholder <| toString initialState.firstParam.interest ++ "%", onInput (NewParam params.id << Interest) ] []
                      else
                        let
                            stockReturn =
                                toString initialState.firstParam.interest
                        in
                        select [ onInput (NewParam params.id << Interest) ]
                            [ option [ value stockReturn ]
                                [ text ("Stocks: " ++ stockReturn ++ "%") ]
                            , option [ value "3.5" ] [ text "Bonds: 3.5%" ]
                            , option [ value "-1" ] [ text "Savings Account: -1%" ]
                            ]
                    ]
               , div [ class "input-box" ]
                    [ label [] [ text "Monthly Contribution:" ]
                    , input [ placeholder <| toString initialState.firstParam.contribution ++ " EUR", onInput (NewParam params.id << Contribution) ] []
                    ]
               , div [ class "input-box" ]
                    [ label [] [ text "Duration (years):" ]
                    , input [ placeholder <| toString initialState.firstParam.years, onInput (NewParam params.id << Duration), maxlength 2 ] []
                    ]
               ]
            -- Show advanced settings, if enabled
            ++ (if showAdvanced then
                    [ div [ class "input-box" ]
                        [ label [] [ text "Contribution Growth:" ]
                        , input [ placeholder <| toString initialState.firstParam.contributionGrowthRate ++ " %", onInput (NewParam params.id << ContributionRate) ] []
                        ]
                    , div [ class "input-box" ]
                        [ label [] [ text "Compound Frequency:" ]
                        , select [ onInput (NewParam 0 << CompoundPerYear) ]
                            [ option [ value "1" ] [ text "Yearly" ]
                            , option [ value "6" ] [ text "Semi Anually" ]
                            , option [ value "12" ] [ text "Monthly" ]
                            , option [ value "365" ] [ text "Daily" ]
                            ]
                        ]
                    ]
                else
                    []
               )


settingsIntro : List String
settingsIntro =
    [ "These parameters determine the final balance. By adding multiple configurations with the plus button, it is possible to change the parameters over time (e.g, save 100 EUR per month for 10 years, then 150 for 10 years)."
    ]
