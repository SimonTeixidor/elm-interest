module View exposing (view)

import AccumulatedInterest exposing (accumulatedInterest)
import FormatNumber
import FormatNumber.Locales exposing (frenchLocale)
import Html exposing (Attribute, Html, br, button, div, h2, h3, input, label, option, p, select, span, text)
import Html.Attributes exposing (class, id, maxlength, placeholder, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Keyed as Keyed
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
            (h3 [ onClick AddParamGroup ] [ text "+ First:" ]
                :: List.concatMap
                    (\p ->
                        (if p.id == initialCalcParams.id then
                            []
                         else
                            [ h3 [ onClick <| RemoveParamGroup p.id ] [ text "- And then:" ] ]
                        )
                            ++ [ calcParamsView model.showAdvanced p ]
                    )
                    (model.firstParam :: model.parameters)
            )
        , div [ class "row" ]
            [ div []
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
    let
        form =
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
                        , input [ placeholder <| toString params.interest ++ "%", onInput (NewParam params.id << Interest) ] []
                        ]
                   , div [ class "input-box" ]
                        [ label [] [ text "Monthly Contribution:" ]
                        , input [ placeholder <| toString params.contribution ++ " EUR", onInput (NewParam params.id << Contribution) ] []
                        ]
                   , div [ class "input-box" ]
                        [ label [] [ text "Duration (years):" ]
                        , input [ placeholder <| toString params.years, onInput (NewParam params.id << Duration), maxlength 2 ] []
                        ]
                   ]
                -- Show advanced settings, if enabled
                ++ (if showAdvanced then
                        [ div [ class "input-box" ]
                            [ label [] [ text "Contribution Growth:" ]
                            , input [ placeholder <| toString params.contributionGrowthRate ++ " %", onInput (NewParam params.id << ContributionRate) ] []
                            ]
                        , div [ class "input-box" ]
                            [ label [] [ text "Compound Frequency:" ]
                            , let
                                attrs i =
                                    [ selected (params.compoundingPerYear == i), value <| toString i ]
                              in
                              select [ onInput (NewParam params.id << CompoundPerYear) ]
                                [ option (attrs 1) [ text "Yearly" ]
                                , option (attrs 6) [ text "Semi Anually" ]
                                , option (attrs 12) [ text "Monthly" ]
                                , option (attrs 365) [ text "Daily" ]
                                ]
                            ]
                        ]
                    else
                        []
                   )
    in
    Keyed.node "div" [ class "row" ] <|
        List.map2 (\i node -> ( toString i ++ "-" ++ toString params.id, node ))
            (List.range 0 <| List.length form)
            form


settingsIntro : List String
settingsIntro =
    [ "These parameters determine the final balance. By adding multiple configurations with the plus button, it is possible to change the parameters over time (e.g, save 100 EUR per month for 10 years, then 150 for 10 years)."
    ]
