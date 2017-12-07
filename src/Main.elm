module Main exposing (..)

import Date
import Date.Extra.Duration as Duration
import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (maxlength, placeholder)
import Html.Events exposing (onInput)
import LineChart exposing (lineChart)
import Task


main =
    Html.program
        { init = ( initialState, Task.perform NewDate Date.now )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { interest : Float
    , years : Int
    , initialPrincipal : Float
    , contribution : Float
    , contributionGrowthRate : Float
    , currentDate : Date.Date
    , compoundingPerYear : Float
    }


initialState : Model
initialState =
    { interest = 8
    , years = 10
    , initialPrincipal = 1000
    , contribution = 100
    , contributionGrowthRate = 3
    , currentDate = Date.fromTime 0
    , compoundingPerYear = 1
    }



-- UPDATE


type Msg
    = Interest String
    | Principal String
    | Duration String
    | Contribution String
    | NewDate Date.Date
    | ContributionRate String
    | CompoundPerYear String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Interest s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | interest = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Principal s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | initialPrincipal = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Duration s ->
            case String.toInt s of
                Ok i ->
                    ( { model | years = i }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Contribution s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | contribution = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        NewDate d ->
            ( { model | currentDate = d }, Cmd.none )

        ContributionRate s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | contributionGrowthRate = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        CompoundPerYear s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | compoundingPerYear = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ p
            []
            [ text "Yearly Interest: "
            , input [ placeholder <| toString initialState.interest ++ "%", onInput Interest ] []
            ]
        , p
            []
            [ text "Starting Principal: "
            , input [ placeholder <| toString initialState.initialPrincipal ++ " EUR", onInput Principal ] []
            ]
        , p
            []
            [ text "Monthly Contribution: "
            , input [ placeholder <| toString initialState.contribution ++ " EUR", onInput Contribution ] []
            ]
        , p
            []
            [ text "Contribution Growth: "
            , input [ placeholder <| toString initialState.contributionGrowthRate ++ " %", onInput ContributionRate ] []
            ]
        , p
            []
            [ text "Duration (years): "
            , input [ placeholder <| toString initialState.years, onInput Duration, maxlength 3 ] []
            ]
        , p
            []
            [ text "Compound interest "
            , input [ placeholder <| toString initialState.compoundingPerYear, onInput CompoundPerYear, maxlength 2 ] []
            , text " times per year."
            ]
        , p [] [ text ("Final balance: " ++ toString (List.maximum <| List.map Tuple.second <| accumulatedInterest model)) ]
        , lineChart <| accumulatedInterest model
        ]



-- LOGIC


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

        futureValueOfSeries =
            m.contribution * (((1 + rate) ^ compounds - (1 + contributionRate) ^ compounds) / (rate - contributionRate))
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
