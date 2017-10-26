module Main exposing (..)

import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Plot
import Svg exposing (svg)
import Svg.Attributes exposing (height, width)


main =
    Html.program
        { init = ( initialState, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { interest : Float, months : Int, init : Float, contribution : Float }


initialState : Model
initialState =
    { interest = 8, months = 8 * 12, init = 1000, contribution = 100 }



-- UPDATE


type Msg
    = Interest String
    | Principal String
    | Duration String
    | Contribution String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Interest s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | interest = monthlyInterest f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Principal s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | init = f }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Duration s ->
            case String.toInt s of
                Ok i ->
                    ( { model | months = i }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        Contribution s ->
            case String.toFloat s of
                Ok f ->
                    ( { model | contribution = f }, Cmd.none )

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
            , input [ placeholder <| toString initialState.init ++ " EUR", onInput Principal ] []
            ]
        , p
            []
            [ text "Monthly Contribution: "
            , input [ placeholder <| toString initialState.contribution ++ " EUR", onInput Contribution ] []
            ]
        , p
            []
            [ text "Duration (Months): "
            , input [ placeholder <| toString initialState.months, onInput Duration ] []
            ]
        , lineChart <| accumulatedInterest model
        ]


lineChart lst =
    Plot.viewSeries
        [ Plot.line (List.map (\( index, val ) -> Plot.clear (toFloat index) val)) ]
        lst



-- LOGIC


accumulatedInterest : Model -> List ( Int, Float )
accumulatedInterest model =
    List.range 1 model.months
        |> List.foldl
            (\m acc ->
                case acc of
                    ( _, v ) :: _ ->
                        ( m, v * monthlyInterest model.interest + model.contribution ) :: acc

                    [] ->
                        []
            )
            [ ( 0, model.init ) ]
        |> List.reverse


monthlyInterest : Float -> Float
monthlyInterest yearlyPercentage =
    (yearlyPercentage / (12 * 100)) + 1
