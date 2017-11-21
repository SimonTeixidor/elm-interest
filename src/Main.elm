module Main exposing (..)

import Date
import Date.Extra.Duration as Duration
import Html exposing (Attribute, Html, div, input, p, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Svg exposing (g, svg)
import Svg.Attributes exposing (class, d, fill, height, stroke, strokeWidth, transform, width)
import Task
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Shape as Shape


main =
    Html.program
        { init = ( initialState, Task.perform NewDate Date.now )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { interest : Float, years : Int, init : Float, contribution : Float, currentDate : Date.Date }


initialState : Model
initialState =
    { interest = 8, years = 10, init = 1000, contribution = 100, currentDate = Date.fromTime 0 }



-- UPDATE


type Msg
    = Interest String
    | Principal String
    | Duration String
    | Contribution String
    | NewDate Date.Date


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
                    ( { model | init = f }, Cmd.none )

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
            [ text "Duration (years): "
            , input [ placeholder <| toString initialState.years, onInput Duration ] []
            ]
        , lineChart model
        ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    100


xScale : List ( Date.Date, Float ) -> ContinuousTimeScale
xScale lst =
    let
        xVals =
            List.map Tuple.first lst

        maxVal =
            Date.fromTime <| Maybe.withDefault 100 <| List.maximum <| List.map Date.toTime xVals

        minVal =
            Date.fromTime <| Maybe.withDefault 0 <| List.minimum <| List.map Date.toTime xVals
    in
    Scale.time ( minVal, maxVal ) ( 0, w - 2 * padding )


yScale : List ( Date.Date, Float ) -> ContinuousScale
yScale lst =
    let
        yVals =
            List.map Tuple.second lst

        maxVal =
            Maybe.withDefault 0 <| List.maximum yVals

        minVal =
            Maybe.withDefault 0 <| List.minimum yVals
    in
    Scale.linear ( minVal / 1.2, maxVal * 1.2 ) ( h - 2 * padding, 0 )


xAxis : List ( Date.Date, Float ) -> Svg.Svg msg
xAxis lst =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = 5 } <| xScale lst


yAxis : List ( Date.Date, Float ) -> Svg.Svg msg
yAxis lst =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 10 } <| yScale lst


lineChart : Model -> Html msg
lineChart model =
    let
        data =
            accumulatedInterest model

        scale ( x, y ) =
            Just ( Scale.convert (xScale data) x, Scale.convert (yScale data) y )

        line =
            d <| Shape.line Shape.linearCurve <| List.map scale <| data
    in
    Svg.svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis data ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis data ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
            [ Svg.path [ line, stroke "red", strokeWidth "3px", fill "none" ] [] ]
        ]



-- LOGIC


accumulatedInterest : Model -> List ( Date.Date, Float )
accumulatedInterest model =
    let
        endDate =
            Duration.add Duration.Month model.years model.currentDate

        dataPoints =
            100
    in
    List.reverse <|
        List.foldl
            (\_ acc ->
                case acc of
                    ( t, v ) :: _ ->
                        let
                            dayDiff =
                                Duration.diffDays nextDay t

                            nextDay =
                                Duration.add Duration.Month 1 t
                        in
                        ( nextDay, model.contribution + v * interestForDays dayDiff model.interest ) :: acc

                    [] ->
                        []
            )
            [ ( model.currentDate, model.init ) ]
        <|
            List.range 1 (model.years * 12)


skipRange : Int -> Int -> Int -> List Int
skipRange begin end step =
    let
        range =
            List.map ((*) step) <| List.range begin (end // step)
    in
    if (end - begin) % step == 0 then
        range
    else
        List.append range [ end ]


interestForDays : Int -> Float -> Float
interestForDays days yearlyPercentage =
    ((yearlyPercentage / 100) + 1) ^ (toFloat days / 365)
