module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))


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

        ShowAdvanced b ->
            ( { model | showAdvanced = b }, Cmd.none )
