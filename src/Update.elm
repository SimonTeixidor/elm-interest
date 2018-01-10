module Update exposing (update)

import Model exposing (CalcParams, Model, initialCalcParams, toBase64)
import Msg exposing (Msg(..), ParamUpdate(..))


updateParamId : Int -> (CalcParams -> CalcParams) -> List CalcParams -> List CalcParams
updateParamId id f =
    List.map
        (\p ->
            if p.id == id then
                f p
            else
                p
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, action ) =
            case msg of
                NewParam id paramUpdate ->
                    if id == model.firstParam.id then
                        ( { model | firstParam = updateCalcParams paramUpdate model.firstParam }, Cmd.none )
                    else
                        ( { model | parameters = updateParamId id (updateCalcParams paramUpdate) model.parameters }, Cmd.none )

                AddParamGroup ->
                    let
                        params =
                            model.parameters

                        newId =
                            model.uid + 1
                    in
                    ( { model | parameters = params ++ [ { initialCalcParams | id = newId } ], uid = newId }
                    , Cmd.none
                    )

                RemoveParamGroup i ->
                    let
                        params =
                            model.parameters
                    in
                    ( { model | parameters = List.filter ((/=) i << .id) params }
                    , Cmd.none
                    )

                Principal s ->
                    ( case String.toFloat s of
                        Ok f ->
                            { model | initialPrincipal = f }

                        Err e ->
                            model
                    , Cmd.none
                    )

                NewDate d ->
                    ( { model | currentDate = d }, Cmd.none )

                ShowAdvanced b ->
                    ( { model | showAdvanced = b }, Cmd.none )
    in
    ( { newModel | shareLink = toBase64 newModel }, action )


updateCalcParams : ParamUpdate -> CalcParams -> CalcParams
updateCalcParams paramUpdate p =
    case paramUpdate of
        Interest s ->
            case String.toFloat s of
                Ok f ->
                    { p | interest = f }

                Err e ->
                    p

        Duration s ->
            case String.toInt s of
                Ok i ->
                    { p | years = i }

                Err e ->
                    p

        Contribution s ->
            case String.toFloat s of
                Ok f ->
                    { p | contribution = f }

                Err e ->
                    p

        ContributionRate s ->
            case String.toFloat s of
                Ok f ->
                    { p | contributionGrowthRate = f }

                Err e ->
                    p

        CompoundPerYear s ->
            case String.toFloat s of
                Ok f ->
                    { p | compoundingPerYear = f }

                Err e ->
                    p
