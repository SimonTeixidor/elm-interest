module Update exposing (update)

import AccumulatedInterest exposing (accumulatedInterest)
import Model exposing (CalcParams, Model, initialCalcParams, toBase64)
import Msg exposing (Msg(..), ParamUpdate(..))
import Plot exposing (lineChart)


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
        newModel =
            case msg of
                NewParam id paramUpdate ->
                    if id == model.firstParam.id then
                        { model | firstParam = updateCalcParams paramUpdate model.firstParam }
                    else
                        { model | parameters = updateParamId id (updateCalcParams paramUpdate) model.parameters }

                AddParamGroup ->
                    let
                        params =
                            model.parameters

                        newId =
                            model.uid + 1
                    in
                    { model | parameters = params ++ [ { initialCalcParams | id = newId } ], uid = newId }

                RemoveParamGroup i ->
                    let
                        params =
                            model.parameters
                    in
                    { model | parameters = List.filter ((/=) i << .id) params }

                Principal s ->
                    case String.toFloat s of
                        Ok f ->
                            { model | initialPrincipal = f }

                        Err e ->
                            model

                NewDate d ->
                    { model | currentDate = d }

                ShowAdvanced b ->
                    { model | showAdvanced = b }
    in
    ( { newModel | shareLink = toBase64 newModel }, lineChart newModel )


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
