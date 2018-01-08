module Update exposing (update)

import Model exposing (CalcParams, Model, initialCalcParams)
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
    case msg of
        NewParam id paramUpdate ->
            let
                updateFunc =
                    \p ->
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
            in
            if id == model.firstParam.id then
                let
                    firstParam =
                        updateFunc model.firstParam
                in
                ( { model | firstParam = firstParam }, Cmd.none )
            else
                let
                    params =
                        updateParamId id updateFunc model.parameters
                in
                ( { model | parameters = params }, Cmd.none )

        AddParamGroup ->
            let
                params =
                    model.parameters

                newId =
                    model.uid + 1
            in
            ( { model | parameters = { initialCalcParams | id = newId } :: params, uid = newId }
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
