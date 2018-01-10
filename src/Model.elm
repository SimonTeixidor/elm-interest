module Model exposing (CalcParams, Model, fromBase64, initialCalcParams, initialState, toBase64)

import Base64 as B64
import Date
import Json.Decode as JsonD
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as JsonE


type alias CalcParams =
    { id : Int
    , interest : Float
    , years : Int
    , contribution : Float
    , contributionGrowthRate : Float
    , compoundingPerYear : Float
    }


type alias Model =
    { firstParam : CalcParams
    , initialPrincipal : Float
    , parameters : List CalcParams
    , currentDate : Date.Date
    , showAdvanced : Bool
    , uid : Int
    }


initialCalcParams : CalcParams
initialCalcParams =
    { id = 1
    , interest = 7
    , years = 10
    , contribution = 100
    , contributionGrowthRate = 0
    , compoundingPerYear = 1
    }


initialState : Model
initialState =
    { firstParam = initialCalcParams
    , initialPrincipal = 1000
    , parameters = []
    , currentDate = Date.fromTime 0
    , showAdvanced = False
    , uid = 1
    }


toBase64 : Model -> String
toBase64 =
    B64.encode << JsonE.encode 0 << encodeModel


fromBase64 : String -> Result String Model
fromBase64 =
    Result.andThen (JsonD.decodeString decodeModel) << B64.decode


decodeCalcParams : JsonD.Decoder CalcParams
decodeCalcParams =
    decode CalcParams
        |> required "id" JsonD.int
        |> required "interest" JsonD.float
        |> required "years" JsonD.int
        |> required "contribution" JsonD.float
        |> required "contribution_growth_rate" JsonD.float
        |> required "compounding_per_year" JsonD.float


decodeModel : JsonD.Decoder Model
decodeModel =
    decode Model
        |> required "first_param" decodeCalcParams
        |> required "initial_principal" JsonD.float
        |> required "parameters" (JsonD.list decodeCalcParams)
        |> required "current_date" (JsonD.map Date.fromTime JsonD.float)
        |> required "show_advanced" JsonD.bool
        |> required "uid" JsonD.int


encodeCalcParams : CalcParams -> JsonE.Value
encodeCalcParams record =
    JsonE.object
        [ ( "id", JsonE.int record.id )
        , ( "interest", JsonE.float record.interest )
        , ( "years", JsonE.int record.years )
        , ( "contribution", JsonE.float record.contribution )
        , ( "contribution_growth_rate", JsonE.float record.contributionGrowthRate )
        , ( "compounding_per_year", JsonE.float record.compoundingPerYear )
        ]


encodeModel : Model -> JsonE.Value
encodeModel record =
    JsonE.object
        [ ( "first_param", encodeCalcParams record.firstParam )
        , ( "initial_principal", JsonE.float record.initialPrincipal )
        , ( "parameters", JsonE.list <| List.map encodeCalcParams record.parameters )
        , ( "current_date", JsonE.float <| Date.toTime record.currentDate )
        , ( "show_advanced", JsonE.bool record.showAdvanced )
        , ( "uid", JsonE.int record.uid )
        ]
