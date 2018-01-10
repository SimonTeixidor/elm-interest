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
    , shareLink : String
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
    , shareLink = ""
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
        |> required "i" JsonD.int
        |> required "a" JsonD.float
        |> required "y" JsonD.int
        |> required "b" JsonD.float
        |> required "c" JsonD.float
        |> required "d" JsonD.float


decodeModel : JsonD.Decoder Model
decodeModel =
    decode Model
        |> required "f" decodeCalcParams
        |> required "i" JsonD.float
        |> required "p" (JsonD.list decodeCalcParams)
        |> hardcoded (Date.fromTime 0)
        |> required "s" JsonD.bool
        |> required "u" JsonD.int
        |> hardcoded ""


encodeCalcParams : CalcParams -> JsonE.Value
encodeCalcParams record =
    JsonE.object
        [ ( "i", JsonE.int record.id )
        , ( "a", JsonE.float record.interest )
        , ( "y", JsonE.int record.years )
        , ( "b", JsonE.float record.contribution )
        , ( "c", JsonE.float record.contributionGrowthRate )
        , ( "d", JsonE.float record.compoundingPerYear )
        ]


encodeModel : Model -> JsonE.Value
encodeModel record =
    JsonE.object
        [ ( "f", encodeCalcParams record.firstParam )
        , ( "i", JsonE.float record.initialPrincipal )
        , ( "p", JsonE.list <| List.map encodeCalcParams record.parameters )
        , ( "s", JsonE.bool record.showAdvanced )
        , ( "u", JsonE.int record.uid )
        ]
