module Model exposing (CalcParams, Model, initialState)

import Date


type alias CalcParams =
    { id : Int
    , interest : Float
    , years : Int
    , initialPrincipal : Float
    , contribution : Float
    , contributionGrowthRate : Float
    , compoundingPerYear : Float
    }


type alias Model =
    { firstParam : CalcParams
    , parameters : List CalcParams
    , currentDate : Date.Date
    , showAdvanced : Bool
    , uid : Int
    }


initialState : Model
initialState =
    { firstParam =
        { id = 1
        , interest = 7
        , years = 10
        , initialPrincipal = 1000
        , contribution = 100
        , contributionGrowthRate = 0
        , compoundingPerYear = 1
        }
    , parameters = []
    , currentDate = Date.fromTime 0
    , showAdvanced = False
    , uid = 1
    }
