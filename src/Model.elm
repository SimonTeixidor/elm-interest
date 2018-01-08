module Model exposing (CalcParams, Model, initialCalcParams, initialState)

import Date


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
