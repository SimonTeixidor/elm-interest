module Model exposing (Model, initialState)

import Date


type alias Model =
    { interest : Float
    , years : Int
    , initialPrincipal : Float
    , contribution : Float
    , contributionGrowthRate : Float
    , currentDate : Date.Date
    , compoundingPerYear : Float
    , showAdvanced : Bool
    }


initialState : Model
initialState =
    { interest = 7
    , years = 10
    , initialPrincipal = 1000
    , contribution = 100
    , contributionGrowthRate = 0
    , currentDate = Date.fromTime 0
    , compoundingPerYear = 1
    , showAdvanced = False
    }
