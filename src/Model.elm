module Model exposing (Model, initialState)

import Date
import Window


type alias Model =
    { interest : Float
    , years : Int
    , initialPrincipal : Float
    , contribution : Float
    , contributionGrowthRate : Float
    , currentDate : Date.Date
    , compoundingPerYear : Float
    , windowSize : Window.Size
    }


initialState : Model
initialState =
    { interest = 8
    , years = 10
    , initialPrincipal = 1000
    , contribution = 100
    , contributionGrowthRate = 3
    , currentDate = Date.fromTime 0
    , compoundingPerYear = 1
    , windowSize = Window.Size 800 640
    }
