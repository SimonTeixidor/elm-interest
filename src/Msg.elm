module Msg exposing (Msg(..), ParamUpdate(..))

import Date


type ParamUpdate
    = Interest String
    | Principal String
    | Duration String
    | Contribution String
    | ContributionRate String
    | CompoundPerYear String


type Msg
    = NewParam Int ParamUpdate
    | NewDate Date.Date
    | ShowAdvanced Bool
