module Msg exposing (Msg(..), ParamUpdate(..))

import Date


type ParamUpdate
    = Interest String
    | Duration String
    | Contribution String
    | ContributionRate String
    | CompoundPerYear String


type Msg
    = NewParam Int ParamUpdate
    | Principal String
    | NewDate Date.Date
    | ShowAdvanced Bool
    | AddParamGroup
    | RemoveParamGroup Int
