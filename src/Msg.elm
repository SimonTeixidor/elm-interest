module Msg exposing (Msg(..))

import Date


type Msg
    = Interest String
    | Principal String
    | Duration String
    | Contribution String
    | NewDate Date.Date
    | ContributionRate String
    | CompoundPerYear String
    | ShowAdvanced Bool
