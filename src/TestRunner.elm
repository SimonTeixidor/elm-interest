module TestRunner exposing (..)

import AccumulatedInterest
import Date
import Expect
import Fuzz exposing (..)
import Model
import Test exposing (..)
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    [ testWithoutContribution, testNoNaN ] |> concat |> Test.Runner.Html.run


allZeroesModel : Model.Model
allZeroesModel =
    { interest = 0
    , years = 0
    , initialPrincipal = 0
    , contribution = 0
    , contributionGrowthRate = 0
    , currentDate = Date.fromTime 0
    , compoundingPerYear = 1
    }


maxResult : List ( Date.Date, Float ) -> Float
maxResult =
    Maybe.withDefault 0 << List.maximum << List.map Tuple.second


closeEnough a b =
    Expect.lessThan 0.0001 <| abs (a - b)


iteratingInterest : Int -> Float -> Float -> Float
iteratingInterest years principal discount =
    List.foldr (always ((*) discount)) principal <| List.range 1 years


iteratingContributionInterest : Int -> Float -> Float -> Float
iteratingContributionInterest years contribution discount =
    List.foldr (\x y -> contribution + discount * y) 0 <| List.map toFloat <| List.range 1 years


testWithoutContribution : Test
testWithoutContribution =
    describe "Basic assumptions"
        [ test "10 years of interest with no contribution." <|
            \() ->
                let
                    model =
                        { allZeroesModel | interest = 5, years = 10, initialPrincipal = 100 }
                in
                closeEnough
                    (AccumulatedInterest.accumulatedInterest model |> maxResult)
                    (iteratingInterest 10 100 1.05)
        , test "10 years of only contributions" <|
            \() ->
                closeEnough
                    (AccumulatedInterest.accumulatedInterest { allZeroesModel | years = 10, contribution = 10 } |> maxResult)
                    (10 * 10 * 12)
        , test "10 years with contributions and interest" <|
            \() ->
                closeEnough
                    (AccumulatedInterest.accumulatedInterest
                        { allZeroesModel
                            | years = 10
                            , contribution = 10
                            , interest = 5
                        }
                        |> maxResult
                    )
                    (iteratingContributionInterest 10 120 1.05)
        , test "10 years with contributions, initial, and interest" <|
            \() ->
                closeEnough
                    (AccumulatedInterest.accumulatedInterest
                        { allZeroesModel
                            | years = 10
                            , contribution = 10
                            , interest = 5
                            , initialPrincipal = 10000
                        }
                        |> maxResult
                    )
                    (iteratingContributionInterest 10 120 1.05 + iteratingInterest 10 10000 1.05)
        ]


testNoNaN : Test
testNoNaN =
    fuzz5
        (floatRange 0 100)
        (floatRange 0 1000000000000)
        (intRange 0 99)
        (floatRange 0 1000000)
        (floatRange 0 99)
        "Find NaN when using somewhat reasonable inputs."
    <|
        \interest principal years contribution contributionGrowth ->
            let
                model =
                    { interest = interest
                    , years = years
                    , initialPrincipal = principal
                    , contribution = contribution
                    , contributionGrowthRate = contributionGrowth
                    , currentDate = Date.fromTime 0
                    , compoundingPerYear = 365
                    }
            in
            Expect.false "Expected the calculation to not yield NaN."
                (AccumulatedInterest.accumulatedInterest model
                    |> maxResult
                    |> isNaN
                )
