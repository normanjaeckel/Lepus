module AlgoTest exposing (suite)

import Algo
import Dict
import Expect
import Test exposing (..)


suite : Test
suite =
    let
        graph1 : Algo.Graph
        graph1 =
            Dict.fromList
                [ ( "P1", [ "A", "B" ] )
                , ( "P2", [ "B", "C" ] )
                , ( "P3", [ "A", "D" ] )
                , ( "P4", [ "D" ] )
                ]
    in
    describe "Main functions"
        [ test "Test run for graph 1" <|
            \_ ->
                Algo.run graph1
                    |> Expect.equal (Dict.fromList [ ( "P1", "B" ), ( "P2", "C" ), ( "P3", "A" ), ( "P4", "D" ) ])
        ]
