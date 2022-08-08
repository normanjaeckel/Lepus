module HopcraftKarpKarzanovTest exposing (suite)

import Dict
import Expect
import HopcroftKarpKarzanov
import Set
import Test exposing (..)


suite : Test
suite =
    let
        graph1 : HopcroftKarpKarzanov.Graph
        graph1 =
            Dict.fromList
                [ ( "P1", Set.fromList [ "A", "B" ] )
                , ( "P2", Set.fromList [ "B", "C" ] )
                , ( "P3", Set.fromList [ "A", "D" ] )
                , ( "P4", Set.fromList [ "D" ] )
                ]
    in
    describe "Main functions"
        [ test "Test run for graph 1" <|
            \_ ->
                HopcroftKarpKarzanov.run graph1
                    |> Expect.equal (Dict.fromList [ ( "-", "-" ) ])
        ]
