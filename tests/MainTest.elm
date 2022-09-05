module MainTest exposing (suite)

import Algo
import Dict
import Event
import Expect
import Main
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        e1 =
            Event.Obj "Töpfern" 2

        e2 =
            Event.Obj "Kochen" 2

        e3 =
            Event.Obj "Theater" 2

        e4 =
            Event.Obj "Outdoor" 2

        e5 =
            Event.Obj "Song des Tages" 2

        p1 =
            Pupil.Obj "Max" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e1 Pupil.Green ]

        p2 =
            Pupil.Obj "Moritz" "1a" [ Pupil.Choice e2 Pupil.Green ]

        p3 =
            Pupil.Obj "Lisa" "1a" [ Pupil.Choice e1 Pupil.Green, Pupil.Choice e2 Pupil.Green ]

        p4 =
            Pupil.Obj "Kim" "1a" [ Pupil.Choice e1 Pupil.Green ]

        p5 =
            Pupil.Obj "Anna" "1a" [ Pupil.Choice e4 Pupil.Green ]

        p6 =
            Pupil.Obj "Maria" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p7 =
            Pupil.Obj "Hans" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p8 =
            Pupil.Obj "Ali" "1a" [ Pupil.Choice e3 Pupil.Green ]

        p9 =
            Pupil.Obj "Richard" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p10 =
            Pupil.Obj "Josua" "1a" []
    in
    describe "Main functions"
        [ describe "The finalize function"
            [ test "assigns all pupils" <|
                \_ ->
                    let
                        pupils =
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 ]
                    in
                    Main.finalize pupils [ e1, e2, e3, e4, e5 ]
                        |> Expect.all
                            [ Dict.toList
                                >> Expect.equalLists
                                    [ ( "Ali (1a)", "Theater-1" )
                                    , ( "Anna (1a)", "Outdoor-1" )
                                    , ( "Hans (1a)", "Outdoor-2" )
                                    , ( "Josua (1a)", "Song des Tages-1" )
                                    , ( "Kim (1a)", "Töpfern-1" )
                                    , ( "Lisa (1a)", "Kochen-1" )
                                    , ( "Maria (1a)", "Theater-2" )
                                    , ( "Max (1a)", "Töpfern-2" )
                                    , ( "Moritz (1a)", "Kochen-2" )
                                    , ( "Richard (1a)", "Song des Tages-2" )
                                    ]
                            , Dict.keys >> List.length >> Expect.equal 10
                            , howManyGreens pupils >> Expect.equal 8
                            ]

            -- Alternative mit alter Datenstruktur:  [ Assignment.Model e5 [ p10, p5 ], Assignment.Model e4 [ p7, p9 ], Assignment.Model e3 [ p8, p6 ], Assignment.Model e1 [ p1, p4 ], Assignment.Model e2 [ p2, p3 ]]
            , test "assigns all pupils with changed choices" <|
                \_ ->
                    let
                        pupils =
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, { p10 | choices = [ Pupil.Choice e5 Pupil.Red ] } ]
                    in
                    Main.finalize pupils [ e1, e2, e3, e4, e5 ]
                        |> Expect.all
                            [ Dict.toList
                                >> Expect.equalLists
                                    [ ( "Ali (1a)", "Theater-1" )
                                    , ( "Anna (1a)", "Song des Tages-2" )
                                    , ( "Hans (1a)", "Outdoor-2" )
                                    , ( "Josua (1a)", "Outdoor-1" )
                                    , ( "Kim (1a)", "Töpfern-1" )
                                    , ( "Lisa (1a)", "Kochen-1" )
                                    , ( "Maria (1a)", "Theater-2" )
                                    , ( "Max (1a)", "Töpfern-2" )
                                    , ( "Moritz (1a)", "Kochen-2" )
                                    , ( "Richard (1a)", "Song des Tages-1" )
                                    ]

                            -- [ ( "Ali (1a)", "Theater-1" )
                            -- , ( "Anna (1a)", "Outdoor-1" )
                            -- , ( "Hans (1a)", "Outdoor-2" )
                            -- , ( "Josua (1a)", "Song des Tages-1" )
                            -- , ( "Kim (1a)", "Töpfern-1" )
                            -- , ( "Lisa (1a)", "Kochen-1" )
                            -- , ( "Maria (1a)", "Theater-2" )
                            -- , ( "Max (1a)", "Töpfern-2" )
                            -- , ( "Moritz (1a)", "Kochen-2" )
                            -- , ( "Richard (1a)", "Song des Tages-2" )
                            -- ]
                            , Dict.keys >> List.length >> Expect.equal 10
                            , howManyGreens pupils >> Expect.equal 7
                            ]
            , test "assign pupils in mini example" <|
                \_ ->
                    let
                        i1 =
                            Event.Obj "1" 1

                        i2 =
                            Event.Obj "2" 1

                        i3 =
                            Event.Obj "3" 1

                        j1 =
                            Pupil.Obj "A" "" [ Pupil.Choice i1 Pupil.Green ]

                        j2 =
                            Pupil.Obj "B" "" []

                        j3 =
                            Pupil.Obj "C" "" [ Pupil.Choice i3 Pupil.Red ]
                    in
                    Main.finalize [ j1, j2, j3 ] [ i3, i2, i1 ]
                        |> Expect.all
                            [ howManyGreens [ j1, j2, j3 ] >> Expect.equal 1
                            , Dict.keys >> List.length >> Expect.equal 3
                            ]
            ]
        ]


howManyGreens : List Pupil.Obj -> Algo.Matching -> Int
howManyGreens pupils matching =
    pupils
        |> List.foldl
            (\pupil count ->
                case Dict.get (Pupil.toVertex pupil) matching of
                    Nothing ->
                        count

                    Just event ->
                        if pupil |> Pupil.greenEvents |> List.foldl Event.toVertexListReducer [] |> List.member event then
                            count + 1

                        else
                            count
            )
            0
