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
            Pupil.Obj "Max" "1a" [] |> setupPupil [ e3, e1 ] [ e5, e4, e3 ] []

        p2 =
            Pupil.Obj "Moritz" "1a" [] |> setupPupil [ e2 ] [ e5, e4, e3, e1 ] []

        p3 =
            Pupil.Obj "Lisa" "1a" [] |> setupPupil [ e1, e2 ] [ e5, e4, e3 ] []

        p4 =
            Pupil.Obj "Kim" "1a" [] |> setupPupil [ e1 ] [ e5, e4, e3, e2 ] []

        p5 =
            Pupil.Obj "Anna" "1a" [] |> setupPupil [ e4 ] [ e5, e3, e2, e1 ] []

        p6 =
            Pupil.Obj "Maria" "1a" [] |> setupPupil [ e3, e4 ] [ e5, e2, e1 ] []

        p7 =
            Pupil.Obj "Hans" "1a" [] |> setupPupil [ e3, e4 ] [ e5, e2, e1 ] []

        p8 =
            Pupil.Obj "Ali" "1a" [] |> setupPupil [ e3 ] [ e5, e4, e2, e1 ] []

        p9 =
            Pupil.Obj "Richard" "1a" [] |> setupPupil [ e3, e4 ] [ e5, e2, e1 ] []

        p10 =
            Pupil.Obj "Josua" "1a" [] |> setupPupil [] [ e5, e4, e4, e2, e1 ] []
    in
    describe "Main functions"
        [ describe "The finalize function"
            [ test "assigns all pupils" <|
                \_ ->
                    let
                        pupils =
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 ]
                    in
                    Main.finalize pupils
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
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 |> setupPupil [] [ e1, e2, e3, e4 ] [ e5 ] ]
                    in
                    Main.finalize pupils
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
                            Pupil.Obj "A" "" [] |> setupPupil [ i1 ] [ i2, i3 ] []

                        j2 =
                            Pupil.Obj "B" "" [] |> setupPupil [] [ i1, i2, i3 ] []

                        j3 =
                            Pupil.Obj "C" "" [] |> setupPupil [] [ i1, i2 ] [ i3 ]
                    in
                    Main.finalize [ j1, j2, j3 ]
                        |> Expect.all
                            [ howManyGreens [ j1, j2, j3 ] >> Expect.equal 1
                            , Dict.keys >> List.length >> Expect.equal 3
                            ]
            ]
        ]


setupPupil : List Event.Obj -> List Event.Obj -> List Event.Obj -> Pupil.Obj -> Pupil.Obj
setupPupil green yellow red pupil =
    let
        g =
            green |> List.map (\e -> Pupil.Choice e Pupil.Green)

        y =
            yellow |> List.map (\e -> Pupil.Choice e Pupil.Yellow)

        r =
            red |> List.map (\e -> Pupil.Choice e Pupil.Red)
    in
    { pupil | choices = g ++ y ++ r }


howManyGreens : List Pupil.Obj -> Algo.Matching -> Int
howManyGreens pupils matching =
    pupils
        |> List.foldl
            (\pupil count ->
                case Dict.get (Pupil.toVertex pupil) matching of
                    Nothing ->
                        count

                    Just event ->
                        if pupil |> Pupil.eventGroup Pupil.Green |> List.foldl Event.toVertexListReducer [] |> List.member event then
                            count + 1

                        else
                            count
            )
            0
