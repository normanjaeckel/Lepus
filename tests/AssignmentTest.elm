module AssignmentTest exposing (suite)

import Algo
import Assignment
import Class
import Dict
import Event
import Expect
import Pupil
import Set
import Test exposing (..)


suite : Test
suite =
    let
        ( id1, e1 ) =
            ( 1, Event.Obj "Töpfern (e1)" 2 0 )

        ( id2, e2 ) =
            ( 2, Event.Obj "Kochen (e2)" 2 0 )

        ( id3, e3 ) =
            ( 3, Event.Obj "Theater (e3)" 2 0 )

        ( id4, e4 ) =
            ( 4, Event.Obj "Outdoor (e4)" 2 0 )

        ( id5, e5 ) =
            ( 5, Event.Obj "Song des Tages (e5)" 2 0 )

        events : Dict.Dict Int Event.Obj
        events =
            [ ( id1, e1 ), ( id2, e2 ), ( id3, e3 ), ( id4, e4 ), ( id5, e5 ) ] |> Dict.fromList

        p1 =
            Pupil.Obj "Max" "1" Dict.empty |> setupPupil [ id3, id1 ] [ id5, id4, id2 ] []

        p2 =
            Pupil.Obj "Moritz" "2" Dict.empty |> setupPupil [ id2 ] [ id5, id4, id3, id1 ] []

        p3 =
            Pupil.Obj "Lisa" "3" Dict.empty |> setupPupil [ id1, id2 ] [ id5, id4, id3 ] []

        p4 =
            Pupil.Obj "Kim" "4" Dict.empty |> setupPupil [ id1 ] [ id5, id4, id3, id2 ] []

        p5 =
            Pupil.Obj "Anna" "5" Dict.empty |> setupPupil [ id4 ] [ id5, id3, id2, id1 ] []

        p6 =
            Pupil.Obj "Maria" "6" Dict.empty |> setupPupil [ id3, id4 ] [ id5, id2, id1 ] []

        p7 =
            Pupil.Obj "Hans" "7" Dict.empty |> setupPupil [ id3, id4 ] [ id5, id2, id1 ] []

        p8 =
            Pupil.Obj "Ali" "8" Dict.empty |> setupPupil [ id3 ] [ id5, id4, id2, id1 ] []

        p9 =
            Pupil.Obj "Richard" "9" Dict.empty |> setupPupil [ id3, id4 ] [ id5, id2, id1 ] []

        p10 =
            Pupil.Obj "Josua" "10" Dict.empty |> setupPupil [] [ id5, id4, id3, id2, id1 ] []
    in
    describe "Main functions"
        [ describe "The finalize function"
            [ test "assigns all pupils" <|
                \_ ->
                    let
                        pupils =
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 ]
                    in
                    Assignment.finalize pupils (Set.fromList (pupils |> List.map .class)) events
                        |> Expect.all
                            [ Expect.equalLists
                                [ ( Algo.VertexLeft p1, Algo.VertexRight { e1 | internalID = 2 } )
                                , ( Algo.VertexLeft p3, Algo.VertexRight { e2 | internalID = 2 } )
                                , ( Algo.VertexLeft p2, Algo.VertexRight { e2 | internalID = 1 } )
                                , ( Algo.VertexLeft p4, Algo.VertexRight { e1 | internalID = 1 } )
                                , ( Algo.VertexLeft p6, Algo.VertexRight { e4 | internalID = 2 } )
                                , ( Algo.VertexLeft p7, Algo.VertexRight { e4 | internalID = 1 } )
                                , ( Algo.VertexLeft p8, Algo.VertexRight { e3 | internalID = 2 } )
                                , ( Algo.VertexLeft p9, Algo.VertexRight { e3 | internalID = 1 } )
                                , ( Algo.VertexLeft p5, Algo.VertexRight { e5 | internalID = 2 } )
                                , ( Algo.VertexLeft p10, Algo.VertexRight { e5 | internalID = 1 } )
                                ]
                            , List.length >> Expect.equal 10
                            , howManyGreens pupils (Set.fromList (pupils |> List.map .class)) events >> Expect.equal 8
                            ]

            -- Alternative mit alter Datenstruktur:  [ Assignment.Model e5 [ p10, p5 ], Assignment.Model e4 [ p7, p9 ], Assignment.Model e3 [ p8, p6 ], Assignment.Model e1 [ p1, p4 ], Assignment.Model e2 [ p2, p3 ]]
            , test "assigns all pupils with changed choices" <|
                \_ ->
                    let
                        p10a =
                            p10 |> setupPupil [] [ id1, id2, id3, id4 ] [ id5 ]

                        pupils =
                            [ p1, p2, p3, p4, p5, p6, p7, p8, p9, p10a ]
                    in
                    Assignment.finalize pupils (Set.fromList (pupils |> List.map .class)) events
                        |> Expect.all
                            [ Expect.equalLists
                                [ ( Algo.VertexLeft p10a, Algo.VertexRight { e1 | internalID = 1 } )
                                , ( Algo.VertexLeft p4, Algo.VertexRight { e5 | internalID = 2 } )
                                , ( Algo.VertexLeft p1, Algo.VertexRight { e1 | internalID = 2 } )
                                , ( Algo.VertexLeft p3, Algo.VertexRight { e2 | internalID = 2 } )
                                , ( Algo.VertexLeft p2, Algo.VertexRight { e2 | internalID = 1 } )
                                , ( Algo.VertexLeft p6, Algo.VertexRight { e4 | internalID = 2 } )
                                , ( Algo.VertexLeft p7, Algo.VertexRight { e4 | internalID = 1 } )
                                , ( Algo.VertexLeft p8, Algo.VertexRight { e3 | internalID = 2 } )
                                , ( Algo.VertexLeft p9, Algo.VertexRight { e3 | internalID = 1 } )
                                , ( Algo.VertexLeft p5, Algo.VertexRight { e5 | internalID = 1 } )
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
                            , List.length >> Expect.equal 10
                            , howManyGreens pupils (Set.fromList (pupils |> List.map .class)) events >> Expect.equal 7
                            ]
            , test "assign pupils in mini example" <|
                \_ ->
                    let
                        ( i1, ii1 ) =
                            ( 1, Event.Obj "1" 1 0 )

                        ( i2, ii2 ) =
                            ( 2, Event.Obj "2" 1 0 )

                        ( i3, ii3 ) =
                            ( 3, Event.Obj "3" 1 0 )

                        el : Dict.Dict Int Event.Obj
                        el =
                            [ ( i1, ii1 ), ( i2, ii2 ), ( i3, ii3 ) ] |> Dict.fromList

                        j1 =
                            Pupil.Obj "A" "1" Dict.empty |> setupPupil [ i1 ] [ i2, i3 ] []

                        j2 =
                            Pupil.Obj "B" "1" Dict.empty |> setupPupil [] [ i1, i2, i3 ] []

                        j3 =
                            Pupil.Obj "C" "1" Dict.empty |> setupPupil [] [ i1, i2 ] [ i3 ]
                    in
                    Assignment.finalize [ j1, j2, j3 ] (Set.fromList [ "1" ]) el
                        |> Expect.all
                            [ howManyGreens [ j1, j2, j3 ] (Set.fromList [ "1" ]) el >> Expect.equal 1
                            , List.length >> Expect.equal 3
                            ]
            ]
        ]


setupPupil : List Int -> List Int -> List Int -> Pupil.Obj -> Pupil.Obj
setupPupil green yellow red pupil =
    let
        g =
            green |> List.map (\color -> ( color, Pupil.Green ))

        y =
            yellow |> List.map (\color -> ( color, Pupil.Yellow ))

        r =
            red |> List.map (\color -> ( color, Pupil.Red ))
    in
    { pupil | choices = Dict.fromList (g ++ y ++ r) }


howManyGreens : List Pupil.Obj -> Set.Set Class.Classname -> Dict.Dict Int Event.Obj -> Algo.Matching Pupil.Obj Event.Obj -> Int
howManyGreens pupils cls events matching =
    pupils
        |> List.foldl
            (\pupil count ->
                case matching |> Algo.getFromMatchingLeft (Algo.VertexLeft pupil) of
                    Nothing ->
                        count

                    Just vertex ->
                        if
                            pupil
                                |> Pupil.eventGroup Pupil.Green
                                |> List.foldl
                                    (\eId l ->
                                        case Dict.get eId events of
                                            Just e ->
                                                e :: l

                                            Nothing ->
                                                l
                                    )
                                    []
                                |> List.foldl (\e l -> Event.extendToCapacityAndRestrictByClass e cls pupil.class ++ l) []
                                |> List.map Algo.VertexRight
                                |> List.member vertex
                        then
                            count + 1

                        else
                            count
            )
            0
