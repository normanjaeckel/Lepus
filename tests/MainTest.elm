module MainTest exposing (suite)

import Assignment
import Event
import Expect
import Main
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        e1 =
            Event.Model "Töpfern" 2

        e2 =
            Event.Model "Kochen" 2

        p1 =
            Pupil.Model "Max" "1a" [ Pupil.Choice e1 Pupil.Green ]

        p2 =
            Pupil.Model "Moritz" "1a" [ Pupil.Choice e2 Pupil.Green ]

        p3 =
            Pupil.Model "Lisa" "1a" []

        p4 =
            Pupil.Model "Kim" "1a" [ Pupil.Choice e1 Pupil.Green ]

        p5 =
            Pupil.Model "Anna" "1a" []
    in
    describe "Main functions"
        [ describe "The assignGreens function"
            [ test "assigs some pupils" <|
                \_ ->
                    Main.assignGreens [ p1, p2, p3, p4, p5 ] [ e1, e2 ]
                        |> Expect.all
                            [ \r -> r.remainingPupils |> List.length |> Expect.equal 2
                            , \r -> r.open |> List.length |> Expect.equal 0
                            , \r -> r.filled |> List.length |> Expect.equal 2
                            , \r -> r.filled |> Expect.equalLists [ Assignment.Model e2 [ p2 ], Assignment.Model e1 [ p1, p4 ] ]
                            ]
            ]
        , describe "The pupilsPrefer function"
            [ test "retrieves p1 for e1" <|
                \_ ->
                    Main.pupilsPrefer [ p1, p2, p3, p4, p5 ] e1
                        |> Expect.equalLists [ p1, p4 ]
            , test "retrieves p2 for e2" <|
                \_ ->
                    Main.pupilsPrefer [ p1, p2, p3 ] e2
                        |> Expect.equalLists [ p2 ]
            , test "retrieves nobody for e1" <|
                \_ ->
                    Main.pupilsPrefer [ p2, p3 ] e1
                        |> Expect.equalLists []
            ]
        ]



-- describe "Main functions"
--     [ describe "The applyToOne function"
--         [ test "applys one pupil to empty event" <|
--             \_ ->
--                 Main.applyToOne p1 [ Main.EventWithPupils e1 [] ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p1 ] ]
--         , test "applys one pupil to non empty events" <|
--             \_ ->
--                 Main.applyToOne p1 [ Main.EventWithPupils e1 [ p1 ] ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p1, p1 ] ]
--         , test "applys one pupil into non empty events into the value with the minimum count" <|
--             \_ ->
--                 Main.applyToOne p1 [ Main.EventWithPupils e1 [], Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [] ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [] ]
--         ]
--     , describe "The applyPupils function"
--         [ test "applys one pupil to one event" <|
--             \_ ->
--                 Main.applyPupils [ e1 ] [ p1 ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p1 ] ]
--         , test "applys two pupils to one event" <|
--             \_ ->
--                 Main.applyPupils [ e1 ] [ p1, p2 ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p2, p1 ] ]
--         , test "applys two pupils to two events" <|
--             \_ ->
--                 Main.applyPupils [ e1, e2 ] [ p1, p2 ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [ p2 ] ]
--         , test "applys five pupils to two events and considers the red choice of p1" <|
--             \_ ->
--                 Main.applyPupils [ e1, e2 ] [ p1, p2, p3, p4, p5 ]
--                     |> Expect.equal [ Main.EventWithPupils e1 [ p5, p3, p1 ], Main.EventWithPupils e2 [ p4, p2 ] ]
--         ]
--     ]
-- Pupil.Choice e1 Pupil.Red
