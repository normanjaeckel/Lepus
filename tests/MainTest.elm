module MainTest exposing (suite)

import Event
import Expect
import Main
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        p1 =
            Pupil.Model "Max" "1a"

        p2 =
            Pupil.Model "Moritz" "1a"

        p3 =
            Pupil.Model "Lisa" "1a"

        p4 =
            Pupil.Model "Kim" "1a"

        p5 =
            Pupil.Model "Anna" "1a"

        e1 =
            Event.Model "Töpfern" 2

        e2 =
            Event.Model "Kochen" 2
    in
    describe "Main functions"
        [ describe "The applyToOne function"
            [ test "applys one pupil to empty event" <|
                \_ ->
                    Main.applyToOne p1 [ Main.EventWithPupils e1 [] ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p1 ] ]
            , test "applys one pupil to non empty events" <|
                \_ ->
                    Main.applyToOne p1 [ Main.EventWithPupils e1 [ p1 ] ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p1, p1 ] ]
            , test "applys one pupil into non empty events into the value with the minimum count" <|
                \_ ->
                    Main.applyToOne p1 [ Main.EventWithPupils e1 [], Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [] ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [] ]
            ]
        , describe "The applyPupils function"
            [ test "applys one pupil to one event" <|
                \_ ->
                    Main.applyPupils [ e1 ] [ p1 ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p1 ] ]
            , test "applys two pupils to one event" <|
                \_ ->
                    Main.applyPupils [ e1 ] [ p1, p2 ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p2, p1 ] ]
            , test "applys two pupils to two events" <|
                \_ ->
                    Main.applyPupils [ e1, e2 ] [ p1, p2 ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p1 ], Main.EventWithPupils e2 [ p2 ] ]
            , test "applys five pupils to two events" <|
                \_ ->
                    Main.applyPupils [ e1, e2 ] [ p1, p2, p3, p4, p5 ]
                        |> Expect.equal [ Main.EventWithPupils e1 [ p5, p3, p1 ], Main.EventWithPupils e2 [ p4, p2 ] ]
            ]
        ]
