module LogicTest exposing (suite)

import Assignment
import Event
import Expect
import Logic
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        e1 =
            Event.Model "Töpfern" 2

        a1 =
            Assignment.Model e1 []

        e2 =
            Event.Model "Kochen" 2

        a2 =
            Assignment.Model e2 []

        e3 =
            Event.Model "Theater" 2

        a3 =
            Assignment.Model e3 []

        e4 =
            Event.Model "Outdoor" 2

        a4 =
            Assignment.Model e4 []

        e5 =
            Event.Model "Song des Tages" 2

        a5 =
            Assignment.Model e5 []

        p1 =
            Pupil.Model "Max" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e1 Pupil.Green ]

        p2 =
            Pupil.Model "Moritz" "1a" [ Pupil.Choice e2 Pupil.Green ]

        p3 =
            Pupil.Model "Lisa" "1a" [ Pupil.Choice e1 Pupil.Green, Pupil.Choice e2 Pupil.Green ]

        p4 =
            Pupil.Model "Kim" "1a" [ Pupil.Choice e1 Pupil.Green ]

        p5 =
            Pupil.Model "Anna" "1a" [ Pupil.Choice e4 Pupil.Green ]

        p6 =
            Pupil.Model "Maria" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p7 =
            Pupil.Model "Hans" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p8 =
            Pupil.Model "Ali" "1a" [ Pupil.Choice e3 Pupil.Green ]

        p9 =
            Pupil.Model "Kim" "1a" [ Pupil.Choice e3 Pupil.Green, Pupil.Choice e4 Pupil.Green ]

        p10 =
            Pupil.Model "Josua" "1a" []
    in
    describe "Basic functions"
        [ test "assignPupil gives the correct result for p1" <|
            \_ ->
                Logic.assignPupil p1 [ a1, a2, a3, a4, a5 ]
                    |> Expect.equal (Just [ { a1 | pupils = [ p1 ] }, a3, a2, a4, a5 ])
        , test "assignPupil gives the correct result for p2" <|
            \_ ->
                Logic.assignPupil p2 [ a1, a2, a3, a4, a5 ]
                    |> Expect.equal (Just [ { a2 | pupils = [ p2 ] }, a1, a3, a4, a5 ])
        , test "assignPupil gives the correct result for p3" <|
            \_ ->
                let
                    a1Modified =
                        { a1 | pupils = [ p1, p4 ] }
                in
                Logic.assignPupil p3 [ a1Modified, a2, a3, a4, a5 ]
                    |> Expect.equal (Just [ { a2 | pupils = [ p3 ] }, a1Modified, a3, a4, a5 ])
        , test "assignPupil gives the correct result for p4" <|
            \_ ->
                Logic.assignPupil p4 [ { a1 | pupils = [ p1, p3 ] }, a2, a3, a4, a5 ]
                    |> Expect.equal (Just [ { a1 | pupils = [ p4, p3 ] }, { a3 | pupils = [ p1 ] }, a2, a4, a5 ])
        ]
