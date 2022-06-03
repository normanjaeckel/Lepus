module PupilTest exposing (suite)

import Event
import Expect
import Fuzz
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        e1 : Event.Model
        e1 =
            Event.Model "Kochen" 2

        p1 : Pupil.Model
        p1 =
            Pupil.Model "Max" "1a" []

        p2 : Pupil.Model
        p2 =
            Pupil.Model "Kim" "1a" [ Pupil.Choice e1 Pupil.Red ]
    in
    describe "Basic pupil functions"
        [ test "toString gives the correct result" <|
            \_ ->
                p1
                    |> Pupil.toString
                    |> Expect.equal "Max (1a)"
        , fuzz Fuzz.string "toString builds the correct result with any class string" <|
            \s ->
                Pupil.Model "Moritz" s []
                    |> Pupil.toString
                    |> Expect.equal ("Moritz (" ++ s ++ ")")
        , test "redEvents gives correct result" <|
            \_ ->
                Pupil.redEvents p2 |> Expect.equal [ e1 ]
        ]
