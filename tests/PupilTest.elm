module PupilTest exposing (suite)

import Event
import Expect
import Fuzz
import Pupil
import Test exposing (..)


suite : Test
suite =
    let
        e1 : Event.Obj
        e1 =
            Event.Obj "Kochen" 2

        p1 : Pupil.Obj
        p1 =
            Pupil.Obj "Max" "1a" []

        p2 : Pupil.Obj
        p2 =
            Pupil.Obj "Kim" "1a" [ Pupil.Choice e1 Pupil.Red ]
    in
    describe "Basic pupil functions"
        [ test "toString gives the correct result" <|
            \_ ->
                p1
                    |> Pupil.toVertex
                    |> Expect.equal "Max (1a)"
        , fuzz Fuzz.string "toString builds the correct result with any class string" <|
            \s ->
                Pupil.Obj "Moritz" s []
                    |> Pupil.toVertex
                    |> Expect.equal ("Moritz (" ++ s ++ ")")
        , test "redEvents gives correct result" <|
            \_ ->
                Pupil.redEvents p2 |> Expect.equal [ e1 ]
        ]
