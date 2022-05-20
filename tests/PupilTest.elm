module PupilTest exposing (suite)

import Expect
import Fuzz
import Pupil
import Test exposing (..)


suite : Test
suite =
    describe "Basic pupil functions"
        [ test "toString builds the correct result" <|
            \_ ->
                Pupil.Model "Max" "1a"
                    |> Pupil.toString
                    |> Expect.equal "Max (1a)"
        , fuzz Fuzz.string "toString builds the correct result with any name" <|
            \s ->
                Pupil.Model "Moritz" s
                    |> Pupil.toString
                    |> Expect.equal ("Moritz (" ++ s ++ ")")
        ]
