module EventTest exposing (suite)

import Event
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "Basic event functions"
        [ test "toString builds the correct result" <|
            \_ ->
                Event.Model "Töpfern" 10
                    |> Event.toString
                    |> Expect.equal "Töpfern"
        , fuzz Fuzz.string "toString builds the correct result with any name" <|
            \s ->
                Event.Model s 10
                    |> Event.toString
                    |> Expect.equal s
        ]
