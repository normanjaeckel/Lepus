module EventTest exposing (suite)

import Event
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "Basic event functions"
        [ fuzz Fuzz.string "toVertexList builds the correct result with any name" <|
            \s ->
                Event.Model s 5
                    |> Event.toVertexList
                    |> Expect.equal [ s ++ "-1", s ++ "-2", s ++ "-3", s ++ "-4", s ++ "-5" ]
        ]
