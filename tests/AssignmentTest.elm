module AssignmentTest exposing (suite)

import Assignment
import Event
import Expect
import Html
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
            Pupil.Model "Kim" "1a" []
    in
    describe "Basic assignment functions"
        [ test "toHtml builds the correct result" <|
            \_ ->
                Assignment.Model e1 [ p1, p2 ]
                    |> Assignment.toHtml
                    |> Expect.equal
                        (Html.div []
                            [ Html.h3 [] [ Html.text "Kochen" ]
                            , Html.ol []
                                [ Html.li [] [ Html.text "Max (1a)" ]
                                , Html.li [] [ Html.text "Kim (1a)" ]
                                ]
                            ]
                        )

        --: Max (1a), Kim (1a)" ])
        ]
