module Assignment exposing (Model, empty, toHtml)

import Event
import Html exposing (..)
import Pupil


type alias Model =
    { event : Event.Model
    , pupils : List Pupil.Model
    }


toHtml : Model -> Html msg
toHtml m =
    div []
        [ h3 [] [ text <| Event.toString m.event ]
        , ol []
            (m.pupils |> List.map (\p -> li [] [ text <| Pupil.toString p ]))
        ]


empty : List Event.Model -> List Model
empty events =
    events |> List.map (\e -> Model e [])
