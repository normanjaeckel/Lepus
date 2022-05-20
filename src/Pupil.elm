module Pupil exposing (Model, init, toString)


init : List Model
init =
    []


type alias Model =
    { name : String
    , class : String
    }


toString : Model -> String
toString m =
    m.name ++ " (" ++ m.class ++ ")"
