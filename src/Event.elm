module Event exposing (Model, init, toString)

import Html exposing (..)


type alias Model =
    { name : String
    , capacity : Int
    }


init : List Model
init =
    []


toString : Model -> String
toString m =
    m.name



-- type Event
--     = Toepfern
--     | SongDesTages
--     | Kochen
--     | Gelaendespiel
