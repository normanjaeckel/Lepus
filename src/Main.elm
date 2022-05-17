module Main exposing (main)

import Browser
import Html exposing (..)


main : Program () () ()
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


init : ()
init =
    ()



-- UPDATE


update : () -> () -> ()
update _ _ =
    ()



-- VIEW


view : () -> Html ()
view _ =
    div [] [ text "Hallo" ]
