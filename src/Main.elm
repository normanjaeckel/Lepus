module Main exposing (finalize, main)

import Algo
import Browser
import Dict
import Event
import Html exposing (..)
import Pupil


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { pupils : List Pupil.Model
    , events : List Event.Model
    }


init : Model
init =
    { pupils = Pupil.init
    , events = Event.init
    }



-- UPDATE


update : msg -> Model -> Model
update _ m =
    m



-- VIEW


view : Model -> Html msg
view m =
    div []
        [ div []
            (finalize m.pupils m.events
                |> matchingToHtml
            )
        ]


matchingToHtml : Algo.Matching -> List (Html msg)
matchingToHtml _ =
    [ text "foo" ]



-- LOGIC


finalize : List Pupil.Model -> List Event.Model -> Algo.Matching
finalize pupils events =
    Dict.empty
        |> Algo.run (toGraphFromGreen pupils)
        |> Algo.run (toGraphFromGreenAndYellow pupils events)



--|> Algo.run (toGraphFromYellow pupils)


toGraphFromGreen : List Pupil.Model -> Algo.Graph
toGraphFromGreen pupils =
    let
        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Model -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (pupil |> Pupil.greenEvents |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph


toGraphFromGreenAndYellow : List Pupil.Model -> List Event.Model -> Algo.Graph
toGraphFromGreenAndYellow pupils events =
    let
        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Model -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (pupil |> Pupil.greenAndYellowEvents events |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph
