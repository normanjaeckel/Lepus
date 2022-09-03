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
    let
        step1 : Algo.Matching
        step1 =
            Dict.empty |> Algo.run (toGraphFromGreen pupils)

        step2 : Algo.Matching
        step2 =
            Dict.empty |> Algo.run (toGraphFromYellowWithoutMatched pupils events step1)

        step3 : Algo.Matching -> Algo.Matching
        step3 =
            Algo.run (toGraphFromGreenAndYellow pupils events)
    in
    Dict.union step1 step2 |> step3


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


toGraphFromYellowWithoutMatched : List Pupil.Model -> List Event.Model -> Algo.Matching -> Algo.Graph
toGraphFromYellowWithoutMatched pupils events matching =
    let
        onlyRemaining : Pupil.Model -> Bool
        onlyRemaining =
            \pupil ->
                Dict.member (pupil |> Pupil.toVertex) matching |> not

        onlyUnmatchedVertices : Algo.Vertex -> Bool
        onlyUnmatchedVertices =
            \vertex -> matching |> Dict.values |> List.member vertex |> not

        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Model -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (pupil
                            |> Pupil.yellowEvents events
                            |> List.foldl Event.toVertexListReducer []
                            |> List.filter onlyUnmatchedVertices
                        )
    in
    pupils
        |> List.filter onlyRemaining
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
