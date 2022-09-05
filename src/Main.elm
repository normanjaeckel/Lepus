module Main exposing (finalize, main)

import Algo
import Browser
import Dict
import Event
import Html exposing (..)
import Pupil


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { pupils : Pupil.Model
    , events : Event.Model
    }


init : Model
init =
    { pupils = Pupil.init
    , events = Event.init
    }



-- UPDATE


type Msg
    = NewEventMsg Event.Msg
    | NewPupilMsg Pupil.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewEventMsg innerMsg ->
            { model | events = Event.update innerMsg model.events }

        NewPupilMsg innerMsg ->
            { model | pupils = Pupil.update innerMsg model.pupils }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ readme
        , Event.view model.events |> map NewEventMsg
        , Pupil.view model.pupils model.events.events |> map NewPupilMsg

        --, result model
        ]


readme : Html Msg
readme =
    div []
        [ h1 [] [ text "Überschrift hier" ]
        , p [] [ text "Beschreibung hier" ]
        ]



-- LOGIC


finalize : List Pupil.Obj -> List Event.Obj -> Algo.Matching
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


toGraphFromGreen : List Pupil.Obj -> Algo.Graph
toGraphFromGreen pupils =
    let
        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Obj -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (pupil |> Pupil.greenEvents |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph


toGraphFromYellowWithoutMatched : List Pupil.Obj -> List Event.Obj -> Algo.Matching -> Algo.Graph
toGraphFromYellowWithoutMatched pupils events matching =
    let
        onlyRemaining : Pupil.Obj -> Bool
        onlyRemaining =
            \pupil ->
                Dict.member (pupil |> Pupil.toVertex) matching |> not

        onlyUnmatchedVertices : Algo.Vertex -> Bool
        onlyUnmatchedVertices =
            \vertex -> matching |> Dict.values |> List.member vertex |> not

        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Obj -> Algo.Graph -> Algo.Graph
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


toGraphFromGreenAndYellow : List Pupil.Obj -> List Event.Obj -> Algo.Graph
toGraphFromGreenAndYellow pupils events =
    let
        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Obj -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (pupil |> Pupil.greenAndYellowEvents events |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph
