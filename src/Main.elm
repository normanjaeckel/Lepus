module Main exposing (finalize, main)

import Algo
import Browser
import Dict
import Event
import Helpers exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (class, scope)
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
    = EventMsg Event.Msg
    | PupilMsg Pupil.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        EventMsg innerMsg ->
            let
                eventsModel =
                    Event.update innerMsg model.events
            in
            { model | events = eventsModel, pupils = Pupil.updateEvents eventsModel.events model.pupils }

        PupilMsg innerMsg ->
            { model | pupils = Pupil.update innerMsg model.pupils model.events.events }



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 py-md-5" ]
        [ main_ []
            [ readme
            , Event.view model.events |> map EventMsg
            , Pupil.view model.pupils |> map PupilMsg
            , result model
            ]
        ]


readme : Html Msg
readme =
    div [ class "mb-5" ]
        [ h1 [] [ text "Projektgruppenverteilung" ]
        , p [ classes "fs-5 col-md-8" ] [ text "Dieses Tool speichert die Eingaben im Local Storage des Browsers. Es werden keine eigegebenen Daten über das Internet gesendet." ]
        ]


result : Model -> Html Msg
result model =
    let
        ( matched, unmatched ) =
            matchedAndUnmatchedPupils model.pupils.pupils
    in
    div []
        [ h2 [] [ text "Ergebnis" ]
        , p [] [ text "Das Ergebnis wird mit jeder Eingabe automatisch aktualisiert." ]
        , div [ class "col-md-8" ]
            [ h3 []
                [ text "Zugeteilte Schüler/Schülerinnen" ]
            , table
                [ class "table" ]
                [ thead [] [ tr [] [ th [ scope "col" ] [ text "Name" ], th [ scope "col" ] [ text "Gruppe" ] ] ]
                , tbody []
                    (matched
                        |> Dict.toList
                        |> List.map (\( a, b ) -> tr [] [ td [] [ text a ], td [] [ text b ] ])
                    )
                ]
            ]
        , div [ class "col-md-8" ]
            [ h3 [] [ text "Schüler/Schülerinnen ohne Platz" ]
            , ol [ classes "list-group list-group-flush list-group-numbered" ]
                (unmatched
                    |> List.map (\v -> li [ class "list-group-item" ] [ span [ class "ms-2" ] [ text v ] ])
                )
            ]
        ]



-- LOGIC


matchedAndUnmatchedPupils : List Pupil.Obj -> ( Algo.Matching, List Algo.Vertex )
matchedAndUnmatchedPupils pupils =
    let
        matched =
            finalize pupils
    in
    ( matched
    , pupils
        |> List.map Pupil.toVertex
        |> List.filter (\v -> not <| List.member v (Dict.keys matched))
    )


finalize : List Pupil.Obj -> Algo.Matching
finalize pupils =
    let
        step1 : Algo.Matching
        step1 =
            Dict.empty |> Algo.run (toGraphFromGreen pupils)

        step2 : Algo.Matching
        step2 =
            Dict.empty |> Algo.run (toGraphFromYellowWithoutMatched pupils step1)

        step3 : Algo.Matching -> Algo.Matching
        step3 =
            Algo.run (toGraphFromGreenAndYellow pupils)
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
                        (pupil |> Pupil.eventGroup Pupil.Green |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph


toGraphFromYellowWithoutMatched : List Pupil.Obj -> Algo.Matching -> Algo.Graph
toGraphFromYellowWithoutMatched pupils matching =
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
                            |> Pupil.eventGroup Pupil.Yellow
                            |> List.foldl Event.toVertexListReducer []
                            |> List.filter onlyUnmatchedVertices
                        )
    in
    pupils
        |> List.filter onlyRemaining
        |> List.foldl fn emptyGraph


toGraphFromGreenAndYellow : List Pupil.Obj -> Algo.Graph
toGraphFromGreenAndYellow pupils =
    let
        emptyGraph : Algo.Graph
        emptyGraph =
            Dict.empty

        fn : Pupil.Obj -> Algo.Graph -> Algo.Graph
        fn =
            \pupil graph ->
                let
                    events : List Event.Obj
                    events =
                        (pupil |> Pupil.eventGroup Pupil.Green) ++ (pupil |> Pupil.eventGroup Pupil.Yellow)
                in
                graph
                    |> Dict.insert
                        (pupil |> Pupil.toVertex)
                        (events |> List.foldl Event.toVertexListReducer [])
    in
    pupils
        |> List.foldl fn emptyGraph
