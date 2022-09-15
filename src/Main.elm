port module Main exposing (finalize, main)

import Algo
import Browser
import Dict
import Event
import Helpers exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (class, scope)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Pupil


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { pupils : Pupil.Model
    , events : Event.Model
    }


init : String -> ( Model, Cmd Msg )
init s =
    case D.decodeString decoder s of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( { pupils = Pupil.init
              , events = Event.init
              }
            , Cmd.none
            )


decoder : D.Decoder Model
decoder =
    D.map2 Model
        (D.field "pupils" Pupil.decoder)
        (D.field "events" Event.decoder)


modelToJSON : Model -> E.Value
modelToJSON model =
    E.object
        [ ( "pupils", Pupil.modelToJSON model.pupils )
        , ( "events", Event.modelToJSON model.events )
        ]



-- UPDATE


type Msg
    = EventMsg Event.Msg
    | PupilMsg Pupil.Msg
    | DeleteAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel : Model
        updatedModel =
            case msg of
                EventMsg innerMsg ->
                    let
                        eventsModel =
                            Event.update innerMsg model.events
                    in
                    { model | events = eventsModel, pupils = Pupil.updateEvents eventsModel.events model.pupils }

                PupilMsg innerMsg ->
                    { model | pupils = Pupil.update innerMsg model.pupils model.events.events }

                DeleteAll ->
                    init "" |> Tuple.first
    in
    ( updatedModel, modelToJSON updatedModel |> E.encode 0 |> setStorage )



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 py-md-5" ]
        [ main_ []
            [ readme
            , Event.view model.events |> map EventMsg
            , Pupil.view model.pupils |> map PupilMsg
            , result model
            , admin
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
    div [ class "mb-5" ]
        [ h2 [] [ text "Ergebnis" ]
        , p [] [ text "Das Ergebnis wird mit jeder Eingabe automatisch aktualisiert. Man kann es markieren, kopieren und anschließend in Excel, Word u. a. einfügen." ]
        , div [ class "col-md-8" ]
            [ h3 []
                [ text "Zugeteilte Schüler/Schülerinnen" ]
            , if Dict.isEmpty matched then
                p [] [ text "keine" ]

              else
                table
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
            , if List.isEmpty unmatched then
                p [] [ text "Keine" ]

              else
                ol [ classes "list-group list-group-flush list-group-numbered" ]
                    (unmatched
                        |> List.map (\v -> li [ class "list-group-item" ] [ span [ class "ms-2" ] [ text v ] ])
                    )
            ]
        ]


admin : Html Msg
admin =
    div []
        [ h2 [] [ text "Administration" ]
        , button [ classes "btn btn-danger", onClick DeleteAll ] [ text "Alle Daten löschen" ]
        ]



-- PORTS


port setStorage : String -> Cmd msg



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
