port module Main exposing (finalize, main)

import Algo
import Browser
import Event
import Helpers exposing (classes, svgIconSortAlphaDown)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, scope, tabindex, title, type_)
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
        ( updatedModel, setToStorage ) =
            case msg of
                EventMsg innerMsg ->
                    let
                        ( eventsModel, action ) =
                            Event.update innerMsg model.events
                    in
                    case action of
                        Event.FormChanged ->
                            ( { model | events = eventsModel }, False )

                        Event.EventsChanged ->
                            ( { model | events = eventsModel, pupils = Pupil.updateEvents eventsModel.events model.pupils }, True )

                PupilMsg innerMsg ->
                    let
                        ( pupilsModel, action ) =
                            Pupil.update innerMsg model.pupils model.events.events
                    in
                    case action of
                        Pupil.FormChanged ->
                            ( { model | pupils = pupilsModel }, False )

                        Pupil.PupilsChanged ->
                            ( { model | pupils = pupilsModel }, True )

                DeleteAll ->
                    ( init "" |> Tuple.first, True )
    in
    if setToStorage then
        ( updatedModel, modelToJSON updatedModel |> E.encode 0 |> setStorage )

    else
        ( updatedModel, Cmd.none )



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

        tableRow : Pupil.Obj -> Event.Obj -> Html Msg
        tableRow =
            \p e ->
                tr []
                    [ td [] [ text <| Pupil.pupilDisplay p ]
                    , td []
                        [ span
                            [ classes <|
                                if p |> Pupil.eventGroup Pupil.Green |> List.member { e | internalID = 0 } then
                                    "badge text-bg-success"

                                else
                                    "badge text-bg-warning"
                            ]
                            [ text e.name ]
                        ]
                    ]
    in
    div [ class "mb-5" ]
        [ h2 [] [ text "Ergebnis" ]
        , p [] [ text "Das Ergebnis wird mit jeder Eingabe automatisch aktualisiert. Man kann es markieren, kopieren und anschließend in Excel, Word u. a. einfügen." ]
        , div [ class "col-md-8" ]
            [ h3 []
                [ text "Zugeteilte Schüler/Schülerinnen" ]
            , if List.isEmpty matched then
                p [] [ text "keine" ]

              else
                table
                    [ class "table" ]
                    [ thead []
                        [ tr []
                            [ th [ scope "col" ]
                                [ text "Name und Klasse"
                                , a
                                    [ classes "link-primary ms-2"
                                    , title "Nach Name und Klasse aufsteigend sortieren"
                                    , tabindex 0
                                    , attribute "role" "button"
                                    , attribute "aria-label" "Nach Name und Klasse aufsteigend sortieren"
                                    ]
                                    [ svgIconSortAlphaDown ]
                                ]
                            , th [ scope "col" ]
                                [ text "Gruppe"
                                , a
                                    [ classes "link-primary ms-2"
                                    , title "Nach Gruppe aufsteigend sortieren"
                                    , tabindex 0
                                    , attribute "role" "button"
                                    , attribute "aria-label" "Nach Gruppe aufsteigend sortieren"
                                    ]
                                    [ svgIconSortAlphaDown ]
                                ]
                            ]
                        ]
                    , tbody []
                        (matched
                            |> List.sortBy
                                (\( k, _ ) ->
                                    case k of
                                        Algo.Left p ->
                                            Pupil.pupilSorting p

                                        _ ->
                                            ""
                                )
                            |> List.map
                                (\( k, v ) ->
                                    case ( k, v ) of
                                        ( Algo.Left p, Algo.Right e ) ->
                                            tableRow p e

                                        _ ->
                                            tr [] []
                                )
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
                        |> List.sortBy Pupil.pupilSorting
                        |> List.map (\p -> li [ class "list-group-item" ] [ span [ class "ms-2" ] [ text <| Pupil.pupilDisplay p ] ])
                    )
            ]
        , div [ class "col-md-8" ]
            [ h3 [] [ text "Statistik" ]
            , p []
                [ span [ classes "badge text-bg-secondary me-2", title "Gesamt" ] [ text <| String.fromInt <| List.length matched + List.length unmatched ]
                , span [ class "me-2" ] [ text "=" ]
                , span [ classes "badge text-bg-success me-2", title "Grün" ] [ text <| String.fromInt <| List.length <| onColor Pupil.Green matched ]
                , span [ class "me-2" ] [ text "+" ]
                , span [ classes "badge text-bg-warning me-2", title "Gelb" ] [ text <| String.fromInt <| List.length <| onColor Pupil.Yellow matched ]
                , span [ class "me-2" ] [ text "+" ]
                , span [ classes "badge text-bg-danger", title "Rot" ] [ text <| String.fromInt <| List.length unmatched ]
                ]
            ]
        ]


admin : Html Msg
admin =
    div []
        [ h2 [] [ text "Administration" ]
        , button [ classes "btn btn-danger", type_ "button", onClick DeleteAll ] [ text "Alle Daten löschen" ]
        ]


onColor : Pupil.ChoiceType -> Algo.Matching Pupil.Obj Event.Obj -> List (Algo.Vertex Pupil.Obj Event.Obj)
onColor color matching =
    let
        fn : ( Algo.Vertex Pupil.Obj Event.Obj, Algo.Vertex Pupil.Obj Event.Obj ) -> Bool
        fn =
            \( left, right ) ->
                case ( left, right ) of
                    ( Algo.Left pupil, Algo.Right event ) ->
                        pupil.choices
                            |> List.any
                                (\c ->
                                    case ( c.type_, color ) of
                                        ( Pupil.Green, Pupil.Green ) ->
                                            { event | internalID = 0 } == c.event

                                        ( Pupil.Yellow, Pupil.Yellow ) ->
                                            { event | internalID = 0 } == c.event

                                        _ ->
                                            False
                                )

                    _ ->
                        False
    in
    matching
        |> List.filter fn
        |> List.map Tuple.first



-- PORTS


port setStorage : String -> Cmd msg



-- LOGIC


matchedAndUnmatchedPupils : List Pupil.Obj -> ( Algo.Matching Pupil.Obj Event.Obj, List Pupil.Obj )
matchedAndUnmatchedPupils pupils =
    let
        matched : Algo.Matching Pupil.Obj Event.Obj
        matched =
            finalize pupils
    in
    ( matched
    , pupils
        |> List.filter
            (\p ->
                case matched |> Algo.getFromMatching (Algo.Left p) of
                    Nothing ->
                        True

                    Just _ ->
                        False
            )
    )


finalize : List Pupil.Obj -> Algo.Matching Pupil.Obj Event.Obj
finalize pupils =
    let
        step1 : Algo.Matching Pupil.Obj Event.Obj
        step1 =
            [] |> Algo.run (toGraphFromGreen pupils)

        step2 : Algo.Matching Pupil.Obj Event.Obj
        step2 =
            [] |> Algo.run (toGraphFromYellowWithoutMatched pupils step1)

        step3 : Algo.Matching Pupil.Obj Event.Obj -> Algo.Matching Pupil.Obj Event.Obj
        step3 =
            Algo.run (toGraphFromGreenAndYellow pupils)
    in
    (step1 ++ step2) |> step3


toGraphFromGreen : List Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromGreen pupils =
    let
        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    k : Algo.Vertex Pupil.Obj Event.Obj
                    k =
                        Algo.Left pupil

                    v : List (Algo.Vertex Pupil.Obj Event.Obj)
                    v =
                        pupil
                            |> Pupil.eventGroup Pupil.Green
                            |> List.foldl (\e l -> Event.extendToCapacity e ++ l) []
                            |> List.map Algo.Right
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.foldl fn emptyGraph


toGraphFromYellowWithoutMatched : List Pupil.Obj -> Algo.Matching Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromYellowWithoutMatched pupils matching =
    let
        onlyRemaining : Pupil.Obj -> Bool
        onlyRemaining =
            \pupil ->
                matching
                    |> List.any
                        (\( k, _ ) ->
                            case k of
                                Algo.Left p ->
                                    p == pupil

                                _ ->
                                    False
                        )
                    |> not

        onlyUnmatchedVertices : Algo.Vertex Pupil.Obj Event.Obj -> Bool
        onlyUnmatchedVertices =
            \vertex -> matching |> List.any (Tuple.second >> (==) vertex) |> not

        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    k : Algo.Vertex Pupil.Obj Event.Obj
                    k =
                        Algo.Left pupil

                    v : List (Algo.Vertex Pupil.Obj Event.Obj)
                    v =
                        pupil
                            |> Pupil.eventGroup Pupil.Yellow
                            |> List.foldl (\e l -> Event.extendToCapacity e ++ l) []
                            |> List.map Algo.Right
                            |> List.filter onlyUnmatchedVertices
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.filter onlyRemaining
        |> List.foldl fn emptyGraph


toGraphFromGreenAndYellow : List Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromGreenAndYellow pupils =
    let
        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    events : List Event.Obj
                    events =
                        (pupil |> Pupil.eventGroup Pupil.Green) ++ (pupil |> Pupil.eventGroup Pupil.Yellow)

                    k : Algo.Vertex Pupil.Obj Event.Obj
                    k =
                        Algo.Left pupil

                    v : List (Algo.Vertex Pupil.Obj Event.Obj)
                    v =
                        events
                            |> List.foldl (\e l -> Event.extendToCapacity e ++ l) []
                            |> List.map Algo.Right
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.foldl fn emptyGraph
