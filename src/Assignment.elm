module Assignment exposing (Model, Msg, finalize, init, update, view)

import Algo
import Class
import Event
import Helpers exposing (classes, svgIconSortAlphaDown)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, id, scope, tabindex, title)
import Html.Events exposing (onClick)
import Process
import Pupil
import Set
import Task



-- MODEL


type alias Model =
    { sortBy : SortBy
    , visibility : Visibility
    }


init : Model
init =
    Model NameSort Hidden


type SortBy
    = NameSort
    | EventSort


type Visibility
    = Hidden
    | Loading
    | Visible



-- UPDATE


type Msg
    = SortBy SortBy
    | SetVisibility Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SortBy s ->
            ( { model | sortBy = s }, Cmd.none )

        SetVisibility v ->
            ( { model | visibility = v }
            , case v of
                Hidden ->
                    Cmd.none

                Loading ->
                    Process.sleep 50
                        |> Task.andThen (\_ -> Task.succeed Visible)
                        |> Task.perform SetVisibility

                Visible ->
                    Cmd.none
            )



-- VIEW


view : Model -> List Pupil.Obj -> Set.Set Class.Classname -> Html Msg
view model pupils cls =
    div [ class "mb-5" ]
        [ h2 [ id "result", class "nav-anchor" ] [ text "Ergebnis" ]
        , case model.visibility of
            Hidden ->
                button [ classes "btn btn-primary", onClick <| SetVisibility Loading ]
                    [ text "Ergebnis berechnen und anzeigen" ]

            Loading ->
                div [ classes "spinner-border text-primary", attribute "role" "status" ]
                    [ span [ class "visually-hidden" ] [ text "Loading..." ] ]

            Visible ->
                div []
                    [ button [ classes "btn btn-primary mb-3", onClick <| SetVisibility Hidden ]
                        [ text "Ergebnis ausblenden" ]
                    , innerView model pupils cls
                    ]
        ]


innerView : Model -> List Pupil.Obj -> Set.Set Class.Classname -> Html Msg
innerView model pupils cls =
    let
        ( matched, unmatched ) =
            pupils |> matchedAndUnmatchedPupils cls

        ( matched2, unmatched2 ) =
            pupils
                |> applyMatchingToRedState matched
                |> matchedAndUnmatchedPupils cls
    in
    div []
        [ p [] [ text "Das Ergebnis wird mit jeder Eingabe automatisch aktualisiert. Man kann es markieren, kopieren und anschließend in Excel, Word u. a. einfügen." ]
        , day 1 model matched unmatched
        , day 2 model matched2 unmatched2
        ]


day : Int -> Model -> List ( Pupil.Obj, Event.Obj ) -> List Pupil.Obj -> Html Msg
day num model matched unmatched =
    let
        tableRow : Pupil.Obj -> Event.Obj -> Html msg
        tableRow =
            \p e ->
                tr []
                    [ td [] [ text <| Pupil.pupilDisplay p ]
                    , td []
                        [ span
                            [ classes <|
                                if p |> Pupil.eventGroup Pupil.Green |> List.member e then
                                    "badge text-bg-success"

                                else
                                    "badge text-bg-warning"
                            ]
                            [ text e.name ]
                        ]
                    ]
    in
    div [ classes "col-md-8 mb-4" ]
        [ h3 [] [ text <| "Tag " ++ String.fromInt num ]
        , h4 []
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
                                , onClick <| SortBy NameSort
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
                                , onClick <| SortBy EventSort
                                ]
                                [ svgIconSortAlphaDown ]
                            ]
                        ]
                    ]
                , tbody []
                    (matched
                        |> List.sortBy
                            (\( p, e ) ->
                                case model.sortBy of
                                    NameSort ->
                                        Pupil.pupilSorting p

                                    EventSort ->
                                        e.name
                            )
                        |> List.map (\( p, e ) -> tableRow p e)
                    )
                ]
        , h4 [] [ text "Schüler/Schülerinnen ohne Platz" ]
        , if List.isEmpty unmatched then
            p [] [ text "Keine" ]

          else
            ol [ classes "list-group list-group-flush list-group-numbered" ]
                (unmatched
                    |> List.sortBy Pupil.pupilSorting
                    |> List.map (\p -> li [ class "list-group-item" ] [ span [ class "ms-2" ] [ text <| Pupil.pupilDisplay p ] ])
                )
        , h4 [] [ text "Statistik" ]
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


onColor : Pupil.ChoiceType -> List ( Pupil.Obj, Event.Obj ) -> List Pupil.Obj
onColor color matching =
    let
        fn : ( Pupil.Obj, Event.Obj ) -> Bool
        fn =
            \( pupil, event ) ->
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
    in
    matching
        |> List.filter fn
        |> List.map Tuple.first



-- LOGIC


matchedAndUnmatchedPupils : Set.Set Class.Classname -> List Pupil.Obj -> ( List ( Pupil.Obj, Event.Obj ), List Pupil.Obj )
matchedAndUnmatchedPupils cls pupils =
    let
        matched : Algo.Matching Pupil.Obj Event.Obj
        matched =
            finalize pupils cls

        matchedTransformed =
            matched |> List.map (\( Algo.VertexLeft p, Algo.VertexRight e ) -> ( p, { e | internalID = 0 } ))
    in
    ( matchedTransformed
    , pupils
        |> List.filter
            (\p ->
                case matched |> Algo.getFromMatchingLeft (Algo.VertexLeft p) of
                    Nothing ->
                        True

                    Just _ ->
                        False
            )
    )


applyMatchingToRedState : List ( Pupil.Obj, Event.Obj ) -> List Pupil.Obj -> List Pupil.Obj
applyMatchingToRedState matching pupils =
    pupils
        |> List.map
            (\pupil ->
                case matching |> List.filter (\( p, _ ) -> pupil == p) |> List.head of
                    Nothing ->
                        pupil

                    Just ( _, e ) ->
                        { pupil
                            | choices =
                                pupil.choices
                                    |> List.map
                                        (\c ->
                                            if c.event == e then
                                                { c | type_ = Pupil.Red }

                                            else
                                                c
                                        )
                        }
            )


finalize : List Pupil.Obj -> Set.Set Class.Classname -> Algo.Matching Pupil.Obj Event.Obj
finalize pupils cls =
    let
        step1 : Algo.Matching Pupil.Obj Event.Obj
        step1 =
            [] |> Algo.run (toGraphFromGreen pupils cls)

        step2 : Algo.Matching Pupil.Obj Event.Obj
        step2 =
            [] |> Algo.run (toGraphFromYellowWithoutMatched pupils cls step1)

        step3 : Algo.Matching Pupil.Obj Event.Obj -> Algo.Matching Pupil.Obj Event.Obj
        step3 =
            Algo.run (toGraphFromGreenAndYellow pupils cls)
    in
    (step1 ++ step2) |> step3


toGraphFromGreen : List Pupil.Obj -> Set.Set Class.Classname -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromGreen pupils cls =
    let
        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    k : Algo.VertexLeft Pupil.Obj
                    k =
                        Algo.VertexLeft pupil

                    v : List (Algo.VertexRight Event.Obj)
                    v =
                        pupil
                            |> Pupil.eventGroup Pupil.Green
                            |> List.foldl (\e l -> Event.extendToCapacityAndRestrictByClass e cls pupil.class ++ l) []
                            |> List.map Algo.VertexRight
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.foldl fn emptyGraph


toGraphFromYellowWithoutMatched : List Pupil.Obj -> Set.Set Class.Classname -> Algo.Matching Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromYellowWithoutMatched pupils cls matching =
    let
        onlyRemaining : Pupil.Obj -> Bool
        onlyRemaining =
            \pupil ->
                matching
                    |> List.any (\( Algo.VertexLeft p, _ ) -> p == pupil)
                    |> not

        onlyUnmatchedVertices : Algo.VertexRight Event.Obj -> Bool
        onlyUnmatchedVertices =
            \vertex -> matching |> List.any (Tuple.second >> (==) vertex) |> not

        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    k : Algo.VertexLeft Pupil.Obj
                    k =
                        Algo.VertexLeft pupil

                    v : List (Algo.VertexRight Event.Obj)
                    v =
                        pupil
                            |> Pupil.eventGroup Pupil.Yellow
                            |> List.foldl (\e l -> Event.extendToCapacityAndRestrictByClass e cls pupil.class ++ l) []
                            |> List.map Algo.VertexRight
                            |> List.filter onlyUnmatchedVertices
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.filter onlyRemaining
        |> List.foldl fn emptyGraph


toGraphFromGreenAndYellow : List Pupil.Obj -> Set.Set Class.Classname -> Algo.Graph Pupil.Obj Event.Obj
toGraphFromGreenAndYellow pupils cls =
    let
        emptyGraph : Algo.Graph Pupil.Obj Event.Obj
        emptyGraph =
            []

        fn : Pupil.Obj -> Algo.Graph Pupil.Obj Event.Obj -> Algo.Graph Pupil.Obj Event.Obj
        fn =
            \pupil graph ->
                let
                    k : Algo.VertexLeft Pupil.Obj
                    k =
                        Algo.VertexLeft pupil

                    events : List Event.Obj
                    events =
                        (pupil |> Pupil.eventGroup Pupil.Green) ++ (pupil |> Pupil.eventGroup Pupil.Yellow)

                    v : List (Algo.VertexRight Event.Obj)
                    v =
                        events
                            |> List.foldl (\e l -> Event.extendToCapacityAndRestrictByClass e cls pupil.class ++ l) []
                            |> List.map Algo.VertexRight
                in
                ( k, v ) :: graph
    in
    pupils
        |> List.foldl fn emptyGraph
