module Event exposing (Model, Msg, Obj, decoder, decoderEvent, eventToJSON, extendToCapacityAndRestrictByClass, init, modelToJSON, update, view)

import Class
import Helpers exposing (Persistence(..), classes, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, id, placeholder, required, tabindex, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Json.Decode as D
import Json.Encode as E
import Set



-- MODEL


type alias Model =
    { events : List Obj
    , formData : Obj
    , formInvalid : Bool
    }


init : Model
init =
    Model [] emptyFormData False


emptyFormData : Obj
emptyFormData =
    Obj "" 1 0


type alias Obj =
    { name : String
    , capacity : Int
    , internalID : Int
    }


type Seat
    = Reserved Int Class.Classname
    | Free Int


extendToCapacityAndRestrictByClass : Obj -> Set.Set Class.Classname -> Class.Classname -> List Obj
extendToCapacityAndRestrictByClass event cls pupilsCl =
    let
        num : Int
        num =
            event.capacity // Set.size cls

        fn : Class.Classname -> ( Int, List Seat ) -> ( Int, List Seat )
        fn =
            \cl ( i, l ) ->
                let
                    newIndex : Int
                    newIndex =
                        i + 1
                in
                ( newIndex + num - 1, l ++ (List.range newIndex (newIndex + num - 1) |> List.map (\n -> Reserved n cl)) )

        seatsReserved : ( Int, List Seat )
        seatsReserved =
            cls
                |> Set.toList
                |> List.foldl fn ( 0, [] )

        allSeats : List Seat
        allSeats =
            Tuple.second seatsReserved ++ (List.range (Tuple.first seatsReserved + 1) event.capacity |> List.map Free)
    in
    allSeats
        |> List.filter
            (\s ->
                case s of
                    Free _ ->
                        True

                    Reserved _ c ->
                        c == pupilsCl
            )
        |> List.map
            (\s ->
                let
                    i =
                        case s of
                            Free j ->
                                j

                            Reserved j _ ->
                                j
                in
                { event | internalID = i }
            )


decoder : D.Decoder Model
decoder =
    D.map
        (\l -> Model l emptyFormData False)
        (D.list decoderEvent)


decoderEvent : D.Decoder Obj
decoderEvent =
    D.map2
        (\n c -> Obj n c 0)
        (D.field "name" D.string)
        (D.field "capacity" D.int)


modelToJSON : Model -> E.Value
modelToJSON model =
    model.events |> E.list eventToJSON


eventToJSON : Obj -> E.Value
eventToJSON e =
    E.object
        [ ( "name", E.string e.name )
        , ( "capacity", E.int e.capacity )
        ]



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | Delete Obj


type FormDataInput
    = Name String
    | Capacity Int


update : Msg -> Model -> ( Model, Persistence )
update msg model =
    case msg of
        FormDataMsg data ->
            ( { model | formData = updateFormdata data model.formData, formInvalid = False }, DontSetStorage )

        Save ->
            case validate model of
                Just new ->
                    ( { model
                        | formData = emptyFormData
                        , events = model.events ++ [ new ]
                        , formInvalid = False
                      }
                    , SetStorage
                    )

                Nothing ->
                    ( { model | formInvalid = True }, DontSetStorage )

        Delete obj ->
            ( { model | events = model.events |> List.filter ((/=) obj), formInvalid = False }, SetStorage )


updateFormdata : FormDataInput -> Obj -> Obj
updateFormdata msg formData =
    case msg of
        Name name ->
            { formData | name = name }

        Capacity capacity ->
            { formData | capacity = capacity }


validate : Model -> Maybe Obj
validate model =
    let
        name : String
        name =
            model.formData.name |> String.trim
    in
    if
        (name == "")
            || (model.formData.capacity <= 0)
            || (model.events |> List.any (\e -> e.name == name))
    then
        Nothing

    else
        Just (Obj name model.formData.capacity 0)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mb-5" ]
        [ h2 [ id "events", class "nav-anchor" ] [ text "Projektgruppen" ]
        , p [ class "col-md-8" ]
            [ text
                """Abhängig von der Anzahl der Klassen und der Anzahl der Plätze in einer Gruppe gibt es feste Plätze
                für Schüler und Schülerinnen aus bestimmten Klassen und freie Plätze. Es erfolgt eine ganzzahlige Teilung
                der Plätze durch die Anzahl der Klassen. Der Rest gibt die freien Plätze an."""
            ]
        , p [ classes "col-md-8 fst-italic pb-2" ]
            [ text
                """Beispiel: Bei vier Klassen und 13 Plätzen gibt es 3 feste Plätze pro Klasse und einen freien
                Platz, der von jedem beliebigen Schüler besetzt werden kann."""
            ]
        , form [ class "mb-3", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Neue Gruppe hinzufügen" ]
            , div [ classes "row g-3" ]
                [ div [ class "col-md-3" ]
                    (tagWithInvalidFeedback
                        input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Name"
                        , attribute "aria-label" "Name"
                        , required True
                        , onInput (Name >> FormDataMsg)
                        , value model.formData.name
                        ]
                        "newGroupName"
                        "Gruppe ist bereits vorhanden"
                        model.formInvalid
                    )
                , div [ class "col-md-3" ]
                    [ input
                        [ class "form-control"
                        , type_ "number"
                        , Html.Attributes.min "1"
                        , attribute "aria-label" "Anzahl der Plätze"
                        , onInput (String.toInt >> Maybe.withDefault 0 >> Capacity >> FormDataMsg)
                        , value (model.formData.capacity |> String.fromInt)
                        ]
                        []
                    , div [ class "form-text" ] [ text "Anzahl der Plätze" ]
                    ]
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Hinzufügen" ] ]
                ]
            ]
        , div []
            [ h3 [ hidden True ] [ text "Alle Gruppen" ]
            , lazy allEvents model.events
            ]
        ]


allEvents : List Obj -> Html Msg
allEvents events =
    if List.isEmpty events then
        p [ hidden True ] [ text "Noch keine Gruppen angelegt" ]

    else
        ol [ classes "list-group list-group-flush list-group-numbered" ] (events |> List.sortBy .name |> List.map (lazy oneEventLi))


oneEventLi : Obj -> Html Msg
oneEventLi event =
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
        [ div [ classes "ms-2 me-auto" ]
            [ text event.name
            , span [ classes "ms-2 badge bg-primary rounded-pill", title "Anzahl der Plätze", attribute "aria-label" "Anzahl der Plätze" ] [ text <| String.fromInt event.capacity ]
            ]
        , a
            [ class "link-danger"
            , title "Löschen"
            , tabindex 0
            , attribute "role" "button"
            , attribute "aria-label" "Löschen"
            , onClick <| Delete event
            ]
            [ svgIconXLg ]
        ]
