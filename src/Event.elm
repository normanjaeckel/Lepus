module Event exposing (Model, Msg, Obj, decoder, decoderEvent, eventToJSON, extendToCapacityAndRestrictByClass, init, modelToJSON, update, view)

import Class
import Dict
import Helpers exposing (Persistence(..), classes, svgIconCheckLg, svgIconPencil, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, id, placeholder, required, tabindex, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as D
import Json.Encode as E
import Set



-- MODEL


type alias Model =
    { events : Dict.Dict String Obj
    , formData : Obj
    , formInvalid : Bool
    , editMode : EditMode
    }


init : Model
init =
    Model Dict.empty emptyFormData False Disabled


type Id
    = Id String


toString : Id -> String
toString (Id str) =
    str


type alias Obj =
    { name : String
    , capacity : Int
    , internalID : Int
    }


emptyFormData : Obj
emptyFormData =
    Obj "" 1 0


type EditMode
    = Disabled
    | Editing Id Obj


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
    -- TODO: Use random number here.
    D.map
        (\l -> (l |> List.map (\e -> ( "---", e )) |> Dict.fromList |> Model) emptyFormData False Disabled)
        (D.list decoderEvent)


decoderEvent : D.Decoder Obj
decoderEvent =
    D.map2
        (\n c -> Obj n c 0)
        (D.field "name" D.string)
        (D.field "capacity" D.int)


modelToJSON : Model -> E.Value
modelToJSON model =
    -- TODO: Add ID to JSON output.
    model.events |> Dict.values |> E.list eventToJSON


eventToJSON : Obj -> E.Value
eventToJSON e =
    E.object
        [ ( "name", E.string e.name )
        , ( "capacity", E.int e.capacity )
        ]



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | SaveForm
    | Edit Id
    | EditFormMsg FormDataInput
    | SaveEdit
    | CancelEdit
    | Delete Id


type FormDataInput
    = Name String
    | Capacity Int


update : Msg -> Model -> ( Model, Persistence )
update msg model =
    case msg of
        FormDataMsg data ->
            ( { model | formData = updateFormdata data model.formData, formInvalid = False }, DontSetStorage )

        SaveForm ->
            case validate model.formData.name model.formData.capacity (Dict.values model.events) of
                Just newObj ->
                    let
                        newId =
                            -- TODO: Use random number here.
                            "HAHAHAHAHA"
                    in
                    ( { model
                        | events = model.events |> Dict.insert newId newObj
                        , formData = emptyFormData
                        , formInvalid = False
                      }
                    , SetStorage
                    )

                Nothing ->
                    ( { model | formInvalid = True }, DontSetStorage )

        Edit eId ->
            case model.events |> Dict.get (eId |> toString) of
                Just obj ->
                    ( { model | editMode = Editing eId obj }, DontSetStorage )

                Nothing ->
                    -- Just ignore such a message when object does not exist.
                    ( model, DontSetStorage )

        EditFormMsg data ->
            case model.editMode of
                Editing eId new ->
                    ( { model | editMode = Editing eId (updateFormdata data new) }, DontSetStorage )

                Disabled ->
                    -- Just ignore such a message when editing is disabled.
                    ( model, DontSetStorage )

        SaveEdit ->
            case model.editMode of
                Editing eId new ->
                    case validate new.name new.capacity (Dict.values model.events) of
                        Just updated ->
                            ( { model
                                | events =
                                    model.events
                                        |> Dict.update (eId |> toString) (\_ -> Just updated)
                                , editMode = Disabled
                              }
                            , SetStorage
                            )

                        Nothing ->
                            ( model, DontSetStorage )

                Disabled ->
                    -- Just ignore such a message when editing is disabled.
                    ( model, DontSetStorage )

        CancelEdit ->
            ( { model | editMode = Disabled }, DontSetStorage )

        Delete eId ->
            ( { model | events = model.events |> Dict.remove (eId |> toString), formInvalid = False, editMode = Disabled }, SetStorage )


updateFormdata : FormDataInput -> Obj -> Obj
updateFormdata msg formData =
    case msg of
        Name name ->
            { formData | name = name }

        Capacity capacity ->
            { formData | capacity = capacity }


validate : String -> Int -> List Obj -> Maybe Obj
validate nameRaw capacity events =
    let
        name : String
        name =
            nameRaw |> String.trim
    in
    if
        (name == "")
            || (capacity <= 0)
            || (events |> List.any (\e -> e.name == name))
    then
        Nothing

    else
        Just (Obj name capacity 0)



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
        , form [ class "mb-3", onSubmit SaveForm ]
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
            , lazy2 allEvents model.editMode model.events
            ]
        ]


allEvents : EditMode -> Dict.Dict String Obj -> Html Msg
allEvents editMode events =
    if Dict.isEmpty events then
        p [ hidden True ] [ text "Noch keine Gruppen angelegt" ]

    else
        ol [ classes "list-group list-group-flush list-group-numbered" ]
            (events
                |> Dict.toList
                |> List.map (\t -> ( Tuple.first t |> Id, Tuple.second t ))
                |> List.sortBy (\t -> Tuple.second t |> .name)
                |> List.map (lazy <| oneEventLi editMode)
            )


oneEventLi : EditMode -> ( Id, Obj ) -> Html Msg
oneEventLi editMode ( eId, event ) =
    let
        fixedLine : Html Msg
        fixedLine =
            li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
                [ div [ classes "ms-2 me-auto" ]
                    [ text event.name
                    , span [ classes "ms-2 badge bg-primary rounded-pill", title "Anzahl der Plätze", attribute "aria-label" "Anzahl der Plätze" ] [ text <| String.fromInt event.capacity ]
                    ]
                , a
                    [ class "link-secondary"
                    , title "Bearbeiten"
                    , tabindex 0
                    , attribute "role" "button"
                    , attribute "aria-label" "Bearbeiten"
                    , onClick <| Edit eId
                    ]
                    [ svgIconPencil ]
                , a
                    [ classes "ms-3 link-secondary"
                    , title "Löschen"
                    , tabindex 0
                    , attribute "role" "button"
                    , attribute "aria-label" "Löschen"
                    , onClick <| Delete eId
                    ]
                    [ svgIconXLg ]
                ]
    in
    case editMode of
        Disabled ->
            fixedLine

        Editing editingEId new ->
            if editingEId /= eId then
                fixedLine

            else
                li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
                    [ form [ classes "ms-2 row g-1", onSubmit SaveEdit ]
                        [ div [ class "col-7" ]
                            [ input
                                [ classes "form-control form-control-sm"
                                , type_ "text"
                                , placeholder "Name"
                                , attribute "aria-label" "Name"
                                , required True
                                , onInput (Name >> EditFormMsg)
                                , value new.name
                                ]
                                []
                            ]
                        , div [ class "col-3" ]
                            [ input
                                [ classes "form-control form-control-sm"
                                , type_ "number"
                                , Html.Attributes.min "1"
                                , attribute "aria-label" "Anzahl der Plätze"
                                , onInput (String.toInt >> Maybe.withDefault 0 >> Capacity >> EditFormMsg)
                                , value (new.capacity |> String.fromInt)
                                ]
                                []
                            ]
                        , div [ class "col-1" ]
                            [ button
                                [ classes "btn btn-outline-success btn-sm"
                                , type_ "submit"
                                , title "Speichern"
                                , attribute "aria-label" "Speichern"
                                ]
                                [ svgIconCheckLg ]
                            ]
                        , div [ class "col-1" ]
                            [ button
                                [ classes "btn btn-outline-danger btn-sm"
                                , type_ "button"
                                , title "Abbrechen"
                                , attribute "aria-label" "Abbrechen"
                                , onClick CancelEdit
                                ]
                                [ svgIconXLg ]
                            ]
                        ]
                    ]
