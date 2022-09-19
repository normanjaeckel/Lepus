module Event exposing (Action(..), Model, Msg, Obj, decoder, decoderEvent, eventToJSON, extendToCapacity, init, modelToJSON, update, view)

import Helpers exposing (classes, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, placeholder, required, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E



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


extendToCapacity : Obj -> List Obj
extendToCapacity event =
    let
        fn : Obj -> ( Int, List Obj ) -> ( Int, List Obj )
        fn =
            \e t ->
                let
                    newIndex : Int
                    newIndex =
                        Tuple.first t - 1
                in
                ( newIndex, { e | internalID = newIndex } :: Tuple.second t )
    in
    event
        |> List.repeat event.capacity
        |> List.foldl fn ( event.capacity + 1, [] )
        |> Tuple.second


decoder : D.Decoder Model
decoder =
    D.map
        (\p -> Model p emptyFormData False)
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


type Action
    = EventsChanged
    | FormChanged


type FormDataInput
    = Name String
    | Capacity Int


update : Msg -> Model -> ( Model, Action )
update msg model =
    case msg of
        FormDataMsg data ->
            ( { model | formData = updateFormdata data model.formData, formInvalid = False }, FormChanged )

        Save ->
            case validate model of
                Just new ->
                    ( { model
                        | formData = emptyFormData
                        , events = model.events ++ [ new ]
                        , formInvalid = False
                      }
                    , EventsChanged
                    )

                Nothing ->
                    ( { model | formInvalid = True }, FormChanged )

        Delete obj ->
            ( { model | events = model.events |> List.filter ((/=) obj), formInvalid = False }, EventsChanged )


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
        [ h2 [] [ text "Projektgruppen" ]
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
            , allEvents model.events
            ]
        ]


allEvents : List Obj -> Html Msg
allEvents events =
    if List.isEmpty events then
        p [ hidden True ] [ text "Noch keine Gruppen angelegt" ]

    else
        ol [ classes "list-group list-group-flush list-group-numbered" ] (events |> List.sortBy .name |> List.map oneEventLi)


oneEventLi : Obj -> Html Msg
oneEventLi event =
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
        [ div [ classes "ms-2 me-auto" ]
            [ text event.name
            , span [ classes "ms-2 badge bg-primary rounded-pill", title "Anzahl der Plätze", attribute "aria-label" "Anzahl der Plätze" ] [ text <| String.fromInt event.capacity ]
            ]
        , a [ class "link-danger", title "Löschen", attribute "role" "button", attribute "aria-label" "Löschen", onClick <| Delete event ] [ svgIconXLg ]
        ]
