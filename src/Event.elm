module Event exposing (Model, Msg, Obj, init, toVertexList, toVertexListReducer, update, view)

import Algo exposing (Vertex)
import Helpers exposing (classes, svgIconXLg)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, href, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MODEL


type alias Model =
    { events : List Obj
    , formData : Obj
    }


init : Model
init =
    Model [] emptyFormData


emptyFormData : Obj
emptyFormData =
    Obj "" 1


type alias Obj =
    { name : String
    , capacity : Int
    }


toVertexList : Obj -> List Vertex
toVertexList m =
    let
        fn : String -> ( Int, List String ) -> ( Int, List String )
        fn =
            \e t ->
                let
                    newIndex : Int
                    newIndex =
                        Tuple.first t - 1
                in
                ( newIndex, (e ++ "-" ++ String.fromInt newIndex) :: Tuple.second t )
    in
    m.name
        |> List.repeat m.capacity
        |> List.foldl fn ( m.capacity + 1, [] )
        |> Tuple.second


toVertexListReducer : Obj -> List Algo.Vertex -> List Algo.Vertex
toVertexListReducer event list =
    toVertexList event ++ list



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | Delete Obj


type FormDataInput
    = Name String
    | Capacity Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormDataMsg data ->
            { model | formData = updateFormdata data model.formData }

        Save ->
            { model | formData = emptyFormData, events = model.events ++ [ model.formData ] }

        Delete obj ->
            { model | events = model.events |> List.filter ((/=) obj) }


updateFormdata : FormDataInput -> Obj -> Obj
updateFormdata msg formData =
    case msg of
        Name name ->
            { formData | name = name }

        Capacity capacity ->
            { formData | capacity = capacity }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mb-3" ]
        [ h2 [] [ text "Projektgruppen" ]
        , form [ onSubmit Save ]
            [ h3 [ hidden True ] [ text "Neue Gruppe hinzufügen" ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Name"
                        , attribute "aria-label" "Name"
                        , onInput (Name >> FormDataMsg)
                        , value model.formData.name
                        ]
                        []
                    ]
                , div [ class "col" ]
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
                , div [ class "col" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Hinzufügen" ] ]
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
        ol [] (events |> List.map oneEventLi)


oneEventLi : Obj -> Html Msg
oneEventLi event =
    li [ class "mb-3" ]
        [ span [ class "me-3" ] [ text <| event.name ++ " (" ++ String.fromInt event.capacity ++ " Plätze)" ]
        , a [ class "link-danger", title "Löschen", href "#", attribute "aria-label" "Löschen", onClick <| Delete event ] [ svgIconXLg ]
        ]
