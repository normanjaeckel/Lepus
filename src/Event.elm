module Event exposing (Model, Msg, Obj, init, toVertexList, toVertexListReducer, update, view)

import Algo exposing (Vertex)
import Html exposing (..)
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onInput, onSubmit)



-- MODEL


type alias Model =
    { events : List Obj
    , formData : Obj
    }


init : Model
init =
    Model [] (Obj "" 0)


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


type FormDataInput
    = Name String
    | Capacity Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormDataMsg data ->
            { model | formData = updateFormdata data model.formData }

        Save ->
            { model | formData = Obj "" 0, events = model.events ++ [ model.formData ] }


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
    div []
        [ h2 [] [ text "Gruppen" ]
        , div []
            [ h3 [] [ text "Alle Gruppen" ]
            , allEvents model
            ]
        , div []
            [ h3 [] [ text "Neue Gruppe hinzufügen" ]
            , form [ onSubmit Save ]
                [ label [ for "newEventName" ] [ text "Name" ]
                , input
                    [ id "newEventName"
                    , type_ "text"
                    , onInput (Name >> FormDataMsg)
                    , value model.formData.name
                    ]
                    []
                , label [ for "newEventCapacity" ] [ text "Anzahl der Plätze" ]
                , input
                    [ id "newEventCapacity"
                    , type_ "number"
                    , onInput (String.toInt >> Maybe.withDefault 0 >> Capacity >> FormDataMsg)
                    , value (model.formData.capacity |> String.fromInt)
                    ]
                    []
                , button [ type_ "submit" ] [ text "Speichern" ]
                ]
            ]
        ]


allEvents : Model -> Html Msg
allEvents model =
    if List.isEmpty model.events then
        p [] [ text "Noch keine Gruppen angelegt" ]

    else
        ol
            []
            (model.events
                |> List.map
                    (\event -> li [] [ text <| event.name ++ " (" ++ String.fromInt event.capacity ++ " Plätze)" ])
            )
