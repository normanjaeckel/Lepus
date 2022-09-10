module Pupil exposing (Choice, ChoiceType(..), Model, Msg, Obj, eventGroup, init, toVertex, update, updateEvents, view)

import Algo
import Event
import Helpers exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, placeholder, required, rows, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)



-- MODEL


type alias Model =
    { pupils : List Obj
    , formData : FormData
    }


type alias FormData =
    { name : String
    , class : String
    , multiClass : String
    , multiNames : String
    }


init : Model
init =
    Model [] emptyFormData


emptyFormData : FormData
emptyFormData =
    FormData "" "" "" ""


isValidNameOrClass : String -> Bool
isValidNameOrClass name =
    String.trim name /= ""


type alias Obj =
    { name : String
    , class : String
    , choices : List Choice
    }


type alias Choice =
    { event : Event.Obj
    , type_ : ChoiceType
    }


type ChoiceType
    = Green
    | Yellow
    | Red


toVertex : Obj -> Algo.Vertex
toVertex m =
    m.name ++ " (" ++ m.class ++ ")"


eventGroup : ChoiceType -> Obj -> List Event.Obj
eventGroup choice pupil =
    pupil.choices
        |> List.filter (\c -> c.type_ == choice)
        |> List.map (\c -> c.event)



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | MultiSave
    | Delete Obj
    | ChangeChoice Obj Event.Obj ChoiceType


type FormDataInput
    = Name String
    | Class String
    | MultiClass String
    | MultiNames String


update : Msg -> Model -> List Event.Obj -> Model
update msg model events =
    case msg of
        FormDataMsg data ->
            { model | formData = updateFormdata data model.formData }

        Save ->
            saveSingle model events

        MultiSave ->
            saveMulti model events

        Delete pupil ->
            { model | pupils = model.pupils |> List.filter ((/=) pupil) }

        ChangeChoice pupil event choice ->
            { model | pupils = changeChoice model.pupils pupil event choice }


updateFormdata : FormDataInput -> FormData -> FormData
updateFormdata msg formData =
    case msg of
        Name name ->
            { formData | name = name }

        Class class ->
            { formData | class = class }

        MultiClass class ->
            { formData | multiClass = class }

        MultiNames names ->
            { formData | multiNames = names }


saveSingle : Model -> List Event.Obj -> Model
saveSingle model events =
    if isValidNameOrClass model.formData.name && isValidNameOrClass model.formData.class then
        { model
            | formData = emptyFormData
            , pupils =
                model.pupils
                    ++ [ Obj
                            (model.formData.name |> String.trim)
                            (model.formData.class |> String.trim)
                            (events |> List.map (\e -> Choice e Yellow))
                       ]
        }

    else
        model


saveMulti : Model -> List Event.Obj -> Model
saveMulti model events =
    let
        yellowEvents : List Choice
        yellowEvents =
            events |> List.map (\e -> Choice e Yellow)

        newPupils : List Obj
        newPupils =
            model.formData.multiNames
                |> String.split ","
                |> List.filter isValidNameOrClass
                |> List.map
                    (\n ->
                        Obj (n |> String.trim) (model.formData.multiClass |> String.trim) yellowEvents
                    )
    in
    if not <| isValidNameOrClass model.formData.multiClass || List.isEmpty newPupils then
        model

    else
        { model | formData = emptyFormData, pupils = model.pupils ++ newPupils }


changeChoice : List Obj -> Obj -> Event.Obj -> ChoiceType -> List Obj
changeChoice pupils pupil event newChoiceType =
    pupils
        |> List.map
            (\p ->
                if p == pupil then
                    { p
                        | choices =
                            p.choices
                                |> List.map
                                    (\c ->
                                        if c.event == event then
                                            Choice event newChoiceType

                                        else
                                            c
                                    )
                    }

                else
                    p
            )


updateEvents : List Event.Obj -> Model -> Model
updateEvents events model =
    let
        fn1 : Event.Obj -> List Choice -> List Choice
        fn1 =
            \e cl ->
                if List.member e (cl |> List.map .event) then
                    cl

                else
                    Choice e Yellow :: cl

        fn2 : Choice -> Bool
        fn2 =
            \c -> List.member c.event events
    in
    { model
        | pupils =
            model.pupils
                |> List.map (\p -> { p | choices = events |> List.foldl fn1 p.choices |> List.filter fn2 })
    }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mb-3" ]
        [ h2 [] [ text "Schüler/Schülerinnen" ]
        , form [ class "mb-2", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Neuen Schüler oder neue Schülerin hinzufügen" ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Name"
                        , attribute "aria-label" "Name"
                        , required True
                        , onInput (Name >> FormDataMsg)
                        , value model.formData.name
                        ]
                        []
                    ]
                , div [ class "col" ]
                    [ input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Klasse"
                        , attribute "aria-label" "Klasse"
                        , required True
                        , onInput (Class >> FormDataMsg)
                        , value model.formData.class
                        ]
                        []
                    ]
                , div [ class "col" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Eine/n Hinzufügen" ] ]
                ]
            ]
        , form [ onSubmit MultiSave ]
            [ h3 [ hidden True ] [ text "Mehrere Schüler und Schülerinnen der gleichen Klasse hinzufügen" ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Klasse"
                        , attribute "aria-label" "Klasse"
                        , required True
                        , onInput (MultiClass >> FormDataMsg)
                        , value model.formData.multiClass
                        ]
                        []
                    ]
                , div [ class "col" ]
                    [ textarea
                        [ class "form-control"
                        , rows 1
                        , placeholder "Namen (mit Komma getrennt)"
                        , attribute "aria-label" "Namen (mit Komma getrennt)"
                        , required True
                        , onInput (MultiNames >> FormDataMsg)
                        , value model.formData.multiNames
                        ]
                        []
                    ]
                , div [ class "col" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Mehrere Hinzufügen" ] ]
                ]
            ]
        , div []
            [ h3 [ hidden True ] [ text "Alle Schüler/Schülerinnen" ]
            , allPupils model.pupils
            ]
        ]


allPupils : List Obj -> Html Msg
allPupils pupils =
    if List.isEmpty pupils then
        p [ hidden True ] [ text "Noch keine Schüler oder Schülerinnen angelegt" ]

    else
        ol [] (pupils |> List.map onePupilLi)


onePupilLi : Obj -> Html Msg
onePupilLi pupil =
    li []
        [ h4 []
            [ text <| pupil.name ++ " (Klasse " ++ pupil.class ++ ")"
            , button [ type_ "button", onClick <| Delete pupil ] [ text "Löschen" ]
            ]
        , div []
            [ h5 [] [ text "Grün" ]
            , pupil |> eventList Green
            ]
        , div []
            [ h5 [] [ text "Gelb" ]
            , pupil |> eventList Yellow
            ]
        , div []
            [ h5 [] [ text "Rot" ]
            , pupil |> eventList Red
            ]
        ]


eventList : ChoiceType -> Obj -> Html Msg
eventList choice pupil =
    let
        events : List Event.Obj
        events =
            pupil |> eventGroup choice
    in
    if List.isEmpty events then
        div [] [ text "keine" ]

    else
        ul [] (events |> List.map (oneEventLi choice pupil))


oneEventLi : ChoiceType -> Obj -> Event.Obj -> Html Msg
oneEventLi choice pupil event =
    let
        buttons : List (Html Msg)
        buttons =
            case choice of
                Green ->
                    [ button [ onClick <| ChangeChoice pupil event Yellow ] [ text "zu Gelb" ] ]

                Yellow ->
                    [ button [ onClick <| ChangeChoice pupil event Green ] [ text "zu Grün" ]
                    , button [ onClick <| ChangeChoice pupil event Red ] [ text "zu Rot" ]
                    ]

                Red ->
                    [ button [ onClick <| ChangeChoice pupil event Yellow ] [ text "zu Gelb" ] ]
    in
    li [] (text event.name :: buttons)
