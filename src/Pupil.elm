module Pupil exposing (Choice, ChoiceType(..), Model, Msg, Obj, eventGroup, init, toVertex, update, updateEvents, view)

import Algo
import Event
import Helpers exposing (classes, isValidNameOrClass, svgIconArrowDown, svgIconArrowUp, svgIconXLg)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, href, placeholder, required, rows, title, type_, value)
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
    | MultiNames String
    | MultiClass String


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

        MultiNames names ->
            { formData | multiNames = names }

        MultiClass class ->
            { formData | multiClass = class }


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
    div [ class "mb-5" ]
        [ h2 [] [ text "Schüler/Schülerinnen" ]
        , form [ class "mb-3", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Neuen Schüler oder neue Schülerin hinzufügen" ]
            , div [ class "row g-3" ]
                [ div [ class "col-md-3" ]
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
                , div [ class "col-md-3" ]
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
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Eine/n Hinzufügen" ] ]
                ]
            ]
        , form [ class "mb-3", onSubmit MultiSave ]
            [ h3 [ hidden True ] [ text "Mehrere Schüler und Schülerinnen der gleichen Klasse hinzufügen" ]
            , div [ class "row g-3" ]
                [ div [ class "col-md-3" ]
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
                , div [ class "col-md-3" ]
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
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Mehrere Hinzufügen" ] ]
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
        ol [ classes "list-group list-group-flush list-group-numbered" ] (pupils |> List.map onePupilLi)


onePupilLi : Obj -> Html Msg
onePupilLi pupil =
    let
        innerLi : ChoiceType -> Html Msg
        innerLi =
            \ct ->
                let
                    s : Html Msg
                    s =
                        case ct of
                            Green ->
                                span [ classes "badge text-bg-success" ] [ text "Grün" ]

                            Yellow ->
                                span [ classes "badge text-bg-warning" ] [ text "Gelb" ]

                            Red ->
                                span [ classes "badge text-bg-danger" ] [ text "Rot" ]
                in
                li [ class "list-group-item d-flex justify-content-between align-items-start" ]
                    [ div [ class "row container-fluid" ]
                        [ div [ class "col-2" ] [ s ]
                        , div [ class "col-10" ] [ pupil |> eventList ct ]
                        ]
                    ]
    in
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
        [ div [ class "ms-2 w-100" ]
            [ div [] [ text <| pupil.name ++ " (Klasse " ++ pupil.class ++ ")" ]
            , ul [ classes "list-group list-group-flush" ]
                [ innerLi Green, innerLi Yellow, innerLi Red ]
            ]
        , a [ class "link-danger", title "Löschen", href "#", attribute "aria-label" "Löschen", onClick <| Delete pupil ] [ svgIconXLg ]
        ]


eventList : ChoiceType -> Obj -> Html Msg
eventList choice pupil =
    let
        events : List Event.Obj
        events =
            pupil |> eventGroup choice
    in
    if List.isEmpty events then
        span [ class "ms-3" ] [ text "–" ]

    else
        ul [ classes "list-group list-group-flush " ] (events |> List.map (oneEventLi choice pupil))


oneEventLi : ChoiceType -> Obj -> Event.Obj -> Html Msg
oneEventLi choice pupil event =
    let
        buttons : List (Html Msg)
        buttons =
            case choice of
                Green ->
                    [ button
                        [ class "btn btn-outline-warning"
                        , title "zu Gelb"
                        , attribute "aria-label" "zu Gelb"
                        , onClick <| ChangeChoice pupil event Yellow
                        ]
                        [ svgIconArrowDown ]
                    ]

                Yellow ->
                    [ button
                        [ class "btn btn-outline-success"
                        , title "zu Grün"
                        , attribute "aria-label" "zu Grün"
                        , onClick <| ChangeChoice pupil event Green
                        ]
                        [ svgIconArrowUp ]
                    , button
                        [ class "btn btn-outline-danger ms-1"
                        , title "zu Rot"
                        , attribute "aria-label" "zu Rot"
                        , onClick <| ChangeChoice pupil event Red
                        ]
                        [ svgIconArrowDown ]
                    ]

                Red ->
                    [ button
                        [ class "btn btn-outline-warning"
                        , title "zu Gelb"
                        , attribute "aria-label" "zu Gelb"
                        , onClick <| ChangeChoice pupil event Yellow
                        ]
                        [ svgIconArrowUp ]
                    ]
    in
    li [ class "list-group-item d-flex justify-content-between align-items-start" ] [ text event.name, span [] buttons ]
