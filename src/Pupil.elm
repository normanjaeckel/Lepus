module Pupil exposing (Choice, ChoiceType(..), Model, Msg, Obj, eventGroup, init, toVertex, update, updateEvents, view)

import Algo
import Event
import Html exposing (..)
import Html.Attributes exposing (for, id, rows, type_, value)
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



-- greenEvents : Obj -> List Event.Obj
-- greenEvents pupil =
--     pupil.choices
--         |> List.filter
--             (\c ->
--                 case c.type_ of
--                     Green ->
--                         True
--                     _ ->
--                         False
--             )
--         |> List.map (\c -> c.event)
-- yellowEvents : Obj -> List Event.Obj
-- yellowEvents pupil =
--     pupil.choices
--         |> List.filter
--             (\c ->
--                 case c.type_ of
--                     Yellow ->
--                         True
--                     _ ->
--                         False
--             )
--         |> List.map (\c -> c.event)
-- -- events
-- --     |> List.filter
-- --         (\e ->
-- --             pupil.choices
-- --                 |> List.any
-- --                     (\c ->
-- --                         c.event == e
-- --                     )
-- --                 |> not
-- --         )
-- greenAndYellowEvents : Obj -> List Event.Obj
-- greenAndYellowEvents pupil =
--     greenEvents pupil ++ yellowEvents pupil
-- redEvents : Obj -> List Event.Obj
-- redEvents p =
--     p.choices
--         |> List.filter
--             (\c ->
--                 case c.type_ of
--                     Red ->
--                         True
--                     _ ->
--                         False
--             )
--         |> List.map (\c -> c.event)
-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | MultiSave
    | Delete Obj


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

        Delete obj ->
            { model | pupils = model.pupils |> List.filter ((/=) obj) }


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
    div []
        [ h2 [] [ text "Schüler/Schülerinnen" ]
        , div []
            [ h3 [] [ text "Alle Schüler/Schülerinnen" ]
            , allPupils model.pupils
            ]
        , div []
            [ h3 [] [ text "Neuen Schüler oder neue Schülerin hinzufügen" ]
            , form [ onSubmit Save ]
                [ label [ for "newPupilName" ] [ text "Name" ]
                , input
                    [ id "newPupilName"
                    , type_ "text"
                    , onInput (Name >> FormDataMsg)
                    , value model.formData.name
                    ]
                    []
                , label [ for "newPupilClass" ] [ text "Klasse" ]
                , input
                    [ id "newPupilClass"
                    , type_ "text"
                    , onInput (Class >> FormDataMsg)
                    , value model.formData.class
                    ]
                    []
                , button [ type_ "submit" ] [ text "Speichern" ]
                ]
            ]
        , div []
            [ h3 [] [ text "Mehrere Schüler und Schülerinnen der gleichen Klasse hinzufügen" ]
            , form [ onSubmit MultiSave ]
                [ label [ for "newPupilMultiClass" ] [ text "Klasse" ]
                , input
                    [ id "newPupilMultiClass"
                    , type_ "text"
                    , onInput (MultiClass >> FormDataMsg)
                    , value model.formData.multiClass
                    ]
                    []
                , label [ for "newPupilMultiName" ] [ text "Namen (mit Komma getrennt)" ]
                , textarea
                    [ rows 5
                    , id "newPupilMultiName"
                    , onInput (MultiNames >> FormDataMsg)
                    , value model.formData.multiNames
                    ]
                    []
                , button [ type_ "submit" ] [ text "Speichern" ]
                ]
            ]
        ]


allPupils : List Obj -> Html Msg
allPupils pupils =
    if List.isEmpty pupils then
        p [] [ text "Noch keine Schüler oder Schülerinnen angelegt" ]

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
        ul [] (events |> List.map (oneEventLi choice))


oneEventLi : ChoiceType -> Event.Obj -> Html Msg
oneEventLi choice event =
    let
        buttons : List (Html msg)
        buttons =
            case choice of
                Green ->
                    [ button [] [ text "zu Gelb" ] ]

                Yellow ->
                    [ button [] [ text "zu Grün" ], button [] [ text "zu Rot" ] ]

                Red ->
                    [ button [] [ text "zu Gelb" ] ]
    in
    li [] (text event.name :: buttons)
