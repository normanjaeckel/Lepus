module Pupil exposing (Choice, ChoiceType(..), Model, Msg, Obj, greenAndYellowEvents, greenEvents, init, redEvents, toVertex, update, view, yellowEvents)

import Algo
import Event
import Html exposing (..)
import Html.Attributes exposing (for, id, rows, type_, value)
import Html.Events exposing (onInput, onSubmit)



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
    | Red


toVertex : Obj -> Algo.Vertex
toVertex m =
    m.name ++ " (" ++ m.class ++ ")"


greenEvents : Obj -> List Event.Obj
greenEvents pupil =
    pupil.choices
        |> List.filter
            (\c ->
                case c.type_ of
                    Red ->
                        False

                    Green ->
                        True
            )
        |> List.map (\c -> c.event)


yellowEvents : List Event.Obj -> Obj -> List Event.Obj
yellowEvents events pupil =
    events
        |> List.filter
            (\e ->
                pupil.choices
                    |> List.any
                        (\c ->
                            c.event == e
                        )
                    |> not
            )


greenAndYellowEvents : List Event.Obj -> Obj -> List Event.Obj
greenAndYellowEvents events pupil =
    greenEvents pupil ++ yellowEvents events pupil


redEvents : Obj -> List Event.Obj
redEvents p =
    p.choices
        |> List.filter
            (\c ->
                case c.type_ of
                    Red ->
                        True

                    Green ->
                        False
            )
        |> List.map (\c -> c.event)



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | MultiSave


type FormDataInput
    = Name String
    | Class String
    | MultiClass String
    | MultiNames String


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormDataMsg data ->
            { model | formData = updateFormdata data model.formData }

        Save ->
            saveSingle model

        MultiSave ->
            saveMulti model


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


saveSingle : Model -> Model
saveSingle model =
    if isValidNameOrClass model.formData.name && isValidNameOrClass model.formData.class then
        { model
            | formData = emptyFormData
            , pupils =
                model.pupils
                    ++ [ Obj
                            (model.formData.name |> String.trim)
                            (model.formData.class |> String.trim)
                            []
                       ]
        }

    else
        model


saveMulti : Model -> Model
saveMulti model =
    let
        newPupils : List Obj
        newPupils =
            model.formData.multiNames
                |> String.split ","
                |> List.filter isValidNameOrClass
                |> List.map (\n -> Obj (n |> String.trim) (model.formData.multiClass |> String.trim) [])
    in
    if not <| isValidNameOrClass model.formData.multiClass || List.isEmpty newPupils then
        model

    else
        { model | formData = emptyFormData, pupils = model.pupils ++ newPupils }



-- VIEW


view : Model -> List Event.Obj -> Html Msg
view model allEvents =
    div []
        [ h2 [] [ text "Schüler/Schülerinnen" ]
        , div []
            [ h3 [] [ text "Alle Schüler/Schülerinnen" ]
            , allPupils model allEvents
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


allPupils : Model -> List Event.Obj -> Html Msg
allPupils model allEvents =
    if List.isEmpty model.pupils then
        p [] [ text "Noch keine Schüler oder Schülerinnen angelegt" ]

    else
        ol [] (model.pupils |> List.map (onePupil allEvents))


onePupil : List Event.Obj -> Obj -> Html Msg
onePupil allEvents pupil =
    li []
        [ h4 [] [ text <| pupil.name ++ " (Klasse " ++ pupil.class ++ ")" ]
        , div []
            [ h5 [] [ text "Grün" ]
            , eventList <| greenEvents pupil
            ]
        , div []
            [ h5 [] [ text "Gelb" ]
            , eventList <| yellowEvents allEvents pupil
            ]
        , div []
            [ h5 [] [ text "Rot" ]
            , eventList <| redEvents pupil
            ]
        ]


eventList : List Event.Obj -> Html Msg
eventList events =
    if List.isEmpty events then
        div [] [ text "keine" ]

    else
        ul [] (events |> List.map (\e -> li [] [ text e.name ]))
