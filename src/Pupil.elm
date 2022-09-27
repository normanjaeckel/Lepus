module Pupil exposing (Action(..), Choice, ChoiceType(..), Model, Msg, Obj, decoder, eventGroup, init, modelToJSON, pupilDisplay, pupilSorting, update, updateEvents, view)

import Event
import Helpers exposing (classes, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, for, hidden, id, name, placeholder, required, rows, scope, tabindex, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Json.Decode as D
import Json.Encode as E
import Set



-- MODEL


type alias Model =
    { pupils : List Obj
    , formData : FormData
    , formInvalid : Bool
    }


type alias FormData =
    { names : String
    , class : String
    }


init : Model
init =
    Model [] emptyFormData False


emptyFormData : FormData
emptyFormData =
    FormData "" ""


type alias Obj =
    { name : String
    , class : String
    , choices : List Choice
    }


pupilDisplay : Obj -> String
pupilDisplay pupil =
    pupil.name ++ " (Klasse " ++ pupil.class ++ ")"


pupilSorting : Obj -> String
pupilSorting pupil =
    pupil.class ++ pupil.name


type alias Choice =
    { event : Event.Obj
    , type_ : ChoiceType
    }


type ChoiceType
    = Green
    | Yellow
    | Red


choiceTypeToString : ChoiceType -> String
choiceTypeToString c =
    case c of
        Green ->
            "green"

        Yellow ->
            "yellow"

        Red ->
            "red"


eventGroup : ChoiceType -> Obj -> List Event.Obj
eventGroup choiceType pupil =
    pupil.choices
        |> List.filter (\c -> c.type_ == choiceType)
        |> List.map (\c -> c.event)


decoder : D.Decoder Model
decoder =
    D.map
        (\p -> Model p emptyFormData False)
        (D.list
            (D.map3
                Obj
                (D.field "name" D.string)
                (D.field "class" D.string)
                (D.field "choices" (D.list decoderChoice))
            )
        )


decoderChoice : D.Decoder Choice
decoderChoice =
    D.map2
        Choice
        (D.field "event" Event.decoderEvent)
        (D.field "type"
            (D.string
                |> D.andThen
                    (\t ->
                        if t == "green" then
                            D.succeed Green

                        else if t == "yellow" then
                            D.succeed Yellow

                        else if t == "red" then
                            D.succeed Red

                        else
                            D.fail "invalid choice type"
                    )
            )
        )


modelToJSON : Model -> E.Value
modelToJSON model =
    model.pupils |> E.list pupilToJSON


pupilToJSON : Obj -> E.Value
pupilToJSON =
    \p ->
        E.object
            [ ( "name", E.string p.name )
            , ( "class", E.string p.class )
            , ( "choices", p.choices |> E.list choiceToJSON )
            ]


choiceToJSON : Choice -> E.Value
choiceToJSON choice =
    E.object
        [ ( "event", Event.eventToJSON choice.event )
        , ( "type", E.string <| choiceTypeToString choice.type_ )
        ]



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | Delete Obj
    | ChangeChoice Obj Event.Obj ChoiceType


type Action
    = PupilsChanged
    | FormChanged


type FormDataInput
    = Names String
    | Class String


update : Msg -> Model -> List Event.Obj -> Set.Set String -> ( Model, Action )
update msg model events classes =
    case msg of
        FormDataMsg data ->
            ( { model | formData = updateFormdata data model.formData, formInvalid = False }, FormChanged )

        Save ->
            case savePupils model events classes model.formData.names model.formData.class of
                Just pupils ->
                    ( { model | formData = emptyFormData, pupils = pupils, formInvalid = False }, PupilsChanged )

                Nothing ->
                    ( { model | formInvalid = True }, FormChanged )

        Delete pupil ->
            ( { model | pupils = model.pupils |> List.filter ((/=) pupil), formInvalid = False }, PupilsChanged )

        ChangeChoice pupil event choice ->
            ( { model | pupils = changeChoice model.pupils pupil event choice, formInvalid = False }, PupilsChanged )


updateFormdata : FormDataInput -> FormData -> FormData
updateFormdata msg formData =
    case msg of
        Names n ->
            { formData | names = n }

        Class c ->
            { formData | class = c }


savePupils : Model -> List Event.Obj -> Set.Set String -> String -> String -> Maybe (List Obj)
savePupils model events cls namesRaw classRaw =
    let
        cl : String
        cl =
            String.trim classRaw

        names : List String
        names =
            namesRaw |> String.split "\n" |> List.map String.trim |> List.filter ((/=) "")
    in
    if (cl == "") || List.isEmpty names || (not <| Set.member cl cls) then
        Nothing

    else
        let
            ( pupils, err ) =
                let
                    yellowEvents : List Choice
                    yellowEvents =
                        events |> List.map (\e -> Choice e Yellow)

                    fn : String -> ( List Obj, Bool ) -> ( List Obj, Bool )
                    fn =
                        \name ( currentPupils, e ) ->
                            if e || (currentPupils |> List.any (\p -> p.name == name && p.class == cl)) then
                                ( [], True )

                            else
                                ( currentPupils ++ [ Obj name cl yellowEvents ], False )
                in
                names |> List.foldl fn ( model.pupils, False )
        in
        if err then
            Nothing

        else
            Just pupils


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
        [ h2 [ id "pupils", class "nav-anchor" ] [ text "Schüler/Schülerinnen" ]
        , form [ class "mb-3", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Schüler und Schülerinnen der gleichen Klasse hinzufügen" ]
            , div [ classes "row g-3" ]
                [ div [ class "col-md-3" ]
                    (tagWithInvalidFeedback
                        textarea
                        [ class "form-control"
                        , rows 1
                        , placeholder "Namen (mit Zeilenumbrüchen getrennt)"
                        , attribute "aria-label" "Namen (mit Zeilenumbrüchen getrennt)"
                        , required True
                        , onInput (Names >> FormDataMsg)
                        , value model.formData.names
                        ]
                        "newPupilNames"
                        "Schüler/Schülerin ist in dieser Klasse bereits vorhanden oder unbekannte Klasse oder sonst ungültige Eingabe"
                        model.formInvalid
                    )
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
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Hinzufügen" ] ]
                ]
            ]
        , div []
            [ h3 [ hidden True ] [ text "Alle Schüler/Schülerinnen" ]
            , lazy allPupils model.pupils
            ]
        ]


allPupils : List Obj -> Html Msg
allPupils pupils =
    if List.isEmpty pupils then
        p [ hidden True ] [ text "Noch keine Schüler oder Schülerinnen angelegt" ]

    else
        ol [ classes "list-group list-group-flush list-group-numbered" ] (pupils |> List.sortBy pupilSorting |> List.map (lazy onePupilLi))


onePupilLi : Obj -> Html Msg
onePupilLi pupil =
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5 pt-4 pb-2" ]
        [ div [ classes "ms-2 w-100" ]
            [ div [] [ text <| pupilDisplay pupil ]
            , innerTable pupil
            ]
        , a
            [ class "link-danger"
            , title "Löschen"
            , tabindex 0
            , attribute "role" "button"
            , attribute "aria-label" "Löschen"
            , onClick <| Delete pupil
            ]
            [ svgIconXLg ]
        ]


innerTable : Obj -> Html Msg
innerTable pupil =
    table [ classes "table table-striped" ]
        [ thead []
            [ tr []
                [ th [ scope "col" ] [ text "Gruppe" ]
                , th [ scope "col" ] [ text "Farbe" ]
                ]
            ]
        , tbody []
            (pupil.choices
                |> List.sortBy (.event >> .name)
                |> List.map
                    (\c ->
                        let
                            radioGroup : String
                            radioGroup =
                                pupilDisplay pupil ++ c.event.name
                        in
                        tr []
                            [ th [ scope "row" ] [ text c.event.name ]
                            , td []
                                [ div [ class "form-check form-check-inline" ]
                                    [ input
                                        [ class "form-check-input"
                                        , type_ "radio"
                                        , name radioGroup
                                        , id (radioGroup ++ "Green")
                                        , checked (c.type_ == Green)
                                        , onClick <| ChangeChoice pupil c.event Green
                                        ]
                                        []
                                    , label [ class "form-check-label", for (radioGroup ++ "Green") ] [ span [ class "badge text-bg-success" ] [ text "Grün" ] ]
                                    ]
                                , div [ class "form-check form-check-inline" ]
                                    [ input
                                        [ class "form-check-input"
                                        , type_ "radio"
                                        , name radioGroup
                                        , id (radioGroup ++ "Yellow")
                                        , checked (c.type_ == Yellow)
                                        , onClick <| ChangeChoice pupil c.event Yellow
                                        ]
                                        []
                                    , label [ class "form-check-label", for (radioGroup ++ "Yellow") ] [ span [ class "badge text-bg-warning" ] [ text "Gelb" ] ]
                                    ]
                                , div [ class "form-check form-check-inline" ]
                                    [ input
                                        [ class "form-check-input"
                                        , type_ "radio"
                                        , name radioGroup
                                        , id (radioGroup ++ "Red")
                                        , checked (c.type_ == Red)
                                        , onClick <| ChangeChoice pupil c.event Red
                                        ]
                                        []
                                    , label [ class "form-check-label", for (radioGroup ++ "Red") ] [ span [ class "badge text-bg-danger" ] [ text "Rot" ] ]
                                    ]
                                ]
                            ]
                    )
            )
        ]
