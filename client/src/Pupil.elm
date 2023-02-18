module Pupil exposing (ChoiceType(..), Model, Msg, Obj, decoder, eventGroup, init, modelToJSON, pupilDisplay, pupilSorting, update, updateEvents, view)

import Class
import Dict
import Event
import Helpers exposing (Persistence(..), classes, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, colspan, for, hidden, id, name, placeholder, required, rows, scope, tabindex, title, type_, value)
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
    , class : Class.Classname
    , choices : Dict.Dict Int ChoiceType
    }


pupilDisplay : Obj -> String
pupilDisplay pupil =
    pupil.name ++ " (Klasse " ++ pupil.class ++ ")"


pupilSorting : Obj -> String
pupilSorting pupil =
    pupil.class ++ pupil.name


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


eventGroup : ChoiceType -> Obj -> List Int
eventGroup choiceType pupil =
    pupil.choices
        |> Dict.filter (\_ c -> c == choiceType)
        |> Dict.keys


decoder : D.Decoder Model
decoder =
    D.map
        (\p -> Model p emptyFormData False)
        (D.list
            (D.map3
                (\n c ch ->
                    Obj
                        n
                        c
                        (ch
                            |> Dict.toList
                            |> List.map (\( k, v ) -> ( String.toInt k |> Maybe.withDefault 0, v ))
                            |> Dict.fromList
                        )
                )
                (D.field "name" D.string)
                (D.field "class" D.string)
                (D.field "choices" (D.dict decoderChoiceType))
            )
        )


decoderChoiceType : D.Decoder ChoiceType
decoderChoiceType =
    D.string
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


modelToJSON : Model -> E.Value
modelToJSON model =
    model.pupils |> E.list pupilToJSON


pupilToJSON : Obj -> E.Value
pupilToJSON =
    \p ->
        E.object
            [ ( "name", E.string p.name )
            , ( "class", E.string p.class )
            , ( "choices"
              , p.choices
                    |> E.dict
                        String.fromInt
                        (choiceTypeToString >> E.string)
              )
            ]



-- UPDATE


type Msg
    = FormDataMsg FormDataInput
    | Save
    | Delete Obj
    | ChangeChoice Obj Event.Id ChoiceType


type FormDataInput
    = Names String
    | Class String


update : Msg -> Model -> Dict.Dict Int Event.Obj -> Set.Set Class.Classname -> ( Model, Persistence )
update msg model events classes =
    case msg of
        FormDataMsg data ->
            ( { model | formData = updateFormdata data model.formData, formInvalid = False }, DontSetStorage )

        Save ->
            case savePupils model events classes model.formData.names model.formData.class of
                Just pupils ->
                    ( { model | formData = emptyFormData, pupils = pupils, formInvalid = False }, SetStorage )

                Nothing ->
                    ( { model | formInvalid = True }, DontSetStorage )

        Delete pupil ->
            ( { model | pupils = model.pupils |> List.filter ((/=) pupil), formInvalid = False }, SetStorage )

        ChangeChoice pupil event choice ->
            ( { model | pupils = changeChoice model.pupils pupil event choice, formInvalid = False }, SetStorage )


updateFormdata : FormDataInput -> FormData -> FormData
updateFormdata msg formData =
    case msg of
        Names n ->
            { formData | names = n }

        Class c ->
            { formData | class = c }


savePupils : Model -> Dict.Dict Int Event.Obj -> Set.Set Class.Classname -> String -> String -> Maybe (List Obj)
savePupils model events cls namesRaw classRaw =
    let
        cl : Class.Classname
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
                    yellowEvents : Dict.Dict Int ChoiceType
                    yellowEvents =
                        events |> Dict.map (\_ _ -> Yellow)

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


changeChoice : List Obj -> Obj -> Event.Id -> ChoiceType -> List Obj
changeChoice pupils pupil event newChoiceType =
    pupils
        |> List.map
            (\p ->
                if p == pupil then
                    let
                        newChoices : Dict.Dict Int ChoiceType
                        newChoices =
                            p.choices |> Dict.update (event |> Event.idToInt) (\_ -> Just newChoiceType)
                    in
                    { p | choices = newChoices }

                else
                    p
            )


updateEvents : Dict.Dict Int Event.Obj -> Model -> Model
updateEvents events model =
    let
        newChoices : Dict.Dict Int ChoiceType -> Dict.Dict Int ChoiceType
        newChoices =
            \current ->
                Dict.merge
                    -- If the key is in only in events, it is a new event. Add it with default color.
                    (\i _ r -> r |> Dict.insert i Yellow)
                    -- If the key is in both, the event is unchanged. Add it with current color.
                    (\i _ c r -> r |> Dict.insert i c)
                    -- If the key is only in current, it is a deleted event. Do not add it.
                    (\_ _ r -> r)
                    events
                    current
                    Dict.empty
    in
    { model | pupils = model.pupils |> List.map (\p -> { p | choices = newChoices p.choices }) }



-- VIEW


view : Model -> Set.Set Class.Classname -> Dict.Dict Int Event.Obj -> Html Msg
view model cls events =
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
                    [ select
                        [ class "form-select"
                        , attribute "aria-label" "Klasse"
                        , required True
                        , onInput (Class >> FormDataMsg)
                        ]
                        (option [ value "", hidden True ] [ text "Klasse bitte auswählen" ]
                            :: (cls
                                    |> Set.toList
                                    |> List.map (\c -> option [ value c ] [ text c ])
                               )
                        )
                    ]
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Hinzufügen" ] ]
                ]
            ]
        , div []
            [ h3 [ hidden True ] [ text "Alle Schüler/Schülerinnen" ]
            , lazy (allPupils events) model.pupils
            ]
        ]


allPupils : Dict.Dict Int Event.Obj -> List Obj -> Html Msg
allPupils events pupils =
    if List.isEmpty pupils then
        p [ hidden True ] [ text "Noch keine Schüler oder Schülerinnen angelegt" ]

    else
        ol [ classes "list-group list-group-flush list-group-numbered" ] (pupils |> List.sortBy pupilSorting |> List.map (lazy <| onePupilLi events))


onePupilLi : Dict.Dict Int Event.Obj -> Obj -> Html Msg
onePupilLi events pupil =
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-9 col-lg-7 col-xl-7 pt-4 pb-2" ]
        [ div [ classes "ms-2 w-100" ]
            [ div [] [ text <| pupilDisplay pupil ]
            , div [ class "table-responsive" ] [ innerTable events pupil ]
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


innerTable : Dict.Dict Int Event.Obj -> Obj -> Html Msg
innerTable events pupil =
    let
        getEventName : ( Int, ChoiceType ) -> String
        getEventName =
            \( i, _ ) ->
                events
                    |> Dict.get i
                    |> Maybe.andThen (Just << .name)
                    |> Maybe.withDefault "---"
    in
    if Dict.isEmpty events then
        div [] []

    else
        table [ classes "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ scope "col", class "col-9" ] [ text "Gruppe" ]
                    , th [ scope "col", colspan 3, class "col-3" ] [ text "Farbe" ]
                    ]
                ]
            , tbody []
                (pupil.choices
                    |> Dict.toList
                    |> List.sortBy getEventName
                    |> List.map
                        (\( i, c ) ->
                            let
                                radioGroup : String
                                radioGroup =
                                    pupilDisplay pupil ++ (i |> String.fromInt)
                            in
                            tr []
                                [ th [ scope "row" ] [ text <| getEventName ( i, c ) ]
                                , td []
                                    [ div [ class "form-check form-check-inline" ]
                                        [ input
                                            [ class "form-check-input"
                                            , type_ "radio"
                                            , name radioGroup
                                            , id (radioGroup ++ "Green")
                                            , checked (c == Green)
                                            , onClick <| ChangeChoice pupil (i |> Event.intToId) Green
                                            ]
                                            []
                                        , label [ class "form-check-label", for (radioGroup ++ "Green") ] [ span [ class "badge text-bg-success" ] [ text "Grün" ] ]
                                        ]
                                    ]
                                , td []
                                    [ div [ class "form-check form-check-inline" ]
                                        [ input
                                            [ class "form-check-input"
                                            , type_ "radio"
                                            , name radioGroup
                                            , id (radioGroup ++ "Yellow")
                                            , checked (c == Yellow)
                                            , onClick <| ChangeChoice pupil (i |> Event.intToId) Yellow
                                            ]
                                            []
                                        , label [ class "form-check-label", for (radioGroup ++ "Yellow") ] [ span [ class "badge text-bg-warning" ] [ text "Gelb" ] ]
                                        ]
                                    ]
                                , td []
                                    [ div [ class "form-check form-check-inline" ]
                                        [ input
                                            [ class "form-check-input"
                                            , type_ "radio"
                                            , name radioGroup
                                            , id (radioGroup ++ "Red")
                                            , checked (c == Red)
                                            , onClick <| ChangeChoice pupil (i |> Event.intToId) Red
                                            ]
                                            []
                                        , label [ class "form-check-label", for (radioGroup ++ "Red") ] [ span [ class "badge text-bg-danger" ] [ text "Rot" ] ]
                                        ]
                                    ]
                                ]
                        )
                )
            ]
