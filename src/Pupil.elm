module Pupil exposing (Choice, ChoiceType(..), Model, Msg, Obj, decoder, eventGroup, init, modelToJSON, toVertex, update, updateEvents, view)

import Algo
import Event
import Helpers exposing (classes, svgIconArrowDown, svgIconArrowUp, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, placeholder, required, rows, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E



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


toVertex : Obj -> Algo.Vertex
toVertex m =
    m.name ++ " (" ++ m.class ++ ")"


eventGroup : ChoiceType -> Obj -> List Event.Obj
eventGroup choice pupil =
    pupil.choices
        |> List.filter (\c -> c.type_ == choice)
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


type FormDataInput
    = Names String
    | Class String


update : Msg -> Model -> List Event.Obj -> Model
update msg model events =
    case msg of
        FormDataMsg data ->
            { model | formData = updateFormdata data model.formData, formInvalid = False }

        Save ->
            case savePupils model events model.formData.names model.formData.class of
                Just pupils ->
                    { model | formData = emptyFormData, pupils = pupils, formInvalid = False }

                Nothing ->
                    { model | formInvalid = True }

        Delete pupil ->
            { model | pupils = model.pupils |> List.filter ((/=) pupil), formInvalid = False }

        ChangeChoice pupil event choice ->
            { model | pupils = changeChoice model.pupils pupil event choice, formInvalid = False }


updateFormdata : FormDataInput -> FormData -> FormData
updateFormdata msg formData =
    case msg of
        Names n ->
            { formData | names = n }

        Class c ->
            { formData | class = c }


savePupils : Model -> List Event.Obj -> String -> String -> Maybe (List Obj)
savePupils model events namesRaw classRaw =
    let
        class : String
        class =
            String.trim classRaw

        names : List String
        names =
            namesRaw |> String.split "," |> List.map String.trim |> List.filter ((/=) "")
    in
    if (class == "") || List.isEmpty names then
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
                            if e || (currentPupils |> List.any (\p -> p.name == name && p.class == class)) then
                                ( [], True )

                            else
                                ( currentPupils ++ [ Obj name class yellowEvents ], False )
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
        [ h2 [] [ text "Schüler/Schülerinnen" ]
        , form [ class "mb-3", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Mehrere Schüler und Schülerinnen der gleichen Klasse hinzufügen" ]
            , div [ classes "row g-3" ]
                [ div [ class "col-md-3" ]
                    (tagWithInvalidFeedback
                        textarea
                        [ class "form-control"
                        , rows 1
                        , placeholder "Namen (mit Komma getrennt)"
                        , attribute "aria-label" "Namen (mit Komma getrennt)"
                        , required True
                        , onInput (Names >> FormDataMsg)
                        , value model.formData.names
                        ]
                        "newPupilNames"
                        "Schüler/Schülerin ist in dieser Klasse bereits vorhanden oder sonst ungültige Eingabe"
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
                li [ classes "list-group-item d-flex justify-content-between align-items-start" ]
                    [ div [ classes "row container-fluid" ]
                        [ div [ class "col-2" ] [ s ]
                        , div [ class "col-10" ] [ pupil |> eventList ct ]
                        ]
                    ]
    in
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
        [ div [ classes "ms-2 w-100" ]
            [ div [] [ text <| pupil.name ++ " (Klasse " ++ pupil.class ++ ")" ]
            , ul [ classes "list-group list-group-flush" ]
                [ innerLi Green, innerLi Yellow, innerLi Red ]
            ]
        , a [ class "link-danger", title "Löschen", attribute "role" "button", attribute "aria-label" "Löschen", onClick <| Delete pupil ] [ svgIconXLg ]
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
                        [ classes "btn btn-outline-warning"
                        , title "zu Gelb"
                        , attribute "aria-label" "zu Gelb"
                        , onClick <| ChangeChoice pupil event Yellow
                        ]
                        [ svgIconArrowDown ]
                    ]

                Yellow ->
                    [ button
                        [ classes "btn btn-outline-success"
                        , title "zu Grün"
                        , attribute "aria-label" "zu Grün"
                        , onClick <| ChangeChoice pupil event Green
                        ]
                        [ svgIconArrowUp ]
                    , button
                        [ classes "btn btn-outline-danger ms-1"
                        , title "zu Rot"
                        , attribute "aria-label" "zu Rot"
                        , onClick <| ChangeChoice pupil event Red
                        ]
                        [ svgIconArrowDown ]
                    ]

                Red ->
                    [ button
                        [ classes "btn btn-outline-warning"
                        , title "zu Gelb"
                        , attribute "aria-label" "zu Gelb"
                        , onClick <| ChangeChoice pupil event Yellow
                        ]
                        [ svgIconArrowUp ]
                    ]
    in
    li [ classes "list-group-item d-flex justify-content-between align-items-start" ] [ text event.name, span [] buttons ]
