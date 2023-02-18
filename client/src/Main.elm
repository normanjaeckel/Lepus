port module Main exposing (main)

import Assignment
import Browser
import Class
import Event
import File
import File.Download
import File.Select
import Helpers exposing (Persistence(..), classes)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy3, lazy4)
import Json.Decode as D
import Json.Encode as E
import Pupil
import Task


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { data : String
    }



-- MODEL


type alias Model =
    { classes : Class.Model
    , events : Event.Model
    , pupils : Pupil.Model
    , assignment : Assignment.Model
    }


init : Flags -> Model
init flags =
    case D.decodeString decoder flags.data of
        Ok model ->
            model

        Err _ ->
            { classes = Class.init
            , events = Event.init
            , pupils = Pupil.init
            , assignment = Assignment.init
            }


decoder : D.Decoder Model
decoder =
    D.map3
        (\c e p -> Model c e p Assignment.init)
        (D.field "classes" Class.decoder)
        (D.field "events" Event.decoder)
        -- TODO: Care about historical data with missing field classes.
        (D.field "pupils" Pupil.decoder)


modelToJSON : Model -> E.Value
modelToJSON model =
    E.object
        [ ( "classes", Class.modelToJSON model.classes )
        , ( "events", Event.modelToJSON model.events )
        , ( "pupils", Pupil.modelToJSON model.pupils )
        ]



-- UPDATE


type Msg
    = ClassMsg Class.Msg
    | EventMsg Event.Msg
    | PupilMsg Pupil.Msg
    | AssignmentMsg Assignment.Msg
    | DeleteAll
    | Export
    | ImportRequested
    | ImportSelected File.File
    | ImportLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        s =
            \updatedModel -> ( updatedModel, modelToJSON updatedModel |> E.encode 0 |> setStorage )
    in
    case msg of
        ClassMsg innerMsg ->
            let
                ( classesModel, pers ) =
                    Class.update innerMsg model.classes
            in
            case pers of
                DontSetStorage ->
                    ( { model | classes = classesModel }, Cmd.none )

                SetStorage ->
                    { model | classes = classesModel } |> s

        EventMsg innerMsg ->
            let
                ( eventsModel, pers ) =
                    Event.update innerMsg model.events
            in
            case pers of
                DontSetStorage ->
                    ( { model | events = eventsModel }, Cmd.none )

                SetStorage ->
                    { model | events = eventsModel, pupils = Pupil.updateEvents eventsModel.events model.pupils } |> s

        PupilMsg innerMsg ->
            let
                ( pupilsModel, pers ) =
                    Pupil.update innerMsg model.pupils model.events.events model.classes.classes
            in
            case pers of
                DontSetStorage ->
                    ( { model | pupils = pupilsModel }, Cmd.none )

                SetStorage ->
                    { model | pupils = pupilsModel } |> s

        AssignmentMsg innerMsg ->
            let
                ( assignmentModel, cmd ) =
                    Assignment.update innerMsg model.assignment
            in
            ( { model | assignment = assignmentModel }, cmd |> Cmd.map AssignmentMsg )

        DeleteAll ->
            init (Flags "") |> s

        Export ->
            ( model, File.Download.string "export.json" "application/json" (modelToJSON model |> E.encode 4) )

        ImportRequested ->
            ( model, File.Select.file [ "application/json" ] ImportSelected )

        ImportSelected file ->
            ( model, File.toString file |> Task.perform ImportLoaded )

        ImportLoaded content ->
            init (Flags content) |> s



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navbar
        , div [ classes "container p-3 pb-5" ]
            [ main_ []
                [ readme
                , lazy Class.view model.classes |> map ClassMsg
                , lazy Event.view model.events |> map EventMsg
                , lazy3 Pupil.view model.pupils model.classes.classes model.events.events |> map PupilMsg
                , lazy4 Assignment.view model.assignment model.pupils.pupils model.classes.classes model.events.events |> map AssignmentMsg
                , admin
                ]
            ]
        ]


navbar : Html Msg
navbar =
    nav [ classes "navbar navbar-expand-md navbar-dark fixed-top bg-dark" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ] [ text "Home" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navbarCollapse"
                , attribute "aria-controls" "navbarCollapse"
                , attribute "aria-expanded" "false"
                , attribute "aria-label" "Toggle navigation"
                ]
                [ span [ class "navbar-toggler-icon" ] []
                ]
            , div [ classes "collapse navbar-collapse", id "navbarCollapse" ]
                [ ul [ classes "navbar-nav me-auto mb-2 mb-md-0" ]
                    [ li [ class "nav-item" ] [ a [ classes "nav-link", href "#classes" ] [ text "Klassen" ] ]
                    , li [ class "nav-item" ] [ a [ classes "nav-link", href "#events" ] [ text "Projektgruppen" ] ]
                    , li [ class "nav-item" ] [ a [ classes "nav-link", href "#pupils" ] [ text "Schüler/Schülerinnen" ] ]
                    , li [ class "nav-item" ] [ a [ classes "nav-link", href "#result" ] [ text "Ergebnis" ] ]
                    , li [ class "nav-item" ] [ a [ classes "nav-link", href "#admin" ] [ text "Administration" ] ]
                    ]
                ]
            ]
        ]


readme : Html Msg
readme =
    div [ class "mb-5" ]
        [ h1 [] [ text "Projektgruppenverteilung" ]
        , p [ classes "fs-5 col-md-9" ] [ text "Dieses Tool speichert die Eingaben im Local Storage des Browsers. Es werden keine eigegebenen Daten über das Internet gesendet." ]
        ]


admin : Html Msg
admin =
    div []
        [ h2 [ id "admin", class "nav-anchor" ] [ text "Administration" ]
        , p [] [ text "Hier kann man die eingegebenen Daten löschen oder exportieren. Beim Import werden alle bisherigen Eingaben überschrieben." ]
        , button [ classes "btn btn-danger", type_ "button", onClick DeleteAll ] [ text "Alle Daten löschen" ]
        , button [ classes "btn btn-secondary ms-2", type_ "button", onClick Export ] [ text "Export" ]
        , button [ classes "btn btn-secondary ms-2", type_ "button", onClick ImportRequested ] [ text "Import" ]
        ]



-- PORTS


port setStorage : String -> Cmd msg
