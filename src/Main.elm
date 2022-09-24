port module Main exposing (main)

import Assignment
import Browser
import Event
import File
import File.Download
import File.Select
import Helpers exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as D
import Json.Encode as E
import Pupil
import Task


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { pupils : Pupil.Model
    , events : Event.Model
    , assignment : Assignment.Model
    }


init : String -> ( Model, Cmd Msg )
init s =
    case D.decodeString decoder s of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( { pupils = Pupil.init
              , events = Event.init
              , assignment = Assignment.init
              }
            , Cmd.none
            )


decoder : D.Decoder Model
decoder =
    D.map2
        (\p e -> Model p e Assignment.init)
        (D.field "pupils" Pupil.decoder)
        (D.field "events" Event.decoder)


modelToJSON : Model -> E.Value
modelToJSON model =
    E.object
        [ ( "pupils", Pupil.modelToJSON model.pupils )
        , ( "events", Event.modelToJSON model.events )
        ]



-- UPDATE


type Msg
    = EventMsg Event.Msg
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
        EventMsg innerMsg ->
            let
                ( eventsModel, action ) =
                    Event.update innerMsg model.events
            in
            case action of
                Event.FormChanged ->
                    ( { model | events = eventsModel }, Cmd.none )

                Event.EventsChanged ->
                    { model | events = eventsModel, pupils = Pupil.updateEvents eventsModel.events model.pupils } |> s

        PupilMsg innerMsg ->
            let
                ( pupilsModel, action ) =
                    Pupil.update innerMsg model.pupils model.events.events
            in
            case action of
                Pupil.FormChanged ->
                    ( { model | pupils = pupilsModel }, Cmd.none )

                Pupil.PupilsChanged ->
                    { model | pupils = pupilsModel } |> s

        AssignmentMsg innerMsg ->
            ( { model | assignment = Assignment.update innerMsg model.assignment }, Cmd.none )

        DeleteAll ->
            (init "" |> Tuple.first) |> s

        Export ->
            ( model, File.Download.string "export.json" "application/json" (modelToJSON model |> E.encode 4) )

        ImportRequested ->
            ( model, File.Select.file [ "application/json" ] ImportSelected )

        ImportSelected file ->
            ( model, File.toString file |> Task.perform ImportLoaded )

        ImportLoaded content ->
            (init content |> Tuple.first) |> s



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 py-md-5" ]
        [ main_ []
            [ readme
            , lazy Event.view model.events |> map EventMsg
            , lazy Pupil.view model.pupils |> map PupilMsg
            , lazy2 Assignment.view model.assignment model.pupils.pupils |> map AssignmentMsg
            , admin
            ]
        ]


readme : Html Msg
readme =
    div [ class "mb-5" ]
        [ h1 [] [ text "Projektgruppenverteilung" ]
        , p [ classes "fs-5 col-md-8" ] [ text "Dieses Tool speichert die Eingaben im Local Storage des Browsers. Es werden keine eigegebenen Daten über das Internet gesendet." ]
        ]


admin : Html Msg
admin =
    div []
        [ h2 [] [ text "Administration" ]
        , p [] [ text "Hier kann man die eingegebenen Daten löschen oder exportieren. Beim Import werden alle bisherigen Eingaben überschrieben." ]
        , button [ classes "btn btn-danger", type_ "button", onClick DeleteAll ] [ text "Alle Daten löschen" ]
        , button [ classes "btn btn-secondary ms-2", type_ "button", onClick Export ] [ text "Export" ]
        , button [ classes "btn btn-secondary ms-2", type_ "button", onClick ImportRequested ] [ text "Import" ]
        ]



-- PORTS


port setStorage : String -> Cmd msg
