module Class exposing (Classname, Model, Msg, decoder, init, modelToJSON, update, view)

import Helpers exposing (Persistence(..), classes, svgIconXLg, tagWithInvalidFeedback)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, hidden, id, placeholder, required, tabindex, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy)
import Json.Decode as D
import Json.Encode as E
import Set



-- MODEL


type alias Model =
    { classes : Set.Set Classname
    , formData : Classname
    , formInvalid : Bool
    }


type alias Classname =
    String


init : Model
init =
    Model Set.empty "" False


emptyFormData : String
emptyFormData =
    ""


decoder : D.Decoder Model
decoder =
    D.map
        (\l -> Model (Set.fromList l) emptyFormData False)
        (D.list D.string)


modelToJSON : Model -> E.Value
modelToJSON model =
    model.classes |> Set.toList |> E.list E.string



-- UPDATE


type Msg
    = FormDataMsg String
    | Save
    | Delete Classname


update : Msg -> Model -> ( Model, Persistence )
update msg model =
    case msg of
        FormDataMsg s ->
            ( { model | formData = s, formInvalid = False }, DontSetStorage )

        Save ->
            case validate model of
                Just c ->
                    ( { model
                        | classes = Set.insert c model.classes
                        , formData = emptyFormData
                        , formInvalid = False
                      }
                    , SetStorage
                    )

                Nothing ->
                    ( { model | formInvalid = True }, DontSetStorage )

        Delete s ->
            ( { model | classes = Set.remove s model.classes, formInvalid = False }, SetStorage )


validate : Model -> Maybe Classname
validate model =
    let
        c : Classname
        c =
            model.formData |> String.trim
    in
    if (c == "") || (model.classes |> Set.member c) then
        Nothing

    else
        Just c



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "mb-5" ]
        [ h2 [ id "classes", class "nav-anchor" ] [ text "Klassen" ]
        , form [ class "mb-3", onSubmit Save ]
            [ h3 [ hidden True ] [ text "Neue Klasse hinzufügen" ]
            , div [ classes "row g-3" ]
                [ div [ class "col-md-3" ]
                    (tagWithInvalidFeedback
                        input
                        [ class "form-control"
                        , type_ "text"
                        , placeholder "Bezeichner"
                        , attribute "aria-label" "Bezeichner"
                        , required True
                        , onInput FormDataMsg
                        , value model.formData
                        ]
                        "newClassName"
                        "Klasse ist bereits vorhanden"
                        model.formInvalid
                    )
                , div [ class "col-md-3" ] [ button [ classes "btn btn-primary", type_ "submit" ] [ text "Hinzufügen" ] ]
                ]
            ]
        , div []
            [ h3 [ hidden True ] [ text "Alle Klassen" ]
            , lazy allClasses model.classes
            ]
        ]


allClasses : Set.Set Classname -> Html Msg
allClasses c =
    if Set.isEmpty c then
        p [ hidden True ] [ text "Noch keine Klassen angelegt" ]

    else
        ul [ classes "list-group list-group-flush" ] (c |> Set.toList |> List.sort |> List.map (lazy oneClassLi))


oneClassLi : Classname -> Html Msg
oneClassLi c =
    li [ classes "list-group-item d-flex justify-content-between align-items-start col-md-8 col-lg-7 col-xl-5" ]
        [ div [ classes "ms-2 me-auto" ] [ text c ]
        , a
            [ class "link-danger"
            , title "Löschen"
            , tabindex 0
            , attribute "role" "button"
            , attribute "aria-label" "Löschen"
            , onClick <| Delete c
            ]
            [ svgIconXLg ]
        ]
