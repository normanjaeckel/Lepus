module Helpers exposing (classes, isValidNameOrClass, svgIconArrowDown, svgIconArrowUp, svgIconXLg)

import Html
import Html.Attributes
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, fillRule, height, viewBox, width)


{-| This helper takes a string with class names separated by one whitespace. All
classes are applied to the result.

    import Html exposing (..)

    view : Model -> Html msg
    view model =
        div [ classes "center with-border nice-color" ] [ text model.content ]

-}
classes : String -> Html.Attribute msg
classes s =
    let
        cl : List ( String, Bool )
        cl =
            String.split " " s |> List.map (\c -> ( c, True ))
    in
    Html.Attributes.classList cl


svgIconXLg : Html.Html msg
svgIconXLg =
    svg
        [ width "16"
        , height "16"
        , fill "currentColor"
        , class "bi"
        , class "bi-x-lg"
        , viewBox "0 0 16 16"
        ]
        [ path
            [ d "M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z" ]
            []
        ]


svgIconArrowUp : Html.Html msg
svgIconArrowUp =
    svg
        [ width "16"
        , height "16"
        , fill "currentColor"
        , class "bi"
        , class "bi-arrow-up"
        , viewBox "0 0 16 16"
        ]
        [ path
            [ fillRule "evenodd"
            , d "M8 15a.5.5 0 0 0 .5-.5V2.707l3.146 3.147a.5.5 0 0 0 .708-.708l-4-4a.5.5 0 0 0-.708 0l-4 4a.5.5 0 1 0 .708.708L7.5 2.707V14.5a.5.5 0 0 0 .5.5z"
            ]
            []
        ]


svgIconArrowDown : Html.Html msg
svgIconArrowDown =
    svg
        [ width "16"
        , height "16"
        , fill "currentColor"
        , class "bi"
        , class "bi-arrow-down"
        , viewBox "0 0 16 16"
        ]
        [ path
            [ fillRule "evenodd"
            , d "M8 1a.5.5 0 0 1 .5.5v11.793l3.146-3.147a.5.5 0 0 1 .708.708l-4 4a.5.5 0 0 1-.708 0l-4-4a.5.5 0 0 1 .708-.708L7.5 13.293V1.5A.5.5 0 0 1 8 1z"
            ]
            []
        ]


isValidNameOrClass : String -> Bool
isValidNameOrClass name =
    String.trim name /= ""
