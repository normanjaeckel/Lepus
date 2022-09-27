module Helpers exposing (Persistence(..), classes, svgIconSortAlphaDown, svgIconSortAlphaDownAlt, svgIconXLg, tagWithInvalidFeedback)

import Html
import Html.Attributes
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, fillRule, height, viewBox, width)


{-| Indicates whether an update should be stored or not.
-}
type Persistence
    = SetStorage
    | DontSetStorage


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


svgIconSortAlphaDown : Html.Html msg
svgIconSortAlphaDown =
    svg
        [ width "16"
        , height "16"
        , fill "currentColor"
        , class "bi"
        , class "bi-sort-alpha-down"
        , viewBox "0 0 16 16"
        ]
        [ path
            [ fillRule "evenodd"
            , d "M10.082 5.629 9.664 7H8.598l1.789-5.332h1.234L13.402 7h-1.12l-.419-1.371h-1.781zm1.57-.785L11 2.687h-.047l-.652 2.157h1.351z"
            ]
            []
        , path
            [ d "M12.96 14H9.028v-.691l2.579-3.72v-.054H9.098v-.867h3.785v.691l-2.567 3.72v.054h2.645V14zM4.5 2.5a.5.5 0 0 0-1 0v9.793l-1.146-1.147a.5.5 0 0 0-.708.708l2 1.999.007.007a.497.497 0 0 0 .7-.006l2-2a.5.5 0 0 0-.707-.708L4.5 12.293V2.5z"
            ]
            []
        ]


svgIconSortAlphaDownAlt : Html.Html msg
svgIconSortAlphaDownAlt =
    svg
        [ width "16"
        , height "16"
        , fill "currentColor"
        , class "bi"
        , class "bi-sort-alpha-down-alt"
        , viewBox "0 0 16 16"
        ]
        [ path [ d "M12.96 7H9.028v-.691l2.579-3.72v-.054H9.098v-.867h3.785v.691l-2.567 3.72v.054h2.645V7z" ] []
        , path
            [ fillRule "evenodd"
            , d "M10.082 12.629 9.664 14H8.598l1.789-5.332h1.234L13.402 14h-1.12l-.419-1.371h-1.781zm1.57-.785L11 9.688h-.047l-.652 2.156h1.351z"
            ]
            []
        , path
            [ d "M4.5 2.5a.5.5 0 0 0-1 0v9.793l-1.146-1.147a.5.5 0 0 0-.708.708l2 1.999.007.007a.497.497 0 0 0 .7-.006l2-2a.5.5 0 0 0-.707-.708L4.5 12.293V2.5z"
            ]
            []
        ]


tagWithInvalidFeedback :
    (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg)
    -> List (Html.Attribute msg)
    -> String
    -> String
    -> Bool
    -> List (Html.Html msg)
tagWithInvalidFeedback tag attrs identifier feedback isInvalid =
    if isInvalid then
        let
            i : String
            i =
                identifier ++ "Feedback"
        in
        [ tag (Html.Attributes.class "is-invalid" :: (Html.Attributes.attribute "aria-describedby" i :: attrs)) []
        , Html.div [ Html.Attributes.id i, class "invalid-feedback" ] [ Html.text feedback ]
        ]

    else
        [ tag attrs [] ]
