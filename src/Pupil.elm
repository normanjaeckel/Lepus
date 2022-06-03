module Pupil exposing (Choice, ChoiceType(..), Model, greenEvents, init, redEvents, toString)

import Event
import Html exposing (..)


init : List Model
init =
    []


type alias Model =
    { name : String
    , class : String
    , choices : List Choice
    }


type alias Choice =
    { event : Event.Model
    , type_ : ChoiceType
    }


type ChoiceType
    = Green
    | Red


toString : Model -> String
toString m =
    m.name ++ " (" ++ m.class ++ ")"


redEvents : Model -> List Event.Model
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


greenEvents : Model -> List Event.Model
greenEvents p =
    p.choices
        |> List.filter
            (\c ->
                case c.type_ of
                    Red ->
                        False

                    Green ->
                        True
            )
        |> List.map (\c -> c.event)
