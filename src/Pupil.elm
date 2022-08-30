module Pupil exposing (Choice, ChoiceType(..), Model, greenAndYellowEvents, greenEvents, init, redEvents, toVertex)

import Algo
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


toVertex : Model -> Algo.Vertex
toVertex m =
    m.name ++ " (" ++ m.class ++ ")"


greenEvents : Model -> List Event.Model
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


greenAndYellowEvents : List Event.Model -> Model -> List Event.Model
greenAndYellowEvents events pupil =
    events
        |> List.filter
            (\e ->
                pupil.choices
                    |> List.any
                        (\c ->
                            case c.type_ of
                                Red ->
                                    c.event == e

                                _ ->
                                    False
                        )
                    |> not
            )


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
