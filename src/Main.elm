module Main exposing (EventWithPupils, applyPupils, applyToOne, main)

import Array
import Browser
import Event
import Html exposing (..)
import Pupil


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { pupils : List Pupil.Model
    , events : List Event.Model
    }


init : Model
init =
    { pupils = Pupil.init
    , events = Event.init
    }



-- UPDATE


update : msg -> Model -> Model
update _ m =
    m



-- VIEW


view : Model -> Html msg
view _ =
    div [] [ text "hallo" ]


type alias EventsWithPupils =
    List EventWithPupils


type alias EventWithPupils =
    { event : Event.Model
    , pupils : List Pupil.Model
    }


{-| Applys pupils to events. Use the event with the smallest number of members
for each.
-}
applyPupils : List Event.Model -> List Pupil.Model -> EventsWithPupils
applyPupils events pupils =
    events
        |> List.map (\e -> EventWithPupils e [])
        |> (\e -> List.foldl applyToOne e pupils)


{-| Applys one pupil to the event with the smallest number of members.
-}
applyToOne : Pupil.Model -> EventsWithPupils -> EventsWithPupils
applyToOne p events =
    let
        min =
            events
                |> List.map (\e -> List.length e.pupils)
                |> List.minimum
                |> Maybe.withDefault 0

        i : Int
        i =
            events
                |> Array.fromList
                |> Array.toIndexedList
                |> List.filter
                    (\t1 -> min == (t1 |> Tuple.second |> .pupils |> List.length))
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault 0

        changed =
            events
                |> Array.fromList
                |> Array.get i
                |> Maybe.map (\e -> { e | pupils = p :: e.pupils })
    in
    case changed of
        Nothing ->
            events

        Just new ->
            events |> Array.fromList |> Array.set i new |> Array.toList
