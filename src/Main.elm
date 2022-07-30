module Main exposing (assignGreens, finalize, main, pupilsPrefer)

import Assignment
import Browser
import Event
import Html exposing (..)
import Pupil
import String exposing (split)


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
view m =
    div []
        [ div []
            (finalize m.pupils m.events
                |> List.map Assignment.toHtml
            )
        ]



-- LOGIC


type alias RemainingPupils =
    List Pupil.Model


type alias OpenAssignments =
    List Assignment.Model


type alias FilledAssignments =
    List Assignment.Model


finalize : RemainingPupils -> List Event.Model -> FilledAssignments
finalize pupils events =
    let
        r : ReturnValue
        r =
            assignGreens pupils events
    in
    -- TODO: open assignments is empty in the result of assignGreens. Maybe change this.
    assignRest r.remainingPupils (r.open ++ r.filled)


type alias ReturnValue =
    { remainingPupils : RemainingPupils
    , open : OpenAssignments
    , filled : FilledAssignments
    }


{-| Assigns all pupils to the events of their choice. Returns a list of pupils
that can not be assigned and the open and filled assignments.
-}
assignGreens : RemainingPupils -> List Event.Model -> ReturnValue
assignGreens pupils events =
    assignGreensStepA pupils (Assignment.empty events) []


{-| Assigns pupils to the events of their choice. It accepts a list of
assingments that have to be done and a list of assignments that are already
done.
-}
assignGreensStepA : RemainingPupils -> OpenAssignments -> FilledAssignments -> ReturnValue
assignGreensStepA pupils open filled =
    let
        splitUp : ( List Assignment.Model, List Assignment.Model )
        splitUp =
            open |> List.partition (\a -> lessPupilsPreferThanCapacity pupils a.event)

        assignFn : Assignment.Model -> ReturnValue -> ReturnValue
        assignFn =
            \a rv ->
                let
                    pref : List Pupil.Model
                    pref =
                        pupilsPrefer rv.remainingPupils a.event
                in
                ReturnValue
                    (rv.remainingPupils |> List.filter (\p -> not <| List.member p pref))
                    rv.open
                    ({ a | pupils = pref } :: rv.filled)

        r : ReturnValue
        r =
            Tuple.first splitUp
                |> List.foldl
                    assignFn
                    (ReturnValue
                        pupils
                        (Tuple.second splitUp)
                        filled
                    )
    in
    assignGreensStepB
        r.remainingPupils
        r.open
        r.filled


assignGreensStepB : RemainingPupils -> OpenAssignments -> FilledAssignments -> ReturnValue
assignGreensStepB pupils open filled =
    let
        checkFn : Assignment.Model -> Bool
        checkFn =
            \a -> lessPupilsPreferThanCapacity pupils a.event
    in
    if List.any checkFn open then
        assignGreensStepA pupils open filled

    else
        assignGreensStepC pupils open filled


assignGreensStepC : RemainingPupils -> OpenAssignments -> FilledAssignments -> ReturnValue
assignGreensStepC pupils open filled =
    case open of
        a :: remainingOpen ->
            let
                thereIsAnotherChance : Pupil.Model -> List Event.Model -> Bool
                thereIsAnotherChance pupil events =
                    pupil.choices
                        |> List.any
                            (\c -> c.type_ == Pupil.Green && List.member c.event events)

                sortIndex : Pupil.Model -> Int
                sortIndex =
                    \p ->
                        if pupilsPrefer [ p ] a.event |> List.isEmpty |> not then
                            if thereIsAnotherChance p (remainingOpen |> List.map .event) then
                                1

                            else
                                0

                        else
                            2

                sortedPupils : List Pupil.Model
                sortedPupils =
                    pupils |> List.sortBy sortIndex
            in
            if pupilsPrefer pupils a.event |> List.isEmpty then
                -- Just put the event that nobody wants to have to the "filled" list.
                assignGreensStepA
                    pupils
                    remainingOpen
                    (a :: filled)

            else
                assignGreensStepA
                    (sortedPupils |> List.drop a.event.capacity)
                    remainingOpen
                    ({ a | pupils = sortedPupils |> List.take a.event.capacity } :: filled)

        [] ->
            -- Just return the input if there is no open assignment left.
            ReturnValue pupils open filled



-- HELPERS


pupilsPrefer : RemainingPupils -> Event.Model -> List Pupil.Model
pupilsPrefer pupils event =
    pupils
        |> List.filter
            (\p ->
                p.choices
                    |> List.any
                        (\c ->
                            c.type_ == Pupil.Green && c.event == event
                        )
            )


lessPupilsPreferThanCapacity : RemainingPupils -> Event.Model -> Bool
lessPupilsPreferThanCapacity pupils event =
    let
        howManyPrefer : Int
        howManyPrefer =
            pupilsPrefer pupils event |> List.length
    in
    howManyPrefer > 0 && howManyPrefer <= event.capacity



-- REST


{-| Given a list of remaining pupils and all assignments it assigns everybody
and tries not to assign anybody to a red flagged event.
-}
assignRest : RemainingPupils -> List Assignment.Model -> FilledAssignments
assignRest pupils assignments =
    let
        splitUp : ( List Assignment.Model, List Assignment.Model )
        splitUp =
            assignments |> List.partition (\a -> a.event.capacity > List.length a.pupils)

        rv : ReturnValue
        rv =
            assignRestStepA
                pupils
                (splitUp |> Tuple.first)
                (splitUp |> Tuple.second)
    in
    rv.filled



-- Nimm einen Schüler, suche alle events raus, wo er kein ROT hat und


assignRestStepA : RemainingPupils -> OpenAssignments -> FilledAssignments -> ReturnValue
assignRestStepA pupils open filled =
    let
        assignFn : Pupil.Model -> ReturnValue -> ReturnValue
        assignFn =
            \p rv ->
                let
                    splitUp : ( OpenAssignments, OpenAssignments )
                    splitUp =
                        rv.open |> List.partition (\a -> pupilCanLiveWith p a.event)
                in
                case List.head (Tuple.first splitUp) of
                    -- TODO: Sort assignables before calling head.
                    Just a ->
                        ReturnValue
                            rv.remainingPupils
                            (({ a | pupils = p :: a.pupils } :: (Tuple.first splitUp |> List.drop 1)) ++ Tuple.second splitUp)
                            rv.filled

                    Nothing ->
                        { rv | remainingPupils = p :: rv.remainingPupils }

        r : ReturnValue
        r =
            pupils
                |> List.foldl
                    assignFn
                    (ReturnValue
                        []
                        open
                        filled
                    )
    in
    assignRestStepB
        r.remainingPupils
        r.open
        r.filled


assignRestStepB : RemainingPupils -> OpenAssignments -> FilledAssignments -> ReturnValue
assignRestStepB pupils open filled =
    ReturnValue
        pupils
        []
        (open ++ filled)



-- Helpers


pupilCanLiveWith : Pupil.Model -> Event.Model -> Bool
pupilCanLiveWith p event =
    not <|
        (p.choices
            |> List.any
                (\c ->
                    c.type_ == Pupil.Red && c.event == event
                )
        )



-- {-| Applys pupils to events. Use the event with the smallest number of members
-- for each.
-- -}
-- applyPupils : List Event.Model -> List Pupil.Model -> EventsWithPupils
-- applyPupils events pupils =
--     events
--         |> List.map (\e -> EventWithPupils e [])
--         |> (\e -> List.foldl applyToOne e pupils)
-- {-| Applys one pupil to the event with the smallest number of members but do not
-- take "red" events.
-- -}
-- applyToOne : Pupil.Model -> EventsWithPupils -> EventsWithPupils
-- applyToOne p events =
--     let
--         theReds : List Event.Model
--         theReds =
--             Pupil.redEvents p
--         min : Int
--         min =
--             events
--                 |> List.filter (\e -> not <| List.member e.event theReds)
--                 |> List.map (\e -> List.length e.pupils)
--                 |> List.minimum
--                 |> Maybe.withDefault 0
--         i : Int
--         i =
--             events
--                 |> Array.fromList
--                 |> Array.toIndexedList
--                 |> List.filter
--                     (\t1 ->
--                         (min == (t1 |> Tuple.second |> .pupils |> List.length))
--                             && (not <| List.member (t1 |> Tuple.second |> .event) theReds)
--                     )
--                 |> List.map Tuple.first
--                 |> List.head
--                 |> Maybe.withDefault 0
--         changed =
--             events
--                 |> Array.fromList
--                 |> Array.get i
--                 |> Maybe.map (\e -> { e | pupils = p :: e.pupils })
--     in
--     case changed of
--         Nothing ->
--             events
--         Just new ->
--             events |> Array.fromList |> Array.set i new |> Array.toList
