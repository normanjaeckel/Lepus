module Logic exposing (Assignments, assignPupil)

import Assignment
import Pupil



-- type alias RemainingPupils =
--     List Pupil.Model
-- type alias OpenAssignments =
--     List Assignment.Model
-- type alias FilledAssignments =
--     List Assignment.Model


type alias Assignments =
    List Assignment.Model


assignPupil : Pupil.Model -> Assignments -> Maybe Assignments
assignPupil p a =
    case assignPupilForFree p a of
        Just result ->
            Just result

        Nothing ->
            assignPupilWithSwitch p a


assignPupilForFree : Pupil.Model -> Assignments -> Maybe Assignments
assignPupilForFree p assignments =
    let
        parts =
            assignments |> List.partition (\a -> freeCapacity a && pupilLikes p a)
    in
    case Tuple.first parts of
        a :: rest ->
            { a | pupils = p :: a.pupils } :: rest ++ Tuple.second parts |> Just

        [] ->
            Nothing


assignPupilWithSwitch : Pupil.Model -> Assignments -> Maybe Assignments
assignPupilWithSwitch p assignments =
    let
        parts =
            assignments |> List.partition (\a -> pupilLikes p a)
    in
    case Tuple.first parts of
        a :: rest ->
            case trySubstutition p a (rest ++ Tuple.second parts) of
                Just newAssignments ->
                    Just newAssignments

                Nothing ->
                    assignPupilWithSwitch p (rest ++ Tuple.second parts)
                        |> Maybe.andThen
                            (\newAssignments -> Just (a :: newAssignments))

        [] ->
            Nothing


trySubstutition : Pupil.Model -> Assignment.Model -> Assignments -> Maybe Assignments
trySubstutition p0 a rest =
    let
        addPupil =
            \newRest ->
                let
                    newPupils =
                        p0 :: (a.pupils |> List.drop 1)
                in
                Just ({ a | pupils = newPupils } :: newRest)
    in
    case a.pupils of
        subsCandidate :: otherPupils ->
            case assignPupil subsCandidate rest |> Maybe.andThen addPupil of
                Just assignments ->
                    Just assignments

                Nothing ->
                    trySubstutition p0 { a | pupils = otherPupils } rest
                        |> Maybe.andThen
                            (\assignments ->
                                case assignments of
                                    head :: others ->
                                        Just ({ head | pupils = subsCandidate :: head.pupils } :: others)

                                    [] ->
                                        Nothing
                            )

        [] ->
            Nothing



-- HELPER


pupilLikes : Pupil.Model -> Assignment.Model -> Bool
pupilLikes p a =
    p.choices |> List.any (\c -> c.type_ == Pupil.Green && c.event == a.event)


freeCapacity : Assignment.Model -> Bool
freeCapacity a =
    a.event.capacity > List.length a.pupils
