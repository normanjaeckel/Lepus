module Algo exposing (Graph, Matching, VertexLeft(..), VertexRight(..), getFromMatchingLeft, run)

import List


type VertexLeft a
    = VertexLeft a


type VertexRight b
    = VertexRight b


{-| We construct the bipartite graph with all vertices on the left side and
their edges to vertices on the right side. Isolated vertices on the right
side are ignored.
-}
type alias Graph a b =
    List ( VertexLeft a, List (VertexRight b) )


type alias Matching a b =
    List ( VertexLeft a, VertexRight b )


type PathElement a b
    = PathElementLeft (VertexLeft a)
    | PathElementRight (VertexRight b)


type alias Path a b =
    List (PathElement a b)


type alias IntermediateResult a b =
    { paths : List (Path a b)
    , free : Maybe (Path a b)
    }


run : Graph a b -> Matching a b -> Matching a b
run graph initialMatching =
    graph
        |> List.map Tuple.first
        |> List.filter
            (\left ->
                case initialMatching |> getFromMatchingLeft left of
                    Nothing ->
                        True

                    Just _ ->
                        False
            )
        |> List.foldl
            (\left matching ->
                case find graph matching (extend graph [] [ PathElementLeft left ] []) of
                    Nothing ->
                        matching

                    Just path ->
                        matching |> apply path
            )
            initialMatching


find : Graph a b -> Matching a b -> List (Path a b) -> Maybe (Path a b)
find graph matching paths =
    if paths |> List.isEmpty then
        Nothing

    else
        let
            res : IntermediateResult a b
            res =
                update matching paths
        in
        case res.free of
            Just path ->
                Just path

            Nothing ->
                res.paths
                    |> List.foldl (extend graph paths) []
                    |> find graph matching


update : Matching a b -> List (Path a b) -> IntermediateResult a b
update matching paths =
    case paths of
        [] ->
            IntermediateResult [] Nothing

        firstPath :: remainingPaths ->
            case List.head firstPath of
                Nothing ->
                    IntermediateResult [] Nothing

                Just headOfPath ->
                    case headOfPath of
                        PathElementLeft _ ->
                            IntermediateResult [] Nothing

                        PathElementRight right ->
                            case matching |> getFromMatchingRight right of
                                Nothing ->
                                    IntermediateResult [] (Just firstPath)

                                Just left ->
                                    let
                                        res : IntermediateResult a b
                                        res =
                                            update matching remainingPaths
                                    in
                                    case res.free of
                                        Just path ->
                                            IntermediateResult [] (Just path)

                                        Nothing ->
                                            IntermediateResult ((PathElementLeft left :: firstPath) :: res.paths) Nothing


extend : Graph a b -> List (Path a b) -> Path a b -> List (Path a b) -> List (Path a b)
extend graph old path new =
    let
        verticesRight : List (PathElement a b)
        verticesRight =
            case List.head path of
                Nothing ->
                    []

                Just elem ->
                    case elem of
                        PathElementLeft l ->
                            graph |> getFromGraph l |> List.map PathElementRight

                        PathElementRight _ ->
                            []
    in
    verticesRight
        |> List.filter (\right -> old |> List.any (\p -> List.member right p) |> not)
        |> List.map (\right -> right :: path)
        |> List.append new


apply : Path a b -> Matching a b -> Matching a b
apply path matching =
    case path of
        rightElem :: leftElem :: rest ->
            case ( rightElem, leftElem ) of
                ( PathElementRight right, PathElementLeft left ) ->
                    matching
                        |> List.filter (\( l, _ ) -> l /= left)
                        |> (::) ( left, right )
                        |> apply rest

                _ ->
                    matching

        _ ->
            matching


getFromMatchingLeft : VertexLeft a -> Matching a b -> Maybe (VertexRight b)
getFromMatchingLeft left matching =
    matching
        |> List.filter (\( a, _ ) -> left == a)
        |> List.head
        |> Maybe.andThen (\( _, b ) -> Just b)


getFromMatchingRight : VertexRight b -> Matching a b -> Maybe (VertexLeft a)
getFromMatchingRight right matching =
    matching
        |> List.filter (\( _, b ) -> right == b)
        |> List.head
        |> Maybe.andThen (\( a, _ ) -> Just a)


getFromGraph : VertexLeft a -> Graph a b -> List (VertexRight b)
getFromGraph left graph =
    graph
        |> List.filter (Tuple.first >> (==) left)
        |> List.head
        |> Maybe.andThen (Just << Tuple.second)
        |> Maybe.withDefault []
