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


{-| Augmenting path of vertices, always starting with a VertexLeft.
The number of vertices is odd.

Example: A path A -> 1 -> B -> 2 -> C is saved as

    { head = C
    , tail = [ ( B, 2 ), ( A, 1 ) ]
    }

-}
type alias PathOdd a b =
    { head : VertexLeft a
    , tail : List ( VertexLeft a, VertexRight b )
    }


{-| Augmenting path with even number of vertices, like A -> 2 -> B -> 2.
See also PathOdd.
-}
type alias PathEven a b =
    { head : ( VertexLeft a, VertexRight b )
    , tail : List ( VertexLeft a, VertexRight b )
    }


type alias Visited b =
    List (VertexRight b)


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
                let
                    newPath : VertexLeft a -> PathOdd a b
                    newPath =
                        \l ->
                            { head = l
                            , tail = []
                            }
                in
                case find graph matching (extend graph (newPath left) ( [], [] )) of
                    Nothing ->
                        matching

                    Just path ->
                        matching |> apply path
            )
            initialMatching


find : Graph a b -> Matching a b -> ( List (PathEven a b), Visited b ) -> Maybe (PathEven a b)
find graph matching ( paths, visited ) =
    if paths |> List.isEmpty then
        Nothing

    else
        let
            res : IntermediateUpdateResult a b
            res =
                update matching paths
        in
        case res.free of
            Just path ->
                Just path

            Nothing ->
                res.paths
                    |> List.foldl (extend graph) ( [], visited )
                    |> find graph matching


type alias IntermediateUpdateResult a b =
    { paths : List (PathOdd a b)
    , free : Maybe (PathEven a b)
    }


update : Matching a b -> List (PathEven a b) -> IntermediateUpdateResult a b
update matching paths =
    case paths of
        [] ->
            IntermediateUpdateResult [] Nothing

        currentPath :: remainingPaths ->
            let
                right : VertexRight b
                right =
                    currentPath.head |> Tuple.second
            in
            case matching |> getFromMatchingRight right of
                Nothing ->
                    -- The head of the path is free so we found a free path. Stop further searching.
                    IntermediateUpdateResult [] (Just currentPath)

                Just left ->
                    -- The head of the path is not free. So search in all others paths
                    -- and if this does not fit update the paths.
                    let
                        res : IntermediateUpdateResult a b
                        res =
                            update matching remainingPaths
                    in
                    case res.free of
                        Just path ->
                            IntermediateUpdateResult [] (Just path)

                        Nothing ->
                            let
                                updatedPath : PathOdd a b
                                updatedPath =
                                    { head = left
                                    , tail = currentPath.head :: currentPath.tail
                                    }
                            in
                            IntermediateUpdateResult (updatedPath :: res.paths) Nothing


extend : Graph a b -> PathOdd a b -> ( List (PathEven a b), Visited b ) -> ( List (PathEven a b), Visited b )
extend graph path acc =
    let
        left : VertexLeft a
        left =
            path.head

        newVertices : List (VertexRight b)
        newVertices =
            graph
                |> getFromGraph left
                |> List.filter (\right -> Tuple.second acc |> List.member right |> not)
    in
    ( newVertices
        |> List.map (\right -> { head = ( left, right ), tail = path.tail })
        |> List.append (Tuple.first acc)
    , newVertices |> List.append (Tuple.second acc)
    )


apply : PathEven a b -> Matching a b -> Matching a b
apply path matching =
    let
        left =
            path.head |> Tuple.first

        right =
            path.head |> Tuple.second

        newMatching =
            matching
                |> List.filter (\( l, _ ) -> l /= left)
                |> (::) ( left, right )

        splitFirstElem : List ( VertexLeft a, VertexRight b ) -> Maybe ( ( VertexLeft a, VertexRight b ), List ( VertexLeft a, VertexRight b ) )
        splitFirstElem =
            \l ->
                List.head l |> Maybe.andThen (\h -> Just ( h, List.drop 1 l ))
    in
    case splitFirstElem path.tail of
        Nothing ->
            newMatching

        Just ( h, t ) ->
            newMatching |> apply { head = h, tail = t }



-- HELPERS


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
