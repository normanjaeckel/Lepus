module Algo exposing (Graph, Matching, Vertex(..), getFromMatching, run)

import List


type Vertex a b
    = Left a
    | Right b


{-| We construct the bipartite graph with all vertices on the left side and
their edges to vertices on the right side. Isolated vertices on the right
side are ignored.
-}
type alias Graph a b =
    List ( Vertex a b, List (Vertex a b) )


type alias Matching a b =
    List ( Vertex a b, Vertex a b )


type alias Path a b =
    List (Vertex a b)


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
                case initialMatching |> getFromMatching left of
                    Nothing ->
                        True

                    Just _ ->
                        False
            )
        |> List.foldl
            (\left matching ->
                case find graph matching (extend graph [] [ left ] []) of
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
                    case matching |> getFromMatching headOfPath of
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
                                    IntermediateResult ((left :: firstPath) :: res.paths) Nothing


extend : Graph a b -> List (Path a b) -> Path a b -> List (Path a b) -> List (Path a b)
extend graph old path new =
    List.head path
        |> Maybe.andThen (\left -> graph |> getFromGraph left |> Just)
        |> Maybe.withDefault []
        |> List.filter (\right -> old |> List.any (\p -> List.member right p) |> not)
        |> List.map (\right -> right :: path)
        |> List.append new


apply : Path a b -> Matching a b -> Matching a b
apply path matching =
    case path of
        right :: left :: rest ->
            matching
                |> List.filter (\( l, _ ) -> l /= left)
                |> (::) ( left, right )
                |> apply rest

        _ ->
            matching


getFromMatching : Vertex a b -> Matching a b -> Maybe (Vertex a b)
getFromMatching vertex matching =
    case vertex of
        Left _ ->
            matching
                |> List.filter (\( a, _ ) -> vertex == a)
                |> List.head
                |> Maybe.andThen (\( _, b ) -> Just b)

        Right _ ->
            matching
                |> List.filter (\( _, b ) -> vertex == b)
                |> List.head
                |> Maybe.andThen (\( a, _ ) -> Just a)


getFromGraph : Vertex a b -> Graph a b -> List (Vertex a b)
getFromGraph left graph =
    graph
        |> List.filter (Tuple.first >> (==) left)
        |> List.head
        |> Maybe.andThen (Just << Tuple.second)
        |> Maybe.withDefault []



-- run : Graph -> Matching
-- run graph =
--     let
--         loop : Vertex -> List Vertex -> IntermediateResult -> IntermediateResult
--         loop =
--             \vertex neighbours res ->
--                 let
--                     newRes =
--                         dfs graph vertex neighbours { res | visited = vertex :: res.visited, path = [ vertex ], success = False }
--                 in
--                 if newRes.success then
--                     IntermediateResult
--                         (res.matching |> changeMatching (newRes.path |> List.reverse))
--                         []
--                         []
--                         False
--                 else
--                     IntermediateResult
--                         res.matching
--                         newRes.visited
--                         []
--                         False
--     in
--     graph
--         |> Dict.foldl loop (IntermediateResult Dict.empty [] [] False)
--         |> .matching
-- dfs : Graph -> Vertex -> List Vertex -> IntermediateResult -> IntermediateResult
-- dfs graph vertex neighbours res =
--     case neighbours of
--         [] ->
--             res
--         neighbour :: rest ->
--             if List.member neighbour res.visited then
--                 dfs graph vertex rest res
--             else
--                 case getMatchedRight res.matching neighbour of
--                     Just nextVertex ->
--                         -- Go to next level in the tree
--                         let
--                             newRes : IntermediateResult
--                             newRes =
--                                 dfs
--                                     graph
--                                     nextVertex
--                                     (graph |> Dict.get nextVertex |> Maybe.withDefault [])
--                                     { res
--                                         | visited = nextVertex :: (neighbour :: res.visited)
--                                         , path = nextVertex :: (neighbour :: res.path)
--                                     }
--                         in
--                         if newRes.success then
--                             newRes
--                         else
--                             dfs graph vertex rest { res | visited = newRes.visited }
--                     Nothing ->
--                         -- Neighbour is free so return the path
--                         { res | visited = neighbour :: res.visited, path = neighbour :: res.path, success = True }
-- changeMatching : List Vertex -> Matching -> Matching
-- changeMatching path matching =
--     case path of
--         k :: v :: rest ->
--             matching
--                 |> Dict.insert k v
--                 |> changeMatching rest
--         _ ->
--             matching
-- -- Helpers
-- getMatchedRight : Matching -> Vertex -> Maybe Vertex
-- getMatchedRight matching vertex =
--     matching |> Dict.toList |> searchingMatch vertex
-- searchingMatch : Vertex -> List ( Vertex, Vertex ) -> Maybe Vertex
-- searchingMatch vertex parts =
--     case parts of
--         elem :: rest ->
--             if Tuple.second elem == vertex then
--                 Just (Tuple.first elem)
--             else
--                 rest |> searchingMatch vertex
--         [] ->
--             Nothing
