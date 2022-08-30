module Algo exposing (Graph, Matching, run)

import Dict
import List


{-| Vertex must be a comparable so we can use Elm's Dict.
-}
type alias Vertex =
    String


{-| We construct the bipartite graph with all vertices on the left side and
their edges to vertices on the right side. Isolated vertices on the right
side are ignored.
-}
type alias Graph =
    Dict.Dict Vertex (List Vertex)


type alias Matching =
    Dict.Dict Vertex Vertex


type alias Path =
    List Vertex


type alias IntermediateResult =
    { paths : List Path
    , free : Maybe Path
    }


run : Graph -> Matching
run graph =
    let
        emptyMatching : Matching
        emptyMatching =
            Dict.empty
    in
    graph
        |> Dict.keys
        |> List.foldl
            (\vertex matching ->
                case find graph matching (extend graph [] [ vertex ] []) of
                    Nothing ->
                        matching

                    Just path ->
                        matching |> apply path
            )
            emptyMatching
        |> reverseMatching


find : Graph -> Matching -> List Path -> Maybe Path
find graph matching paths =
    if paths |> List.isEmpty then
        Nothing

    else
        let
            res : IntermediateResult
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


update : Matching -> List Path -> IntermediateResult
update matching paths =
    case paths of
        [] ->
            IntermediateResult [] Nothing

        one :: rest ->
            case matching |> Dict.get (headOf one) of
                Nothing ->
                    IntermediateResult [] (Just one)

                Just vertex ->
                    let
                        res : IntermediateResult
                        res =
                            update matching rest
                    in
                    case res.free of
                        Just path ->
                            IntermediateResult [] (Just path)

                        Nothing ->
                            IntermediateResult ((vertex :: one) :: res.paths) Nothing


headOf : Path -> Vertex
headOf path =
    List.head path |> Maybe.withDefault ""


extend : Graph -> List Path -> Path -> List Path -> List Path
extend graph old path new =
    Dict.get (headOf path) graph
        |> Maybe.withDefault []
        |> List.filter (\e -> old |> List.any (\p -> List.member e p) |> not)
        |> List.map (\e -> e :: path)
        |> List.append new


apply : Path -> Matching -> Matching
apply path matching =
    case path of
        first :: second :: rest ->
            matching |> Dict.insert first second |> apply rest

        _ ->
            matching


reverseMatching : Matching -> Matching
reverseMatching matching =
    matching |> Dict.toList |> List.map (\t -> ( Tuple.second t, Tuple.first t )) |> Dict.fromList



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
