module Algo exposing (Graph, Matching, run)

import Dict


{-| We construct the bipartite graph with all vertices on the left side and
their edges to vertices on the right side. Isolated vertices on the right
side are ignored.
-}
type alias Graph =
    Dict.Dict Vertex (List Vertex)


{-| Vertex must be a comparable so we can use Elm's Dict and Set.
-}
type alias Vertex =
    String


type alias Matching =
    Dict.Dict Vertex Vertex


type alias IntermediateResult =
    { matching : Matching
    , visited : List Vertex
    , path : List Vertex
    , success : Bool
    }


run : Graph -> Matching
run graph =
    let
        loop : Vertex -> List Vertex -> IntermediateResult -> IntermediateResult
        loop =
            \vertex neighbours res ->
                let
                    newRes =
                        dfs graph vertex neighbours { res | visited = vertex :: res.visited, path = [ vertex ], success = False }
                in
                if newRes.success then
                    IntermediateResult
                        (res.matching |> changeMatching (newRes.path |> List.reverse))
                        []
                        []
                        False

                else
                    IntermediateResult
                        res.matching
                        newRes.visited
                        []
                        False
    in
    graph
        |> Dict.foldl loop (IntermediateResult Dict.empty [] [] False)
        |> .matching


dfs : Graph -> Vertex -> List Vertex -> IntermediateResult -> IntermediateResult
dfs graph vertex neighbours res =
    case neighbours of
        [] ->
            res

        neighbour :: rest ->
            if List.member neighbour res.visited then
                dfs graph vertex rest res

            else
                case getMatchedRight res.matching neighbour of
                    Just nextVertex ->
                        -- Go to next level in the tree
                        let
                            newRes : IntermediateResult
                            newRes =
                                dfs
                                    graph
                                    nextVertex
                                    (graph |> Dict.get nextVertex |> Maybe.withDefault [])
                                    { res
                                        | visited = nextVertex :: (neighbour :: res.visited)
                                        , path = nextVertex :: (neighbour :: res.path)
                                    }
                        in
                        if newRes.success then
                            newRes

                        else
                            dfs graph vertex rest { res | visited = newRes.visited }

                    Nothing ->
                        -- Neighbour is free so return the path
                        { res | visited = neighbour :: res.visited, path = neighbour :: res.path, success = True }


changeMatching : List Vertex -> Matching -> Matching
changeMatching path matching =
    case path of
        k :: v :: rest ->
            matching
                |> Dict.insert k v
                |> changeMatching rest

        _ ->
            matching



-- Helpers


getMatchedRight : Matching -> Vertex -> Maybe Vertex
getMatchedRight matching vertex =
    matching |> Dict.toList |> searchingMatch vertex


searchingMatch : Vertex -> List ( Vertex, Vertex ) -> Maybe Vertex
searchingMatch vertex parts =
    case parts of
        elem :: rest ->
            if Tuple.second elem == vertex then
                Just (Tuple.first elem)

            else
                rest |> searchingMatch vertex

        [] ->
            Nothing
