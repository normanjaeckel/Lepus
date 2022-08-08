module HopcroftKarpKarzanov exposing (Graph, Matching, run)

import Dict
import Set


{-| We construct the bipartite graph with all vertices on the left side and
their edges to vertices on the right side. Isolated vertices on the right
side are ignored.
-}
type alias Graph =
    Dict.Dict Vertex (Set.Set Vertex)


{-| Vertex must be a comparable so we can use Elm's Dict and Set.
-}
type alias Vertex =
    String


type alias Matching =
    Dict.Dict Vertex Vertex


run : Graph -> Matching
run graph =
    Dict.empty |> loop graph


loop : Graph -> Matching -> Matching
loop graph matching =
    let
        layers : Maybe (List (Set.Set Vertex))
        layers =
            let
                initialLayers : List (Set.Set Vertex)
                initialLayers =
                    []

                freeVerticesLeft : Dict.Dict Vertex (Set.Set Vertex)
                freeVerticesLeft =
                    graph
                        |> Dict.filter
                            (\v _ ->
                                case matching |> Dict.get v of
                                    Just _ ->
                                        False

                                    Nothing ->
                                        True
                            )

                visited : Set.Set Vertex
                visited =
                    Set.empty
            in
            bfsLeft graph matching initialLayers freeVerticesLeft visited
    in
    case layers of
        Nothing ->
            -- There arn't any augmenting paths left. Terminate the algorithm with the last matching.
            matching

        Just l ->
            -- There are some augmenting paths. Update matching using the depth-first search and run loop again.
            matching |> dfs graph l |> loop graph


type alias IntermediateResult =
    { newLayer : Set.Set Vertex
    , visited : Set.Set Vertex
    }


{-| Breadth-first search from left side vertices.
-}
bfsLeft : Graph -> Matching -> List (Set.Set Vertex) -> Dict.Dict Vertex (Set.Set Vertex) -> Set.Set Vertex -> Maybe (List (Set.Set Vertex))
bfsLeft graph matching layers vertices visited =
    let
        result : IntermediateResult
        result =
            vertices |> Dict.toList |> List.foldl searchAugmentingPaths (IntermediateResult Set.empty visited)

        searchAugmentingPaths : ( Vertex, Set.Set Vertex ) -> IntermediateResult -> IntermediateResult
        searchAugmentingPaths v res =
            let
                unvisitedNeighbours : Set.Set Vertex
                unvisitedNeighbours =
                    Tuple.second v
                        |> Set.filter (\n -> not (visited |> Set.member n))
            in
            { res
                | newLayer = unvisitedNeighbours |> Set.union res.newLayer
                , visited = res.visited |> Set.insert (Tuple.first v)
            }
    in
    if Set.isEmpty result.newLayer then
        -- There are no more edges so we have to stop.
        Nothing

    else
        let
            checkFn : Vertex -> Bool
            checkFn vertexRight =
                matching |> Dict.values |> List.any (\v -> v == vertexRight) |> not

            extendedLayers =
                (vertices |> Dict.keys |> Set.fromList) :: layers
        in
        if result.newLayer |> Set.toList |> List.any checkFn then
            -- At least one of the vertices in newLayer is free, so stop an return the layers
            Just (result.newLayer :: extendedLayers)

        else
            -- Go down to the next layer.
            bfsRight graph matching extendedLayers result.newLayer result.visited


{-| Breadth-first search from right side vertices.
-}
bfsRight : Graph -> Matching -> List (Set.Set Vertex) -> Set.Set Vertex -> Set.Set Vertex -> Maybe (List (Set.Set Vertex))
bfsRight graph matching layers vertices visited =
    let
        getMatchedNeigbours : Dict.Dict Vertex (Set.Set Vertex)
        getMatchedNeigbours =
            vertices
                |> Set.map (\v -> v |> getMatchedRight matching |> Maybe.withDefault v)
                -- It is impossible that the default is coming one here, but TODO: Fix this.
                |> Set.toList
                |> List.map (\left -> ( left, Dict.get left graph |> Maybe.withDefault Set.empty ))
                |> Dict.fromList
    in
    bfsLeft graph matching (vertices :: layers) getMatchedNeigbours (Set.union visited vertices)


dfs : Graph -> List (Set.Set Vertex) -> Matching -> Matching
dfs graph layers matching =
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
