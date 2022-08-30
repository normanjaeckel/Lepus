module Event exposing (Model, init, toVertexList, toVertexListReducer)

import Algo exposing (Vertex)
import Html exposing (..)


type alias Model =
    { name : String
    , capacity : Int
    }


init : List Model
init =
    []


toVertexList : Model -> List Vertex
toVertexList m =
    let
        fn : String -> ( Int, List String ) -> ( Int, List String )
        fn =
            \e t ->
                let
                    newIndex : Int
                    newIndex =
                        Tuple.first t - 1
                in
                ( newIndex, (e ++ "-" ++ String.fromInt newIndex) :: Tuple.second t )
    in
    m.name
        |> List.repeat m.capacity
        |> List.foldl fn ( m.capacity + 1, [] )
        |> Tuple.second


toVertexListReducer : Model -> List Algo.Vertex -> List Algo.Vertex
toVertexListReducer event list =
    toVertexList event ++ list
