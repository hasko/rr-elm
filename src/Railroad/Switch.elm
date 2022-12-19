module Railroad.Switch exposing (Switch, activeEdges, inactiveEdges)

{-| Represents a switchable part of the layout, e.g. a point.
Consists of a list of edges that comprise the switch, a list of allowed configurations of the switch.
The current state of the switch is managed elsewhere to cleanly separate the static parts of the simulation from the dynamic ones.
-}

import Array exposing (Array)
import Maybe.Extra
import Set


type alias Switch =
    { edges : Array ( Int, Int ), configs : Array (List Int) }


activeEdges : Switch -> Int -> List ( Int, Int )
activeEdges switch state =
    case Array.get state switch.configs of
        Nothing ->
            []

        Just edgeIds ->
            edgeIds |> List.map (\eid -> Array.get eid switch.edges) |> Maybe.Extra.values


inactiveEdges : Switch -> Int -> List ( Int, Int )
inactiveEdges switch state =
    case Array.get state switch.configs of
        Nothing ->
            []

        Just edgeIds ->
            List.range 0 (Array.length switch.edges - 1)
                |> Set.fromList
                |> Set.diff (Set.fromList edgeIds)
                |> Set.toList
                |> List.map (\eid -> Array.get eid switch.edges)
                |> Maybe.Extra.values
