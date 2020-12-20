module Railroad exposing
    ( TrainState
    , move
    , normalizePosition
    , switches
    )

import Dict exposing (Dict)
import Graph exposing (Graph)
import List
import Maybe exposing (andThen, map, withDefault)
import Railroad.Layout exposing (..)
import Set exposing (Set)


type alias TrainState =
    { name : String
    , length : Float -- in m
    , speed : Float -- in m/s
    , track : Int
    , trackPosition : Float -- location of train head in m from the track start
    }


normalizePosition : ( Float, Int ) -> Layout -> ( Float, Int )
normalizePosition ( trackPosition, trackId ) layout =
    if trackPosition < 0 then
        case previousTrack trackId layout of
            Nothing ->
                ( trackPosition, trackId )

            Just ( prevId, prev ) ->
                normalizePosition ( trackPosition + trackLength prev, prevId ) layout

    else
        case Graph.getData trackId layout of
            Nothing ->
                -- TODO Inconsistent layout
                ( trackPosition, trackId )

            Just track ->
                if trackPosition > trackLength track then
                    case nextTrack trackId layout of
                        Nothing ->
                            -- TODO Inconsistent layout
                            ( trackPosition, trackId )

                        Just ( otherId, _ ) ->
                            normalizePosition ( trackPosition - trackLength track, otherId ) layout

                else
                    ( trackPosition, trackId )


previousTrack : Int -> Layout -> Maybe ( Int, Track )
previousTrack =
    getOtherTrack Graph.incoming


nextTrack : Int -> Layout -> Maybe ( Int, Track )
nextTrack =
    getOtherTrack Graph.outgoing


getOtherTrack : (Int -> Layout -> Set Int) -> Int -> Layout -> Maybe ( Int, Track )
getOtherTrack f trackId layout =
    -- TODO Implement switching instead of just taking the head.
    case f trackId layout |> Set.toList |> List.head of
        Nothing ->
            Nothing

        Just otherId ->
            case Graph.getData otherId layout of
                Nothing ->
                    Nothing

                Just otherTrack ->
                    Just ( otherId, otherTrack )


move : Float -> Layout -> TrainState -> TrainState
move millis layout state =
    let
        new_state =
            { state
                | trackPosition = state.trackPosition + state.speed * millis / 1000.0
            }
    in
    move_along layout new_state


move_along : Layout -> TrainState -> TrainState
move_along layout state =
    let
        current_track_length =
            Graph.getData state.track layout |> map trackLength |> withDefault 1000000.0
    in
    if state.trackPosition < current_track_length then
        state

    else
        let
            next_track_id =
                Graph.outgoing state.track layout |> Set.toList |> List.head |> withDefault 0

            new_state =
                { state
                    | track = next_track_id
                    , trackPosition = state.trackPosition - current_track_length
                }
        in
        move_along layout new_state


switches : Layout -> List (List ( Int, Int ))
switches layout =
    Graph.keys layout
        |> List.map
            (\i ->
                Set.foldl
                    (\o acc -> ( i, o ) :: acc)
                    []
                    (Graph.outgoing i layout)
            )
        |> List.filter (\s -> List.length s > 1)
