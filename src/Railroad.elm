module Railroad exposing (normalizePosition)

import Dict exposing (Dict)
import Graph exposing (Graph)
import List
import Maybe exposing (andThen, map, withDefault)
import Railroad.Layout exposing (..)
import Set exposing (Set)


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
