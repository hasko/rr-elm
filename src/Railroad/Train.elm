module Railroad.Train exposing (TrainLocation, TrainState, move, normalizeLocation)

import Graph
import Graph.Pair exposing (getEdgeData)
import Maybe exposing (andThen, withDefault)
import Railroad.Layout as Layout exposing (Layout, Track, trackLength)
import Set


type alias TrainState =
    { name : String
    , length : Float -- in m
    , speed : Float -- in m/s
    , location : Maybe TrainLocation
    }


type alias TrainLocation =
    { edge : ( Int, Int ) -- The vertices
    , pos : Float -- The position on the track
    , track : Track -- The track information for convenience
    }


move : Layout -> Int -> TrainState -> TrainState
move layout millis trainState =
    case trainState.location of
        Nothing ->
            -- If the train has no location, no need to move.
            trainState

        Just loc ->
            { trainState
                | location =
                    { loc | pos = loc.pos + trainState.speed * toFloat millis / 1000.0 }
                        |> normalizeLocation layout
            }


normalizeLocation : Layout -> TrainLocation -> Maybe TrainLocation
normalizeLocation layout loc =
    -- If the position is beyond the end of the current track ...
    if loc.pos > trackLength loc.track then
        -- ... get the next track.
        case nextTrack loc.edge layout of
            Nothing ->
                -- If there is no next track, the layout is inconsistent. Return.
                Nothing

            Just ( otherEdge, otherTrack ) ->
                -- Calculate the new position.
                { loc
                  -- Subtract the current track length from the position.
                    | pos = loc.pos - trackLength loc.track

                    -- Set the edge ...
                    , edge = otherEdge

                    -- ... and track info for the new location.
                    , track = otherTrack
                }
                    -- ... and repeat until done.
                    |> normalizeLocation layout

    else
        -- We are within the track bounds, so return.
        Just loc


previousTrack : ( Int, Int ) -> Layout -> Maybe ( ( Int, Int ), Track )
previousTrack ( fromId, toId ) layout =
    Graph.incoming fromId layout
        |> Set.toList
        -- TODO Consider switching
        |> List.head
        |> andThen (\otherId -> Just ( otherId, fromId ))
        |> andThen
            (\edge ->
                case Graph.Pair.getEdgeData edge layout of
                    Nothing ->
                        Nothing

                    Just track ->
                        Just ( edge, track )
            )


nextTrack : ( Int, Int ) -> Layout -> Maybe ( ( Int, Int ), Track )
nextTrack ( fromId, toId ) layout =
    Graph.outgoing toId layout
        |> Set.toList
        -- TODO Consider switching
        |> List.head
        |> andThen (\otherId -> Just ( toId, otherId ))
        |> andThen
            (\edge ->
                case Graph.Pair.getEdgeData edge layout of
                    Nothing ->
                        Nothing

                    Just track ->
                        Just ( edge, track )
            )
