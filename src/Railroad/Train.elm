module Railroad.Train exposing (TrainState, move, normalizePosition)

import Graph
import Graph.Pair exposing (getEdgeData)
import Maybe exposing (andThen, withDefault)
import Railroad.Layout as Layout exposing (Layout, Track, trackLength)
import Set


type alias TrainState =
    { name : String
    , length : Float -- in m
    , speed : Float -- in m/s
    , track : ( Int, Int ) -- Vertice numbers for the track
    , trackPosition : Float -- location of train head in m from the track start
    }


move : Layout -> Float -> TrainState -> TrainState
move layout millis trainState =
    { trainState | trackPosition = trainState.trackPosition + trainState.speed * millis / 1000.0 }
        |> normalizePosition layout


normalizePosition : Layout -> TrainState -> TrainState
normalizePosition layout trainState =
    -- If the track posision is before the track start ...
    if trainState.trackPosition < 0 then
        -- ... get the previous track.
        case previousTrack trainState.track layout of
            Nothing ->
                -- If there is no previous track, the layout is inconsistent. Return what we have.
                trainState

            Just ( n, t ) ->
                -- Calculate the new position on the previous track and recurse.
                { trainState
                    | trackPosition = trainState.trackPosition + trackLength t
                    , track = n
                }
                    |> normalizePosition layout

    else
        case getEdgeData trainState.track layout of
            Nothing ->
                -- The track has no information. Just return.
                trainState

            Just track ->
                -- If the position is beyond the end of the current track ...
                if trainState.trackPosition > trackLength track then
                    -- ... get the next track.
                    case nextTrack trainState.track layout of
                        Nothing ->
                            -- If there is no next track, the layout is inconsistent. Return.
                            trainState

                        Just ( n, _ ) ->
                            -- Calculate the new position and recurse.
                            { trainState
                                | trackPosition = trainState.trackPosition - trackLength track
                                , track = n
                            }
                                |> normalizePosition layout

                else
                    -- We are within the track bounds, so return.
                    trainState


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
