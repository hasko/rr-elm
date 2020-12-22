module Railroad.Train exposing (Orientation(..), TrainLocation, TrainState, move, normalizeLocation, stopped)

import Dict exposing (Dict)
import Graph
import Graph.Pair exposing (getEdgeData)
import List.Extra
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
    , orientation : Orientation
    , track : Track -- The track information for convenience
    }


type Orientation
    = Aligned
    | Reverse


move : Layout -> Dict Int Int -> Int -> TrainState -> TrainState
move layout switchState millis trainState =
    case trainState.location of
        Nothing ->
            -- If the train has no location, no need to move.
            trainState

        Just loc ->
            -- Calculate new position, disregarding track transitions.
            let
                distanceTraveled =
                    trainState.speed * toFloat millis / 1000.0

                -- Consider which way the train is traveling on the track.
                newPos =
                    case loc.orientation of
                        Aligned ->
                            loc.pos + distanceTraveled

                        Reverse ->
                            loc.pos - distanceTraveled
            in
            -- Create a new train state ...
            { trainState
              -- ... but replace the location with a new location ...
                | location =
                    -- ... based on the old location but with an updated position ...
                    { loc | pos = newPos }
                        -- and finally "normalize" the location in case we are on a different track now.
                        |> normalizeLocation layout switchState
            }


normalizeLocation : Layout -> Dict Int Int -> TrainLocation -> Maybe TrainLocation
normalizeLocation layout switchState loc =
    -- If the position is beyond the end of the current track ...
    if loc.pos > trackLength loc.track then
        -- ... get the next track.
        case nextTrack loc.edge layout switchState of
            Nothing ->
                -- If there is no next track, return.
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
                    |> normalizeLocation layout switchState

    else if loc.pos < 0 then
        case previousTrack loc.edge layout switchState of
            Nothing ->
                Nothing

            Just ( otherEdge, otherTrack ) ->
                { loc
                    | pos = loc.pos + trackLength otherTrack
                    , edge = otherEdge
                    , track = otherTrack
                }
                    |> normalizeLocation layout switchState

    else
        -- We are within the track bounds, so return.
        Just loc


previousTrack : ( Int, Int ) -> Layout -> Dict Int Int -> Maybe ( ( Int, Int ), Track )
previousTrack ( fromId, toId ) layout switchState =
    case Graph.getData toId layout of
        Nothing ->
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

        Just switch ->
            Dict.get fromId switchState
                |> withDefault 0
                -- Select the right switch congiguration.
                |> (\i -> List.Extra.getAt i switch.configs)
                -- Get the right route.
                |> Maybe.map (\cfg -> List.filter (\( _, routeTo ) -> routeTo == toId) cfg)
                |> andThen List.head
                -- Choose the previous edge.
                |> Maybe.map (\( routeFrom, _ ) -> ( routeFrom, fromId ))
                -- Get the track for the edge
                |> andThen
                    (\edge ->
                        case getEdgeData edge layout of
                            Nothing ->
                                Nothing

                            Just track ->
                                Just ( edge, track )
                    )


nextTrack : ( Int, Int ) -> Layout -> Dict Int Int -> Maybe ( ( Int, Int ), Track )
nextTrack ( fromId, toId ) layout switchState =
    case Graph.getData toId layout of
        Nothing ->
            Graph.outgoing toId layout
                |> Set.toList
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

        Just switch ->
            Dict.get toId switchState
                |> withDefault 0
                |> (\i -> List.Extra.getAt i switch.configs)
                |> Maybe.map (\cfg -> List.filter (\( from, to ) -> from == fromId) cfg)
                |> andThen List.head
                |> Maybe.map (\( from, to ) -> ( toId, to ))
                |> andThen
                    (\edge ->
                        case getEdgeData edge layout of
                            Nothing ->
                                Nothing

                            Just track ->
                                Just ( edge, track )
                    )


stopped : TrainState -> TrainState
stopped ts =
    { ts | speed = 0.0 }
