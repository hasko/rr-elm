module Railroad exposing
    ( Cursor
    , Layout
    , Track(..)
    , TrainState
    , move
    , moveCursor
    , trackLength
    )

import Graph exposing (Graph)
import List
import Maybe exposing (map, withDefault)
import Set


type alias TrainState =
    { name : String
    , length : Float -- in m
    , speed : Float -- in m/s
    , track : Int
    , trackPosition : Float -- location of train head in m from the track start
    }


type Track
    = StraightTrack
        { length : Float -- in m
        }
    | CurvedTrack
        { radius : Float -- in m
        , angle : Float -- in degrees, why not
        }


trackLength : Track -> Float
trackLength track =
    case track of
        StraightTrack s ->
            s.length

        CurvedTrack c ->
            pi * c.radius * c.angle / 180.0


type alias Cursor =
    { x : Float, y : Float, dir : Float }


moveCursor : Cursor -> Track -> Cursor
moveCursor cursor track =
    case track of
        StraightTrack s ->
            Cursor
                (cursor.x + s.length * cos cursor.dir)
                (cursor.y + s.length * sin cursor.dir)
                cursor.dir

        CurvedTrack c ->
            Cursor
                (cursor.x + c.radius * cos c.angle)
                (cursor.y + c.radius * sin c.angle)
                (cursor.dir + c.angle)


type alias Layout =
    Graph Int Track ()


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
