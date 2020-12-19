module Railroad exposing
    ( Cursor
    , Layout
    , Track(..)
    , TrainState
    , cursors
    , getPositionOnTrack
    , move
    , moveCursor
    , normalizePosition
    , switches
    , trackLength
    )

import Dict exposing (Dict)
import Graph exposing (Graph)
import List
import Maybe exposing (andThen, map, withDefault)
import Set exposing (Set)


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
                (cursor.x + s.length * cos (degrees cursor.dir))
                (cursor.y + s.length * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack c ->
            let
                newDir =
                    cursor.dir + c.angle

                s =
                    2 * c.radius * sin (degrees c.angle / 2)
            in
            Cursor
                (cursor.x + s * cos (degrees (cursor.dir + c.angle / 2)))
                (cursor.y + s * sin (degrees (cursor.dir + c.angle / 2)))
                newDir


getPositionOnTrack : Float -> Cursor -> Track -> Cursor
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack s ->
            Cursor
                (cursor.x + trackPosition * cos (degrees cursor.dir))
                (cursor.y + trackPosition * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack c ->
            let
                a =
                    c.angle * trackPosition / trackLength track

                newDir =
                    cursor.dir + c.angle

                s =
                    2 * c.radius * sin (degrees a / 2)
            in
            Cursor
                (cursor.x + s * cos (degrees (cursor.dir + a / 2)))
                (cursor.y + s * sin (degrees (cursor.dir + a / 2)))
                newDir


type alias Layout =
    Graph Int Track ()


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


cursors : Layout -> Dict Int Cursor
cursors layout =
    renderLayout 0 (Cursor 0 0 0) layout Dict.empty


renderLayout : Int -> Cursor -> Layout -> Dict Int Cursor -> Dict Int Cursor
renderLayout trackId currentCursor layout knownCursors =
    -- If the track is already rendered...
    if Dict.member trackId knownCursors then
        -- then we are done.
        knownCursors

    else
        let
            -- Remember the position and direction for this track.
            accumulator =
                Dict.insert trackId currentCursor knownCursors
        in
        -- Get the track data so we can move further.
        case Graph.getData trackId layout of
            -- If the track has no data, the layout is inconsistent and we finish.
            Nothing ->
                knownCursors

            Just track ->
                let
                    -- Calculate the next position and direction.
                    newCursor =
                        moveCursor currentCursor track
                in
                -- Now run the same function again for all the connected tracks.
                Graph.outgoing trackId layout
                    |> Set.foldl (\i acc -> renderLayout i newCursor layout acc) accumulator


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
