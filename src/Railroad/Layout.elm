module Railroad.Layout exposing
    ( Cursor
    , Layout
    , Track(..)
    , cursors
    , getPositionOnTrack
    , moveCursor
    , trackLength
    )

import Dict exposing (Dict)
import Graph exposing (Graph)
import Set


type alias Layout =
    Graph Int Track ()


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


cursors : Layout -> Dict Int Cursor
cursors layout =
    -- Render the layout starting at track 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Cursor 0 0 0) layout Dict.empty


renderLayout : Int -> Cursor -> Layout -> Dict Int Cursor -> Dict Int Cursor
renderLayout trackId currentCursor layout knownCursors =
    -- If the track is already rendered...
    if Dict.member trackId knownCursors then
        -- then we are done.
        knownCursors

    else
        let
            -- Add the cursor for the current track to the known cursors.
            newCursors =
                Dict.insert trackId currentCursor knownCursors
        in
        -- Get the track data so we can move further.
        case Graph.getData trackId layout of
            -- If this track has no data, the layout is inconsistent. Just return what we know so far.
            Nothing ->
                knownCursors

            Just track ->
                let
                    -- Calculate the next position and direction.
                    newCursor =
                        moveCursor currentCursor track
                in
                -- Get the outgoing tracks.
                -- TODO Consider incoming tracks as well, working backwards.
                Graph.outgoing trackId layout
                    -- Now run the same function again for all the connected tracks and return the result.
                    |> Set.foldl (\i acc -> renderLayout i newCursor layout acc) newCursors


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
