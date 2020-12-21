module Railroad.Layout exposing
    ( Cursor
    , Layout
    , Track(..)
    , coordsFor
    , cursors
    , getPositionOnTrack
    , moveCursor
    , renderInfo
    , switches
    , trackLength
    )

import Dict exposing (Dict)
import Graph exposing (Graph)
import Graph.Pair exposing (getEdgeData)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe(..))
import Set


type alias Layout =
    Graph Int () Track


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
renderLayout nodeId currentCursor layout knownCursors =
    -- If the node cursor is already calculated ...
    if Dict.member nodeId knownCursors then
        -- ... then we are done.
        knownCursors

    else
        -- Get all the outoing tracks from this node.
        -- TODO Consider incoming tracks too.
        Graph.outgoing nodeId layout
            -- Calculate the next node position for each track and collect them.
            |> Set.foldl
                (\nextNodeId acc ->
                    -- Get the track for this pair of nodes.
                    case Graph.getEdgeData nodeId nextNodeId layout of
                        Nothing ->
                            -- If there is no track, our layout is inconsistent. Return what we know so far.
                            acc

                        Just track ->
                            -- Move the current cursor along the track and recurse.
                            renderLayout
                                -- Start with the next node id.
                                nextNodeId
                                -- Move the cursor along the track.
                                (moveCursor currentCursor track)
                                -- And the rest.
                                layout
                                acc
                )
                -- Begin with the list of known cursors plus the current one.
                (Dict.insert nodeId currentCursor knownCursors)


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


coordsFor : Float -> ( Int, Int ) -> Layout -> Maybe Cursor
coordsFor pos ( fromNode, toNode ) layout =
    case Graph.getEdgeData fromNode toNode layout of
        Nothing ->
            Nothing

        Just track ->
            case cursors layout |> Dict.get fromNode of
                Nothing ->
                    Nothing

                Just cursor ->
                    Just (getPositionOnTrack pos cursor track)


switches : Layout -> List (List ( Int, Int ))
switches layout =
    -- TODO
    []


renderInfo : Layout -> ( Int, Int ) -> Maybe ( Cursor, Cursor, Track )
renderInfo layout ( from, to ) =
    case Graph.getEdgeData from to layout of
        Nothing ->
            Nothing

        Just track ->
            case Dict.get from (cursors layout) of
                Nothing ->
                    Nothing

                Just c1 ->
                    case Dict.get to (cursors layout) of
                        Nothing ->
                            Nothing

                        Just c2 ->
                            Just ( c1, c2, track )
