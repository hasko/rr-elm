module Railroad.Layout exposing
    ( Cursor
    , Layout
    , Switch
    , coordsFor
    , cursors
    , getPositionOnTrack
    , initialLayout
    , moveCursor
    , renderInfo
    , switches
    , toGraph
    , tracks
    )

import Dict exposing (Dict)
import Graph exposing (Graph, insertData, insertEdgeData)
import Graph.Pair exposing (getEdgeData)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Railroad.Track as Track exposing (Track(..))
import Set


type Layout
    = Layout (Graph Int Switch Track)


type alias Switch =
    { configs : List (List ( Int, Int )) }


type alias Cursor =
    { x : Float, y : Float, dir : Float }


cursors : Layout -> Dict Int Cursor
cursors layout =
    -- Render the layout starting at connection 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Cursor 0 0 0) layout Dict.empty


renderLayout : Int -> Cursor -> Layout -> Dict Int Cursor -> Dict Int Cursor
renderLayout nodeId currentCursor ((Layout g) as layout) knownCursors =
    -- If the node cursor is already calculated ...
    if Dict.member nodeId knownCursors then
        -- ... then we are done.
        knownCursors

    else
        -- Get all the outoing tracks from this node.
        -- TODO Consider incoming tracks too.
        Graph.outgoing nodeId g
            -- Calculate the next node position for each track and collect them.
            |> Set.foldl
                (\nextNodeId acc ->
                    -- Get the track for this pair of nodes.
                    case Graph.getEdgeData nodeId nextNodeId g of
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
        StraightTrack l ->
            Cursor
                (cursor.x + l * cos (degrees cursor.dir))
                (cursor.y + l * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack r a ->
            let
                newDir =
                    cursor.dir + a

                s =
                    2 * r * sin (degrees a / 2)
            in
            Cursor
                (cursor.x + s * cos (degrees (cursor.dir + a / 2)))
                (cursor.y + s * sin (degrees (cursor.dir + a / 2)))
                newDir


getPositionOnTrack : Float -> Cursor -> Track -> Cursor
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack s ->
            Cursor
                (cursor.x + trackPosition * cos (degrees cursor.dir))
                (cursor.y + trackPosition * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack r a ->
            let
                a2 =
                    a * trackPosition / Track.length track

                newDir =
                    cursor.dir + a

                s =
                    2 * r * sin (degrees a2 / 2)
            in
            Cursor
                (cursor.x + s * cos (degrees (cursor.dir + a2 / 2)))
                (cursor.y + s * sin (degrees (cursor.dir + a2 / 2)))
                newDir


coordsFor : Float -> ( Int, Int ) -> Layout -> Maybe Cursor
coordsFor pos ( fromNode, toNode ) ((Layout g) as layout) =
    case Graph.getEdgeData fromNode toNode g of
        Nothing ->
            Nothing

        Just track ->
            case cursors layout |> Dict.get fromNode of
                Nothing ->
                    Nothing

                Just cursor ->
                    Just (getPositionOnTrack pos cursor track)


switches : Layout -> List ( Int, Switch )
switches (Layout g) =
    Graph.nodes g
        -- Convert from a list of pairs with a Maybe inside to a list of Maybes
        |> List.map (\( vertex, data ) -> Maybe.map (\switch -> ( vertex, switch )) data)
        -- Filter out the Nothings
        |> Maybe.Extra.values


tracks : Layout -> List Track
tracks (Layout g) =
    Graph.edgesWithData g
        |> List.map (\( from, to, maybeTrack ) -> maybeTrack)
        |> Maybe.Extra.values


renderInfo : Layout -> ( Int, Int ) -> Maybe ( Cursor, Cursor, Track )
renderInfo ((Layout g) as layout) ( from, to ) =
    case Graph.getEdgeData from to g of
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



-- Samples


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdgeData 0 1 (StraightTrack 75.0)
        |> insertEdgeData 1 2 (CurvedTrack 300.0 15.0)
        |> insertEdgeData 2 4 (CurvedTrack 300 -15)
        |> insertEdgeData 1 3 (StraightTrack 75.0)
        |> insertData 1 (Switch [ [ ( 0, 2 ) ], [ ( 0, 3 ) ] ])
        |> Layout



-- TODO: Refactor so we don't need to expose the Graph.


toGraph : Layout -> Graph Int Switch Track
toGraph (Layout g) =
    g
