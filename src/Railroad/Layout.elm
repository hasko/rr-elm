module Railroad.Layout exposing
    ( Layout
    , Switch
    , boundingBox
    , coordsFor
    , cursors
    , getPositionOnTrack
    , initialLayout
    , renderInfo
    , switches
    , toGraph
    , toSvg
    , tracks
    )

import Dict exposing (Dict)
import Graph exposing (Graph, insertData, insertEdgeData)
import Graph.Pair exposing (getEdgeData)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Railroad.Track as Track exposing (Sweep(..), Track(..), moveCursor)
import Railroad.Util exposing (Cursor)
import Rect exposing (Rect(..))
import Set
import Svg exposing (Svg)
import Svg.Attributes


type Layout
    = Layout (Graph Int Switch Track)


type alias Switch =
    { configs : List (List ( Int, Int )) }


cursors : Layout -> Dict Int Cursor
cursors layout =
    -- Render the layout starting at connection 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Cursor 0 0 0) layout Dict.empty


boundingBox : Layout -> Rect
boundingBox layout =
    List.foldl
        (\c (Rect x1 y1 x2 y2) -> Rect (min x1 c.x) (min y1 c.y) (max x2 c.x) (max y2 c.y))
        (Rect 0 0 0 0)
        (cursors layout |> Dict.values)


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


getPositionOnTrack : Float -> Cursor -> Track -> Cursor
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack s ->
            Cursor
                (cursor.x + trackPosition * cos (degrees cursor.dir))
                (cursor.y + trackPosition * sin (degrees cursor.dir))
                cursor.dir

        -- TODO check if we need sweep
        CurvedTrack r a _ ->
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
            -- Should never happen but this edge has no track data.
            Nothing

        Just track ->
            case Dict.get from (cursors layout) of
                Nothing ->
                    -- The position of the from connection cannot be calculated.
                    Nothing

                Just c1 ->
                    case Dict.get to (cursors layout) of
                        Nothing ->
                            Nothing

                        Just c2 ->
                            Just ( c1, c2, track )



-- Views


toSvg : Layout -> Svg msg
toSvg ((Layout g) as layout) =
    Svg.g [ Svg.Attributes.id "layout" ]
        (Graph.edges g |> List.map (viewTrack layout))


viewTrack : Layout -> ( Int, Int ) -> Svg msg
viewTrack layout edge =
    case renderInfo layout edge of
        Nothing ->
            -- Track cannot be rendered.
            Svg.g [] []

        Just ( c1, c2, track ) ->
            case track of
                StraightTrack s ->
                    Svg.line
                        [ Svg.Attributes.x1 (c1.x |> String.fromFloat)
                        , Svg.Attributes.y1 (c1.y |> String.fromFloat)
                        , Svg.Attributes.x2 (c2.x |> String.fromFloat)
                        , Svg.Attributes.y2 (c2.y |> String.fromFloat)
                        , Svg.Attributes.stroke "grey"
                        , Svg.Attributes.strokeWidth "1.435"
                        ]
                        []

                CurvedTrack r a sweep ->
                    Svg.path
                        [ Svg.Attributes.d
                            ("M "
                                ++ (c1.x |> String.fromFloat)
                                ++ " "
                                ++ (c1.y |> String.fromFloat)
                                ++ " A "
                                ++ (r |> String.fromFloat)
                                ++ " "
                                ++ (r |> String.fromFloat)
                                -- ellipse rotation
                                ++ " 0 "
                                -- large arc flag
                                ++ (if a <= 180.0 then
                                        " 0"

                                    else
                                        " 1"
                                   )
                                -- sweep flag
                                ++ (case sweep of
                                        CW ->
                                            " 1 "

                                        CCW ->
                                            " 0 "
                                   )
                                ++ (c2.x |> String.fromFloat)
                                ++ " "
                                ++ (c2.y |> String.fromFloat)
                            )
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke "green"
                        , Svg.Attributes.strokeWidth "1.435"
                        ]
                        []



-- Samples


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdgeData 0 1 (StraightTrack 75.0)
        |> insertEdgeData 1 2 (CurvedTrack 300.0 15.0 CW)
        |> insertEdgeData 2 4 (CurvedTrack 300 15 CCW)
        -- CCW
        |> insertEdgeData 1 3 (StraightTrack 75.0)
        |> insertData 1 (Switch [ [ ( 0, 2 ) ], [ ( 0, 3 ) ] ])
        |> Layout



-- TODO: Refactor so we don't need to expose the Graph.


toGraph : Layout -> Graph Int Switch Track
toGraph (Layout g) =
    g
