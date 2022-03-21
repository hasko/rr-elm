module Railroad.Layout exposing
    ( Layout
    , Switch
    , boundingBox
    , coordsFor
    , cursors
    , initialLayout
    , renderInfo
    , switches
    , toGraph
    , toSvg
    , tracks
    )

import Cursor exposing (Cursor)
import Dict exposing (Dict)
import Graph exposing (Graph, insertData, insertEdgeData)
import Graph.Pair exposing (getEdgeData)
import Json.Decode as D
import Json.Encode as E
import Length exposing (Length)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Railroad.Track as Track exposing (Track(..), getPositionOnTrack, moveCursor)
import Rect exposing (Rect(..))
import Set
import Svg exposing (Svg)
import Svg.Attributes exposing (id)


type Layout
    = Layout (Graph Int Switch Track)


type alias Switch =
    { configs : List (List ( Int, Int )) }


cursors : Layout -> Dict Int Cursor
cursors layout =
    -- Render the layout starting at connection 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Cursor 0 2.5 0) layout Dict.empty


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
                                -- TODO determine and use the appropriate connection instead of 0
                                (moveCursor currentCursor track 0)
                                -- And the rest.
                                layout
                                acc
                )
                -- Begin with the list of known cursors plus the current one.
                (Dict.insert nodeId currentCursor knownCursors)


coordsFor : Length -> ( Int, Int ) -> Layout -> Maybe Cursor
coordsFor pos ( fromNode, toNode ) ((Layout g) as layout) =
    case Graph.getEdgeData fromNode toNode g of
        Nothing ->
            Nothing

        Just track ->
            case cursors layout |> Dict.get fromNode of
                Nothing ->
                    Nothing

                Just cursor ->
                    Just (getPositionOnTrack pos cursor track 0)


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
    let
        allCursors =
            cursors layout
    in
    Svg.g [ Svg.Attributes.id "layout" ]
        (Graph.edgesWithData g
            |> List.map
                (\( from, to, maybeTrack ) ->
                    let
                        trackId =
                            "track-" ++ String.fromInt from ++ "-" ++ String.fromInt to

                        maybeRef =
                            Dict.get from allCursors
                    in
                    case ( maybeTrack, maybeRef ) of
                        ( Just track, Just ref ) ->
                            Svg.g
                                [ id trackId
                                , Svg.Attributes.transform
                                    ("translate("
                                        ++ String.fromFloat ref.x
                                        ++ ","
                                        ++ String.fromFloat ref.y
                                        ++ ") rotate("
                                        ++ String.fromFloat ref.dir
                                        ++ ")"
                                    )
                                ]
                                (Track.toSvg track)

                        _ ->
                            Svg.g [ id trackId ] []
                )
        )



-- JSON
-- Samples


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdgeData 0 1 (StraightTrack (Length.meters 75.0))
        |> insertEdgeData 1 2 (CurvedTrack 300.0 15.0)
        |> insertEdgeData 2 4 (CurvedTrack 300 -15)
        -- CCW
        |> insertEdgeData 1 3 (StraightTrack (Length.meters 75.0))
        |> insertData 1 (Switch [ [ ( 0, 2 ) ], [ ( 0, 3 ) ] ])
        |> Layout



-- TODO: Refactor so we don't need to expose the Graph.


toGraph : Layout -> Graph Int Switch Track
toGraph (Layout g) =
    g
