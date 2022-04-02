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

import Angle
import Dict exposing (Dict)
import Direction2d
import Frame2d
import Graph exposing (Graph, insertData, insertEdgeData)
import Graph.Pair exposing (getEdgeData)
import Length exposing (Length)
import List.Extra exposing (cartesianProduct)
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Point2d
import Railroad.Track as Track exposing (Track(..), getPositionOnTrack, moveFrame)
import Railroad.Util exposing (Frame)
import Rect exposing (Rect(..))
import Set
import Svg exposing (Svg)
import Svg.Attributes exposing (id)


type Layout
    = Layout (Graph Int Switch Track)


type alias Switch =
    { configs : List (List ( Int, Int )) }


cursors : Layout -> Dict Int Frame
cursors layout =
    -- Render the layout starting at connection 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Frame2d.atPoint (Point2d.meters 0 2.5)) layout Dict.empty


boundingBox : Layout -> Rect
boundingBox layout =
    List.foldl
        (\c (Rect x1 y1 x2 y2) -> Rect (min x1 c.x) (min y1 c.y) (max x2 c.x) (max y2 c.y))
        (Rect 0 0 0 0)
        (cursors layout |> Dict.values |> List.map (Frame2d.originPoint >> Point2d.toRecord Length.inMeters))


renderLayout : Int -> Frame -> Layout -> Dict Int Frame -> Dict Int Frame
renderLayout nodeId currentFrame ((Layout g) as layout) knownFrames =
    -- If the node cursor is already calculated ...
    if Dict.member nodeId knownFrames then
        -- ... then we are done.
        knownFrames

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
                                (moveFrame currentFrame track)
                                -- And the rest.
                                layout
                                acc
                )
                -- Begin with the list of known cursors plus the current one.
                (Dict.insert nodeId currentFrame knownFrames)


coordsFor : Length -> ( Int, Int ) -> Layout -> Maybe Frame
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


renderInfo : Layout -> ( Int, Int ) -> Maybe ( Frame, Frame, Track )
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
        allFrames =
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
                            Dict.get from allFrames
                    in
                    case ( maybeTrack, maybeRef ) of
                        ( Just track, Just ref ) ->
                            let
                                refP =
                                    ref |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters

                                refA =
                                    ref |> Frame2d.xDirection |> Direction2d.toAngle |> Angle.inDegrees
                            in
                            Svg.g
                                [ id trackId
                                , Svg.Attributes.transform
                                    ("translate("
                                        ++ String.fromFloat refP.x
                                        ++ ","
                                        ++ String.fromFloat refP.y
                                        ++ ") rotate("
                                        ++ String.fromFloat refA
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
        |> insertEdgeData 1 2 (CurvedTrack (Length.meters 300.0) (Angle.degrees 15.0))
        |> insertEdgeData 2 4 (CurvedTrack (Length.meters 300) (Angle.degrees -15))
        -- CCW
        |> insertEdgeData 1 3 (StraightTrack (Length.meters 75.0))
        |> insertData 1 (Switch [ [ ( 0, 2 ) ], [ ( 0, 3 ) ] ])
        |> Layout



-- TODO: Refactor so we don't need to expose the Graph.


toGraph : Layout -> Graph Int Switch Track
toGraph (Layout g) =
    g