module Railroad.Layout exposing
    ( Layout
    , Location
    , boundingBox
    , coordsFor
    , decoder
    , encode
    , encodeLocation
    , initialLayout
    , locationDecoder
    , nextTrack
    , partitionGraph
    , previousTrack
    , toGraph
    , toSvg
    , trackAt
    )

import Angle
import Array exposing (Array)
import Dict exposing (Dict)
import Direction2d
import Frame2d
import Graph exposing (Graph, getEdgeData, insertEdgeData)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length exposing (Length)
import Maybe exposing (Maybe(..))
import Point2d
import Quantity
import Railroad.Orientation as Orientation exposing (Orientation(..))
import Railroad.Switch as Switch exposing (Switch)
import Railroad.Track as Track exposing (Track(..), getPositionOnTrack, moveFrame)
import Railroad.Util exposing (Frame)
import Rect exposing (Rect(..))
import Set
import Svg exposing (Svg, switch)
import Svg.Attributes exposing (id)


type alias Layout =
    { graph : G, switches : Array Switch }


type alias G =
    Graph Int () Track


type alias Location =
    { edge : ( Int, Int ) -- The vertices
    , pos : Length -- The position on the track
    , orientation : Orientation
    }


trackAt : ( Int, Int ) -> Layout -> Maybe Track
trackAt ( from, to ) layout =
    Graph.getEdgeData from to layout.graph


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
renderLayout nodeId currentFrame layout knownFrames =
    -- If the node cursor is already calculated ...
    if Dict.member nodeId knownFrames then
        -- ... then we are done.
        knownFrames

    else
        -- Get all the outoing tracks from this node.
        -- TODO Consider incoming tracks too.
        Graph.outgoing nodeId layout.graph
            -- Calculate the next node position for each track and collect them.
            |> Set.foldl
                (\nextNodeId acc ->
                    -- Get the track for this pair of nodes.
                    case Graph.getEdgeData nodeId nextNodeId layout.graph of
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
coordsFor pos ( fromNode, toNode ) layout =
    Graph.getEdgeData fromNode toNode layout.graph
        |> Maybe.andThen
            (\track ->
                cursors layout |> Dict.get fromNode |> Maybe.map (\cursor -> getPositionOnTrack pos cursor track)
            )


{-| Split the layout in a traversable graph and a non-traversable, based on the current switch state.
-}
partitionGraph : Layout -> Dict Int Int -> ( G, G )
partitionGraph layout switchStates =
    let
        inactiveEdges =
            switchStates
                |> Dict.foldl
                    (\switchId switchState buf ->
                        buf
                            ++ (layout.switches
                                    |> Array.get switchId
                                    |> Maybe.map (\sw -> Switch.inactiveEdges sw switchState)
                                    |> Maybe.withDefault []
                               )
                    )
                    []
                |> Debug.log "Active edges: "
    in
    layout.graph
        |> Graph.edgesWithData
        |> List.foldl
            (\( from, to, maybeData ) ( usable, unusable ) ->
                case maybeData of
                    Nothing ->
                        -- This should never happen.
                        ( usable, unusable )

                    Just edgeData ->
                        if List.member ( from, to ) inactiveEdges then
                            ( Graph.removeEdge from to usable, Graph.insertEdgeData from to edgeData unusable )

                        else
                            ( usable, unusable )
            )
            ( layout.graph, Graph.empty )


previousTrack : Location -> Layout -> Dict Int Int -> Maybe Location
previousTrack loc layout switchStates =
    let
        ( g, _ ) =
            partitionGraph layout switchStates

        ( from, to ) =
            loc.edge
    in
    case loc.orientation of
        Aligned ->
            let
                inc =
                    Graph.incoming from g |> Set.toList

                out =
                    Graph.outgoing from g |> Set.remove to |> Set.toList
            in
            case ( inc, out ) of
                ( e :: _, _ ) ->
                    Just
                        { edge = ( e, from )
                        , pos = Graph.getEdgeData e from g |> Maybe.map Track.length |> Maybe.withDefault Quantity.zero
                        , orientation = Aligned
                        }

                ( _, e :: _ ) ->
                    Just
                        { edge = ( from, e )
                        , pos = Quantity.zero
                        , orientation = Reversed
                        }

                ( _, _ ) ->
                    Nothing

        Reversed ->
            let
                inc =
                    Graph.incoming to g |> Set.remove from |> Set.toList

                out =
                    Graph.outgoing to g |> Set.toList
            in
            case ( inc, out ) of
                ( e :: _, _ ) ->
                    Just
                        { edge = ( e, to )
                        , pos = Graph.getEdgeData e to g |> Maybe.map Track.length |> Maybe.withDefault Quantity.zero
                        , orientation = Reversed
                        }

                ( _, e :: _ ) ->
                    Just
                        { edge = ( to, e )
                        , pos = Quantity.zero
                        , orientation = Aligned
                        }

                ( _, _ ) ->
                    Nothing


nextTrack : Location -> Layout -> Dict Int Int -> Maybe Location
nextTrack loc layout switchStates =
    let
        iLoc =
            { loc | orientation = Orientation.invert loc.orientation }
    in
    previousTrack iLoc layout switchStates
        |> Maybe.map (\l -> { l | orientation = Orientation.invert l.orientation })



-- Views


toSvg : Layout -> Dict Int Int -> Svg msg
toSvg layout switchStates =
    let
        allFrames =
            cursors layout

        ( usableEdges, _ ) =
            partitionGraph layout switchStates
    in
    Svg.g [ Svg.Attributes.id "layout" ]
        (Graph.edgesWithData layout.graph
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
                                (Track.toSvg track (Graph.memberEdge ( from, to ) usableEdges))

                        _ ->
                            Svg.g [ id trackId ] []
                )
        )



-- Samples


initialLayout : Layout
initialLayout =
    { graph =
        Graph.empty
            |> insertEdgeData 0 1 (StraightTrack (Length.meters 75.0))
            |> insertEdgeData 1 2 (CurvedTrack (Length.meters 300.0) (Angle.degrees 15.0))
            |> insertEdgeData 2 4 (CurvedTrack (Length.meters 300) (Angle.degrees -15))
            |> insertEdgeData 1 3 (StraightTrack (Length.meters 77.645))
            |> insertEdgeData 3 4 (StraightTrack (Length.meters 77.645))
    , switches = Array.fromList [ { edges = Array.fromList [ ( 1, 2 ), ( 1, 3 ) ], configs = Array.fromList [ [ 0 ], [ 1 ] ] } ]
    }



-- TODO: Refactor so we don't need to expose the Graph. Only used for the starting location of the train. Having map exits should fix that.


toGraph : Layout -> Graph Int () Track
toGraph layout =
    layout.graph



-- JSON Decode


decoder : Decoder Layout
decoder =
    -- TODO Fix this
    Decode.succeed { graph = Graph.empty, switches = Array.empty }


locationDecoder : Decoder Location
locationDecoder =
    Decode.map3 Location
        (Decode.field "edge" edgeDecoder)
        (Decode.field "pos" (Decode.float |> Decode.map Length.meters))
        (Decode.field "orientation" orientationDecoder)


edgeDecoder : Decoder ( Int, Int )
edgeDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "from" Decode.int)
        (Decode.field "to" Decode.int)


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "aligned" ->
                        Decode.succeed Aligned

                    _ ->
                        Decode.fail "Invalid orientation"
            )



-- JSON encode


encode : Layout -> Value
encode layout =
    Encode.object
        [ ( "edges", Encode.list encodeEdge (Graph.edgesWithData layout.graph) )
        , ( "switches", Encode.array encodeSwitch layout.switches )
        ]


encodeSwitch : Switch -> Value
encodeSwitch switch =
    Encode.object
        [ ( "edges", Encode.array encodeVertex switch.edges )
        , ( "configs", Encode.array (Encode.list Encode.int) switch.configs )
        ]


encodeEdge : ( Int, Int, Maybe Track ) -> Value
encodeEdge ( from, to, mTrack ) =
    Encode.object
        [ ( "from", Encode.int from )
        , ( "to", Encode.int to )
        , ( "track"
          , case mTrack of
                Nothing ->
                    Encode.null

                Just t ->
                    Track.encode t
          )
        ]


encodeLocation : Location -> Value
encodeLocation loc =
    Encode.object
        [ ( "edge", encodeVertex loc.edge )
        , ( "pos", loc.pos |> Length.inMeters |> Encode.float )
        , ( "orientation", Orientation.encode loc.orientation )
        ]


encodeVertex : ( Int, Int ) -> Value
encodeVertex ( from, to ) =
    Encode.object [ ( "from", Encode.int from ), ( "to", Encode.int to ) ]
