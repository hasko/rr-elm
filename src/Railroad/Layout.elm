module Railroad.Layout exposing
    ( Layout
    , Location
    , boundingBox
    , coordsFor
    , decoder
    , editSvg
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
    , tracksAhead
    , tracksBefore
    )

import Angle
import Array exposing (Array)
import Direction2d
import Frame2d
import Graph exposing (Graph, insertEdgeData)
import Html exposing (Html)
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length exposing (Length)
import Maybe exposing (Maybe(..))
import Maybe.Extra
import Point2d
import Quantity
import Railroad.Orientation as Orientation exposing (Orientation(..))
import Railroad.Switch as Switch exposing (Switch)
import Railroad.Track as Track exposing (Track(..), getPositionOnTrack, moveFrame)
import Railroad.Util exposing (Frame)
import Rect exposing (Rect(..))
import Set
import Svg exposing (Svg, rect, svg, switch)
import Svg.Attributes as SA exposing (id)


type alias Layout =
    { graph : G, switches : Array Switch }


type alias G =
    Graph Int () Track


type alias Edge =
    ( Int, Int, Track )


type alias Location =
    { edge : ( Int, Int ) -- The vertices
    , pos : Length -- The position on the track
    , orientation : Orientation
    }


trackAt : ( Int, Int ) -> Layout -> Maybe Track
trackAt ( from, to ) layout =
    Graph.getEdgeData from to layout.graph



{- Return the edges ahead from a given position, considering switch states, up to a given cut-off distance. -}


tracksAhead : Location -> Length -> Layout -> Array Int -> List Edge
tracksAhead loc cutoff layout switchStates =
    case trackAt loc.edge layout of
        Nothing ->
            []

        Just currentTrack ->
            let
                newCutoff =
                    case loc.orientation of
                        Aligned ->
                            cutoff |> Quantity.minus (Track.length currentTrack) |> Quantity.plus loc.pos

                        Reversed ->
                            cutoff |> Quantity.minus loc.pos

                res =
                    ( Tuple.first loc.edge, Tuple.second loc.edge, currentTrack )
            in
            if newCutoff |> Quantity.lessThanOrEqualToZero then
                [ res ]

            else
                case nextTrack loc layout switchStates of
                    Nothing ->
                        [ res ]

                    Just theNextTrack ->
                        res :: tracksAhead theNextTrack newCutoff layout switchStates


tracksBefore : Location -> Length -> Layout -> Array Int -> List Edge
tracksBefore loc cutoff layout switchStates =
    tracksAhead { loc | orientation = Orientation.invert loc.orientation } cutoff layout switchStates |> List.reverse


cursors : Layout -> IntDict Frame
cursors layout =
    -- Render the layout starting at connection 0 and at the origin, facing east, and return the resulting cursors.
    renderLayout 0 (Frame2d.atPoint (Point2d.meters 0 2.5)) layout IntDict.empty


boundingBox : Layout -> Rect
boundingBox layout =
    List.foldl
        (\c (Rect x1 y1 x2 y2) -> Rect (min x1 c.x) (min y1 c.y) (max x2 c.x) (max y2 c.y))
        (Rect 0 0 0 0)
        (cursors layout |> IntDict.values |> List.map (Frame2d.originPoint >> Point2d.toRecord Length.inMeters))


renderLayout : Int -> Frame -> Layout -> IntDict Frame -> IntDict Frame
renderLayout nodeId currentFrame layout knownFrames =
    -- If the node cursor is already calculated ...
    if IntDict.member nodeId knownFrames then
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
                (IntDict.insert nodeId currentFrame knownFrames)


coordsFor : Length -> ( Int, Int ) -> Layout -> Maybe Frame
coordsFor pos ( fromNode, toNode ) layout =
    Graph.getEdgeData fromNode toNode layout.graph
        |> Maybe.andThen
            (\track ->
                cursors layout |> IntDict.get fromNode |> Maybe.map (\cursor -> getPositionOnTrack pos cursor track)
            )


{-| Split the layout in a traversable graph and a non-traversable, based on the current switch state.
-}
partitionGraph : Layout -> Array Int -> ( List Edge, List Edge )
partitionGraph layout switchStates =
    let
        inactiveEdges =
            switchStates
                |> Array.indexedMap
                    (\switchId switchState ->
                        Array.get switchId layout.switches
                            |> Maybe.map (\sw -> Switch.inactiveEdges sw switchState)
                            |> Maybe.withDefault []
                    )
                |> Array.toList
                |> List.concat
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
                            ( usable, ( from, to, edgeData ) :: unusable )

                        else
                            ( ( from, to, edgeData ) :: usable, unusable )
            )
            ( [], [] )


previousTrack : Location -> Layout -> Array Int -> Maybe Location
previousTrack loc layout switchStates =
    let
        ( g, _ ) =
            -- Only look at usable tracks.
            partitionGraph layout switchStates

        ( from, to ) =
            loc.edge
    in
    case loc.orientation of
        Aligned ->
            -- The train is facing in the same direction as the track.
            let
                inc =
                    -- Get all tracks facing into the _from_ node; there should be at most one.
                    g |> List.filter (\( _, t, _ ) -> t == from) |> List.head

                out =
                    -- Get all tracks facing away from the _from_ node; again, at most one (excluding ours)
                    g |> List.filter (\( f, t, _ ) -> f == from && t /= to) |> List.head
            in
            case ( inc, out ) of
                ( Just ( f, t, e ), Nothing ) ->
                    Just
                        { edge = ( f, t )
                        , pos = Track.length e
                        , orientation = Aligned
                        }

                ( Nothing, Just ( f, t, _ ) ) ->
                    Just
                        { edge = ( f, t )
                        , pos = Quantity.zero
                        , orientation = Reversed
                        }

                ( _, _ ) ->
                    -- Something is wrong with the layout
                    Nothing

        Reversed ->
            let
                inc =
                    g |> List.filter (\( f, t, _ ) -> t == to && f /= from) |> List.head

                out =
                    g |> List.filter (\( f, _, _ ) -> f == to) |> List.head
            in
            case ( inc, out ) of
                ( Just ( f, t, e ), Nothing ) ->
                    Just
                        { edge = ( f, t )
                        , pos = Track.length e
                        , orientation = Aligned
                        }

                ( Nothing, Just ( f, t, _ ) ) ->
                    Just
                        { edge = ( f, t )
                        , pos = Quantity.zero
                        , orientation = Reversed
                        }

                ( _, _ ) ->
                    -- Something is wrong with the layout
                    Nothing


nextTrack : Location -> Layout -> Array Int -> Maybe Location
nextTrack loc layout switchStates =
    let
        iLoc =
            { loc | orientation = Orientation.invert loc.orientation }
    in
    previousTrack iLoc layout switchStates
        |> Maybe.map (\l -> { l | orientation = Orientation.invert l.orientation })



-- Views


toSvg : Layout -> Array Int -> Svg msg
toSvg layout switchStates =
    let
        allFrames =
            cursors layout

        ( usableEdges, unusableEdges ) =
            partitionGraph layout switchStates
    in
    Svg.g [ SA.id "layout" ]
        (tracksToSvg allFrames False unusableEdges ++ tracksToSvg allFrames True usableEdges)


tracksToSvg : IntDict Frame -> Bool -> List Edge -> List (Svg msg)
tracksToSvg allFrames enabled tuples =
    tuples
        |> List.map
            (\( from, to, track ) ->
                let
                    trackId =
                        "track-" ++ String.fromInt from ++ "-" ++ String.fromInt to

                    maybeRef =
                        IntDict.get from allFrames
                in
                case maybeRef of
                    Just ref ->
                        let
                            refP =
                                ref |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters

                            refA =
                                ref |> Frame2d.xDirection |> Direction2d.toAngle |> Angle.inDegrees
                        in
                        Svg.g
                            [ id trackId
                            , SA.transform
                                ("translate("
                                    ++ String.fromFloat refP.x
                                    ++ ","
                                    ++ String.fromFloat refP.y
                                    ++ ") rotate("
                                    ++ String.fromFloat refA
                                    ++ ")"
                                )
                            ]
                            (Track.toSvg track enabled)

                    Nothing ->
                        Svg.g [ id trackId ] []
            )


editSvg : Layout -> Float -> Svg.Attribute msg -> Html msg
editSvg layout zoom zoomHandler =
    svg
        [ SA.width "100%"
        , SA.viewBox "0 0 800 450"
        , SA.preserveAspectRatio "xMidYMid meet"
        , zoomHandler
        ]
        [ rect [ SA.x "0", SA.y "0", SA.width "800", SA.height "450", SA.fill "#fffff0" ] []
        , let
            allFrames =
                cursors layout
          in
          Svg.g [ SA.id "layout", SA.transform ("scale(" ++ String.fromFloat zoom ++ ")") ]
            (tracksToSvg allFrames
                True
                (Graph.edgesWithData layout.graph
                    |> List.map
                        (\( from, to, maybeTrack ) ->
                            case maybeTrack of
                                Just track ->
                                    Just ( from, to, track )

                                Nothing ->
                                    Nothing
                        )
                    |> Maybe.Extra.values
                )
            )
        ]



-- Samples


initialLayout : Layout
initialLayout =
    { graph =
        Graph.empty
            |> insertEdgeData 0 1 (StraightTrack (Length.meters 75.0))
            |> insertEdgeData 1 2 (CurvedTrack (Length.meters 300.0) (Angle.degrees 15.0))
            |> insertEdgeData 2 4 (CurvedTrack (Length.meters 300) (Angle.degrees -15))
            |> insertEdgeData 1 3 (StraightTrack (Length.meters 77.645))
            |> insertEdgeData 3 5 (StraightTrack (Length.meters 77.645))
            |> insertEdgeData 5 1000 MapExit
            |> insertEdgeData 0 1001 MapExit
            |> insertEdgeData 4 1002 MapExit
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
