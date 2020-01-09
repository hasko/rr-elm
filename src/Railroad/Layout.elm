module Railroad.Layout exposing (Connector, Layout, Track, TrackGeometry(..), build, connectors, emptyLayout, getConnectors, getPosition, getTrack, trackLength, tracks)

import Dict exposing (Dict)


type Layout
    = Layout (Dict Int ConnectorData) (Dict Int TrackData)


type Track
    = Track Int Layout


type Connector
    = Connector Int Layout


type TrackGeometry
    = StraightTrack
    | BezierTrack


type alias Point =
    { x : Float, y : Float }


{-|


# Internally used type aliases

-}
type alias ConnectorData =
    { pos : Point }


type alias TrackData =
    { from : Int, to : Int, geometry : TrackGeometry }


{-|


# Constructing

-}
emptyLayout : Layout
emptyLayout =
    Layout Dict.empty Dict.empty


build : { connectors : Dict Int Point, tracks : Dict Int TrackData } -> Result String Layout
build spec =
    let
        connDataDict =
            Dict.map (\index point -> { pos = point }) spec.connectors

        trackDataDict =
            Dict.map (\index trackSpec -> trackSpec) spec.tracks
    in
    Ok (Layout connDataDict trackDataDict)


{-|


# Querying

-}
connectors : Layout -> List Connector
connectors layout =
    let
        (Layout connDict _) =
            layout
    in
    Dict.map (\index _ -> Connector index layout) connDict |> Dict.values


tracks : Layout -> List Track
tracks layout =
    let
        (Layout _ trackDict) =
            layout
    in
    Dict.map (\index _ -> Track index layout) trackDict |> Dict.values


getTrack : Int -> Layout -> Maybe Track
getTrack id layout =
    let
        (Layout _ trackDict) =
            layout
    in
    if Dict.member id trackDict then
        Just (Track id layout)

    else
        Nothing


{-| Return the length of a track. TODO Currently, this is assuming it is a straight track. Later, we will support splines.
-}
trackLength : Track -> Float
trackLength track =
    let
        conns =
            getConnectors track

        fromPos =
            getPosition conns.from

        toPos =
            getPosition conns.to
    in
    sqrt ((toPos.x - fromPos.x) ^ 2 + (toPos.y - fromPos.y) ^ 2)


getPosition : Connector -> Point
getPosition (Connector id layout) =
    getConnectorData id layout |> .pos


getConnectors : Track -> { from : Connector, to : Connector }
getConnectors (Track id layout) =
    let
        trackData =
            getTrackData id layout
    in
    { from = Connector trackData.from layout, to = Connector trackData.to layout }


getTrackData : Int -> Layout -> TrackData
getTrackData id (Layout _ trackDict) =
    case Dict.get id trackDict of
        Nothing ->
            dummyTrackData

        Just td ->
            td


dummyTrackData : TrackData
dummyTrackData =
    { from = 0, to = 0, geometry = StraightTrack }


getConnectorData : Int -> Layout -> ConnectorData
getConnectorData id (Layout connDict _) =
    case Dict.get id connDict of
        Nothing ->
            dummyConnectorData

        Just cd ->
            cd


dummyConnectorData : ConnectorData
dummyConnectorData =
    { pos = { x = 0 / 0, y = 0 / 0 } }
