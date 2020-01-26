module Railroad.Layout exposing
    ( Connector(..)
    , Layout
    , Track(..)
    , addTrack
    , connectors
    , empty
    , getConnectors
    , getNextTrack
    , getPosition
    , getPreviousTrack
    , getTrackName
    , trackLength
    , tracks
    )

import List
import List.Extra
import Railroad.Orientation as Orientation exposing (Orientation(..))


type Layout
    = Layout (List Track)


type Track
    = Track Connector Connector


getTrackName : Track -> Layout -> String
getTrackName t (Layout tl) =
    case List.Extra.elemIndex t tl of
        Nothing ->
            "[Unknown track]"

        Just n ->
            "[Unnamed track " ++ String.fromInt n ++ "]"


type Connector
    = Connector Float Float


type alias Point =
    { x : Float, y : Float }


{-|


# Constructing

-}
empty : Layout
empty =
    Layout []


addTrack : Track -> Layout -> Layout
addTrack track (Layout t) =
    Layout (track :: t)


{-|


# Querying

-}
connectors : Layout -> List Connector
connectors (Layout t) =
    List.map trackConnectors t
        |> List.foldr (++) []
        |> List.Extra.uniqueBy (\(Connector x y) -> x ^ y)


trackConnectors : Track -> List Connector
trackConnectors (Track from to) =
    [ from, to ]


tracks : Layout -> List Track
tracks (Layout t) =
    t


getPreviousTrack : Track -> Layout -> Maybe ( Track, Orientation )
getPreviousTrack t l =
    let
        (Track from _) =
            t
    in
    getSubsequentTrack t from l


getNextTrack : Track -> Layout -> Maybe ( Track, Orientation )
getNextTrack t l =
    let
        (Track _ to) =
            t
    in
    getSubsequentTrack t to l


getSubsequentTrack : Track -> Connector -> Layout -> Maybe ( Track, Orientation )
getSubsequentTrack t c (Layout tl) =
    --TODO Allow for points/switches. Currently we assume that there is only one other track.
    case List.filter (\eachTrack -> eachTrack /= t && List.member c (trackConnectors eachTrack)) tl |> List.head of
        Nothing ->
            Nothing

        Just ot ->
            let
                (Track otherFrom _) =
                    ot

                orient =
                    if c == otherFrom then
                        Orientation.Forward

                    else
                        Orientation.Reverse
            in
            Just ( ot, orient )


{-| Return the length of a track. TODO Currently, this is assuming it is a straight track. Later, we will support splines.
-}
trackLength : Track -> Float
trackLength (Track from to) =
    let
        (Connector x1 y1) =
            from

        (Connector x2 y2) =
            to
    in
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


getPosition : Connector -> Point
getPosition (Connector x y) =
    Point x y


getConnectors : Track -> { from : Connector, to : Connector }
getConnectors (Track from to) =
    { from = from, to = to }
