module Railroad.Layout exposing (Connector, Layout, Track, TrackGeometry(..), connectors, emptyLayout, trackLength)

import List.Unique exposing (filterDuplicates)


type alias Track =
    { from : Connector, to : Connector, geometry : TrackGeometry }


type TrackGeometry
    = StraightTrack
    | BezierTrack


type alias Connector =
    { pos : { x : Float, y : Float } }


type alias Layout =
    { tracks : List Track }


emptyLayout : Layout
emptyLayout =
    { tracks = [] }


connectors : Layout -> List Connector
connectors layout =
    List.map (\t -> [ t.from, t.to ]) layout.tracks |> List.foldl (++) [] |> filterDuplicates


{-| Return the length of a track. Currently, this is assuming it is a straight track. Later, we will support splines.
-}
trackLength : Track -> Float
trackLength track =
    sqrt ((track.to.pos.x - track.from.pos.x) ^ 2 + (track.to.pos.y - track.to.pos.x) ^ 2)
