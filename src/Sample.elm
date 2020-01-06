module Sample exposing (sample)

import Railroad exposing (Layout, addBezierTrack, addConnector, addStraightTrack)


sampleLayout : Maybe Layout
sampleLayout =
    Just Layout.empty
        |> addConnector 1 100 100
        |> addConnector 2 200 120
        |> addConnector 3 80 50
        |> addConnector 4 70 20
        |> addTrack 1 2 StraightTrack
        |> addTrack 3 4 StraightTrack
        |> addTrack 2 3 BezierTrack
