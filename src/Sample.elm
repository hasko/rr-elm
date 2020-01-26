module Sample exposing (sampleLayout)

import Railroad.Layout as Layout exposing (Connector(..), Layout, Track(..), addTrack)


sampleLayout : Layout
sampleLayout =
    let
        c1 =
            Connector 100 100

        c2 =
            Connector 200 120

        c3 =
            Connector 80 50

        c4 =
            Connector 200 30

        t1 =
            Track c1 c2

        t2 =
            Track c1 c3

        t3 =
            Track c3 c4

        t4 =
            Track c2 c4
    in
    List.foldl addTrack Layout.empty [ t1, t2, t3, t4 ]
