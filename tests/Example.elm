module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "Train inside track" (\_ -> trainGeometry case1 |> Expect.equal (LineSegment fromEndpoints (Point2d.meters 90 0) (Point 10 0)))


case1 : Layout
case1 =
    let
        track =
            Track (Connector (Point2d.meters 0 0)) (Connector (Point2d.meters 100 0))
    in
    { layout = { tracks = [ track ] }
    , trains = { track = track, pos = Length.meters 90, orient = Forward, length = Length.meters 80 }
    }
