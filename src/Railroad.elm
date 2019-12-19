module Railroad exposing (Connector, Layout, State, Track, Train, connectors, sample, tracksForTrain)

import Length exposing (Length, Meters)
import List
import List.Unique exposing (filterDuplicates)
import Point2d exposing (Point2d, distanceFrom, meters)


{-| The world coordinate system
-}
type World
    = World


type alias Track =
    { from : Connector, to : Connector }


type alias Connector =
    { position : Point2d Meters World }


type alias Layout =
    { tracks : List Track }


type Orientation
    = Forward
    | Reverse


type alias State =
    { layout : Layout, trains : List Train }


type alias Train =
    { track : Track, pos : Length, orient : Orientation, length : Length }


connectors : Layout -> List Connector
connectors layout =
    List.map (\t -> [ t.from, t.to ]) layout.tracks |> List.foldl (++) [] |> filterDuplicates


tracksForTrain : Train -> List Track
tracksForTrain train =
    --TODO A train can cover multiple tracks.
    [ train.track ]


trackLength : Track -> Length
trackLength track =
    distanceFrom track.to.position track.from.position


sample : State
sample =
    let
        c1 =
            Connector (Point2d.meters 100 100)

        t1 =
            Track c1 (Connector (Point2d.meters 200 120))
    in
    { layout =
        { tracks =
            [ t1
            , Track c1 (Connector (Point2d.meters 80 50))
            ]
        }
    , trains = [ Train t1 (Length.meters 50) Forward (Length.meters 30) ]
    }
