module Railroad exposing (Connector, Layout, State, Track, Train, connectors, sample, tracksForTrain)

import List
import List.Unique exposing (filterDuplicates)


type alias Track =
    { from : Connector, to : Connector }


type alias Connector =
    { position : Position }


type alias Layout =
    { tracks : List Track }


type alias Position =
    { x : Int, y : Int }


type Orientation
    = Forward
    | Reverse


type alias State =
    { layout : Layout, trains : List Train }


type alias Train =
    { track : Track, pos : Float, orient : Orientation, length : Int }


connectors : Layout -> List Connector
connectors layout =
    List.map (\t -> [ t.from, t.to ]) layout.tracks |> List.foldl (++) [] |> filterDuplicates


tracksForTrain : Train -> List Track
tracksForTrain train =
    --TODO A train can cover multiple tracks.
    [ train.track ]


trackLength : Track -> Float
trackLength track =
    sqrt (toFloat ((track.from.position.x - track.to.position.x) ^ 2 + (track.from.position.y - track.to.position.y) ^ 2))


sample : State
sample =
    let
        c1 =
            Connector (Position 100 100)

        t1 =
            Track c1 (Connector (Position 200 120))
    in
    { layout =
        { tracks =
            [ t1
            , Track c1 (Connector (Position 80 50))
            ]
        }
    , trains = [ Train t1 50 Forward 30 ]
    }
