module Railroad exposing (Connector, Layout, State, Track, TrackOccupancy, Train, connectors, moved, sample, trackLength, tracksForTrain)

import List
import List.Unique exposing (filterDuplicates)


type alias Track =
    { from : Connector, to : Connector }


type alias Connector =
    { pos : { x : Float, y : Float } }


type alias Layout =
    { tracks : List Track }


type Orientation
    = Forward
    | Reverse


type alias State =
    { layout : Layout, trains : List Train }


type alias Train =
    { track : Track, pos : Float, orient : Orientation, length : Float, speed : Float }


type alias TrackOccupancy =
    { track : Track, from : Float, to : Float }


connectors : Layout -> List Connector
connectors layout =
    List.map (\t -> [ t.from, t.to ]) layout.tracks |> List.foldl (++) [] |> filterDuplicates


tracksForTrain : Train -> List TrackOccupancy
tracksForTrain train =
    --TODO A train can cover multiple tracks.
    [ { track = train.track
      , from = train.pos
      , to =
            case train.orient of
                Forward ->
                    train.pos - train.length

                Reverse ->
                    train.pos + train.length
      }
    ]


trackLength : Track -> Float
trackLength track =
    sqrt ((track.to.pos.x - track.from.pos.x) ^ 2 + (track.to.pos.y - track.to.pos.x) ^ 2)


moved : Int -> State -> State
moved millis state =
    { state | trains = List.map (movedTrain millis) state.trains }


movedTrain : Int -> Train -> Train
movedTrain millis train =
    { train
        | pos =
            case train.orient of
                Forward ->
                    train.pos + train.speed * toFloat millis / 1000

                Reverse ->
                    train.pos - train.speed * toFloat millis / 1000
    }


sample : State
sample =
    let
        c1 =
            Connector { x = 100, y = 100 }

        t1 =
            Track c1 (Connector { x = 200, y = 120 })
    in
    { layout =
        { tracks =
            [ t1
            , Track c1 (Connector { x = 80, y = 50 })
            ]
        }
    , trains = [ { track = t1, pos = 50, orient = Reverse, length = 30, speed = 11.1 } ]
    }
