module Railroad exposing (State, TrackOccupancy, Train, moved, sample, tracksForTrain)

import List
import Railroad.Layout exposing (Connector, Layout, Track)


type Orientation
    = Forward
    | Reverse


byOrientation : Orientation -> number -> number
byOrientation orient n =
    case orient of
        Forward ->
            n

        Reverse ->
            negate n


reverse : Orientation -> Orientation
reverse orient =
    case orient of
        Forward ->
            Reverse

        Reverse ->
            Forward


type alias State =
    { layout : Layout, trains : List Train }


type alias Train =
    { loc : { track : Track, pos : Float, orient : Orientation }, length : Float, speed : Float, state : TrainState }


{-| TrainState can be:

  - Normal for normal operations
  - EmergencyStop when the train hit a dead end during its last move
  - Crashed if the train crashed into another one
  - OffMap if the train has left via an off-map connector.

In case of a crash or an emergency stop, the train will be located at the site.

-}
type TrainState
    = Normal
    | EmergencyStop
    | Crashed
    | OffMap


type alias TrackOccupancy =
    { track : Track, from : Float, to : Float }


tracksForTrain : Train -> List TrackOccupancy
tracksForTrain train =
    --TODO A train can cover multiple tracks.
    [ { track = train.loc.track
      , from = train.loc.pos
      , to =
            case train.loc.orient of
                Forward ->
                    train.loc.pos - train.length

                Reverse ->
                    train.loc.pos + train.length
      }
    ]


{-| Take a number of milliseconds since the last frame and the old state, and return the new state.
-}
moved : Int -> State -> State
moved millis state =
    --TODO Add building tracks and other things.
    { state | trains = List.map (movedTrain millis) state.trains }


{-| Takes a number of milliseconds and returns a modified Train that has moved by an amount determined by its speed.
-}
movedTrain : Int -> Train -> Train
movedTrain millis train =
    let
        loc =
            train.loc

        distanceMoved =
            byOrientation train.loc.orient (train.speed * toFloat millis / 1000)

        newLoc =
            { loc | pos = train.loc.pos + distanceMoved }
    in
    { train | loc = newLoc }


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
    , trains = [ { loc = { track = t1, pos = 50, orient = Reverse }, length = 30, speed = 11.1, state = Normal } ]
    }
