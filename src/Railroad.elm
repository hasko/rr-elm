module Railroad exposing (State, TrackOccupancy, Train, TrainState(..), moved, sample, tracksForTrain)

import Dict exposing (Dict)
import List
import Railroad.Layout as Layout exposing (Connector, Layout, Track)
import Railroad.Orientation exposing (..)


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

        tl =
            Layout.trackLength newLoc.track
    in
    --TODO Make recursive so it works also for very short successor tracks. Also, refactor.
    if newLoc.pos < 0 then
        case Layout.getPreviousTrack loc.track of
            Nothing ->
                { train | loc = { newLoc | pos = 0 }, speed = 0, state = EmergencyStop }

            Just ( newTrack, newOrient ) ->
                { train
                    | loc =
                        { newLoc
                            | track = newTrack
                            , orient = newOrient
                            , pos =
                                case newOrient of
                                    Forward ->
                                        negate newLoc.pos

                                    Reverse ->
                                        Layout.trackLength newTrack + newLoc.pos
                        }
                }

    else if newLoc.pos > tl then
        case Layout.getNextTrack loc.track of
            Nothing ->
                { train | loc = { newLoc | pos = tl }, speed = 0, state = EmergencyStop }

            Just ( newTrack, newOrient ) ->
                { train
                    | loc =
                        { newLoc
                            | track = newTrack
                            , orient = newOrient
                            , pos =
                                case newOrient of
                                    Forward ->
                                        newLoc.pos - tl

                                    Reverse ->
                                        Layout.trackLength newTrack - (newLoc.pos - tl)
                        }
                }

    else
        --TODO Add > trackLength
        { train | loc = newLoc }


sample : Maybe State
sample =
    let
        cd =
            Dict.empty
                |> Dict.insert 1 { x = 100, y = 100 }
                |> Dict.insert 2 { x = 200, y = 120 }
                |> Dict.insert 3 { x = 80, y = 50 }

        td =
            Dict.empty
                |> Dict.insert 1 { from = 1, to = 2, geometry = Layout.StraightTrack }
                |> Dict.insert 2 { from = 1, to = 3, geometry = Layout.StraightTrack }

        layoutResult =
            Layout.build { connectors = cd, tracks = td }
    in
    case layoutResult of
        Err t ->
            Nothing

        Ok layout ->
            case Layout.getTrack 1 layout of
                Nothing ->
                    Nothing

                Just track ->
                    Just
                        { layout = layout
                        , trains = [ { loc = { track = track, pos = 50, orient = Reverse }, length = 30, speed = 11.1, state = Normal } ]
                        }
