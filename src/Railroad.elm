module Railroad exposing (Location, State, Train, TrainState(..), moved, sample)

import Dict exposing (Dict)
import List
import Railroad.Layout as Layout exposing (Connector, Layout, Track)
import Railroad.Orientation exposing (..)
import Sample


type alias State =
    { layout : Layout, trains : List Train }


type alias Train =
    { loc : Location, length : Float, speed : Float, state : TrainState }


type alias Location =
    { track : Track, pos : Float, orient : Orientation }


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


{-| Take a number of milliseconds since the last frame and the old state, and return the new state.
-}
moved : Int -> State -> State
moved millis state =
    --TODO Add building tracks and other things.
    { state | trains = List.map (\t -> movedTrain millis t state.layout) state.trains }


{-| Takes a number of milliseconds and returns a modified Train that has moved by an amount determined by its speed.
-}
movedTrain : Int -> Train -> Layout -> Train
movedTrain millis train layout =
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
        case Layout.getPreviousTrack loc.track layout of
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
        case Layout.getNextTrack loc.track layout of
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
        layout =
            Sample.sampleLayout

        t1 =
            Layout.Track (Layout.Connector 100 100) (Layout.Connector 200 120)
    in
    Just
        { layout = layout
        , trains = [ { loc = { track = t1, pos = 50, orient = Reverse }, length = 30, speed = 11.1, state = Normal } ]
        }
