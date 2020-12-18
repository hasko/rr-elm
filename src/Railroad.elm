module Railroad exposing
    ( TrainState
    , move
    )


type alias TrainState =
    { name : String
    , length : Float -- in m
    , speed : Float -- in m/s
    , track : Int
    , trackPosition : Float -- location of train head in m from the track start
    }


move : Float -> TrainState -> TrainState
move millis state =
    { state | trackPosition = state.trackPosition + state.speed * millis / 1000.0 }
