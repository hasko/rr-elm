module Railroad exposing
    ( TrainState
    , move
    )


type alias TrainState =
    { name : String
    , length : Float
    , speed : Float
    , track : Int
    , trackPosition : Float
    }


move : Float -> TrainState -> TrainState
move millis state =
    { state | trackPosition = state.trackPosition + state.speed * millis / 1000.0 }
