module Railroad.Track exposing (Track(..), length)


type Track
    = StraightTrack
        { length : Float -- in m
        }
    | CurvedTrack
        { radius : Float -- in m
        , angle : Float -- in degrees, why not
        }


length : Track -> Float
length track =
    case track of
        StraightTrack s ->
            s.length

        CurvedTrack c ->
            pi * c.radius * c.angle / 180.0
