module Railroad.Orientation exposing (..)


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
