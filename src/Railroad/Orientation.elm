module Railroad.Orientation exposing (Orientation(..), byOrientation, reverse, toString)


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


toString : Orientation -> String
toString orient =
    case orient of
        Forward ->
            "Forward"

        Reverse ->
            "Reverse"
