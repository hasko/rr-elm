module Railroad.Orientation exposing (Orientation(..), encode, invert, toString)

import Json.Encode as Encode exposing (Value)


type Orientation
    = Aligned
    | Reversed


invert : Orientation -> Orientation
invert o =
    case o of
        Aligned ->
            Reversed

        Reversed ->
            Aligned


encode : Orientation -> Value
encode o =
    case o of
        Aligned ->
            Encode.string "aligned"

        Reversed ->
            Encode.string "reversed"


toString : Orientation -> String
toString o =
    case o of
        Aligned ->
            "Aligned"

        Reversed ->
            "Reversed"
