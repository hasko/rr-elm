module Railroad.Track exposing (Track(..), decoder, encode, length)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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


encode : Track -> Encode.Value
encode track =
    case track of
        StraightTrack s ->
            Encode.object
                [ ( "type", Encode.string "straight" )
                , ( "length", Encode.float s.length )
                ]

        CurvedTrack c ->
            Encode.object
                [ ( "type", Encode.string "curved" )
                , ( "radius", Encode.float c.radius )
                , ( "angle", Encode.float c.angle )
                ]


decoder : Decoder Track
decoder =
    Decode.field "type" Decode.string |> Decode.andThen trackDecoder


trackDecoder : String -> Decoder Track
trackDecoder trackType =
    case trackType of
        "straight" ->
            Decode.map (\l -> StraightTrack { length = l }) (Decode.field "length" Decode.float)

        "curved" ->
            Decode.map2 (\r a -> CurvedTrack { radius = r, angle = a })
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")
