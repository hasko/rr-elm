module Railroad.Track exposing (Track(..), decoder, encode, length)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Track
    = StraightTrack Float -- length in m
    | CurvedTrack Float Float -- radius in m, angle in degrees


length : Track -> Float
length track =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            pi * r * a / 180.0



-- JSON encode and decode


encode : Track -> Encode.Value
encode track =
    case track of
        StraightTrack l ->
            Encode.object
                [ ( "type", Encode.string "straight" )
                , ( "length", Encode.float l )
                ]

        CurvedTrack r a ->
            Encode.object
                [ ( "type", Encode.string "curved" )
                , ( "radius", Encode.float r )
                , ( "angle", Encode.float a )
                ]


decoder : Decoder Track
decoder =
    Decode.field "type" Decode.string |> Decode.andThen trackDecoder


trackDecoder : String -> Decoder Track
trackDecoder trackType =
    case trackType of
        "straight" ->
            Decode.map StraightTrack (Decode.field "length" Decode.float)

        "curved" ->
            Decode.map2 CurvedTrack
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")
