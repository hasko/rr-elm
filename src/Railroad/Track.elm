module Railroad.Track exposing
    ( Sweep(..)
    , Track(..)
    , decoder
    , encode
    , length
    , moveCursor
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Railroad.Util exposing (Cursor)


type Track
    = StraightTrack Float -- length in m
    | CurvedTrack Float Float Sweep -- radius in m, angle in degrees


type Sweep
    = CW
    | CCW


length : Track -> Float
length track =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a _ ->
            pi * r * a / 180.0


moveCursor : Cursor -> Track -> Cursor
moveCursor cursor track =
    case track of
        StraightTrack l ->
            Cursor
                (cursor.x + l * cos (degrees cursor.dir))
                (cursor.y + l * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack r a sweep ->
            let
                newDir =
                    cursor.dir
                        + (case sweep of
                            CW ->
                                a

                            CCW ->
                                -a
                          )

                avgDirRad =
                    degrees ((cursor.dir + newDir) / 2.0)

                s =
                    2 * r * sin (degrees a / 2)
            in
            Cursor
                (cursor.x + s * cos avgDirRad)
                (cursor.y + s * sin avgDirRad)
                newDir



-- Views
-- JSON encode and decode


encode : Track -> Encode.Value
encode track =
    case track of
        StraightTrack l ->
            Encode.object
                [ ( "type", Encode.string "straight" )
                , ( "length", Encode.float l )
                ]

        CurvedTrack r a sweep ->
            Encode.object
                [ ( "type", Encode.string "curved" )
                , ( "radius", Encode.float r )
                , ( "angle", Encode.float a )
                , ( "sweep"
                  , case sweep of
                        CW ->
                            Encode.string "cw"

                        CCW ->
                            Encode.string "ccw"
                  )
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
            Decode.map3 CurvedTrack
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)
                (Decode.field "sweep" Decode.string |> Decode.andThen sweepDecoder)

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")


sweepDecoder : String -> Decoder Sweep
sweepDecoder sweep =
    case sweep of
        "cw" ->
            Decode.succeed CW

        "ccw" ->
            Decode.succeed CCW

        other ->
            Decode.fail ("Trying to decode a curve sweep but " ++ other ++ " is not supported.")
