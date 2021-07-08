module Railroad.Track exposing
    ( Track(..)
    , TrackId
    , decoder
    , encode
    , getPositionOnTrack
    , intToTrackId
    , length
    , moveCursor
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Railroad.Util exposing (Cursor)


type Track
    = StraightTrack TrackId Float -- length in m
    | CurvedTrack TrackId Float Float -- radius in m, angle in degrees


type TrackId
    = TrackId Int


length : Track -> Float
length track =
    case track of
        StraightTrack _ l ->
            l

        CurvedTrack _ r a ->
            pi * r * abs a / 180.0


moveCursor : Cursor -> Track -> Cursor
moveCursor cursor track =
    case track of
        StraightTrack _ l ->
            getPositionOnTrack l cursor track

        CurvedTrack _ r a ->
            let
                newDir =
                    cursor.dir + a

                avgDirRad =
                    degrees ((cursor.dir + newDir) / 2.0)

                s =
                    2 * r * sin (degrees (abs a) / 2)
            in
            Cursor
                (cursor.x + s * cos avgDirRad)
                (cursor.y + s * sin avgDirRad)
                newDir


getPositionOnTrack : Float -> Cursor -> Track -> Cursor
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack _ _ ->
            Cursor
                (cursor.x + trackPosition * cos (degrees cursor.dir))
                (cursor.y + trackPosition * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack _ r a ->
            let
                -- Calculate amount of angle covered
                a2 =
                    a * trackPosition / length track

                newDir =
                    cursor.dir + a2

                avgDirRad =
                    degrees ((cursor.dir + newDir) / 2.0)

                s =
                    2 * r * sin (degrees (abs a2) / 2)
            in
            Cursor
                (cursor.x + s * cos avgDirRad)
                (cursor.y + s * sin avgDirRad)
                newDir



-- JSON


encode : Track -> Encode.Value
encode track =
    case track of
        StraightTrack trackId l ->
            Encode.object
                [ ( "id", Encode.int (trackIdToInt trackId) )
                , ( "type", Encode.string "straight" )
                , ( "length", Encode.float l )
                ]

        CurvedTrack trackId r a ->
            Encode.object
                [ ( "id", Encode.int (trackIdToInt trackId) )
                , ( "type", Encode.string "curved" )
                , ( "radius", Encode.float r )
                , ( "angle", Encode.float a )
                ]


trackIdToInt : TrackId -> Int
trackIdToInt (TrackId i) =
    i


decoder : Decoder Track
decoder =
    Decode.field "type" Decode.string |> Decode.andThen trackDecoder


trackDecoder : String -> Decoder Track
trackDecoder trackType =
    case trackType of
        "straight" ->
            Decode.map2 StraightTrack trackIdDecoder (Decode.field "length" Decode.float)

        "curved" ->
            Decode.map3 CurvedTrack
                trackIdDecoder
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")


trackIdDecoder : Decoder TrackId
trackIdDecoder =
    Decode.map TrackId (Decode.field "id" Decode.int)



-- TODO Remove temporary breach of opaque type


intToTrackId i =
    TrackId i
