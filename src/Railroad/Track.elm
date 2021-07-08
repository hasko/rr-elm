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

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Railroad.Util exposing (Cursor)


type Track
    = StraightTrack TrackId Float -- length in m
    | CurvedTrack TrackId Float Float -- radius in m, angle in degrees
    | Point TrackId Handedness Float Float Float -- length in m, radius in m, angle in degrees


type Handedness
    = RightHanded
    | LeftHanded


handToString : Handedness -> String
handToString handedness =
    case handedness of
        LeftHanded ->
            "left"

        RightHanded ->
            "right"


type TrackId
    = TrackId Int


length : Track -> Int -> Float
length track connection =
    case track of
        StraightTrack _ l ->
            l

        CurvedTrack _ r a ->
            pi * r * abs a / 180.0

        Point _ hand l r a ->
            if connection == 0 then
                l

            else
                pi * r * abs a / 180.0


connectors : Track -> Int
connectors track =
    case track of
        StraightTrack _ _ ->
            2

        CurvedTrack _ _ _ ->
            2

        Point _ _ _ _ _ ->
            3


connections : Track -> Array ( Int, Int )
connections track =
    case track of
        StraightTrack _ _ ->
            Array.fromList [ ( 0, 1 ) ]

        CurvedTrack _ _ _ ->
            Array.fromList [ ( 0, 1 ) ]

        Point _ _ _ _ _ ->
            Array.fromList [ ( 0, 1 ), ( 0, 2 ) ]


activeConnections : Track -> Array Int
activeConnections _ =
    Array.fromList [ 1 ]


moveCursor : Cursor -> Track -> Int -> Cursor
moveCursor cursor track connection =
    case track of
        StraightTrack _ l ->
            getPositionOnTrack l cursor track connection

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

        Point _ hand l r a ->
            if connection == 0 then
                { cursor | x = cursor.x + l * cos (degrees cursor.dir), y = cursor.y + l * sin (degrees cursor.dir) }

            else
                let
                    newDir =
                        case hand of
                            RightHanded ->
                                cursor.dir + a

                            LeftHanded ->
                                cursor.dir - a

                    avgDirRad =
                        degrees ((cursor.dir + newDir) / 2.0)

                    s =
                        2 * r * sin (degrees (abs a) / 2)
                in
                Cursor
                    (cursor.x + s * cos avgDirRad)
                    (cursor.y + s * sin avgDirRad)
                    newDir


getPositionOnTrack : Float -> Cursor -> Track -> Int -> Cursor
getPositionOnTrack trackPosition cursor track conn =
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
                    a * trackPosition / length track 0

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

        Point _ hand l r a ->
            getPositionOnTrack trackPosition
                cursor
                (if conn == 0 then
                    StraightTrack (TrackId 0) l

                 else
                    CurvedTrack (TrackId 0) r a
                )
                0



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

        Point trackId hand l r a ->
            Encode.object
                [ ( "id", Encode.int (trackIdToInt trackId) )
                , ( "type", Encode.string "point" )
                , ( "hand", Encode.string (handToString hand) )
                , ( "length", Encode.float l )
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

        "point" ->
            Decode.map5 Point
                trackIdDecoder
                handednessDecoder
                (Decode.field "length" Decode.float)
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")


handednessDecoder : Decoder Handedness
handednessDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "right" ->
                        Decode.succeed RightHanded

                    "left" ->
                        Decode.succeed LeftHanded

                    _ ->
                        Decode.fail "Invalid Handedness"
            )


trackIdDecoder : Decoder TrackId
trackIdDecoder =
    Decode.map TrackId (Decode.field "id" Decode.int)



-- TODO Remove temporary breach of opaque type


intToTrackId i =
    TrackId i
