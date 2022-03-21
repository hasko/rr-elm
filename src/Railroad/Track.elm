module Railroad.Track exposing
    ( Track(..)
    , decoder
    , encode
    , getPositionOnTrack
    , length
    , moveCursor
    , toSvg
    )

import Array exposing (Array)
import Cursor exposing (Cursor)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Svg exposing (Svg)
import Svg.Attributes as SA


type Track
    = StraightTrack Float -- length in m
    | CurvedTrack Float Float -- radius in m, angle in degrees
    | Point Handedness Float Float Float -- length in m, radius in m, angle in degrees


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


length : Track -> Int -> Float
length track connection =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            pi * r * abs a / 180.0

        Point hand l r a ->
            if connection == 0 then
                l

            else
                pi * r * abs a / 180.0


connectors : Track -> Int
connectors track =
    case track of
        StraightTrack _ ->
            2

        CurvedTrack _ _ ->
            2

        Point _ _ _ _ ->
            3


connectorCursors : Track -> Array Cursor
connectorCursors track =
    Array.fromList
        (case track of
            StraightTrack l ->
                [ Cursor 0 0 0, Cursor l 0 0 ]

            CurvedTrack r a ->
                [ Cursor 0 0 0, Cursor (r * cos (degrees a)) (r * sin (degrees a)) a ]

            Point hand l r a ->
                let
                    y =
                        r * sin (degrees a)

                    f =
                        case hand of
                            RightHanded ->
                                1

                            LeftHanded ->
                                -1
                in
                [ Cursor 0 0 0, Cursor l 0 0, Cursor (r * cos (degrees a)) (y * f) (a * f) ]
        )


connections : Track -> Array ( Int, Int )
connections track =
    case track of
        StraightTrack _ ->
            Array.fromList [ ( 0, 1 ) ]

        CurvedTrack _ _ ->
            Array.fromList [ ( 0, 1 ) ]

        Point _ _ _ _ ->
            Array.fromList [ ( 0, 1 ), ( 0, 2 ) ]


activeConnections : Track -> Array Int
activeConnections _ =
    Array.fromList [ 1 ]


moveCursor : Cursor -> Track -> Int -> Cursor
moveCursor cursor track connection =
    case track of
        StraightTrack l ->
            getPositionOnTrack l cursor track connection

        CurvedTrack r a ->
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

        Point hand l r a ->
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
        StraightTrack _ ->
            Cursor
                (cursor.x + trackPosition * cos (degrees cursor.dir))
                (cursor.y + trackPosition * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack r a ->
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

        Point hand l r a ->
            getPositionOnTrack trackPosition
                cursor
                (if conn == 0 then
                    StraightTrack l

                 else
                    CurvedTrack r a
                )
                0



-- SVG


toSvg : Track -> Svg msg
toSvg track =
    let
        cc =
            Debug.log "cc" (connectorCursors track)

        cref =
            Array.get 0 cc

        xref =
            cref |> Maybe.map .x |> Maybe.withDefault 0

        yref =
            cref |> Maybe.map .y |> Maybe.withDefault 0

        aref =
            cref |> Maybe.map .dir |> Maybe.withDefault 0
    in
    case track of
        StraightTrack s ->
            Svg.line
                [ SA.x1 "0"
                , SA.y1 "0"
                , Array.get 1 cc |> Maybe.map .x |> Maybe.withDefault 0 |> String.fromFloat |> SA.x2
                , Array.get 1 cc |> Maybe.map .y |> Maybe.withDefault 0 |> String.fromFloat |> SA.y2
                , SA.stroke "grey"
                , SA.strokeWidth "1.435"
                ]
                []

        CurvedTrack r a ->
            Svg.path
                [ SA.d
                    ("M 0,0 A "
                        ++ (r |> String.fromFloat)
                        ++ " "
                        ++ (r |> String.fromFloat)
                        -- ellipse rotation
                        ++ " 0 "
                        -- large arc flag
                        ++ (if abs a <= 180.0 then
                                " 0"

                            else
                                " 1"
                           )
                        -- sweep flag
                        ++ (if a >= 0 then
                                " 1 "

                            else
                                " 0 "
                           )
                        ++ (Array.get 1 cc |> Maybe.map .x |> Maybe.withDefault 0 |> String.fromFloat)
                        ++ " "
                        ++ (Array.get 1 cc |> Maybe.map .y |> Maybe.withDefault 0 |> String.fromFloat)
                    )
                , SA.fill "none"
                , SA.stroke "green"
                , SA.strokeWidth "1.435"
                ]
                []

        Point hand l r a ->
            -- TODO Draw this properly
            Svg.g [] []



-- JSON


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

        Point hand l r a ->
            Encode.object
                [ ( "type", Encode.string "point" )
                , ( "hand", Encode.string (handToString hand) )
                , ( "length", Encode.float l )
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

        "point" ->
            Decode.map4 Point
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
