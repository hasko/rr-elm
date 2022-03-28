module Railroad.Track exposing
    ( Track(..)
    , decoder
    , encode
    , getPositionOnTrack
    , length
    , moveCursor
    , toSvg
    )

import Angle
import Arc2d
import Array exposing (Array)
import Geometry.Svg as Gsvg
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Length exposing (Length)
import Point2d
import Quantity
import Railroad.Util exposing (Cursor)
import Svg exposing (Svg, g)
import Svg.Attributes as SA


type Track
    = StraightTrack Length
    | CurvedTrack Float Float -- radius in m, angle in degrees
    | Point Handedness Length Float Float -- length in m, radius in m, angle in degrees


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


length : Track -> Int -> Length
length track connection =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            Length.meters (pi * r * abs a / 180.0)

        Point hand l r a ->
            if connection == 0 then
                l

            else
                Length.meters (pi * r * abs a / 180.0)


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
                [ Cursor 0 0 0, Cursor (Length.inMeters l) 0 0 ]

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
                [ Cursor 0 0 0, Cursor (Length.inMeters l) 0 0, Cursor (r * cos (degrees a)) (y * f) (a * f) ]
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
                { cursor
                    | x = cursor.x + Length.inMeters l * cos (degrees cursor.dir)
                    , y = cursor.y + Length.inMeters l * sin (degrees cursor.dir)
                }

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


getPositionOnTrack : Length -> Cursor -> Track -> Int -> Cursor
getPositionOnTrack trackPosition cursor track conn =
    case track of
        StraightTrack _ ->
            Cursor
                (cursor.x + Length.inMeters trackPosition * cos (degrees cursor.dir))
                (cursor.y + Length.inMeters trackPosition * sin (degrees cursor.dir))
                cursor.dir

        CurvedTrack r a ->
            let
                -- Calculate amount of angle covered
                a2 =
                    a * Quantity.ratio trackPosition (length track 0)

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


toSvg : Track -> List (Svg msg)
toSvg track =
    case connectorCursors track |> Array.get 1 of
        Nothing ->
            []

        Just cc1 ->
            [ case track of
                StraightTrack s ->
                    Svg.line
                        [ SA.x1 "0"
                        , SA.y1 "0"
                        , cc1.x |> String.fromFloat |> SA.x2
                        , cc1.y |> String.fromFloat |> SA.y2
                        , SA.stroke "grey"
                        , SA.strokeWidth "1.435"
                        ]
                        []

                CurvedTrack r a ->
                    let
                        arc =
                            Point2d.origin
                                |> Arc2d.sweptAround
                                    (Point2d.meters 0
                                        (if a >= 0 then
                                            r

                                         else
                                            -r
                                        )
                                    )
                                    (Angle.degrees a)
                    in
                    Gsvg.arc2d
                        [ SA.fill "none"
                        , SA.stroke "grey"
                        , SA.strokeWidth "1.435"
                        ]
                        arc

                Point hand l r a ->
                    -- TODO Draw this properly
                    Svg.g [] []
            , Svg.line
                [ SA.x1 "0"
                , SA.y1 "-1"
                , SA.x2 "0"
                , SA.y2 "1"
                , SA.stroke "black"
                , SA.strokeWidth "1px"
                , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
                ]
                []
            ]



-- JSON


encode : Track -> Encode.Value
encode track =
    case track of
        StraightTrack l ->
            Encode.object
                [ ( "type", Encode.string "straight" )
                , ( "length", Encode.float (Length.inMeters l) )
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
                , ( "length", Encode.float (Length.inMeters l) )
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
            Decode.map StraightTrack (Decode.map Length.meters (Decode.field "length" Decode.float))

        "curved" ->
            Decode.map2 CurvedTrack
                (Decode.field "radius" Decode.float)
                (Decode.field "angle" Decode.float)

        "point" ->
            Decode.map4 Point
                handednessDecoder
                (Decode.map Length.meters (Decode.field "length" Decode.float))
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
