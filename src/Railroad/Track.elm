module Railroad.Track exposing
    ( Track(..)
    , decoder
    , encode
    , getPositionOnTrack
    , length
    , moveCursor
    , toSvg
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Array exposing (Array)
import Frame2d
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
    | CurvedTrack Length Angle -- radius in m, angle in degrees


length : Track -> Length
length track =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            (Length.inMeters r * Angle.inRadians a) |> abs |> Length.meters


connectorCursors : Track -> ( Cursor, Cursor )
connectorCursors track =
    case track of
        StraightTrack l ->
            ( Cursor 0 0 0, Cursor (Length.inMeters l) 0 0 )

        CurvedTrack r a ->
            ( Cursor 0 0 0, Cursor (Length.inMeters r * Angle.cos a) (Length.inMeters r * Angle.sin a) (Angle.inDegrees a) )


moveCursor : Cursor -> Track -> Cursor
moveCursor cursor track =
    case track of
        StraightTrack l ->
            getPositionOnTrack l cursor track

        CurvedTrack r a ->
            let
                newDir =
                    cursor.dir + Angle.inDegrees a

                avgDirRad =
                    degrees ((cursor.dir + newDir) / 2.0)

                s =
                    2 * Length.inMeters r * sin (degrees (abs (Angle.inDegrees a)) / 2)
            in
            Cursor
                (cursor.x + s * cos avgDirRad)
                (cursor.y + s * sin avgDirRad)
                newDir


getPositionOnTrack : Length -> Cursor -> Track -> Cursor
getPositionOnTrack trackPosition cursor track =
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
                    Quantity.multiplyBy (Quantity.ratio trackPosition (length track)) a

                arc =
                    curveToArc r a2
                        |> Arc2d.placeIn
                            (Frame2d.withAngle (Angle.degrees cursor.dir)
                                (Point2d.meters cursor.x cursor.y)
                            )

                p =
                    Arc2d.endPoint arc |> Point2d.toRecord Length.inMeters
            in
            Cursor p.x p.y (cursor.dir + Angle.inDegrees a2)


curveToArc : Length -> Angle -> Arc2d Length.Meters coords
curveToArc r a =
    Point2d.origin
        |> Arc2d.sweptAround
            (Point2d.meters 0
                (if Quantity.greaterThanOrEqualToZero a then
                    Length.inMeters r

                 else
                    -(Length.inMeters r)
                )
            )
            a



-- SVG


toSvg : Track -> List (Svg msg)
toSvg track =
    let
        cc1 =
            connectorCursors track |> Tuple.second
    in
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
                    curveToArc r a
            in
            Gsvg.arc2d
                [ SA.fill "none"
                , SA.stroke "grey"
                , SA.strokeWidth "1.435"
                ]
                arc
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
                , ( "radius", Encode.float (Length.inMeters r) )
                , ( "angle", Encode.float (Angle.inDegrees a) )
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
                (Decode.map Length.meters (Decode.field "radius" Decode.float))
                (Decode.map Angle.degrees (Decode.field "angle" Decode.float))

        other ->
            Decode.fail ("Trying to decode a track but " ++ other ++ " is not supported.")



{-
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
-}
