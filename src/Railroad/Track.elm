module Railroad.Track exposing
    ( Track(..)
    , decoder
    , encode
    , getPositionOnTrack
    , length
    , moveFrame
    , toSvg
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Array exposing (Array)
import Direction2d
import Frame2d exposing (Frame2d)
import Geometry.Svg as Gsvg
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Length exposing (Length)
import Point2d
import Quantity
import Railroad.Util exposing (Frame)
import Svg exposing (Svg, g)
import Svg.Attributes as SA
import Vector2d


type Track
    = StraightTrack Length
    | CurvedTrack Length Angle -- radius in m, angle in degrees


length : Track -> Length
length track =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            Quantity.multiplyBy (Angle.inRadians a) r |> Quantity.abs


connectorFrames : Track -> ( Frame, Frame )
connectorFrames track =
    case track of
        StraightTrack l ->
            ( Frame2d.atOrigin, Frame2d.atPoint (Point2d.xy l Quantity.zero) )

        CurvedTrack r a ->
            ( Frame2d.atOrigin
            , Frame2d.withAngle a
                (Point2d.xy
                    (Quantity.multiplyBy (Angle.cos a) r)
                    (Quantity.multiplyBy (Angle.sin a) r)
                )
            )


moveFrame : Frame -> Track -> Frame
moveFrame cursor track =
    case track of
        StraightTrack l ->
            getPositionOnTrack l cursor track

        CurvedTrack r a ->
            let
                cursorDir =
                    Frame2d.xDirection cursor |> Direction2d.toAngle |> Angle.inDegrees

                newDir =
                    cursorDir + Angle.inDegrees a

                avgDirRad =
                    degrees ((cursorDir + newDir) / 2.0)

                s =
                    2 * Length.inMeters r * sin (degrees (abs (Angle.inDegrees a)) / 2)
            in
            cursor
                |> Frame2d.translateBy (Vector2d.meters (s * cos avgDirRad) (s * sin avgDirRad))
                |> Frame2d.rotateBy a


getPositionOnTrack : Length -> Frame -> Track -> Frame
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack _ ->
            Frame2d.translateBy (Vector2d.xy trackPosition Quantity.zero) cursor

        CurvedTrack r a ->
            let
                -- Calculate amount of angle covered
                a2 =
                    Quantity.multiplyBy (Quantity.ratio trackPosition (length track)) a

                arc =
                    curveToArc r a2
                        |> Arc2d.placeIn cursor

                p =
                    Arc2d.endPoint arc
            in
            cursor |> Frame2d.moveTo p |> Frame2d.rotateBy a2


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
            connectorFrames track
                |> Tuple.second
                |> Frame2d.originPoint
                |> Point2d.toRecord Length.inMeters
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
