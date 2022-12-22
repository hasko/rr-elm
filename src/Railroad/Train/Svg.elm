module Railroad.Train.Svg exposing (toSvg)

import Array exposing (Array)
import Frame2d
import Length
import Point2d
import Railroad.Layout as Layout exposing (Layout)
import Railroad.Track exposing (Track(..))
import Railroad.Train as Train exposing (..)
import Svg exposing (Svg, g, line)
import Svg.Attributes exposing (stroke, strokeWidth, x1, x2, y1, y2)


toSvg : Train -> Layout -> Array Int -> Svg msg
toSvg train layout switchState =
    case train.location of
        Nothing ->
            -- Train is nowhere to be seen. TODO Should be off map location.
            g [] []

        Just loc ->
            g [ Svg.Attributes.id "train" ]
                (List.foldl
                    -- Iterate through the train cars (including engines) and collect the svgs to draw
                    (\car ( currentLoc, svg ) ->
                        let
                            maybeTrack1 =
                                Layout.trackAt currentLoc.edge layout
                        in
                        case Layout.coordsFor currentLoc.pos currentLoc.edge layout of
                            Nothing ->
                                -- Should never happen.
                                ( currentLoc, svg )

                            Just c1 ->
                                -- We now know the first coordinate.
                                case Train.endLocation car.length layout switchState currentLoc of
                                    Nothing ->
                                        -- If the train end fits on no track, draw nothing. Should never happen.
                                        ( currentLoc, svg )

                                    Just trainEndLocation ->
                                        let
                                            maybeTrack2 =
                                                Layout.trackAt trainEndLocation.edge layout
                                        in
                                        case Layout.coordsFor trainEndLocation.pos trainEndLocation.edge layout of
                                            -- Train end is not on any track.
                                            Nothing ->
                                                ( currentLoc, svg )

                                            Just c2 ->
                                                -- Now we know where both ends of the current car are.
                                                let
                                                    p1 =
                                                        c1 |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters

                                                    p2 =
                                                        c2 |> Frame2d.originPoint |> Point2d.toRecord Length.inMeters
                                                in
                                                case ( maybeTrack1, maybeTrack2 ) of
                                                    ( Just MapExit, Just MapExit ) ->
                                                        if currentLoc.edge == trainEndLocation.edge then
                                                            -- Car is completely on the same map exit, don't draw.
                                                            ( trainEndLocation, svg )

                                                        else
                                                            -- TODO Car is on two different track exits. Draw the inbetween bits.
                                                            ( trainEndLocation
                                                            , carToSvg p1 p2
                                                                :: svg
                                                            )

                                                    ( _, _ ) ->
                                                        -- TODO Handle if car is partially visible.
                                                        ( trainEndLocation
                                                        , carToSvg p1 p2
                                                            :: svg
                                                        )
                    )
                    -- Start with the initial train head location and an empty list of svgs.
                    ( loc, [] )
                    train.composition
                    -- Get only the list of svgs.
                    |> Tuple.second
                )


carToSvg : { x : Float, y : Float } -> { x : Float, y : Float } -> Svg msg
carToSvg p1 p2 =
    line
        [ x1 (p1.x |> String.fromFloat)
        , y1 (p1.y |> String.fromFloat)
        , x2 (p2.x |> String.fromFloat)
        , y2 (p2.y |> String.fromFloat)
        , stroke "#3B3332"
        , strokeWidth "2.990"
        ]
        []
