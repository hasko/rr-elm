module Railroad.Train exposing
    ( Orientation(..)
    , TrainLocation
    , TrainState
    , decoder
    , endLocation
    , initialLocation
    , length
    , move
    , stopped
    )

import Dict exposing (Dict)
import Frame2d
import Graph
import Graph.Pair exposing (getEdgeData)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length exposing (Length)
import List.Extra
import Maybe exposing (andThen, withDefault)
import Point2d
import Quantity
import Railroad.Layout as Layout exposing (Layout)
import Railroad.Track as Track exposing (Track)
import Set


type alias TrainState =
    { name : String
    , composition : List RollingStock
    , speed : Float -- in m/s
    , location : Maybe TrainLocation
    }


type alias RollingStock =
    { length : Length }


type alias TrainLocation =
    { edge : ( Int, Int ) -- The vertices
    , pos : Length -- The position on the track
    , orientation : Orientation
    , track : Track -- The track information for convenience
    }


type Orientation
    = Aligned


length : TrainState -> Length
length train =
    List.map .length train.composition |> Quantity.sum


endLocation : Length -> Layout -> Dict Int Int -> TrainLocation -> Maybe TrainLocation
endLocation l layout switchState startLoc =
    endLocationRec l Quantity.zero layout switchState startLoc


endLocationRec : Length -> Length -> Layout -> Dict Int Int -> TrainLocation -> Maybe TrainLocation
endLocationRec l correction layout switchState startLoc =
    case Layout.coordsFor startLoc.pos startLoc.edge layout |> Maybe.map Frame2d.originPoint of
        Nothing ->
            Nothing

        Just p1 ->
            case { startLoc | pos = startLoc.pos |> Quantity.minus l |> Quantity.minus correction } |> normalizeLocation layout switchState of
                Nothing ->
                    Nothing

                Just loc ->
                    Layout.coordsFor loc.pos loc.edge layout
                        |> Maybe.map Frame2d.originPoint
                        |> Maybe.andThen
                            (\p2 ->
                                let
                                    d =
                                        Point2d.distanceFrom p1 p2

                                    {-
                                       err =
                                           l |> Quantity.minus d

                                       _ =
                                           Debug.log "Calculation" { length = Length.inMeters l, d = Length.inMeters d, err = Length.inMeters err }
                                    -}
                                in
                                if Quantity.equalWithin (Length.centimeters 5) d l then
                                    Just loc

                                else
                                    endLocationRec l (Quantity.plus correction (l |> Quantity.minus d)) layout switchState startLoc
                            )


move : Float -> TrainState -> Layout -> Dict Int Int -> TrainState
move millis trainState layout switchState =
    case trainState.location of
        Nothing ->
            -- If the train has no location, no need to move.
            trainState

        Just loc ->
            -- Calculate new position, disregarding track transitions.
            let
                distanceTraveled =
                    trainState.speed * millis / 1000.0

                -- Consider which way the train is traveling on the track.
                newPos =
                    case loc.orientation of
                        Aligned ->
                            loc.pos |> Quantity.plus (Length.meters distanceTraveled)
            in
            -- Create a new train state ...
            { trainState
              -- ... but replace the location with a new location ...
                | location =
                    -- ... based on the old location but with an updated position ...
                    { loc | pos = newPos }
                        -- and finally "normalize" the location in case we are on a different track now.
                        |> normalizeLocation layout switchState
            }


normalizeLocation : Layout -> Dict Int Int -> TrainLocation -> Maybe TrainLocation
normalizeLocation layout switchState loc =
    -- If the position is beyond the end of the current track ...
    -- TODO Determine and use the appropriate connection nimber instead of 0
    if loc.pos |> Quantity.greaterThan (Track.length loc.track) then
        -- ... get the next track.
        case nextTrack loc.edge layout switchState of
            Nothing ->
                -- If there is no next track, return.
                Nothing

            Just nextLoc ->
                -- Calculate the new position.
                { nextLoc
                  -- Subtract the current track length from the position.
                  -- TODO Determine and use the appropriate connection number instead of 0
                    | pos = loc.pos |> Quantity.minus (Track.length loc.track)
                }
                    -- ... and repeat until done.
                    |> normalizeLocation layout switchState

    else if Quantity.lessThanZero loc.pos then
        previousTrack loc.edge layout switchState
            |> Maybe.andThen
                (\nextLoc ->
                    -- TODO Determine and use the appropriate connection number instead of 0
                    { nextLoc | pos = loc.pos |> Quantity.plus (Track.length nextLoc.track) }
                        |> normalizeLocation layout switchState
                )

    else
        -- We are within the track bounds, so return.
        Just loc


previousTrack : ( Int, Int ) -> Layout -> Dict Int Int -> Maybe TrainLocation
previousTrack ( fromId, toId ) layout switchState =
    let
        g =
            Layout.toGraph layout
    in
    case Graph.getData toId g of
        Nothing ->
            -- If there is no switch, just return the first/only track.
            Graph.incoming fromId g
                |> Set.toList
                |> List.head
                |> Maybe.map (\otherId -> ( otherId, fromId ))
                |> andThen
                    (\edge ->
                        Graph.Pair.getEdgeData edge g
                            |> andThen
                                (\track ->
                                    Just { edge = edge, pos = Quantity.zero, orientation = Aligned, track = track }
                                )
                    )

        Just switch ->
            Dict.get fromId switchState
                |> withDefault 0
                -- Select the right switch configuration.
                |> (\i -> List.Extra.getAt i switch.configs)
                -- Get the right route.
                |> Maybe.map (\cfg -> List.filter (\( _, routeTo ) -> routeTo == toId) cfg)
                |> andThen List.head
                -- Choose the previous edge.
                |> Maybe.map (\( routeFrom, _ ) -> ( routeFrom, fromId ))
                -- Get the track for the edge
                |> andThen
                    (\edge ->
                        getEdgeData edge g
                            |> andThen
                                (\track ->
                                    Just { edge = edge, pos = Quantity.zero, orientation = Aligned, track = track }
                                )
                    )


nextTrack : ( Int, Int ) -> Layout -> Dict Int Int -> Maybe TrainLocation
nextTrack ( fromId, toId ) layout switchState =
    let
        g =
            Layout.toGraph layout
    in
    case Graph.getData toId g of
        Nothing ->
            Graph.outgoing toId g
                |> Set.toList
                |> List.head
                |> andThen (\otherId -> Just ( toId, otherId ))
                |> andThen
                    (\edge ->
                        Graph.Pair.getEdgeData edge g
                            |> Maybe.map
                                (\track ->
                                    { edge = edge, pos = Quantity.zero, orientation = Aligned, track = track }
                                )
                    )

        Just switch ->
            Dict.get toId switchState
                |> withDefault 0
                |> (\i -> List.Extra.getAt i switch.configs)
                |> Maybe.map (\cfg -> List.filter (\( from, _ ) -> from == fromId) cfg)
                |> andThen List.head
                |> Maybe.map (\( _, to ) -> ( toId, to ))
                |> andThen
                    (\edge ->
                        getEdgeData edge g
                            |> Maybe.map
                                (\track ->
                                    { edge = edge, pos = Quantity.zero, orientation = Aligned, track = track }
                                )
                    )


stopped : TrainState -> TrainState
stopped ts =
    { ts | speed = 0.0 }



-- JSON


decoder : Decoder TrainState
decoder =
    Decode.map4 TrainState
        (Decode.field "name" Decode.string)
        (Decode.field "composition" (Decode.list rollingStockDecoder))
        (Decode.field "speed" (Decode.float |> Decode.map Speed.metersPerSecond))
        (Decode.field "location" (Decode.maybe Layout.locationDecoder))


rollingStockDecoder : Decoder RollingStock
rollingStockDecoder =
    Decode.map RollingStock
        (Decode.field "length" Decode.float
            |> Decode.map Length.meters
        )


trainLocationDecoder : Decoder TrainLocation
trainLocationDecoder =
    Decode.map4 TrainLocation
        (Decode.field "edge" edgeDecoder)
        (Decode.field "pos" (Decode.float |> Decode.map Length.meters))
        (Decode.field "orientation" orientationDecoder)
        -- TODO Fix the track assignment
        (Decode.succeed (Track.StraightTrack (Length.meters 1)))


edgeDecoder : Decoder ( Int, Int )
edgeDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "from" Decode.int)
        (Decode.field "to" Decode.int)


orientationDecoder : Decoder Orientation
orientationDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "aligned" ->
                        Decode.succeed Aligned

                    _ ->
                        Decode.fail "Invalid orientation"
            )



-- Samples


initialLocation : Layout -> Maybe TrainLocation
initialLocation layout =
    getEdgeData ( 0, 1 ) (Layout.toGraph layout)
        |> Maybe.map
            (\track ->
                { edge = ( 0, 1 )
                , pos = Length.meters 40.0
                , orientation = Aligned
                , track = track
                }
            )
