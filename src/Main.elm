module Main exposing (Msg(..), main, update, view)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, br, button, div, h3, hr, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Att exposing (attribute, class, disabled, for, href, scope, target, value)
import Html.Entity exposing (nbsp)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode
import Maybe exposing (andThen)
import Railroad exposing (Layout, RailroadState, Switch, Train, createTrain, stateToSvg)
import Task
import Time exposing (posixToMillis)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : Maybe LoadedState
    , lastMessage : Maybe String
    }


type alias LoadedState =
    { rrState : RailroadState
    , isRunning : Bool
    , editingTrain : Maybe String
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | SingleStep
    | Reset
    | LoadLayout
    | LoadLayoutSelected File
    | LoadedLayout String
    | ToggleSwitch String Int
    | CreateTrain String
    | RemoveTrain String
    | EditTrain String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Nothing, lastMessage = Nothing }, Cmd.none )


subscriptions _ =
    Time.every 100 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLayout ->
            ( { model | lastMessage = Just "Select layout file to load" }, Select.file [ "application/json" ] LoadLayoutSelected )

        LoadLayoutSelected file ->
            ( { model | lastMessage = Just "Loading file" }, Task.perform LoadedLayout (File.toString file) )

        LoadedLayout string ->
            case Json.Decode.decodeString Railroad.layoutDecoder string of
                Ok layout ->
                    ( { state = Just { rrState = Railroad.loadedLayout layout, isRunning = False, editingTrain = Nothing }
                      , lastMessage = Just "Loaded successfully"
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | lastMessage = Just (Json.Decode.errorToString err) }, Cmd.none )

        ToggleSwitch name switchState ->
            ( { model | state = Maybe.map (\s -> { s | rrState = Railroad.toggleSwitch s.rrState name switchState }) model.state }, Cmd.none )

        CreateTrain trackName ->
            ( { model | state = Maybe.map (\s -> { s | rrState = Railroad.createTrain s.rrState trackName }) model.state }, Cmd.none )

        RemoveTrain trainName ->
            ( { model | state = Maybe.map (\s -> { s | rrState = Railroad.removeTrain s.rrState trainName }) model.state }, Cmd.none )

        EditTrain trainName ->
            ( { model | state = Maybe.map (\s -> { s | editingTrain = Just trainName }) model.state }, Cmd.none )

        _ ->
            ( model, Cmd.none )



{-
   Tick newTime ->
       let
           newMillis =
               posixToMillis newTime

           duration =
               case model.millis of
                   Nothing ->
                       0

                   Just m ->
                       newMillis - m
       in
       ( { model
           | millis = Just newMillis
           , state =
               case model.state of
                   Just s ->
                       if model.isRunning then
                           Just (RR.moved duration s)

                       else
                           model.state

                   Nothing ->
                       Nothing
           , frame = duration
         }
       , Cmd.none
       )

   Start ->
       ( { model | isRunning = True }, Cmd.none )

   Stop ->
       ( { model | isRunning = False }, Cmd.none )

   SingleStep ->
       ( { model | state = model.state |> Maybe.andThen (\s -> Just (RR.moved 100 s)) }
       , Cmd.none
       )

   Reset ->
       ( { model | state = model.initialState }, Cmd.none )
-}


type alias Document msg =
    { title : String, body : List (Html msg) }


view : Model -> Document Msg
view model =
    { title = "Railroad"
    , body =
        [ div [ class "container" ]
            ((case model.state of
                Nothing ->
                    [ div [ class "alert alert-primary" ] [ text "No model loaded" ] ]

                Just state ->
                    [ div [ class "row mt-3" ]
                        [ div [ class "col" ] [ Railroad.stateToSvg state.rrState ]
                        ]
                    , div [ class "row mt-3" ]
                        [ div [ class "col" ] [ viewSimulationControls state.isRunning ]
                        ]
                    , div [ class "row mt-3" ]
                        [ div [ class "col-6" ] [ h3 [] [ text "Trains" ], viewTrains state ]
                        , div [ class "col" ] [ h3 [] [ text "Tracks" ], viewTracks state.rrState ]
                        , div [ class "col" ] [ h3 [] [ text "Switches" ], viewSwitches state.rrState ]
                        ]
                    ]
             )
                ++ [ div [ class "row" ] [ div [ class "col" ] [ viewLoadSaveControls model ] ]
                   , div [ class "row mt-3" ]
                        [ div [ class "col" ]
                            [ hr [] []
                            , p [] [ a [ href "https://github.com/hasko/rr-elm", target "_blank" ] [ text "Show source" ] ]
                            ]
                        ]
                   ]
                ++ (case model.lastMessage of
                        Nothing ->
                            []

                        Just string ->
                            [ p [] [ text string ] ]
                   )
            )
        ]
    }


viewSimulationControls : Bool -> Html Msg
viewSimulationControls isRunning =
    div []
        [ if isRunning then
            button [ class "btn btn-primary", onClick Stop ] [ text "Stop simulation" ]

          else
            button [ class "btn btn-primary", onClick Start ] [ text "Start simulation" ]
        , button
            [ class "btn btn-secondary ml-2"
            , if isRunning then
                disabled True

              else
                onClick SingleStep
            ]
            [ text "Step" ]
        , button
            [ class "btn btn-secondary ml-2"
            , if isRunning then
                disabled True

              else
                onClick Reset
            ]
            [ text "Reset" ]
        ]


viewLoadSaveControls : Model -> Html Msg
viewLoadSaveControls model =
    div []
        [ button [ class "btn btn-secondary", onClick LoadLayout ] [ text "Load layout" ] ]



{-
   viewTrains : List RR.Train -> Layout.Layout -> Html Msg
   viewTrains trains layout =
       table [ class "table" ]
           (tr []
               [ th [ scope "col" ] [ text "Track" ]
               , th [ class "text-right", scope "col" ] [ text "Pos" ]
               , th [ scope "col" ] [ text "Orientation" ]
               , th [ class "text-right", scope "col" ] [ text "Speed" ]
               , th [ class "text-right", scope "col" ] [ text "Length" ]
               , th [ scope "col" ] [ text "State" ]
               ]
               :: List.map
                   (\train ->
                       tr []
                           [ td []
                               [ text (Layout.getTrackName train.loc.track layout) ]
                           , td [ class "text-right" ] [ text (Round.round 1 train.loc.pos) ]
                           , td [] [ text (Orientation.toString train.loc.orient) ]
                           , td [ class "text-right" ] [ text (Round.round 1 train.speed) ]
                           , td [ class "text-right" ] [ text (Round.round 1 train.length) ]
                           , td []
                               [ case train.state of
                                   RR.Normal ->
                                       text nbsp

                                   RR.EmergencyStop ->
                                       text "Emergency stop"

                                   RR.Crashed ->
                                       text "Crashed"

                                   RR.OffMap ->
                                       text "Off map"
                               ]
                           ]
                   )
                   trains
           )


   scaleTransform : Float -> String
   scaleTransform scale =
       let
           scaleStr =
               String.fromFloat scale
       in
       "scale(" ++ scaleStr ++ " " ++ scaleStr ++ ")"


   trackToSvg : Layout.Track -> Svg Msg
   trackToSvg track =
       let
           conns =
               Layout.getConnectors track

           from =
               Layout.getPosition conns.from

           to =
               Layout.getPosition conns.to
       in
       line
           [ from.x |> String.fromFloat |> x1
           , from.y |> String.fromFloat |> y1
           , to.x |> String.fromFloat |> x2
           , to.y |> String.fromFloat |> y2
           , stroke "black"
           , strokeLinecap "round"
           ]
           []


   connectorToSvg : Layout.Connector -> Svg Msg
   connectorToSvg conn =
       let
           pos =
               Layout.getPosition conn
       in
       circle
           [ pos.x |> String.fromFloat |> cx
           , pos.y |> String.fromFloat |> cy
           , r "5"
           , fill "none"
           , stroke "grey"
           , strokeWidth "1"
           ]
           []


   trainToSvg : RR.Train -> Layout -> Svg Msg
   trainToSvg train layout =
       g [] (trainToSvgRecursive train.loc train.length layout [])


   trainToSvgRecursive : RR.Location -> Float -> Layout -> List (Svg Msg) -> List (Svg Msg)
   trainToSvgRecursive loc length layout svgList =
       if length <= 0 then
           -- Whole train is covered, return what we accumulated so far.
           svgList

       else
           -- Some train length remains.
           let
               currentTrackLength =
                   Layout.trackLength loc.track

               -- Calculate the end of the train ignoring track length.
               newPos =
                   loc.pos - Orientation.byOrientation loc.orient length

               -- Now clamp to track length and calculate the remaining length of the train.
               ( newClampedPos, newLength ) =
                   if newPos < 0 then
                       -- Train spills over beginning of track.
                       ( 0, length - loc.pos )

                   else if newPos > currentTrackLength then
                       -- Train spills over end of track.
                       ( currentTrackLength, length - currentTrackLength + loc.pos )

                   else
                       ( newPos, 0 )
           in
           trackSegment loc.track loc.pos newClampedPos
               :: (if newPos < 0 then
                       -- Train is spilling onto the previous track.
                       case Layout.getPreviousTrack loc.track layout of
                           Nothing ->
                               svgList

                           Just ( newTrack, newOrient ) ->
                               let
                                   newStart =
                                       case newOrient of
                                           Forward ->
                                               0

                                           Reverse ->
                                               Layout.trackLength newTrack
                               in
                               trainToSvgRecursive { track = newTrack, pos = newStart, orient = Orientation.reverse newOrient } newLength layout svgList

                   else if newPos > Layout.trackLength loc.track then
                       -- Train is spilling onto the next track.
                       case Layout.getNextTrack loc.track layout of
                           Nothing ->
                               svgList

                           Just ( newTrack, newOrient ) ->
                               let
                                   newStart =
                                       case newOrient of
                                           Forward ->
                                               0

                                           Reverse ->
                                               Layout.trackLength newTrack
                               in
                               trainToSvgRecursive { track = newTrack, pos = newStart, orient = Orientation.reverse newOrient } newLength layout svgList

                   else
                       -- Train fits onto the current track.
                       svgList
                  )


   trackSegment : Layout.Track -> Float -> Float -> Svg msg
   trackSegment track fromPos toPos =
       let
           tl =
               Layout.trackLength track

           conns =
               Layout.getConnectors track

           from =
               Layout.getPosition conns.from

           to =
               Layout.getPosition conns.to

           dx =
               to.x - from.x

           dy =
               to.y - from.y
       in
       --TODO Curves and such.
       line
           [ from.x + dx * fromPos / tl |> String.fromFloat |> x1
           , from.y + dy * fromPos / tl |> String.fromFloat |> y1
           , from.x + dx * toPos / tl |> String.fromFloat |> x2
           , from.y + dy * toPos / tl |> String.fromFloat |> y2
           , stroke "red"
           , strokeLinecap "round"
           , strokeWidth "5"
           ]
           []
-}


viewTrains : LoadedState -> Html Msg
viewTrains state =
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Length" ]
                , th [] [ text "Tracks" ]
                , th [] [ text "Orientation" ]
                , th [] [ text "Speed" ]
                , th [] [ text "Action" ]
                ]
            ]
        , tbody []
            (List.map
                (\t ->
                    case state.editingTrain of
                        Nothing ->
                            viewTrain state.rrState t

                        Just et ->
                            if et == t.name then
                                editTrain state.rrState t

                            else
                                viewTrain state.rrState t
                )
                (List.sortBy .name state.rrState.trains)
            )
        ]


viewTrain : RailroadState -> Train -> Html Msg
viewTrain state train =
    tr []
        [ td [] [ text train.name ]
        , td [] [ text (String.fromFloat train.length ++ nbsp ++ "m") ]
        , td [] [ text (String.join ", " (Railroad.tracksForTrain state train.name)) ]
        , td [] [ text nbsp ]
        , td [] [ text (String.fromFloat (train.speed * 3.6) ++ nbsp ++ "km/h") ]
        , td []
            (if train.speed == 0.0 then
                [ button [ class "btn btn-sm btn-secondary", onClick (EditTrain train.name) ] [ text "Edit" ]
                , button [ class "btn btn-sm btn-secondary", onClick (RemoveTrain train.name) ] [ text "Remove" ]
                ]

             else
                []
            )
        ]


editTrain : RailroadState -> Train -> Html Msg
editTrain state train =
    tr []
        [ td [] [ input [ class "form-control form-control-sm", value train.name ] [] ]
        , td [] [ text (String.fromFloat train.length ++ nbsp ++ "m") ]
        , td [] [ text (String.join ", " (Railroad.tracksForTrain state train.name)) ]
        , td [] [ text nbsp ]
        , td [] [ text (String.fromFloat (train.speed * 3.6) ++ nbsp ++ "km/h") ]
        , td []
            [ if train.speed == 0.0 then
                button [ class "btn btn-sm btn-secondary", onClick (RemoveTrain train.name) ] [ text "Remove" ]

              else
                text nbsp
            ]
        ]


viewTracks : RailroadState -> Html Msg
viewTracks state =
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Length" ]
                , th [] [ text "Occupied by" ]
                ]
            ]
        , tbody []
            (List.map
                (\t ->
                    tr []
                        [ td [] [ text t.name ]
                        , td [] [ text (String.fromFloat t.length ++ nbsp ++ "m") ]
                        , td []
                            [ if List.length (Railroad.occupants state t.name) == 0 then
                                button [ class "btn btn-sm btn-secondary", onClick (CreateTrain t.name) ] [ text "Create train" ]

                              else
                                text <| String.join ", " <| List.map .name <| Railroad.occupants state t.name
                            ]
                        ]
                )
                state.layout.tracks
            )
        ]


viewSwitches : RailroadState -> Html Msg
viewSwitches state =
    table [ class "table table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "State" ]
                , th [] [ text "Switch" ]
                ]
            ]
        , tbody [] (List.map viewSwitch state.layout.switches)
        ]


viewSwitch : Switch -> Html Msg
viewSwitch switch =
    tr []
        [ td [] [ text switch.name ]
        , td [] [ text (Railroad.switchStateString switch) ]
        , td []
            [ div [ class "btn-group" ]
                (List.indexedMap
                    (\i s ->
                        button
                            (if switch.state == i then
                                [ class "btn btn-sm btn-secondary", disabled True ]

                             else
                                [ class "btn btn-sm btn-secondary", onClick (ToggleSwitch switch.name i) ]
                            )
                            [ text (String.fromInt i) ]
                    )
                    switch.connections
                )
            ]
        ]
