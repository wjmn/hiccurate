port module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (Decoder, Value)
import Svg exposing (svg, line, circle)
import Svg.Attributes as SA
import Json.Decode as Decode
import Browser.Events



------------------------------------------------------------------------------
-- MODEL AND TYPES
------------------------------------------------------------------------------


type State
    = Splash SplashData
    | HicLoaded HicLoadedData


type alias SplashData =
    { loading : Maybe File }


type alias HicLoadedData =
    { hicName : String
    , mouseMoveData : Maybe MouseMoveData
    , currentViewData : Maybe (CurrentViewData, CurrentViewDataPercentages)
    , cursorStrandPair : (Strand, Strand)
    }


type alias DomRect =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type alias SyncState =
    { chr1Name : String
    , chr2Name : String
    , binSize : Int
    , binX : Float
    , binY : Float
    , pixelSize : Float
    }


decoderSyncState : Decoder SyncState
decoderSyncState =
    Json.map6 SyncState
        (Json.at [ "chr1Name" ] Json.string)
        (Json.at [ "chr2Name" ] Json.string)
        (Json.at [ "binSize" ] Json.int)
        (Json.at [ "binX" ] Json.float)
        (Json.at [ "binY" ] Json.float)
        (Json.at [ "pixelSize" ] Json.float)


decoderDomRect : Decoder DomRect
decoderDomRect =
    Json.map8 DomRect
        (Json.at [ "x" ] Json.int)
        (Json.at [ "y" ] Json.int)
        (Json.at [ "width" ] Json.int)
        (Json.at [ "height" ] Json.int)
        (Json.at [ "top" ] Json.int)
        (Json.at [ "right" ] Json.int)
        (Json.at [ "bottom" ] Json.int)
        (Json.at [ "left" ] Json.int)


type alias MouseMoveData =
    { relX : Int
    , relY : Int
    , rect : DomRect
    , syncState : SyncState
    }


type alias CurrentViewData =
    { chrXName : String
    , chrXStart : Int
    , chrXEnd : Int
    , chrXPointer : Int
    , chrYName : String
    , chrYStart : Int
    , chrYEnd : Int
    , chrYPointer : Int
    }

type alias CurrentViewDataPercentages = 
    { chrXName : String
    , chrXStart : Float
    , chrXEnd : Float
    , chrXPointer : Float
    , chrYName : String
    , chrYStart : Float
    , chrYEnd : Float
    , chrYPointer : Float
    }

decoderMouseMoveData : Decoder MouseMoveData
decoderMouseMoveData =
    Json.map4 MouseMoveData
        (Json.at [ "relX" ] Json.int)
        (Json.at [ "relY" ] Json.int)
        (Json.at [ "rect" ] decoderDomRect)
        (Json.at [ "syncState" ] decoderSyncState)


prefixChr : String -> String
prefixChr chrName =
    case String.left 3 chrName of
        "chr" ->
            chrName

        _ ->
            "chr" ++ chrName


mouseMoveDataToCurrentViewData : MouseMoveData -> CurrentViewData
mouseMoveDataToCurrentViewData mouseMoveData =
    let
        chrXStart =
            toFloat mouseMoveData.syncState.binSize * mouseMoveData.syncState.binX |> truncate

        chrYStart =
            toFloat mouseMoveData.syncState.binSize * mouseMoveData.syncState.binY |> truncate

        xWidth =
            toFloat mouseMoveData.rect.width / mouseMoveData.syncState.pixelSize * toFloat mouseMoveData.syncState.binSize - 1 |> truncate

        yWidth =
            toFloat mouseMoveData.rect.height / mouseMoveData.syncState.pixelSize * toFloat mouseMoveData.syncState.binSize - 1 |> truncate

        xPointer =
            chrXStart + (toFloat mouseMoveData.relX / mouseMoveData.syncState.pixelSize * toFloat mouseMoveData.syncState.binSize |> truncate)

        yPointer =
            chrYStart + (toFloat mouseMoveData.relY / mouseMoveData.syncState.pixelSize * toFloat mouseMoveData.syncState.binSize |> truncate)
    in
    { chrXName = prefixChr mouseMoveData.syncState.chr1Name
    , chrXStart = chrXStart
    , chrXEnd = chrXStart + xWidth
    , chrXPointer = xPointer
    , chrYName = prefixChr mouseMoveData.syncState.chr2Name
    , chrYStart = chrYStart
    , chrYEnd = chrYStart + yWidth
    , chrYPointer = yPointer
    }

currentViewDataToPercentages : CurrentViewData -> CurrentViewDataPercentages
currentViewDataToPercentages currentViewData = 
    { chrXName = currentViewData.chrXName
    , chrXStart = 0.0
    , chrXEnd = 100.0
    , chrXPointer = 100 * toFloat (currentViewData.chrXPointer - currentViewData.chrXStart) /  toFloat (currentViewData.chrXEnd - currentViewData.chrXStart)
    , chrYName = currentViewData.chrYName
    , chrYStart = 0.0
    , chrYEnd = 100.0
    , chrYPointer = 100 * toFloat (currentViewData.chrYPointer - currentViewData.chrYStart) / toFloat (currentViewData.chrYEnd - currentViewData.chrYStart)
    }

{-| For chr1 on X axis and chr2 on Y axis:

```text
++ | -+ 
---+---
+- | --
```


-}
type Strand = Pos | Neg


------------------------------------------------------------------------------
-- INIT
------------------------------------------------------------------------------


init : ( State, Cmd Msg )
init =
    ( Splash { loading = Nothing }, Cmd.none )



------------------------------------------------------------------------------
-- UPDATE
------------------------------------------------------------------------------


port sendHicLoadedFile : Value -> Cmd msg


port receiveMouseMoveData : (Value -> msg) -> Sub msg


onFile : (( File.File, Value ) -> msg) -> Attribute msg
onFile toMsg =
    on "change"
        (Json.map toMsg
            (Json.at [ "target", "files", "0" ] fileRaw)
        )


fileRaw : Decoder ( File.File, Value )
fileRaw =
    Json.map2 Tuple.pair
        File.decoder
        Json.value


type Msg
    = GotSelectedFile ( File, Value )
    | GotMouseMoveData Value
    | ChangedStrand (Strand, Strand)


withCmd : Cmd msg -> State -> ( State, Cmd msg )
withCmd cmd model =
    ( model, cmd )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        noop =
            ( state, Cmd.none )
    in
    case state of
        Splash data ->
            case msg of
                GotSelectedFile ( file, value ) ->
                    -- ( Splash { loading = Just file }, sendHicLoadedFile value)
                    HicLoaded
                        { hicName = File.name file
                        , mouseMoveData = Nothing
                        , currentViewData = Nothing
                        , cursorStrandPair = (Pos, Pos)
                        }
                        |> withCmd (sendHicLoadedFile value)

                _ ->
                    noop

        HicLoaded data ->
            case msg of
                GotMouseMoveData value ->
                    case Json.decodeValue decoderMouseMoveData value of
                        Ok mouseMoveData ->
                            let
                                currentViewData =
                                    mouseMoveDataToCurrentViewData mouseMoveData
                            in
                            HicLoaded
                                { data
                                    | mouseMoveData = Just mouseMoveData
                                    , currentViewData = Just (currentViewData, currentViewDataToPercentages currentViewData)
                                }
                                |> withCmd Cmd.none

                        Err e ->
                            let
                                _ =
                                    Debug.log "error" e
                            in
                            noop
                ChangedStrand (strand1, strand2) ->
                    HicLoaded
                        { data
                            | cursorStrandPair = (strand1, strand2)
                        }
                        |> withCmd Cmd.none

                _ ->
                    noop



------------------------------------------------------------------------------
-- VIEW
------------------------------------------------------------------------------


tw : String -> Attribute msg
tw =
    class


view : State -> Html Msg
view state =
    case state of
        Splash data ->
            splashView data

        HicLoaded data ->
            hicView data


splashView : SplashData -> Html Msg
splashView data =
    div [ id "elm-splash", tw "flex justify-center items-center" ]
        [ div [ tw "" ] [ text "Choose a Hi-C File to get started. " ]
        , input [ type_ "file", onFile GotSelectedFile ] []
        ]


styleDefault0 : (obj -> Int) -> Maybe obj -> String
styleDefault0 lookup obj =
    case obj of
        Just value ->
            String.fromInt (lookup value) ++ "px"

        Nothing ->
            "0px"



hicView : HicLoadedData -> Html Msg
hicView data =
    let
        (xPointer, yPointer) = 
            case data.currentViewData of 
                Just (_, currentViewData) ->
                    (currentViewData.chrXPointer |> String.fromFloat, currentViewData.chrYPointer |> String.fromFloat)

                Nothing ->
                    ("0", "0") 
        ((x1, x2), (y1, y2)) =
            case data.cursorStrandPair of
                (Pos, Pos) ->
                    (("0", xPointer), ("0", yPointer))

                (Neg, Pos) ->
                    ((xPointer, "100%"), ("0", yPointer))

                (Pos, Neg) ->
                    (("0", xPointer), (yPointer, "100%"))

                (Neg, Neg) ->
                    ((xPointer, "100%"), (yPointer, "100%"))

    in
    div [ id "elm-screen" ]
        [ div [ id "elm-hic-overlay" ]
            [ svg
                [ id "elm-hic-overlay-svg"
                , style "left" (styleDefault0 (\d -> d.rect.x) data.mouseMoveData)
                , style "top" (styleDefault0 (\d -> d.rect.y) data.mouseMoveData)
                , style "width" (styleDefault0 (\d -> d.rect.width) data.mouseMoveData)
                , style "height" (styleDefault0 (\d -> d.rect.height) data.mouseMoveData)
                , SA.viewBox "0 0 100 100"
                ]
                [ -- Show crosshairs as a horizontal and vertical line through entire axis 
                    -- Base crosshairs are dotted and thinner
                    line
                        [ SA.x1 "0"
                        , SA.y1 yPointer
                        , SA.x2 "100%"
                        , SA.y2 yPointer
                        , SA.stroke "gray"
                        , SA.strokeDasharray "1,1"
                        , SA.strokeWidth "0.1" ]
                        [] 
                    , line
                        [ SA.x1 xPointer
                        , SA.y1 "0"
                        , SA.x2 xPointer
                        , SA.y2 "100%"
                        , SA.stroke "gray"
                        , SA.strokeDasharray "1,2"
                        , SA.strokeWidth "0.1" ]
                        [] 


                    , line
                        [ SA.x1 x1
                        , SA.y1 yPointer
                        , SA.x2 x2
                        , SA.y2 yPointer
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1" ]
                        [] 
                    , line
                        [ SA.x1 xPointer
                        , SA.y1 y1
                        , SA.x2 xPointer
                        , SA.y2 y2
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1" ]
                        [] 
                ]
            ]
        , div [ id "elm-main" ] []
        ]



------------------------------------------------------------------------------
-- SUBSCRIPTIONS
------------------------------------------------------------------------------

decoderKeyPress : Decoder Msg 
decoderKeyPress = 
    Json.field "key" Decode.string |> Json.andThen (\key -> 
        case key of 
            "w" -> Json.succeed (ChangedStrand (Pos, Pos))
            "e" -> Json.succeed (ChangedStrand (Neg, Pos))
            "s" -> Json.succeed (ChangedStrand (Pos, Neg))
            "d" -> Json.succeed (ChangedStrand (Neg, Neg))
            _ -> Json.fail "Invalid key"
    )

subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Splash _ ->
            Sub.none

        HicLoaded _ ->
            Sub.batch 
                [ receiveMouseMoveData GotMouseMoveData
                , Browser.Events.onKeyPress decoderKeyPress
                ]



------------------------------------------------------------------------------
-- PROGRAM
------------------------------------------------------------------------------


main : Program () State Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
