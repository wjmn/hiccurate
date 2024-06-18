port module Main exposing (..)

import Browser
import Browser.Events
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes as SA
import Task
import Dict


------------------------------------------------------------------------------
-- DATA
------------------------------------------------------------------------------

chroms : List String
chroms = 
    [ "chr1"
    , "chr2"
    , "chr3"
    , "chr4"
    , "chr5"
    , "chr6"
    , "chr7"
    , "chr8"
    , "chr9"
    , "chr10"
    , "chr11"
    , "chr12"
    , "chr13"
    , "chr14"
    , "chr15"
    , "chr16"
    , "chr17"
    , "chr18"
    , "chr19"
    , "chr20"
    , "chr21"
    , "chr22"
    , "chrX"
    , "chrY"]

chromIndices : Dict.Dict String Int
chromIndices = 
    Dict.fromList (List.indexedMap (\i c -> (c, i)) chroms)

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
    , currentViewData : Maybe ( CurrentViewData, CurrentViewDataPercentages )
    , cursorStrandPair : ( Strand, Strand )
    , breakpoints : List Breakpoint
    , hoveredBreakpoint : Maybe Breakpoint
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

encodeSyncState : SyncState -> Value
encodeSyncState syncState =
    Encode.object
        [ ("chr1Name", Encode.string syncState.chr1Name)
        , ("chr2Name", Encode.string syncState.chr2Name)
        , ("binSize", Encode.int syncState.binSize)
        , ("binX", Encode.float syncState.binX)
        , ("binY", Encode.float syncState.binY)
        , ("pixelSize", Encode.float syncState.pixelSize )]

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

unprefixChr : String -> String
unprefixChr chrName =
    case String.left 3 chrName of
         "chr" ->
             String.dropLeft 3 chrName
         _ -> chrName


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
    , chrXPointer = 100 * toFloat (currentViewData.chrXPointer - currentViewData.chrXStart) / toFloat (currentViewData.chrXEnd - currentViewData.chrXStart)
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
type Strand
    = Pos
    | Neg


strandToString : Strand -> String
strandToString strand =
    case strand of
        Pos ->
            "+"

        Neg ->
            "-"


stringToStrand : String -> Maybe Strand
stringToStrand strand =
    case strand of
        "+" ->
            Just Pos

        "-" ->
            Just Neg

        _ ->
            Nothing


type alias Breakpoint =
    { chrA : String
    , posA : Int
    , strandA : Strand
    , chrB : String
    , posB : Int
    , strandB : Strand
    , resolution : Int
    }

type BedpeFormat
     = BedpeBreakfinder
     | BedpeGeneric

{-| Read .bedpe string

hic_breakfinder format:

```
#chr1 x1 x2 chr2 y1 y2 strand1 strand2 resolution -logP
chr12 7638000 7670000 chr9 22090000 22242000 + - 1kb 291.003
```

Generic bedpe format:
```
chr1 x1 x2 chr2 y1 y2
```
-}
rowToBreakpoint : String -> Maybe Breakpoint
rowToBreakpoint row =
    let
        cols = String.words row
    in
    case cols of
         (head :: tail) ->
              if String.startsWith "#" head then
                  Nothing
              else
                  case cols of
                      [chr1raw, x1, x2, chr2raw, y1, y2, strand1, strand2, resolution, negLogP ] ->
                          let
                              chr1 = prefixChr chr1raw
                              chr2 =    prefixChr chr2raw
                              pos1Maybe =
                                   if strand1 == "+" then
                                       String.toInt x2
                                   else if strand1 == "-" then
                                       String.toInt x1
                                   else
                                       Nothing
                              pos2Maybe =
                                   if strand2 == "+" then
                                       String.toInt y2
                                   else if strand2 == "-" then
                                       String.toInt y1
                                   else
                                       Nothing
                          in
                          -- Ensure chr1 <= chr2 for indices
                            case (Dict.get chr1 chromIndices, Dict.get chr2 chromIndices) of
                                (Just index1, Just index2) ->
                                    if index1 <= index2 then 
                                        Maybe.map5 (\s1 s2 res pos1 pos2 -> { chrA = chr1, posA = pos1, strandA = s1, chrB = chr2, posB = pos2, strandB = s2, resolution=res })
                                            (stringToStrand strand1)
                                            (stringToStrand strand2)
                                            (suffixedNumberToInt resolution)
                                            pos1Maybe
                                            pos2Maybe
                                    else 
                                        Maybe.map5 (\s1 s2 res pos1 pos2 -> { chrA = chr2, posA = pos2, strandA = s2, chrB = chr1, posB = pos1, strandB = s1, resolution=res })
                                            (stringToStrand strand1)
                                            (stringToStrand strand2)
                                            (suffixedNumberToInt resolution)
                                            pos1Maybe
                                            pos2Maybe
                                _ -> 
                                    Nothing
                      -- Todo: parse a generic bedpe
                      _ -> Nothing
         [] ->
            Nothing

suffixedNumberToInt : String -> Maybe Int
suffixedNumberToInt string =
    if string |> String.endsWith "kb" then
       String.dropRight 2 string
       |> String.toInt
       |> Maybe.map ((*) 1000)
    else if string |> String.endsWith "Mb" then
       String.dropRight 2 string
       |> String.toInt
       |> Maybe.map ((*) 1000000)
    else
        String.toInt string

breakpointToSyncState : DomRect -> Breakpoint -> SyncState
breakpointToSyncState rect breakpoint =
    let
        halfWidth = toFloat rect.width / 2
        halfHeight = toFloat rect.height / 2
        posABin = toFloat breakpoint.posA / toFloat breakpoint.resolution
        posBBin = toFloat breakpoint.posB / toFloat breakpoint.resolution
        startX = (posABin - halfWidth)
        startY = (posBBin - halfHeight)
    in
    { chr1Name = unprefixChr breakpoint.chrA
    , chr2Name = unprefixChr breakpoint.chrB
    , binSize = breakpoint.resolution
    , binX = startX
    , binY = startY
    , pixelSize = 1.0}


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


port rightClickedHicPosition : (Value -> msg) -> Sub msg

port sendNewHicBrowserSyncState : Value -> Cmd msg


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
    | ChangedStrand ( Strand, Strand )
    | RightClickedHicPosition
    | DeleteBreakpoint Int
    | GotSelectedBedpe File
    | GotSelectedBedpeAsString String
    | MouseEnter Breakpoint
    | MouseLeave Breakpoint
    | ClickedBreakpoint Breakpoint


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
                        , cursorStrandPair = ( Pos, Pos )
                        , breakpoints = []
                        , hoveredBreakpoint = Nothing
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
                                    , currentViewData = Just ( currentViewData, currentViewDataToPercentages currentViewData )
                                }
                                |> withCmd Cmd.none

                        Err e ->
                            let
                                _ =
                                    Debug.log "error" e
                            in
                            noop

                ChangedStrand ( strand1, strand2 ) ->
                    HicLoaded
                        { data
                            | cursorStrandPair = ( strand1, strand2 )
                        }
                        |> withCmd Cmd.none

                RightClickedHicPosition ->
                    case ( data.mouseMoveData, data.currentViewData ) of
                        ( Just mouseMoveData, Just ( currentViewData, _ ) ) ->
                            let
                                binSize =
                                    mouseMoveData.syncState.binSize

                                -- Round down to the nearest bin
                                breakpoint =
                                    { chrA = currentViewData.chrXName
                                    , posA = binSize * truncate (toFloat currentViewData.chrXPointer / toFloat binSize)
                                    , strandA = data.cursorStrandPair |> Tuple.first
                                    , chrB = currentViewData.chrYName
                                    , posB = binSize * truncate (toFloat currentViewData.chrYPointer / toFloat binSize)
                                    , strandB = data.cursorStrandPair |> Tuple.second
                                    , resolution = mouseMoveData.syncState.binSize
                                    }
                            in
                            HicLoaded
                                { data
                                    | breakpoints = data.breakpoints ++ [ breakpoint ]
                                }
                                |> withCmd Cmd.none

                        _ ->
                            noop

                DeleteBreakpoint index ->
                    HicLoaded
                        { data
                            | breakpoints =
                                List.filterMap
                                    (\( i, x ) ->
                                        if i /= index then
                                            Just x

                                        else
                                            Nothing
                                    )
                                    (List.indexedMap Tuple.pair data.breakpoints)
                        }
                        |> withCmd Cmd.none

                GotSelectedBedpe file -> 
                    HicLoaded data
                    |> withCmd (Task.perform GotSelectedBedpeAsString (File.toString file))
                GotSelectedBedpeAsString filestring ->
                    let
                        lines =
                            filestring
                            |> String.split "\n"
                        breakpoints =
                            List.filterMap rowToBreakpoint lines
                            |> List.sortBy (\bp -> ((Dict.get bp.chrA chromIndices |> Maybe.withDefault 0, Dict.get bp.chrB chromIndices |> Maybe.withDefault 0), (bp.posA, bp.posB)))
                    in
                    -- For now, add the breakpoints to current breakpoints to avoid data loss
                    HicLoaded { data | breakpoints = data.breakpoints ++ breakpoints }
                    |> withCmd Cmd.none

                MouseEnter breakpoint ->
                    HicLoaded { data | hoveredBreakpoint = Just breakpoint  } |> withCmd Cmd.none

                MouseLeave breakpoint ->
                    case data.hoveredBreakpoint of
                         Just currentBreakpoint ->
                              if currentBreakpoint == breakpoint then
                                 HicLoaded { data | hoveredBreakpoint = Nothing } |> withCmd Cmd.none
                              else
                                noop
                         Nothing -> noop

                ClickedBreakpoint breakpoint ->
                    case data.mouseMoveData of 
                        Just mouseMoveData ->
                            let
                                newSyncState = breakpointToSyncState mouseMoveData.rect breakpoint
                            in
                            HicLoaded data
                            |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))
                        _ ->
                            noop


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
        ( xPointer, yPointer ) =
            case data.currentViewData of
                Just ( _, currentViewData ) ->
                    ( currentViewData.chrXPointer |> String.fromFloat, currentViewData.chrYPointer |> String.fromFloat )

                Nothing ->
                    ( "0", "0" )

        ( ( x1, x2 ), ( y1, y2 ) ) =
            case data.cursorStrandPair of
                ( Pos, Pos ) ->
                    ( ( "0", xPointer ), ( "0", yPointer ) )

                ( Neg, Pos ) ->
                    ( ( xPointer, "100%" ), ( "0", yPointer ) )

                ( Pos, Neg ) ->
                    ( ( "0", xPointer ), ( yPointer, "100%" ) )

                ( Neg, Neg ) ->
                    ( ( xPointer, "100%" ), ( yPointer, "100%" ) )
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
                [ Svg.g []
                    [ -- Show crosshairs as a horizontal and vertical line through entire axis
                      -- Base crosshairs are dotted and thinner
                      line
                        [ SA.x1 "0"
                        , SA.y1 yPointer
                        , SA.x2 "100%"
                        , SA.y2 yPointer
                        , SA.stroke "gray"
                        , SA.strokeDasharray "0.2,0.4"
                        , SA.strokeWidth "0.1"
                        ]
                        []
                    , line
                        [ SA.x1 xPointer
                        , SA.y1 "0"
                        , SA.x2 xPointer
                        , SA.y2 "100%"
                        , SA.stroke "gray"
                        , SA.strokeDasharray "0.2,0.4"
                        , SA.strokeWidth "0.1"
                        ]
                        []
                    , line
                        [ SA.x1 x1
                        , SA.y1 yPointer
                        , SA.x2 x2
                        , SA.y2 yPointer
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1"
                        ]
                        []
                    , line
                        [ SA.x1 xPointer
                        , SA.y1 y1
                        , SA.x2 xPointer
                        , SA.y2 y2
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1"
                        ]
                        []
                    ]
                , viewAllBreakpointSvg data
                ]
            ]
        , div
            [ id "elm-main" ]
            [ div [] [input [ type_ "file", onFile (\(f, _) -> GotSelectedBedpe f) ] []]
            , div [ tw "" ] [ viewBreakpoints data.breakpoints ] ]
        ]


{-| Draw a breakpoint as a crosshair / horizontal and vertical line with correct strand orientation

Assume breakpoint has already been checked to be within the current view data

-}
viewBreakpointSvg : Maybe Breakpoint -> Int -> CurrentViewData -> Breakpoint -> Svg Msg
viewBreakpointSvg maybeHoveredBreakpoint binSize currentViewData breakpoint =
    let
        bpX =
            (toFloat breakpoint.posA - toFloat currentViewData.chrXStart) / toFloat (currentViewData.chrXEnd - currentViewData.chrXStart) * 100

        bpY =
            (toFloat breakpoint.posB - toFloat currentViewData.chrYStart) / toFloat (currentViewData.chrYEnd - currentViewData.chrYStart) * 100

        -- All SVG elements are scaled to 100 x 100 in viewbox
        lineLength =  (3000000 / toFloat binSize) / 200 + 1

        ( x1, x2 ) =
            case breakpoint.strandA of
                Pos ->
                    ( bpX - lineLength, bpX )

                Neg ->
                    ( bpX, bpX + lineLength )

        ( y1, y2 ) =
            case breakpoint.strandB of
                Pos ->
                    ( bpY - lineLength, bpY )

                Neg ->
                    ( bpY, bpY + lineLength )
        color =
              case maybeHoveredBreakpoint of
                    Just hoveredBreakpoint ->
                        if breakpoint == hoveredBreakpoint then
                            "magenta"
                        else
                            "cyan"
                    Nothing ->
                        "cyan"
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (String.fromFloat x1)
            , SA.y1 (String.fromFloat bpY)
            , SA.x2 (String.fromFloat x2)
            , SA.y2 (String.fromFloat bpY)
            , SA.stroke color
            , SA.strokeWidth "0.5"
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat bpX)
            , SA.y1 (String.fromFloat y1)
            , SA.x2 (String.fromFloat bpX)
            , SA.y2 (String.fromFloat y2)
            , SA.stroke color
            , SA.strokeWidth "0.5"
            ]
            []
        ]


viewAllBreakpointSvg : HicLoadedData -> Svg Msg
viewAllBreakpointSvg data =
    case (data.mouseMoveData, data.currentViewData) of
        (Just mouseMoveData, Just ( currentViewData, _ )) ->
            Svg.g []
                (data.breakpoints
                    |> List.filterMap
                        (\breakpoint ->
                            if
                                breakpoint.chrA
                                    == currentViewData.chrXName
                                    && breakpoint.chrB
                                    == currentViewData.chrYName
                                    && breakpoint.posA
                                    >= currentViewData.chrXStart
                                    && breakpoint.posA
                                    <= currentViewData.chrXEnd
                                    && breakpoint.posB
                                    >= currentViewData.chrYStart
                                    && breakpoint.posB
                                    <= currentViewData.chrYEnd
                            then
                                Just (viewBreakpointSvg data.hoveredBreakpoint mouseMoveData.syncState.binSize currentViewData breakpoint)

                            else
                                Nothing
                        )
                )

        _ ->
            Svg.g [] []


viewBreakpoint : Int -> Breakpoint -> Html Msg
viewBreakpoint index breakpoint =
    let
        onClickEvent = 
            onClick (ClickedBreakpoint breakpoint)
        cellStyles = 
            tw "pr-2"
        cell contents = 
            td [cellStyles, onClickEvent] contents
    in
    tr [onMouseEnter (MouseEnter breakpoint), onMouseLeave (MouseLeave breakpoint) 
    , tw "hover:bg-blue-400 hover:text-white cursor-pointer"]
        [ cell [text breakpoint.chrA ]
        , cell [ text (String.fromInt breakpoint.posA) ]
        , cell [ text (strandToString breakpoint.strandA) ]
        , cell [ text breakpoint.chrB ]
        , cell [ text (String.fromInt breakpoint.posB) ]
        , cell [ text (strandToString breakpoint.strandB) ]
        , cell [ text (String.fromInt breakpoint.resolution) ]
        , td [cellStyles, tw "hover:bg-red-400" ] [ button [ onClick (DeleteBreakpoint index) ] [ text "Delete" ] ]
        ]


viewBreakpoints : List Breakpoint -> Html Msg
viewBreakpoints breakpoints =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "chrA" ]
                , th [] [ text "posA" ]
                , th [] [ text "strandA" ]
                , th [] [ text "chrB" ]
                , th [] [ text "posB" ]
                , th [] [ text "strandB" ]
                , th [] [ text "resolution" ]
                ]
            ]
        , tbody [] (List.indexedMap viewBreakpoint breakpoints)
        ]



------------------------------------------------------------------------------
-- SUBSCRIPTIONS
------------------------------------------------------------------------------


decoderKeyPress : Decoder Msg
decoderKeyPress =
    Json.field "key" Json.string
        |> Json.andThen
            (\key ->
                case key of
                    "w" ->
                        Json.succeed (ChangedStrand ( Pos, Pos ))

                    "e" ->
                        Json.succeed (ChangedStrand ( Neg, Pos ))

                    "s" ->
                        Json.succeed (ChangedStrand ( Pos, Neg ))

                    "d" ->
                        Json.succeed (ChangedStrand ( Neg, Neg ))

                    _ ->
                        Json.fail "Invalid key"
            )


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Splash _ ->
            Sub.none

        HicLoaded _ ->
            Sub.batch
                [ receiveMouseMoveData GotMouseMoveData
                , rightClickedHicPosition (\_ -> RightClickedHicPosition)
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
