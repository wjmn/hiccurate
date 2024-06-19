port module Main exposing (..)

import Browser
import Browser.Events
import Dict
import File exposing (File)
import File.Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (Decoder, Value)
import Json.Encode as Encode
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes as SA
import Task



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
    , "chrY"
    ]


chromIndices : Dict.Dict String Int
chromIndices =
    Dict.fromList (List.indexedMap (\i c -> ( c, i )) chroms)


chromSizes : Dict.Dict String Int
chromSizes =
    Dict.fromList
        [ ( "chr1", 248956422 )
        , ( "chr2", 242193529 )
        , ( "chr3", 198295559 )
        , ( "chr4", 190214555 )
        , ( "chr5", 181538259 )
        , ( "chr6", 170805979 )
        , ( "chr7", 159345973 )
        , ( "chr8", 145138636 )
        , ( "chr9", 138394717 )
        , ( "chr10", 133797422 )
        , ( "chr11", 135086622 )
        , ( "chr12", 133275309 )
        , ( "chr13", 114364328 )
        , ( "chr14", 107043718 )
        , ( "chr15", 101991189 )
        , ( "chr16", 90338345 )
        , ( "chr17", 83257441 )
        , ( "chr18", 80373285 )
        , ( "chr19", 58617616 )
        , ( "chr20", 64444167 )
        , ( "chr21", 46709983 )
        , ( "chr22", 50818468 )
        , ( "chrX", 156040895 )
        , ( "chrY", 57227415 )
        ]


chromCumulativeSizes : Dict.Dict String Int
chromCumulativeSizes =
    Dict.fromList
        [ ( "chr1", 0 )
        , ( "chr2", 248956422 )
        , ( "chr3", 491149951 )
        , ( "chr4", 689445510 )
        , ( "chr5", 879660065 )
        , ( "chr6", 1066191324 )
        , ( "chr7", 1236997303 )
        , ( "chr8", 1396343276 )
        , ( "chr9", 1541481912 )
        , ( "chr10", 1689876629 )
        , ( "chr11", 1823674051 )
        , ( "chr12", 1958760673 )
        , ( "chr13", 2092035982 )
        , ( "chr14", 2206400310 )
        , ( "chr15", 2317444028 )
        , ( "chr16", 2419435217 )
        , ( "chr17", 2509773562 )
        , ( "chr18", 2593031003 )
        , ( "chr19", 2673404288 )
        , ( "chr20", 2732021904 )
        , ( "chr21", 2796466071 )
        , ( "chr22", 2843176054 )
        , ( "chrX", 2893997522 )
        , ( "chrY", 3040038417 )
        ]


totalChromSize : Int
totalChromSize =
    3097265832



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
    , lastClicked : Maybe Breakpoint
    , lastClickedIndex : Int
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
        [ ( "chr1Name", Encode.string syncState.chr1Name )
        , ( "chr2Name", Encode.string syncState.chr2Name )
        , ( "binSize", Encode.int syncState.binSize )
        , ( "binX", Encode.float syncState.binX )
        , ( "binY", Encode.float syncState.binY )
        , ( "pixelSize", Encode.float syncState.pixelSize )
        ]


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

        _ ->
            chrName


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
    , checked : Bool
    }


type BedpeFormat
    = BedpeBreakfinder
    | BedpeGeneric


{-| Read .bedpe string

hic\_breakfinder format:

    #chr1 x1 x2 chr2 y1 y2 strand1 strand2 resolution -logP
    chr12 7638000 7670000 chr9 22090000 22242000 + - 1kb 291.003

Generic bedpe format:

    chr1 x1 x2 chr2 y1 y2

-}
rowToBreakpoint : String -> Maybe Breakpoint
rowToBreakpoint row =
    let
        cols =
            String.words row
    in
    case cols of
        head :: tail ->
            if String.startsWith "#" head then
                Nothing

            else
                case cols of
                    [ chr1raw, x1, x2, chr2raw, y1, y2, strand1, strand2, resolution, negLogP ] ->
                        let
                            chr1 =
                                prefixChr chr1raw

                            chr2 =
                                prefixChr chr2raw

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
                        case ( Dict.get chr1 chromIndices, Dict.get chr2 chromIndices ) of
                            ( Just index1, Just index2 ) ->
                                if index1 <= index2 then
                                    Maybe.map5 (\s1 s2 res pos1 pos2 -> { chrA = chr1, posA = pos1, strandA = s1, chrB = chr2, posB = pos2, strandB = s2, resolution = res, checked = False })
                                        (stringToStrand strand1)
                                        (stringToStrand strand2)
                                        (suffixedNumberToInt resolution)
                                        pos1Maybe
                                        pos2Maybe

                                else
                                    Maybe.map5 (\s1 s2 res pos1 pos2 -> { chrA = chr2, posA = pos2, strandA = s2, chrB = chr1, posB = pos1, strandB = s1, resolution = res, checked = False })
                                        (stringToStrand strand1)
                                        (stringToStrand strand2)
                                        (suffixedNumberToInt resolution)
                                        pos1Maybe
                                        pos2Maybe

                            _ ->
                                Nothing

                    -- Todo: parse a generic bedpe
                    _ ->
                        Nothing

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
        halfWidth =
            toFloat rect.width / 2

        halfHeight =
            toFloat rect.height / 2

        posABin =
            toFloat breakpoint.posA / toFloat breakpoint.resolution

        posBBin =
            toFloat breakpoint.posB / toFloat breakpoint.resolution

        maxX =
            Dict.get breakpoint.chrA chromSizes |> Maybe.map (\x -> x - rect.width * breakpoint.resolution) |> Maybe.withDefault 0 |> toFloat |> (\x -> x / toFloat breakpoint.resolution)

        maxY =
            Dict.get breakpoint.chrB chromSizes |> Maybe.map (\x -> x - rect.height * breakpoint.resolution) |> Maybe.withDefault 0 |> toFloat |> (\x -> x / toFloat breakpoint.resolution)

        startX =
            (posABin - halfWidth) |> Basics.min maxX |> Basics.max 0

        startY =
            (posBBin - halfHeight) |> Basics.min maxY |> Basics.max 0
    in
    { chr1Name = unprefixChr breakpoint.chrA
    , chr2Name = unprefixChr breakpoint.chrB
    , binSize = breakpoint.resolution
    , binX = startX
    , binY = startY
    , pixelSize = 1.0
    }


breakpointToString : Breakpoint -> String
breakpointToString breakpoint =
    let
        ( x1, x2 ) =
            case breakpoint.strandA of
                Pos ->
                    ( breakpoint.posA - breakpoint.resolution, breakpoint.posA )

                Neg ->
                    ( breakpoint.posA, breakpoint.posA + breakpoint.resolution )

        ( y1, y2 ) =
            case breakpoint.strandB of
                Pos ->
                    ( breakpoint.posB - breakpoint.resolution, breakpoint.posB )

                Neg ->
                    ( breakpoint.posB, breakpoint.posB + breakpoint.resolution )
    in
    String.join "\t"
        [ breakpoint.chrA
        , String.fromInt x1
        , String.fromInt x2
        , breakpoint.chrB
        , String.fromInt y1
        , String.fromInt y2
        , strandToString breakpoint.strandA
        , strandToString breakpoint.strandB
        , String.fromInt breakpoint.resolution
        , "0"
        ]


allBreakpointsToString : List Breakpoint -> String
allBreakpointsToString breakpoints =
    String.join "\n"
        (String.join "\t" [ "#chr1", "x1", "x2", "chr2", "y1", "y2", "strand1", "strand2", "resolution", "-logP" ] :: (breakpoints |> List.filter (\x -> x.checked) |> List.map breakpointToString ))


changeResolution : MouseMoveData -> Int -> SyncState
changeResolution mouseMoveData newBinSize =
    let
        oldBinSize =
            mouseMoveData.syncState.binSize

        chr1Name =
            mouseMoveData.syncState.chr1Name

        chr2Name =
            mouseMoveData.syncState.chr2Name

        centerX =
            (mouseMoveData.syncState.binX * toFloat oldBinSize) + (toFloat mouseMoveData.rect.width * toFloat oldBinSize / 2)

        centerY =
            (mouseMoveData.syncState.binY * toFloat oldBinSize) + (toFloat mouseMoveData.rect.height * toFloat oldBinSize / 2)

        maxX =
            Dict.get (prefixChr chr1Name) chromSizes |> Maybe.map (\x -> x - mouseMoveData.rect.width * newBinSize) |> Maybe.withDefault 0 |> toFloat

        maxY =
            Dict.get (prefixChr chr2Name) chromSizes |> Maybe.map (\x -> x - mouseMoveData.rect.width * newBinSize) |> Maybe.withDefault 0 |> toFloat

        newStartX =
            (centerX - (toFloat newBinSize * toFloat mouseMoveData.rect.width / 2)) |> Basics.min maxX |> Basics.max 0 |> (\x -> x / toFloat newBinSize)

        newStartY =
            (centerY - (toFloat newBinSize * toFloat mouseMoveData.rect.height / 2)) |> Basics.min maxY |> Basics.max 0 |> (\x -> x / toFloat newBinSize)

        newSyncState =
            { chr1Name = chr1Name
            , chr2Name = chr2Name
            , binSize = newBinSize
            , binX = newStartX
            , binY = newStartY
            , pixelSize = 1.0
            }
    in
    newSyncState


changeZoom : MouseMoveData -> Float -> SyncState
changeZoom mouseMoveData multiplier =
    let
        oldPixelSize =
            mouseMoveData.syncState.pixelSize

        newPixelSize =
            oldPixelSize * multiplier

        binSize =
            mouseMoveData.syncState.binSize

        chr1Name =
            mouseMoveData.syncState.chr1Name

        chr2Name =
            mouseMoveData.syncState.chr2Name

        centerX =
            (mouseMoveData.syncState.binX * toFloat binSize) + (toFloat mouseMoveData.rect.width / oldPixelSize * toFloat binSize / 2)

        centerY =
            (mouseMoveData.syncState.binY * toFloat binSize) + (toFloat mouseMoveData.rect.height / oldPixelSize * toFloat binSize / 2)

        maxX =
            Dict.get (prefixChr chr1Name) chromSizes |> Maybe.map (\x -> toFloat x - toFloat mouseMoveData.rect.width / newPixelSize * toFloat binSize) |> Maybe.withDefault 0

        maxY =
            Dict.get (prefixChr chr2Name) chromSizes |> Maybe.map (\x -> toFloat x - toFloat mouseMoveData.rect.width / newPixelSize * toFloat binSize) |> Maybe.withDefault 0

        newStartX =
            (centerX - (toFloat binSize * toFloat mouseMoveData.rect.width / (2 * newPixelSize))) |> Basics.min maxX |> Basics.max 0 |> (\x -> x / toFloat binSize)

        newStartY =
            (centerY - (toFloat binSize * toFloat mouseMoveData.rect.height / (2 * newPixelSize))) |> Basics.min maxY |> Basics.max 0 |> (\x -> x / toFloat binSize)

        newSyncState =
            { chr1Name = chr1Name
            , chr2Name = chr2Name
            , binSize = binSize
            , binX = newStartX
            , binY = newStartY
            , pixelSize = newPixelSize
            }
    in
    newSyncState


{-| Choose a default zoom of 500kb
-}
changeChromosomes : DomRect -> ( String, String ) -> SyncState
changeChromosomes rect ( chrA, chrB ) =
    if chrA == "ALL" && chrB == "ALL" then
        { chr1Name = chrA
        , chr2Name = chrB
        , binSize = 2500000
        , binX = 0
        , binY = 0
        , pixelSize = 1.1598925747162019
        }

    else
        let
            -- Calculate pixelSize based on maximum chromosome size
            maxChromSize = 
                Basics.max (Dict.get chrA chromSizes |> Maybe.withDefault 0) (Dict.get chrB chromSizes |> Maybe.withDefault 0)
            resolution = 
                if maxChromSize > 100000000 then 
                    500000
                else if maxChromSize > 70000000 then
                    250000
                else
                    100000
                
            pixelSize = 
                Basics.max 1.0 (toFloat rect.width * resolution / toFloat maxChromSize)
        in
        { chr1Name = chrA
        , chr2Name = chrB
        , binSize = resolution
        , binX = 0
        , binY = 0
        , pixelSize = pixelSize
        }



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
    | DeleteBreakpointByValue Breakpoint
    | GotSelectedBedpe File
    | GotSelectedBedpeAsString String
    | MouseEnter Breakpoint
    | MouseLeave Breakpoint
    | ClickedBreakpoint Int Breakpoint
    | ClickedSaveBreakpoints
    | ClickedResolution Int
    | ClickedChromosomeBox ( String, String )
    | ClickedZoom Float
    | ClickedCheckBreakpoint Int
    | ClickedCheckBreakpointByValue Breakpoint
    | ClickedNextOrPreviousIndex Int


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
                        , lastClicked = Nothing
                        , lastClickedIndex = 0
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
                            if currentViewData.chrXName == "chrALL" && currentViewData.chrYName == "chrALL" then
                                noop
                            else if currentViewData.chrXName == currentViewData.chrYName && currentViewData.chrXPointer >= currentViewData.chrYPointer then
                                noop 

                            else
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
                                        , checked = True
                                        }

                                    newBreakpoints = 
                                        (breakpoint :: data.breakpoints ) |> List.sortBy (\bp -> ( ( Dict.get bp.chrA chromIndices |> Maybe.withDefault 0, Dict.get bp.chrB chromIndices |> Maybe.withDefault 0 ), ( bp.posA, bp.posB ) ))

                                    newIndex = 
                                        List.indexedMap Tuple.pair newBreakpoints |> List.filter (\(i, bp) -> bp == breakpoint) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
                                in
                                HicLoaded
                                    { data
                                        | breakpoints = newBreakpoints
                                        , lastClicked = Just breakpoint
                                        , lastClickedIndex = newIndex
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
                            , lastClicked = Nothing
                        }
                        |> withCmd Cmd.none
                DeleteBreakpointByValue breakpoint -> 
                    HicLoaded
                        { data
                            | breakpoints =
                                List.filter (\bp -> bp /= breakpoint) data.breakpoints
                            , lastClicked = Nothing
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
                                |> List.sortBy (\bp -> ( ( Dict.get bp.chrA chromIndices |> Maybe.withDefault 0, Dict.get bp.chrB chromIndices |> Maybe.withDefault 0 ), ( bp.posA, bp.posB ) ))
                    in
                    -- For now, add the breakpoints to current breakpoints to avoid data loss
                    HicLoaded { data | breakpoints = data.breakpoints ++ breakpoints }
                        |> withCmd Cmd.none

                MouseEnter breakpoint ->
                    HicLoaded { data | hoveredBreakpoint = Just breakpoint } |> withCmd Cmd.none

                MouseLeave breakpoint ->
                    case data.hoveredBreakpoint of
                        Just currentBreakpoint ->
                            if currentBreakpoint == breakpoint then
                                HicLoaded { data | hoveredBreakpoint = Nothing } |> withCmd Cmd.none

                            else
                                noop

                        Nothing ->
                            noop

                ClickedBreakpoint index breakpoint ->
                    case data.mouseMoveData of
                        Just mouseMoveData ->
                            let
                                newSyncState =
                                    breakpointToSyncState mouseMoveData.rect breakpoint
                            in
                            HicLoaded { data | lastClicked = Just breakpoint, lastClickedIndex = index}
                                |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))

                        _ ->
                            noop

                ClickedSaveBreakpoints ->
                    -- TODO implement
                    let
                        outString =
                            allBreakpointsToString data.breakpoints
                    in
                    HicLoaded data
                        |> withCmd (File.Download.string ((data.hicName |> String.replace "_inter_30" "" |> String.replace ".hic" "") ++ ".breaks.bedpe") "text/plain" outString)

                ClickedResolution newResolution ->
                    case data.mouseMoveData of
                        Just mouseMoveData ->
                            if mouseMoveData.syncState.chr1Name == "ALL" && mouseMoveData.syncState.chr2Name == "ALL" then
                                HicLoaded data
                                    |> withCmd Cmd.none

                            else
                                let
                                    newSyncState =
                                        changeResolution mouseMoveData newResolution
                                in
                                HicLoaded data
                                    |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))
                        _ ->
                            noop

                ClickedZoom multiplier ->
                    case data.mouseMoveData of
                        Just mouseMoveData ->
                            if mouseMoveData.syncState.chr1Name == "ALL" && mouseMoveData.syncState.chr2Name == "ALL" then
                                HicLoaded data
                                    |> withCmd Cmd.none

                            else
                                let
                                    newSyncState =
                                        changeZoom mouseMoveData multiplier
                                in
                                HicLoaded data
                                    |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))

                        Nothing ->
                            noop

                ClickedChromosomeBox chromosomes ->
                    case data.mouseMoveData of
                        Just mouseMoveData ->
                            let
                                newSyncState =
                                    changeChromosomes mouseMoveData.rect chromosomes
                            in
                            HicLoaded data
                                |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))

                        Nothing ->
                            noop

                ClickedCheckBreakpoint index ->
                    let
                        breakpoint =
                            List.indexedMap Tuple.pair data.breakpoints |> List.filter (\(i, bp) -> i == index) |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault { chrA = "", posA = 0, strandA = Pos, chrB = "", posB = 0, strandB = Pos, resolution = 0, checked = False }
                    in
                    HicLoaded
                        { data
                            | breakpoints =
                                List.indexedMap
                                    (\i bp ->
                                        if i == index then
                                            { bp | checked = not bp.checked }

                                        else
                                            bp
                                    )
                                    data.breakpoints
                            , lastClicked = Just { breakpoint | checked = not breakpoint.checked }
                        }
                        |> withCmd Cmd.none
                ClickedCheckBreakpointByValue breakpoint -> 
                    HicLoaded
                        { data
                            | breakpoints =
                                List.map
                                    (\bp ->
                                        if bp == breakpoint then
                                            { bp | checked = not bp.checked }

                                        else
                                            bp
                                    )
                                    data.breakpoints
                            , lastClicked = Just { breakpoint | checked = not breakpoint.checked }
                        }
                        |> withCmd Cmd.none

                ClickedNextOrPreviousIndex delta -> 
                    let
                        oldIndex = 
                            data.lastClickedIndex
                        newIndex = 
                            
                            if oldIndex + delta >= List.length data.breakpoints  then 
                                0
                            else if oldIndex + delta < 0 then 
                                List.length data.breakpoints - 1
                            else 
                                oldIndex + delta
                        breakpointAtIndex = 
                            case data.lastClicked of
                                Just _ -> 
                                    List.indexedMap Tuple.pair data.breakpoints |> List.filter (\(i, bp) -> i == newIndex) |> List.head
                                Nothing -> 
                                    List.indexedMap Tuple.pair data.breakpoints |> List.filter (\(i, bp) -> i == oldIndex) |> List.head
                    in
                    case breakpointAtIndex of 
                        Just (i, bp) -> 
                            case data.mouseMoveData of 
                                Just mouseMoveData ->
                                    let
                                        newSyncState =
                                            breakpointToSyncState mouseMoveData.rect bp
                                    in
                                    HicLoaded { data | lastClicked = Just bp, lastClickedIndex = i }
                                        |> withCmd (sendNewHicBrowserSyncState (encodeSyncState newSyncState))
                                Nothing -> noop
                        Nothing -> 
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
    div [ id "elm-splash", tw "flex justify-center items-center flex-col justify-center items-center" ]
        [ div [ tw "text-3xl mb-8" ] [ text "Choose a Hi-C File to get started. " ]
        , input [ type_ "file", onFile GotSelectedFile, tw " text-center" ] []
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

        disableZoomOut =
            case data.mouseMoveData of
                Just mouseMoveData ->
                    if mouseMoveData.syncState.pixelSize <= 1.0 then
                        True

                    else
                        False

                Nothing ->
                    False

        currentResolution = 
            case data.mouseMoveData of 
                Just mouseMoveData -> 
                    mouseMoveData.syncState.binSize
                Nothing -> 
                    0

        opacity = 
            case data.currentViewData of 
                Just (currentViewData, _) -> 
                    if currentViewData.chrXName == currentViewData.chrYName && currentViewData.chrXPointer >= currentViewData.chrYPointer && currentViewData.chrXName /= "chrALL" then
                        "0"
                    else 
                        "1"
                Nothing -> 
                    "1"
        
        resButton int string = 
            button [ onClick (ClickedResolution int), tw "px-2 py-1 text-xs", classList [("bg-red-500 text-white", int == currentResolution)]] [ text string ]
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
                        , SA.opacity opacity
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
                        , SA.opacity opacity
                        ]
                        []
                    , line
                        [ SA.x1 x1
                        , SA.y1 yPointer
                        , SA.x2 x2
                        , SA.y2 yPointer
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1"
                        , SA.opacity opacity
                        ]
                        []
                    , line
                        [ SA.x1 xPointer
                        , SA.y1 y1
                        , SA.x2 xPointer
                        , SA.y2 y2
                        , SA.stroke "black"
                        , SA.strokeWidth "0.1"
                        , SA.opacity opacity
                        ]
                        []
                    ]
                , viewAllBreakpointSvg data
                ]
            ]
        , div [ id "elm-hic-overlay-buttons", tw "flex items-center justify-center" ]
            [ div [tw "m-2"] 
                [chromosomeGridSelector data.currentViewData]
            , div [ tw "flex flex-col mx-4"]
                [ resButton 500000 "500kb" 
                , resButton 250000 "250kb" 
                , resButton 100000 "100kb" 
                , resButton 50000 "50kb" 
                , resButton 25000 "25kb" 
                , resButton 10000 "10kb" 
                , resButton 5000 "5kb" 
                , resButton 1000 "1kb" 
                ]
            , div [ tw "flex flex-col-reverse"]
                [ button [ tw "px-2 py-1 text-sm hover:bg-gray-300", onClick (ClickedZoom 0.5), classList [("bg-gray-300 text-gray-500", disableZoomOut), ("bg-white", not disableZoomOut) ], disabled disableZoomOut ] [ text "Zoom Out" ]
                , button [ tw "bg-white px-2 py-1 text-sm hover:bg-gray-300", onClick (ClickedZoom 2) ] [ text "Zoom In"] 
                ]
            ]
        , div
            [ id "elm-main", tw "p-8 flex flex-col w-full" ]
            [ div [tw "text-gray-500 text-xs mb-4"] [text "Right click map: add breakpoint at cursor. 1/2/3/4: change crosshair strandness. W/S: navigate breakpoint table. A: delete selected breakpoint. D: toggle checked."]
            , div [] [h1 [ tw "w-full text-2xl mb-4"] [text data.hicName]]
            , div [ tw "bg-gray-100 w-full flex rounded px-2 py-1 justify-between items-center text-sm" ]
                [ div [tw "text-sm mr-4 text-gray-700"] [text "(Optional) Add breaks from Hi-C Breakfinder .bedpe file..."]
                ,  input [ type_ "file", onFile (\( f, _ ) -> GotSelectedBedpe f) ] [] ]
            , div [ tw "my-4 w-full" ] [ viewBreakpoints data.lastClicked data.breakpoints ]
            , div [ ] 
                [ button [ tw "w-full text-lg bg-gray-300 text-gray-800 px-2 py-1 text-center hover:bg-blue-500 hover:text-white rounded pointer", onClick ClickedSaveBreakpoints ] [ text "Download Checked Breakpoints" ] ]
            ]
        ]


{-| Draw a breakpoint as a crosshair / horizontal and vertical line with correct strand orientation

Assume breakpoint has already been checked to be within the current view data

-}
viewBreakpointSvg : Maybe Breakpoint -> Maybe Breakpoint -> Int -> CurrentViewData -> Breakpoint -> Svg Msg
viewBreakpointSvg maybeLastClicked maybeHoveredBreakpoint binSize currentViewData breakpoint =
    let
        chrA =
            breakpoint.chrA

        chrB =
            breakpoint.chrB

        isAll = currentViewData.chrXName == "chrALL" && currentViewData.chrYName == "chrALL"

        bpX =
            if  isAll then
                ((Dict.get chrA chromCumulativeSizes |> Maybe.withDefault 0 |> toFloat) + toFloat breakpoint.posA) / toFloat totalChromSize * 100

            else
                (toFloat breakpoint.posA - toFloat currentViewData.chrXStart) / toFloat (currentViewData.chrXEnd - currentViewData.chrXStart) * 100

        bpY =
            if isAll then 
                ((Dict.get chrB chromCumulativeSizes |> Maybe.withDefault 0 |> toFloat) + toFloat breakpoint.posB) / toFloat totalChromSize * 100

            else
                (toFloat breakpoint.posB - toFloat currentViewData.chrYStart) / toFloat (currentViewData.chrYEnd - currentViewData.chrYStart) * 100

        -- All SVG elements are scaled to 100 x 100 in viewbox
        lineLength =
            if isAll then 
                1
            else
                (3000000 / toFloat binSize) / 200 + 1
        strokeWidth = 
            if isAll then 
                0.3
            else
                0.5

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
            case (maybeLastClicked, maybeHoveredBreakpoint) of
                (Just lastClickedbreakpoint, Just hoveredBreakpoint) ->
                    if breakpoint == lastClickedbreakpoint then
                        "blue"

                    else if breakpoint == hoveredBreakpoint then
                        "green"

                    else
                        "black"
                (Nothing, Just hoveredBreakpoint) ->
                    if breakpoint == hoveredBreakpoint then
                        "green"

                    else
                        "black"
                (Just lastClickedbreakpoint, Nothing) ->
                    if breakpoint == lastClickedbreakpoint then
                        "blue"

                    else
                        "black"

                (Nothing, Nothing) ->
                    "black"
    in
    Svg.g []
        [ Svg.line
            [ SA.x1 (String.fromFloat x1)
            , SA.y1 (String.fromFloat bpY)
            , SA.x2 (String.fromFloat x2)
            , SA.y2 (String.fromFloat bpY)
            , SA.stroke color
            , SA.strokeWidth (String.fromFloat strokeWidth)
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat bpX)
            , SA.y1 (String.fromFloat y1)
            , SA.x2 (String.fromFloat bpX)
            , SA.y2 (String.fromFloat y2)
            , SA.stroke color
            , SA.strokeWidth (String.fromFloat strokeWidth)
            ]
            []
        ]


viewAllBreakpointSvg : HicLoadedData -> Svg Msg
viewAllBreakpointSvg data =
    case ( data.mouseMoveData, data.currentViewData ) of
        ( Just mouseMoveData, Just ( currentViewData, _ ) ) ->
            Svg.g []
                (data.breakpoints
                    |> List.filterMap
                        (\breakpoint ->
                            if
                                (currentViewData.chrXName == "chrALL" && currentViewData.chrYName == "chrALL") || 
                                (breakpoint.chrA
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
                                    <= currentViewData.chrYEnd)
                            then
                                Just (viewBreakpointSvg data.lastClicked data.hoveredBreakpoint mouseMoveData.syncState.binSize currentViewData breakpoint)

                            else
                                Nothing
                        )
                )

        _ ->
            Svg.g [] []


viewBreakpoint : Maybe Breakpoint -> Int -> Breakpoint -> Html Msg
viewBreakpoint lastClicked index breakpoint =
    let
        onClickEvent =
            onClick (ClickedBreakpoint index breakpoint)

        cellStyles =
            tw ""

        extraStyles =
            case lastClicked of
                Just lastClickedbreakpoint ->
                    if breakpoint == lastClickedbreakpoint then
                        tw "bg-blue-500 text-white"

                    else
                        tw ""

                Nothing ->
                    tw ""

        cell contents =
            td [ cellStyles, extraStyles, onClickEvent ] contents
    in
    tr
        [ onMouseOver (MouseEnter breakpoint)
        , onMouseOut (MouseLeave breakpoint)
        , tw "hover:bg-gray-300 cursor-pointer"
        , classList [("bg-gray-100 text-gray-600", breakpoint.checked)]
        ]
        [ td [ cellStyles, extraStyles, tw "" ] [ input [ type_ "checkbox",  tw "w-full h-full", checked breakpoint.checked, onClick (ClickedCheckBreakpoint index) ] [] ]
        , cell [ text breakpoint.chrA ]
        , cell [ text (String.fromInt breakpoint.posA) ]
        , cell [ text (strandToString breakpoint.strandA) ]
        , cell [ text breakpoint.chrB ]
        , cell [ text (String.fromInt breakpoint.posB) ]
        , cell [ text (strandToString breakpoint.strandB) ]
        , cell [ text (String.fromInt breakpoint.resolution) ]
        , td [ tw "text-center", cellStyles, extraStyles ] [ button [ tw "hover:bg-red-500 w-full m-0", onClick (DeleteBreakpoint index) ] [ text "X" ] ]
        ]


viewBreakpoints : Maybe Breakpoint -> List Breakpoint -> Html Msg
viewBreakpoints lastClicked breakpoints =
    table [ tw "w-full"]
        [ thead []
            [ tr [ tw "text-left text-gray-700 bg-gray-200"]
                [ th [tw "text-center"] [ text "✓" ]
                , th [] [ text "chrA" ]
                , th [] [ text "posA" ]
                , th [] [ text "strandA" ]
                , th [] [ text "chrB" ]
                , th [] [ text "posB" ]
                , th [] [ text "strandB" ]
                , th [] [ text "resolution" ]
                , th [] [text ""]
                ]
            ]
        , tbody [] (List.indexedMap (viewBreakpoint lastClicked) breakpoints)
        ]

allChromosomePairs : List ( String, String, Bool )
allChromosomePairs = 
    chroms 
    |> List.indexedMap (\i2 chr2 -> chroms |> List.indexedMap (\i1 chr1 -> (chr1, chr2, i1 <= i2)))
    |> List.concat

chromosomeGridSelector : Maybe (CurrentViewData, e) -> Html Msg 
chromosomeGridSelector maybeCurrentViewData = 
    let
        current = 
            case maybeCurrentViewData of
                Just (currentViewData, _) -> 
                    (currentViewData.chrXName, currentViewData.chrYName)
                _ -> 
                    ("", "")
        isAll = 
            current == ("chrALL", "chrALL")
        isAllStyle = 
            if isAll then 
                tw "bg-red-500 highlight"
            else 
                tw ""
    in
    
    div [id "chromosome-grid-selector-container", tw "border border-gray-400"]
        [ div [id "chromosome-grid-selector"]
            (allChromosomePairs |> List.map (viewChromosomeBox current))]

viewChromosomeBox : (String, String) -> (String, String, Bool) -> Html Msg
viewChromosomeBox current (chr1, chr2, show) =
    let
        showStyle = 
            if (chr1, chr2) == current then
                tw "bg-red-500 highlight"
            else
                tw "border"
        otherStyle = 
            if current == ("chrALL", "chrALL") then 
                tw "bg-red-500 highlight"
            else 
                tw "bg-gray"
    in
    if show then 
        button [onClick (ClickedChromosomeBox (chr1, chr2)), showStyle, id "chromosome-box"] [div [id "chromosome-box-tooltip"] [text (chr1 ++ " x " ++ chr2)]]
    else
        button [otherStyle, onClick (ClickedChromosomeBox ("ALL", "ALL")), id "all-chromosome-box"] [ div [id "chromosome-box-tooltip"] [text "ALL"]]


------------------------------------------------------------------------------
-- SUBSCRIPTIONS
------------------------------------------------------------------------------


decoderKeyPress : HicLoadedData -> Decoder Msg
decoderKeyPress data =
    Json.field "key" Json.string
        |> Json.andThen
            (\key ->
                case key of
                    "1" ->
                        Json.succeed (ChangedStrand ( Pos, Pos ))

                    "2" ->
                        Json.succeed (ChangedStrand ( Neg, Pos ))

                    "3" ->
                        Json.succeed (ChangedStrand ( Pos, Neg ))

                    "4" ->
                        Json.succeed (ChangedStrand ( Neg, Neg ))

                    "a" ->
                        case data.lastClicked of
                            Just breakpoint ->
                                Json.succeed (DeleteBreakpointByValue breakpoint)

                            Nothing ->
                                Json.fail "No breakpoint selected"
                    "d" -> 
                        case data.lastClicked of 
                            Just breakpoint -> 
                                Json.succeed (ClickedCheckBreakpointByValue breakpoint)
                            Nothing -> 
                                Json.fail "No breakpoint selected"

                    "w" -> 
                        Json.succeed (ClickedNextOrPreviousIndex -1)
                    "s" -> 
                        Json.succeed (ClickedNextOrPreviousIndex 1)

                    _ ->
                        Json.fail "Invalid key"
            )


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Splash _ ->
            Sub.none

        HicLoaded data ->
            Sub.batch
                [ receiveMouseMoveData GotMouseMoveData
                , rightClickedHicPosition (\_ -> RightClickedHicPosition)
                , Browser.Events.onKeyPress (decoderKeyPress data)
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
