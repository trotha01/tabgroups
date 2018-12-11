port module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import List.Extra as List
import Set exposing (Set)
import String.Extra exposing (ellipsis)



{-
   TODO:
   - fix ability to change model without clearing local storage (use versioning)
   - click and drag tabs within tab group
   - click and drag tabs to new tab groups
   - catch tab rearrangement
   - add top-level iterator for tabID and tabGroup id instead of looking at dict size (for when one in the middle is deleted)
-}


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tabGroups : TabGroups
    , tabs : Tabs
    , windowIDs : Set WindowID
    , activeTabGroupID : TabGroupID
    , tabDrag : Maybe TabID
    , error : Maybe String
    }


type alias WindowID =
    Int


activeDraggingTab : Model -> Maybe ( TabID, Tab )
activeDraggingTab model =
    case model.tabDrag of
        Nothing ->
            Nothing

        Just id ->
            Dict.get id model.tabs
                |> Maybe.map (\tab -> ( id, tab ))


mapTabDrag : Maybe TabID -> Model -> Model
mapTabDrag draggingTab model =
    { model | tabDrag = draggingTab }


tabsInTabGroup : Dict TabID Tab -> TabGroup -> List ( TabID, Tab )
tabsInTabGroup tabDict tabgroup =
    List.filterMap
        (\tabID ->
            Dict.get tabID tabDict
                |> Maybe.map (\tab -> ( tabID, tab ))
        )
        (tabgroup.tabIDs |> Set.toList)


insertTabGroup : TabGroup -> TabGroups -> TabGroups
insertTabGroup newTabGroup tabGroups =
    Dict.insert (Dict.size tabGroups) newTabGroup tabGroups


updateTabGroupAt : TabGroupID -> (TabGroup -> TabGroup) -> TabGroups -> TabGroups
updateTabGroupAt id f tabGroups =
    Dict.update id (Maybe.map f) tabGroups


updateTabAt : TabID -> (Tab -> Tab) -> Tabs -> Tabs
updateTabAt id f tabs =
    Dict.update id (Maybe.map f) tabs


mapScreenshot : Maybe String -> Tab -> Tab
mapScreenshot screenshot tab =
    { tab | screenshot = screenshot }


mapTitle : String -> TabGroup -> TabGroup
mapTitle title tabGroup =
    { tabGroup | title = title }


mapChangingTitle : Bool -> TabGroup -> TabGroup
mapChangingTitle changingTitle tabGroup =
    { tabGroup | changingTitle = changingTitle }


removeTabGroup : TabGroupID -> TabGroups -> TabGroups
removeTabGroup id tabGroups =
    Dict.remove id tabGroups


activeTabGroup : Model -> TabGroup
activeTabGroup model =
    Dict.get model.activeTabGroupID model.tabGroups
        |> Maybe.withDefault (initTabGroup "Main" Set.empty)



{--
tabsByWindow : List IncomingTabInfo -> Dict WindowID (List Tab)
tabsByWindow tabInfo =
    let
        tabsByWindow =
            -- List (List IncomingTabInfo)
            groupWhile (\tab1 tab2 -> tab1.windowID == tab2.windowID)

        tabDictsByWindow =
            -- List (Maybe WindowID, Tabs)
            List.map (tabDictFromIncomingTabInfo startingID) tabsByWindow

        newTabGroups =
            List.foldl (\windowTabs tabGroups -> insertWindowTabs windowTabs windowID) tabGroups tabDictsByWindow

        newTabs =
            tab
    in
    tabInfo
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


insertWindowTabs : ( Maybe WindowID, Tabs ) -> TabGroups -> TabGroups
insertWindowTabs ( windowID, tabs ) tabGroups =
    let
        matchingGroup =
            findGroup tabs tabGroups

        tabGroupID =
            windowID |> Maybe.withDefault (Dict.size tabGroup)

        newTabGroup =
            initTabGroup "New Group" (initTabGroup.keys tabs)
    in
    Dict.insert tabGroupID newTabGroup tabGroups
--}


{-| findGroup will return a group, given a list of tabs
if enough of the tabs match
-}
findGroup : Set TabID -> TabGroups -> Maybe TabGroup
findGroup tabIDs tabGroups =
    let
        ( matchPercent, closestMatchingGroup ) =
            Dict.foldl
                (\tgID tg ( maxPercMatch, closestMatch ) ->
                    let
                        correlation =
                            tabCorrelation tabIDs tg
                    in
                    if correlation > maxPercMatch then
                        ( correlation, Just tg )

                    else
                        ( maxPercMatch, closestMatch )
                )
                ( 0, Nothing )
                tabGroups
    in
    closestMatchingGroup


tabCorrelation : Set TabID -> TabGroup -> Float
tabCorrelation tabIDs tabGroup =
    let
        intersection =
            Set.intersect tabIDs tabGroup.tabIDs
                |> Set.size
    in
    toFloat intersection / (toFloat <| Set.size tabIDs)



{--
tabDictFromIncomingTabInfo : TabID -> List IncomingTabInfo -> ( Maybe WindowID, Tabs )
tabDictFromIncomingTabInfo startingID incomingTabInfo =
    ( List.head incomingTabInfo |> Maybe.map .windowID
    , List.foldl
        (\tabInfo ( id, tabs ) ->
            ( id + 1, Dict.insert id (newTabFromInfo tabInfo) tabs )
        )
        ( startingID, Dict.empty )
        incomingTabInfo
    )
--}


newTabFromInfo : IncomingTabInfo -> Tab
newTabFromInfo tabInfo =
    { title = tabInfo.title
    , url = tabInfo.url
    , screenshot = tabInfo.screenshot
    , height = 0
    , width = 0
    , drag = Nothing
    }



{--
If we query for a new group of tabs,
we want the resulting tabs to be made into new groups by window
then added to our current dict of group ids
and also add the tabIDs to our current set of tabIDs

[{tab}, {tab}]
-> [ [{tab}, {tab}, {tab}]

need something to convert List
--}


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    listSpan (eq x) xs
            in
            (x :: ys) :: groupWhile eq zs


listSpan : (a -> Bool) -> List a -> ( List a, List a )
listSpan p xs =
    ( List.takeWhile p xs, List.dropWhile p xs )


type alias TabGroups =
    Dict TabGroupID TabGroup


type alias Tabs =
    Dict TabID Tab


type alias TabGroupID =
    Int


type alias TabID =
    Int


modelEncode : Model -> Encode.Value
modelEncode model =
    Encode.object
        [ ( "tabGroups", Encode.dict String.fromInt tabGroupEncode model.tabGroups )
        , ( "tabs", Encode.dict String.fromInt tabEncode model.tabs )
        , ( "windowIDs", Encode.set Encode.int model.windowIDs )
        , ( "activeTabGroupID", Encode.int model.activeTabGroupID )
        , ( "tabDrag", maybeEncode model.tabDrag Encode.int )
        , ( "error", maybeEncode model.error Encode.string )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.map6 Model
        (Decode.field "tabGroups" <| Decode.dict2 Decode.int tabGroupDecoder)
        (Decode.field "tabs" <| Decode.dict2 Decode.int tabDecoder)
        (Decode.field "windowIDs" <| Decode.set Decode.int)
        (Decode.field "activeTabGroupID" <| Decode.int)
        (Decode.field "tabDrag" <| Decode.maybe <| Decode.int)
        (Decode.field "error" <| Decode.maybe <| Decode.string)


type alias TabGroup =
    { title : GroupTitle
    , tabIDs : Set TabID
    , windowID : Maybe WindowID
    , position : Position
    , drag : Maybe Drag
    , changingTitle : Bool
    , dimensions : Dimensions
    , resize : Maybe Drag
    }


tabGroupEncode : TabGroup -> Encode.Value
tabGroupEncode tabGroup =
    Encode.object
        [ ( "title", Encode.string tabGroup.title )
        , ( "tabIDs", Encode.set Encode.int tabGroup.tabIDs )
        , ( "windowID", maybeEncode tabGroup.windowID Encode.int )
        , ( "position", positionEncode tabGroup.position )
        , ( "drag", maybeEncode tabGroup.drag dragEncode )
        , ( "changingTitle", Encode.bool tabGroup.changingTitle )
        , ( "dimensions", dimensionsEncode tabGroup.dimensions )
        , ( "resize", maybeEncode tabGroup.resize dragEncode )
        ]


tabGroupDecoder : Decoder TabGroup
tabGroupDecoder =
    Decode.map8 TabGroup
        (Decode.field "title" Decode.string)
        (Decode.field "tabIDs" <| Decode.set Decode.int)
        (Decode.field "windowID" <| Decode.nullable <| Decode.int)
        (Decode.field "position" positionDecoder)
        (Decode.field "drag" <| Decode.nullable <| dragDecoder)
        (Decode.field "changingTitle" Decode.bool)
        (Decode.field "dimensions" dimensionsDecoder)
        (Decode.field "resize" <| Decode.nullable <| dragDecoder)


maybeEncode : Maybe a -> (a -> Encode.Value) -> Encode.Value
maybeEncode value encoder =
    case value of
        Nothing ->
            Encode.null

        Just j ->
            encoder j


type alias GroupTitle =
    String


type alias Dimensions =
    { height : Int
    , width : Int
    }


dimensionsDecoder : Decoder Dimensions
dimensionsDecoder =
    Decode.map2 Dimensions
        (Decode.field "height" Decode.int)
        (Decode.field "width" Decode.int)


dimensionsEncode : Dimensions -> Encode.Value
dimensionsEncode dimensions =
    Encode.object
        [ ( "height", Encode.int dimensions.height )
        , ( "width", Encode.int dimensions.width )
        ]


type alias Position =
    { x : Int
    , y : Int
    }


positionEncode : Position -> Encode.Value
positionEncode position =
    Encode.object
        [ ( "x", Encode.int position.x )
        , ( "y", Encode.int position.y )
        ]


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


type alias Drag =
    { start : Position
    , current : Position
    }


dragEncode : Drag -> Encode.Value
dragEncode drag =
    Encode.object
        [ ( "start", positionEncode drag.start )
        , ( "current", positionEncode drag.current )
        ]


dragDecoder : Decoder Drag
dragDecoder =
    Decode.map2 Drag
        (Decode.field "start" positionDecoder)
        (Decode.field "current" positionDecoder)


initTabGroup : GroupTitle -> Set TabID -> TabGroup
initTabGroup title initialTabs =
    { title = title
    , tabIDs = initialTabs
    , windowID = Nothing
    , position = Position 10 10
    , drag = Nothing
    , changingTitle = False
    , dimensions = { width = 400, height = 300 }
    , resize = Nothing
    }


blankTabGroup : TabGroup
blankTabGroup =
    initTabGroup "Add Title Here" Set.empty


type alias IncomingTabInfo =
    { title : String
    , url : String
    , screenshot : Maybe String
    , height : Int
    , width : Int
    , drag : Maybe Drag
    , windowID : Maybe WindowID
    }


type alias Tab =
    { title : String
    , url : String
    , screenshot : Maybe String
    , height : Int
    , width : Int
    , drag : Maybe Drag
    }


tabEncode : Tab -> Encode.Value
tabEncode tab =
    Encode.object
        [ ( "title", Encode.string tab.title )
        , ( "url", Encode.string tab.url )
        , ( "screenshot", maybeEncode tab.screenshot Encode.string )
        , ( "height", Encode.int tab.height )
        , ( "width", Encode.int tab.width )
        , ( "drag", maybeEncode tab.drag dragEncode )
        ]


tabDecoder : Decoder Tab
tabDecoder =
    Decode.map6 Tab
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "screenshot" <| Decode.nullable <| Decode.string)
        (Decode.field "height" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "drag" <| Decode.nullable <| dragDecoder)


incomingTabInfoDecoder : Decoder IncomingTabInfo
incomingTabInfoDecoder =
    Decode.map7 IncomingTabInfo
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "screenshot" <| Decode.nullable <| Decode.string)
        (Decode.field "height" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "drag" <| Decode.nullable <| dragDecoder)
        (Decode.field "windowID" <| Decode.nullable <| Decode.int)


type alias TabScreenshot =
    { id : Int
    , img : Maybe String
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { tabGroups = Dict.empty
      , tabs = Dict.empty
      , windowIDs = Set.empty
      , activeTabGroupID = 0
      , tabDrag = Nothing
      , error = Nothing
      }
    , getModel ()
    )



-- UPDATE


type Msg
    = GotTabs (List IncomingTabInfo)
    | GotTabGroup TabGroup
    | GotTabScreenshot TabScreenshot
    | GotSavedModel (Maybe Decode.Value)
    | StartGroupTitleEdit TabGroupID
    | FinishGroupTitleEdit TabGroupID
    | ChangeGroupTitle TabGroupID String
    | DeleteTabGroup TabGroupID
    | AddTabGroup
    | TabDragMsg ( TabID, DragMsg )
    | TabGroupDragMsg ( TabGroupID, DragMsg )
    | TabGroupResizeMsg ( TabGroupID, DragMsg )


type DragMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


mapTabGroups : (TabGroups -> TabGroups) -> Model -> Model
mapTabGroups f model =
    { model | tabGroups = f model.tabGroups }


mapTabs : (Tabs -> Tabs) -> Model -> Model
mapTabs f model =
    { model | tabs = f model.tabs }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSavedModel Nothing ->
            ( model, getTabs () )

        GotSavedModel (Just newEncodedSavedModel) ->
            let
                modelDecodeResult =
                    Decode.decodeValue modelDecoder newEncodedSavedModel
            in
            case modelDecodeResult of
                Err error ->
                    ( { model | error = Just (Decode.errorToString error) }, getTabs () )

                Ok newSavedModel ->
                    if Dict.size newSavedModel.tabGroups == 0 then
                        ( model, getTabs () )

                    else
                        ( newSavedModel, Cmd.none )

        GotTabGroup tabGroup ->
            ( mapTabGroups (insertTabGroup tabGroup) model, Cmd.none )

        GotTabs incomingTabInfo ->
            ( model, Cmd.none )

        {--
            let
                newTabs =
                    tabsFromList incomingTabInfo

                newTabGroups =
                    tabsGroupsFromIncomingTabInfo incomingTabInfo

                newTabGroup =
                    activeTabGroup model
                        |> (\tabGroup -> { tabGroup | tabs = Dict.keys newTabs })

                calculatedTabs =
                    newTabs
                        |> fillTabDimensions newTabGroup
            in
            model
                |> mapTabs (always newTabs)
                |> mapTabGroups (insertTabGroup newTabGroup)
                |> (\newModel -> ( newModel, saveModel <| modelEncode newModel ))
--}
        GotTabScreenshot tabscreenshot ->
            model
                |> mapTabs (updateTabAt tabscreenshot.id (mapScreenshot tabscreenshot.img))
                |> (\newModel -> ( newModel, saveModel <| modelEncode newModel ))

        DeleteTabGroup id ->
            model
                |> mapTabGroups (removeTabGroup id)
                |> (\newModel -> ( newModel, saveModel <| modelEncode newModel ))

        AddTabGroup ->
            model
                |> mapTabGroups (insertTabGroup blankTabGroup)
                |> (\newModel -> ( newModel, saveModel <| modelEncode newModel ))

        ChangeGroupTitle id newTitle ->
            model
                |> mapTabGroups (updateTabGroupAt id (mapTitle newTitle))
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )

        FinishGroupTitleEdit id ->
            model
                |> mapTabGroups (updateTabGroupAt id (mapChangingTitle False))
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )

        StartGroupTitleEdit id ->
            model
                |> mapTabGroups (updateTabGroupAt id (mapChangingTitle True))
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )

        TabGroupResizeMsg ( groupID, resizeMsg ) ->
            model
                |> mapTabGroups (updateTabGroupAt groupID (resizeTabGroup groupID resizeMsg))
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )

        TabGroupDragMsg ( groupId, dragMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            Dict.map (dragTabGroup groupId dragMsg) model.tabGroups
                    }
            in
            ( newModel, saveModel <| modelEncode newModel )

        TabDragMsg ( tabID, DragEnd _ ) ->
            model
                |> mapTabDrag Nothing
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )

        TabDragMsg ( tabID, dragMsg ) ->
            model
                |> mapTabs (updateTabAt tabID (dragTab dragMsg))
                |> mapTabDrag (Just tabID)
                |> (\newModel ->
                        ( newModel, saveModel <| modelEncode newModel )
                   )


type alias Draggable a =
    { a | drag : Maybe Drag, position : Position }


dragTab : DragMsg -> Tab -> Tab
dragTab msg tab =
    case msg of
        DragStart xy ->
            { tab | drag = Just (Drag xy xy) }

        DragAt xy ->
            { tab | drag = Maybe.map (\{ start } -> Drag start xy) tab.drag }

        DragEnd _ ->
            tab


dragTabGroup : Int -> DragMsg -> TabGroupID -> TabGroup -> TabGroup
dragTabGroup targetID msg tabGroupID tabGroup =
    if targetID /= tabGroupID then
        tabGroup

    else
        case msg of
            DragStart xy ->
                { tabGroup | drag = Just (Drag xy xy) }

            DragAt xy ->
                { tabGroup | drag = Maybe.map (\{ start } -> Drag start xy) tabGroup.drag }

            DragEnd _ ->
                { tabGroup | position = getPosition tabGroup, drag = Nothing }


resizeTabGroup : TabGroupID -> DragMsg -> TabGroup -> TabGroup
resizeTabGroup tabGroupID msg tabGroup =
    case msg of
        DragStart xy ->
            { tabGroup | resize = Just (Drag xy xy) }

        DragAt xy ->
            { tabGroup | resize = Maybe.map (\{ start } -> Drag start xy) tabGroup.resize }

        DragEnd _ ->
            { tabGroup | dimensions = getDimensions tabGroup, resize = Nothing }


getDimensions : TabGroup -> Dimensions
getDimensions { dimensions, resize } =
    case resize of
        Nothing ->
            dimensions

        Just { start, current } ->
            { height = dimensions.height + (current.y - start.y)
            , width = dimensions.width + (current.x - start.x)
            }



-- VIEW


view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            div []
                (div [] [ text err ]
                    :: addTabGroupButton
                    :: viewTabDragging (activeDraggingTab model)
                    :: (Dict.values <| Dict.map (viewTabGroup model.tabs) model.tabGroups)
                )

        Nothing ->
            div []
                (div [] [ text "hello world" ]
                    :: addTabGroupButton
                    :: viewTabDragging (activeDraggingTab model)
                    :: (Dict.values <| Dict.map (viewTabGroup model.tabs) model.tabGroups)
                )


viewTabDragging : Maybe ( TabID, Tab ) -> Html Msg
viewTabDragging maybeTab =
    case maybeTab of
        Nothing ->
            span [] []

        Just ( tabID, tab ) ->
            viewTab 100 ( tabID, tab )


addTabGroupButton : Html Msg
addTabGroupButton =
    button [ Html.Events.onClick AddTabGroup ] [ text "New Tab Group" ]


viewTabGroup : Dict TabID Tab -> TabGroupID -> TabGroup -> Html Msg
viewTabGroup tabDict tabGroupID tabGroup =
    let
        realPosition =
            getPosition tabGroup

        realDimensions =
            getDimensions tabGroup

        width =
            toFloat realDimensions.width

        height =
            toFloat realDimensions.height

        tabCount =
            toFloat (Set.size tabGroup.tabIDs)

        tabLength =
            calculateTabLength tabGroup
    in
    div
        [ dragGroupOnMouseDown tabGroupID
        , class "tabGroup"
        , style "padding" "10px"
        , style "cursor" "move"
        , style "width" <| px realDimensions.width
        , style "height" <| px realDimensions.height
        , style "border-radius" "4px"
        , style "position" "absolute"
        , style "left" <| px realPosition.x
        , style "top" <| px realPosition.y
        , style "background-color" "#F8F8F8"
        , style "color" "#B0B1BB"
        ]
        (viewTabGroupDeleteButton tabGroupID tabGroup
            :: viewTabGroupTitle tabGroupID tabGroup
            :: viewDraggableCorner tabGroupID tabGroup
            :: List.map (viewTab tabLength) (tabsInTabGroup tabDict tabGroup)
        )


newIndex : Position -> Int -> Int -> Dimensions -> Int
newIndex position width height dimensions =
    let
        itemsPerRow =
            dimensions.width // width
    in
    (position.x // width)
        + ((position.y // height) * itemsPerRow)


maxWholeSquares : Float -> Float -> Float -> Float
maxWholeSquares w h x =
    toFloat <| (floor <| w / x) * (floor <| h / x)


perfectlyEfficientSquareEdge : Float -> Float -> Float -> Float
perfectlyEfficientSquareEdge w h n =
    sqrt (w * h / n)


lessEfficientSquareEdge : Float -> Float -> Float -> Float -> Float
lessEfficientSquareEdge w h n x =
    if maxWholeSquares w h x >= n then
        x

    else
        lessEfficientSquareEdge w h n (x - 1)


bestFit : Float -> Float -> Float -> Float
bestFit w h n =
    lessEfficientSquareEdge w h n (perfectlyEfficientSquareEdge w h n)


viewDraggableCorner : TabGroupID -> TabGroup -> Html Msg
viewDraggableCorner tabGroupID tabGroup =
    div
        [ resizeOnMouseDown tabGroupID
        , style "background-color" "darkblue"
        , style "height" "20px"
        , style "width" "20px"
        , style "position" "absolute"
        , style "bottom" "0px"
        , style "right" "0px"
        , style "cursor" "nwse-resize"
        ]
        []


viewTabGroupTitle : TabGroupID -> TabGroup -> Html Msg
viewTabGroupTitle tabGroupID tabGroup =
    if tabGroup.changingTitle then
        div []
            [ input
                [ onInput (ChangeGroupTitle tabGroupID)

                -- , onEnter (FinishGroupTitleEdit tabGroup.id)
                , onBlur (FinishGroupTitleEdit tabGroupID)
                , value tabGroup.title
                ]
                [ text tabGroup.title ]
            ]

    else
        div
            [ Html.Events.onClick (StartGroupTitleEdit tabGroupID)
            , style "cursor" "pointer"
            ]
            [ text tabGroup.title ]


viewTabGroupDeleteButton : TabGroupID -> TabGroup -> Html Msg
viewTabGroupDeleteButton tabGroupID tabGroup =
    div
        [ Html.Events.onClick (DeleteTabGroup tabGroupID)
        , style "float" "right"
        , style "cursor" "pointer"
        ]
        [ text "X" ]


px : Int -> String
px number =
    String.fromInt number ++ "px"


getPosition : Draggable a -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


dragGroupOnMouseDown : TabGroupID -> Attribute Msg
dragGroupOnMouseDown tabGroupID =
    on "mousedown"
        (Decode.map
            (\pos -> TabGroupDragMsg ( tabGroupID, DragStart pos ))
            mousePositionDecoder
        )


fillTabDimensions : TabGroup -> Tabs -> Tabs
fillTabDimensions tabGroup tabs =
    let
        length =
            calculateTabLength tabGroup
    in
    Dict.map (\k v -> { v | height = length, width = length }) tabs


calculateTabLength : TabGroup -> Int
calculateTabLength tabGroup =
    let
        realDimensions =
            getDimensions tabGroup

        width =
            toFloat realDimensions.width

        height =
            toFloat realDimensions.height

        tabCount =
            toFloat (Set.size tabGroup.tabIDs)
    in
    floor <| bestFit width height tabCount


dragTabOnMouseDown : TabID -> Attribute Msg
dragTabOnMouseDown tabID =
    stopPropagationOn "mousedown"
        (Decode.map
            (\pos -> ( TabDragMsg ( tabID, DragStart pos ), True ))
            mousePositionDecoder
        )


resizeOnMouseDown : TabGroupID -> Attribute Msg
resizeOnMouseDown tabGroupID =
    custom "mousedown"
        (Decode.map
            (\pos ->
                { message = TabGroupResizeMsg ( tabGroupID, DragStart pos )
                , stopPropagation = True
                , preventDefault = True
                }
            )
            mousePositionDecoder
        )


mousePositionDecoder : Decode.Decoder Position
mousePositionDecoder =
    Decode.map2 Position
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


viewTab : Int -> ( TabID, Tab ) -> Html Msg
viewTab length ( tabID, tab ) =
    let
        border =
            1

        margin =
            10

        size =
            length - border - (margin * 2) - 1

        positioning =
            case tab.drag of
                Nothing ->
                    [ style "float" "left" ]

                Just position ->
                    [ style "position" "absolute"
                    , style "top" (px position.current.y)
                    , style "left" (px position.current.x)
                    ]

        screenshot =
            case tab.screenshot of
                Nothing ->
                    div
                        [ style "width" "100%"
                        , style "height" "100%"
                        , style "background-color" "white"
                        ]
                        []

                Just screenshotSrc ->
                    img
                        [ src screenshotSrc
                        , style "width" "100%"
                        , style "height" "100%"
                        ]
                        []
    in
    div
        ([ dragTabOnMouseDown tabID
         , style "width" (px size)
         , style "height" (px size)
         , style "margin" (px margin)
         , style "border" (px border ++ " solid black")
         ]
            ++ positioning
        )
        [ screenshot
        , div [] [ text (ellipsis 20 tab.title) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubs =
            Sub.batch <| Dict.values <| Dict.map tabGroupDragSub model.tabGroups

        resizeSubs =
            Sub.batch <| Dict.values <| Dict.map tabGroupResizeSub model.tabGroups
    in
    Sub.batch
        [ savedModel GotSavedModel

        -- tabs is called when a tab is added, removed, or we queried
        -- for all the tabs from elm
        , updatedTabList GotTabs
        , tabScreenshot GotTabScreenshot
        , dragSubs
        , resizeSubs
        , tabDragSub model
        ]


tabDragSub : Model -> Sub Msg
tabDragSub model =
    case activeDraggingTab model of
        Nothing ->
            Sub.none

        Just ( tabID, tab ) ->
            Sub.batch
                [ Sub.map (\msg -> TabDragMsg ( tabID, msg )) (mouseMoves DragAt)
                , Sub.map (\msg -> TabDragMsg ( tabID, msg )) (mouseUps DragEnd)
                ]


mouseMoves : (Position -> msg) -> Sub msg
mouseMoves tagger =
    Browser.Events.onMouseMove (Decode.map tagger mousePositionDecoder)


mouseUps : (Position -> msg) -> Sub msg
mouseUps tagger =
    Browser.Events.onMouseUp (Decode.map tagger mousePositionDecoder)


tabGroupDragSub : TabGroupID -> TabGroup -> Sub Msg
tabGroupDragSub tabGroupID tabGroup =
    case tabGroup.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> TabGroupDragMsg ( tabGroupID, msg )) (mouseMoves DragAt)
                , Sub.map (\msg -> TabGroupDragMsg ( tabGroupID, msg )) (mouseUps DragEnd)
                ]


tabGroupResizeSub : TabGroupID -> TabGroup -> Sub Msg
tabGroupResizeSub tabGroupID tabGroup =
    case tabGroup.resize of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> TabGroupResizeMsg ( tabGroupID, msg )) (mouseMoves DragAt)
                , Sub.map (\msg -> TabGroupResizeMsg ( tabGroupID, msg )) (mouseUps DragEnd)
                ]



-- PORTS
{- TAB PORTS -}


port getTabs : () -> Cmd msg


{-| tabs returns a list of opened tabs
-}
port updatedTabList : (List IncomingTabInfo -> msg) -> Sub msg


port tabScreenshot : (TabScreenshot -> msg) -> Sub msg



{- LOCAL STORAGE PORTS -}


port saveModel : Encode.Value -> Cmd msg


port getModel : () -> Cmd msg


port savedModel : (Maybe Decode.Value -> msg) -> Sub msg
