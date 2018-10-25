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
import String.Extra exposing (ellipsis)



{-
   TODO:
   - fix ability to change model without clearing local storage
   - click and drag tabs within tab group
   - click and drag tabs to new tab groups
   - catch tab rearrangement
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
    , activeTabGroupID : TabGroupID
    , tabDrag : Maybe TabID
    , error : Maybe String
    }


tabsInTabGroup : Dict TabID Tab -> TabGroup -> List ( TabID, Tab )
tabsInTabGroup tabDict tabgroup =
    List.filterMap
        (\tabID ->
            Dict.get tabID tabDict
                |> Maybe.map (\tab -> ( tabID, tab ))
        )
        tabgroup.tabs


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
        |> Maybe.withDefault (initTabGroup "Main" [])


tabsFromList : List Tab -> Dict TabID Tab
tabsFromList newTabs =
    newTabs
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


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
        , ( "activeTabGroupID", Encode.int model.activeTabGroupID )
        , ( "tabDrag", maybeEncode model.tabDrag Encode.int )
        , ( "error", maybeEncode model.error Encode.string )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.map5 Model
        (Decode.field "tabGroups" <| Decode.dict2 Decode.int tabGroupDecoder)
        (Decode.field "tabs" <| Decode.dict2 Decode.int tabDecoder)
        (Decode.field "activeTabGroupID" <| Decode.int)
        (Decode.field "tabDrag" <| Decode.maybe <| Decode.int)
        (Decode.field "error" <| Decode.maybe <| Decode.string)


type alias TabGroup =
    { title : GroupTitle
    , tabs : List TabID
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
        , ( "tabs", Encode.list Encode.int tabGroup.tabs )
        , ( "position", positionEncode tabGroup.position )
        , ( "drag", maybeEncode tabGroup.drag dragEncode )
        , ( "changingTitle", Encode.bool tabGroup.changingTitle )
        , ( "dimensions", dimensionsEncode tabGroup.dimensions )
        , ( "resize", maybeEncode tabGroup.resize dragEncode )
        ]


tabGroupDecoder : Decoder TabGroup
tabGroupDecoder =
    Decode.map7 TabGroup
        (Decode.field "title" Decode.string)
        (Decode.field "tabs" <| Decode.list Decode.int)
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


initTabGroup : GroupTitle -> List TabID -> TabGroup
initTabGroup title initialTabs =
    { title = title
    , tabs = initialTabs
    , position = Position 10 10
    , drag = Nothing
    , changingTitle = False
    , dimensions = { width = 400, height = 300 }
    , resize = Nothing
    }


blankTabGroup : TabGroup
blankTabGroup =
    initTabGroup "Add Title Here" []


type alias Tab =
    { title : String
    , url : String
    , screenshot : Maybe String
    , drag : Maybe Drag
    }


tabEncode : Tab -> Encode.Value
tabEncode tab =
    Encode.object
        [ ( "title", Encode.string tab.title )
        , ( "url", Encode.string tab.url )
        , ( "screenshot", maybeEncode tab.screenshot Encode.string )
        , ( "drag", maybeEncode tab.drag dragEncode )
        ]


tabDecoder : Decoder Tab
tabDecoder =
    Decode.map4 Tab
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "screenshot" <| Decode.nullable <| Decode.string)
        (Decode.field "drag" <| Decode.nullable <| dragDecoder)


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
      , activeTabGroupID = 0
      , tabDrag = Nothing
      , error = Nothing
      }
    , getModel ()
    )



-- UPDATE


type Msg
    = GotTabs (List Tab)
    | GotTabGroup TabGroup
    | GotTabScreenshot TabScreenshot
    | GotSavedModel (Maybe Decode.Value)
    | StartGroupTitleEdit TabGroupID
    | FinishGroupTitleEdit TabGroupID
    | ChangeGroupTitle TabGroupID String
    | DeleteTabGroup TabGroupID
    | AddTabGroup
      -- | TabDragMsg ( Tab, DragMsg )
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

        GotTabs gotTabs ->
            let
                newTabs =
                    tabsFromList gotTabs

                newTabGroup =
                    activeTabGroup model
                        |> (\tabGroup -> { tabGroup | tabs = Dict.keys newTabs })
            in
            model
                |> mapTabs (always newTabs)
                |> mapTabGroups (insertTabGroup newTabGroup)
                |> (\newModel -> ( newModel, saveModel <| modelEncode newModel ))

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



{--
        TabDragMsg ( tab, dragMsg ) ->
            let
                newModel =
                    { model | tabDrag = dragTab tab.id dragMsg tab }

                -- TODO: fix tab final position, add to new tab group, remove from old tabgroup
            in
            ( newModel, saveModel <| modelEncode newModel )
            --}


type alias Draggable a =
    { a | drag : Maybe Drag, position : Position }



{--
dragTab : Int -> DragMsg -> Tab -> Maybe Tab
dragTab id msg item =
    if id /= item.id then
        Just item

    else
        case msg of
            DragStart xy ->
                Just { item | drag = Just (Drag xy xy) }

            DragAt xy ->
                Just { item | drag = Maybe.map (\{ start } -> Drag start xy) item.drag }

            DragEnd _ ->
                Nothing
                --}


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
                    -- :: viewTabDragging model.tabDrag
                    :: (Dict.values <| Dict.map (viewTabGroup model.tabs) model.tabGroups)
                )

        Nothing ->
            div []
                (div [] [ text "hello world" ]
                    :: addTabGroupButton
                    -- :: viewTabDragging model.tabDrag
                    :: (Dict.values <| Dict.map (viewTabGroup model.tabs) model.tabGroups)
                )



{--
viewTabDragging : Maybe Tab -> Html Msg
viewTabDragging maybeTab =
    case maybeTab of
        Nothing ->
            span [] []

        Just tab ->
            viewTab 20 tab
            --}


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
            toFloat (List.length tabGroup.tabs)

        tabLength =
            floor <| bestFit width height tabCount
    in
    div
        [ dragOnMouseDown tabGroupID
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


dragOnMouseDown : TabGroupID -> Attribute Msg
dragOnMouseDown tabGroupID =
    on "mousedown"
        (Decode.map
            (\pos -> TabGroupDragMsg ( tabGroupID, DragStart pos ))
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
        [ style "float" "left"
        , style "width" (px size)
        , style "height" (px size)
        , style "margin" (px margin)
        , style "border" (px border ++ " solid black")
        ]
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
        ]



{--
tabDragSub : Tab -> Sub Msg
tabDragSub tab =
    case tab.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> TabDragMsg ( tab, msg )) (mouseMoves DragAt)
                , Sub.map (\msg -> TabDragMsg ( tab, msg )) (mouseUps DragEnd)
                ]
                --}


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
port updatedTabList : (List Tab -> msg) -> Sub msg


port tabScreenshot : (TabScreenshot -> msg) -> Sub msg



{- LOCAL STORAGE PORTS -}


port saveModel : Encode.Value -> Cmd msg


port getModel : () -> Cmd msg


port savedModel : (Maybe Decode.Value -> msg) -> Sub msg
