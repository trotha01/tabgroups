port module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
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
    { tabGroups : List TabGroup
    , tabDrag : Maybe Tab
    }


type alias TabGroup =
    { id : Int
    , title : GroupTitle
    , tabs : List Tab
    , position : Position
    , drag : Maybe Drag
    , changingTitle : Bool
    , dimensions : Dimensions
    , resize : Maybe Drag
    }


type alias GroupTitle =
    String


type alias Dimensions =
    { height : Int
    , width : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


initTabGroup : Int -> String -> List Tab -> TabGroup
initTabGroup id title tabs =
    { id = id
    , title = title
    , tabs = tabs
    , position = Position 10 10
    , drag = Nothing
    , changingTitle = False
    , dimensions = { width = 400, height = 300 }
    , resize = Nothing
    }


blankTabGroup : Int -> TabGroup
blankTabGroup id =
    initTabGroup id "Add Title Here" []


type alias Tab =
    { id : Int
    , title : String
    , url : String
    , screenshot : Maybe String
    , drag : Maybe Drag
    }


type alias TabScreenshot =
    { id : Int
    , img : Maybe String
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    --getModel () )
    ( { tabGroups = [], tabDrag = Nothing }, Cmd.none )


sampleTabs : List TabGroup
sampleTabs =
    [ initTabGroup 1
        "Group 1"
        [ { id = 1
          , title = "tab1"
          , url = "www.tab1.com"
          , screenshot = Nothing
          , drag = Nothing
          }
        , { id = 2
          , title = "tab2"
          , url = "www.tab2.com"
          , screenshot = Nothing
          , drag = Nothing
          }
        ]
    ]



-- UPDATE


type Msg
    = GotTabs (List Tab)
    | GotTabGroup TabGroup
    | GotTabScreenshot TabScreenshot
    | GotSavedModel (Maybe Model)
    | StartGroupTitleEdit Int
    | FinishGroupTitleEdit Int
    | ChangeGroupTitle Int String
    | DeleteTabGroup Int
    | AddTabGroup
    | TabDragMsg ( Tab, DragMsg )
    | TabGroupDragMsg ( Int, DragMsg )
    | TabGroupResizeMsg ( Int, DragMsg )


type DragMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSavedModel Nothing ->
            -- ( model, getTabs () )
            ( model, Cmd.none )

        GotSavedModel (Just newModel) ->
            ( newModel, Cmd.none )

        GotTabGroup tabGroup ->
            ( { model | tabGroups = tabGroup :: model.tabGroups }, Cmd.none )

        GotTabs tabs ->
            let
                tabGroup =
                    List.head model.tabGroups
                        |> Maybe.withDefault (initTabGroup 0 "Main" [])

                newTabGroup =
                    { tabGroup | tabs = tabs }

                newModel =
                    { model | tabGroups = [ newTabGroup ] }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        GotTabScreenshot tabscreenshot ->
            let
                tabGroup =
                    List.head model.tabGroups
                        |> Maybe.withDefault (initTabGroup 0 "Main" [])

                newTabs =
                    List.map
                        (\tab ->
                            if tab.id == tabscreenshot.id then
                                { tab | screenshot = tabscreenshot.img }

                            else
                                tab
                        )
                        tabGroup.tabs

                newTabGroup =
                    { tabGroup | tabs = newTabs }

                newModel =
                    { model | tabGroups = [ newTabGroup ] }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        DeleteTabGroup id ->
            let
                newTabGroups =
                    List.filter (\tg -> tg.id /= id) model.tabGroups

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        AddTabGroup ->
            let
                newTabGroups =
                    blankTabGroup (List.length model.tabGroups) :: model.tabGroups

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        ChangeGroupTitle id newTitle ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.id == id then
                                    { tabGroup | title = newTitle }

                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        FinishGroupTitleEdit id ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.id == id then
                                    { tabGroup | changingTitle = False }

                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        StartGroupTitleEdit id ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.id == id then
                                    { tabGroup | changingTitle = True }

                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        TabGroupResizeMsg ( groupId, resizeMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            List.map (resizeTabGroup groupId resizeMsg) model.tabGroups
                    }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        TabGroupDragMsg ( groupId, dragMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            List.map (dragTabGroup groupId dragMsg) model.tabGroups
                    }
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )

        TabDragMsg ( tab, dragMsg ) ->
            let
                newModel =
                    { model | tabDrag = dragTab tab.id dragMsg tab }

                -- TODO: fix tab final position, add to new tab group, remove from old tabgroup
            in
            -- ( newModel, saveModel newModel )
            ( newModel, Cmd.none )


type alias Draggable a =
    { a | id : Int, drag : Maybe Drag, position : Position }


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


dragTabGroup : Int -> DragMsg -> TabGroup -> TabGroup
dragTabGroup id msg tabGroup =
    if id /= tabGroup.id then
        tabGroup

    else
        case msg of
            DragStart xy ->
                { tabGroup | drag = Just (Drag xy xy) }

            DragAt xy ->
                { tabGroup | drag = Maybe.map (\{ start } -> Drag start xy) tabGroup.drag }

            DragEnd _ ->
                { tabGroup | position = getPosition tabGroup, drag = Nothing }


resizeTabGroup : Int -> DragMsg -> TabGroup -> TabGroup
resizeTabGroup id msg tabGroup =
    if id /= tabGroup.id then
        tabGroup

    else
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
    div []
        (div [] [ text "hello world" ]
            :: addTabGroupButton
            :: viewTabDragging model.tabDrag
            :: List.map viewTabGroup model.tabGroups
        )


viewTabDragging : Maybe Tab -> Html Msg
viewTabDragging maybeTab =
    case maybeTab of
        Nothing ->
            span [] []

        Just tab ->
            viewTab 20 tab


addTabGroupButton : Html Msg
addTabGroupButton =
    button [ Html.Events.onClick AddTabGroup ] [ text "New Tab Group" ]


viewTabGroup : TabGroup -> Html Msg
viewTabGroup tabGroup =
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
        [ -- dragOnMouseDown tabGroup
          class "tabGroup"
        , (\( a, b ) -> style a b) ( "padding", "10px" )
        , (\( a, b ) -> style a b) ( "cursor", "move" )
        , (\( a, b ) -> style a b) ( "width", px realDimensions.width )
        , (\( a, b ) -> style a b) ( "height", px realDimensions.height )
        , (\( a, b ) -> style a b) ( "border-radius", "4px" )
        , (\( a, b ) -> style a b) ( "position", "absolute" )
        , (\( a, b ) -> style a b) ( "left", px realPosition.x )
        , (\( a, b ) -> style a b) ( "top", px realPosition.y )
        , (\( a, b ) -> style a b) ( "background-color", "#F8F8F8" )
        , (\( a, b ) -> style a b) ( "color", "#B0B1BB" )
        ]
        (viewTabGroupDeleteButton tabGroup
            :: viewTabGroupTitle tabGroup
            :: viewDraggableCorner tabGroup
            :: List.map (viewTab tabLength) tabGroup.tabs
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


viewDraggableCorner : TabGroup -> Html Msg
viewDraggableCorner tabGroup =
    div
        [ -- resizeOnMouseDown tabGroup
          (\( a, b ) -> style a b) ( "background-color", "darkblue" )
        , (\( a, b ) -> style a b) ( "height", "20px" )
        , (\( a, b ) -> style a b) ( "width", "20px" )
        , (\( a, b ) -> style a b) ( "position", "absolute" )
        , (\( a, b ) -> style a b) ( "bottom", "0px" )
        , (\( a, b ) -> style a b) ( "right", "0px" )
        , (\( a, b ) -> style a b) ( "cursor", "nwse-resize" )
        ]
        []


viewTabGroupTitle : TabGroup -> Html Msg
viewTabGroupTitle tabGroup =
    if tabGroup.changingTitle then
        div []
            [ input
                [ onInput (ChangeGroupTitle tabGroup.id)

                -- , onEnter (FinishGroupTitleEdit tabGroup.id)
                , onBlur (FinishGroupTitleEdit tabGroup.id)
                , value tabGroup.title
                ]
                [ text tabGroup.title ]
            ]

    else
        div
            [ Html.Events.onClick (StartGroupTitleEdit tabGroup.id)
            , style "cursor" "pointer"
            ]
            [ text tabGroup.title ]


viewTabGroupDeleteButton : TabGroup -> Html Msg
viewTabGroupDeleteButton tabGroup =
    div
        [ Html.Events.onClick (DeleteTabGroup tabGroup.id)
        , (\( a, b ) -> style a b) ( "float", "right" )
        , (\( a, b ) -> style a b) ( "cursor", "pointer" )
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



{--
dragOnMouseDown : TabGroup -> Attribute Msg
dragOnMouseDown tabGroup =
    on "mousedown" (Decode.map (\pos -> TabGroupDragMsg ( tabGroup.id, DragStart pos )) Mouse.position)


resizeOnMouseDown : TabGroup -> Attribute Msg
resizeOnMouseDown tabGroup =
    onWithOptions "mousedown" stopPropagation (Decode.map (\pos -> TabGroupResizeMsg ( tabGroup.id, DragStart pos )) Mouse.position)


stopPropagation : Options
stopPropagation =
    { stopPropagation = True
    , preventDefault = True
    }
    --}


viewTab : Int -> Tab -> Html Msg
viewTab length tab =
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
                        [ (\( a, b ) -> style a b) ( "width", "100%" )
                        , (\( a, b ) -> style a b) ( "height", "100%" )
                        , (\( a, b ) -> style a b) ( "background-color", "white" )
                        ]
                        []

                Just screenshotSrc ->
                    img
                        [ src screenshotSrc
                        , (\( a, b ) -> style a b) ( "width", "100%" )
                        , (\( a, b ) -> style a b) ( "height", "100%" )
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
            Sub.batch <| List.map tabGroupDragSub model.tabGroups

        resizeSubs =
            Sub.batch <| List.map tabGroupResizeSub model.tabGroups
    in
    Sub.batch
        -- savedModel GotSavedModel
        -- tabGroup GotTabGroup
        -- :: tabs GotTabs
        -- :: tabScreenshot GotTabScreenshot
        (dragSubs
            :: resizeSubs
            :: []
        )


tabDragSub : Tab -> Sub Msg
tabDragSub tab =
    case tab.drag of
        Nothing ->
            Sub.none

        Just _ ->
            {--
            Sub.batch []
                [ Sub.map (\msg -> TabDragMsg ( tab, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> TabDragMsg ( tab, msg )) (Mouse.ups DragEnd)
                ]
                --}
            Sub.none


tabGroupDragSub : TabGroup -> Sub Msg
tabGroupDragSub tabGroup =
    case tabGroup.drag of
        Nothing ->
            Sub.none

        Just _ ->
            {--
            Sub.batch
                [ Sub.map (\msg -> TabGroupDragMsg ( tabGroup.id, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> TabGroupDragMsg ( tabGroup.id, msg )) (Mouse.ups DragEnd)
                ]
                --}
            Sub.none


tabGroupResizeSub : TabGroup -> Sub Msg
tabGroupResizeSub tabGroup =
    case tabGroup.resize of
        Nothing ->
            Sub.none

        Just _ ->
            {--
            Sub.batch
                [ Sub.map (\msg -> TabGroupResizeMsg ( tabGroup.id, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> TabGroupResizeMsg ( tabGroup.id, msg )) (Mouse.ups DragEnd)
                ]
                --}
            Sub.none



-- PORTS
{- TAB PORTS
   port getTabs : () -> Cmd msg

   port tabs : (List Tab -> msg) -> Sub msg

   port tabScreenshot : (TabScreenshot -> msg) -> Sub msg


-}
{- LOCAL STORAGE PORTS
   port saveModel : Model -> Cmd msg

   port getModel : () -> Cmd msg

   port savedModel : (Maybe Model -> msg) -> Sub msg

   port tabGroup : (TabGroup -> msg) -> Sub msg


-}
