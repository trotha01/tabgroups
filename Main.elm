port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Json.Decode as Decode
import Mouse exposing (Position)
import String.Extra exposing (ellipsis)


{-
   TODO:
   - fix ability to change model without clearing local storage
   - click and drag tabs within tab group
   - click and drag tabs to new tab groups
   - catch tab rearrangement
-}


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tabGroups : List TabGroup }


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
    , position : Position
    , drag : Maybe Drag
    }


type alias TabScreenshot =
    { id : Int
    , img : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { tabGroups = [] }, getModel () )


sampleTabs : List TabGroup
sampleTabs =
    [ initTabGroup 1
        "Group 1"
        [ { id = 1
          , title = "tab1"
          , url = "www.tab1.com"
          , screenshot = Nothing
          , position = Position 10 10
          , drag = Nothing
          }
        , { id = 2
          , title = "tab2"
          , url = "www.tab2.com"
          , screenshot = Nothing
          , position = Position 10 10
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
    | DragMsg ( Int, DragMsg )
    | ResizeMsg ( Int, DragMsg )


type DragMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSavedModel Nothing ->
            ( model, getTabs () )

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
            ( newModel, saveModel newModel )

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
            ( newModel, saveModel newModel )

        DeleteTabGroup id ->
            let
                newTabGroups =
                    List.filter (\tg -> tg.id /= id) model.tabGroups

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            ( newModel, saveModel newModel )

        AddTabGroup ->
            let
                newTabGroups =
                    blankTabGroup (List.length model.tabGroups) :: model.tabGroups

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            ( newModel, saveModel newModel )

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
            ( newModel, saveModel newModel )

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
            ( newModel, saveModel newModel )

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
            ( newModel, saveModel newModel )

        ResizeMsg ( groupId, resizeMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            List.map (resizeTabGroup groupId resizeMsg) model.tabGroups
                    }
            in
            ( newModel, saveModel newModel )

        DragMsg ( groupId, dragMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            List.map (dragTabGroup groupId dragMsg) model.tabGroups
                    }
            in
            ( newModel, saveModel newModel )


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
    div [] <|
        addTabGroupButton
            :: List.map viewTabGroup model.tabGroups


addTabGroupButton : Html Msg
addTabGroupButton =
    button [ onClick AddTabGroup ] [ text "New Tab Group" ]


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
        [ dragOnMouseDown tabGroup
        , class "tabGroup"
        , style
            [ "padding" => "10px"
            , "cursor" => "move"
            , "width" => px realDimensions.width
            , "height" => px realDimensions.height
            , "border-radius" => "4px"
            , "position" => "absolute"
            , "left" => px realPosition.x
            , "top" => px realPosition.y
            , "background-color" => "#F8F8F8"
            , "color" => "#B0B1BB"
            ]
        ]
        (viewTabGroupDeleteButton tabGroup
            :: viewTabGroupTitle tabGroup
            :: viewDraggableCorner tabGroup
            :: List.map (viewTab tabLength) tabGroup.tabs
        )


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
        [ resizeOnMouseDown tabGroup
        , style
            [ "background-color" => "darkblue"
            , "height" => "20px"
            , "width" => "20px"
            , "position" => "absolute"
            , "bottom" => "0px"
            , "right" => "0px"
            , "cursor" => "nwse-resize"
            ]
        ]
        []


viewTabGroupTitle : TabGroup -> Html Msg
viewTabGroupTitle tabGroup =
    if tabGroup.changingTitle then
        div []
            [ input
                [ onInput (ChangeGroupTitle tabGroup.id)
                , onEnter (FinishGroupTitleEdit tabGroup.id)
                , onBlur (FinishGroupTitleEdit tabGroup.id)
                , value tabGroup.title
                ]
                [ text tabGroup.title ]
            ]
    else
        div
            [ onClick (StartGroupTitleEdit tabGroup.id)
            , style [ ( "cursor", "pointer" ) ]
            ]
            [ text tabGroup.title ]


viewTabGroupDeleteButton : TabGroup -> Html Msg
viewTabGroupDeleteButton tabGroup =
    div
        [ onClick (DeleteTabGroup tabGroup.id)
        , style
            [ "float" => "right"
            , "cursor" => "pointer"
            ]
        ]
        [ text "X" ]


(=>) =
    (,)


px : Int -> String
px number =
    toString number ++ "px"


getPosition : TabGroup -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


dragOnMouseDown : TabGroup -> Attribute Msg
dragOnMouseDown tabGroup =
    on "mousedown" (Decode.map (\pos -> DragMsg ( tabGroup.id, DragStart pos )) Mouse.position)


resizeOnMouseDown : TabGroup -> Attribute Msg
resizeOnMouseDown tabGroup =
    onWithOptions "mousedown" stopPropagation (Decode.map (\pos -> ResizeMsg ( tabGroup.id, DragStart pos )) Mouse.position)


stopPropagation : Options
stopPropagation =
    { stopPropagation = True
    , preventDefault = True
    }


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
                        [ style
                            [ "width" => "100%"
                            , "height" => "100%"
                            , "background-color" => "white"
                            ]
                        ]
                        []

                Just screenshot ->
                    img
                        [ src screenshot
                        , style
                            [ "width" => "100%"
                            , "height" => "100%"
                            ]
                        ]
                        []
    in
    div
        [ style
            [ ( "float", "left" )
            , ( "width", px size )
            , ( "height", px size )
            , ( "margin", px margin )
            , ( "border", px border ++ " solid black" )
            ]
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
        (savedModel GotSavedModel
            :: tabGroup GotTabGroup
            :: tabs GotTabs
            :: tabScreenshot GotTabScreenshot
            :: dragSubs
            :: resizeSubs
            :: []
        )


tabGroupDragSub : TabGroup -> Sub Msg
tabGroupDragSub tabGroup =
    case tabGroup.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> DragMsg ( tabGroup.id, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> DragMsg ( tabGroup.id, msg )) (Mouse.ups DragEnd)
                ]


tabGroupResizeSub : TabGroup -> Sub Msg
tabGroupResizeSub tabGroup =
    case tabGroup.resize of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> ResizeMsg ( tabGroup.id, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> ResizeMsg ( tabGroup.id, msg )) (Mouse.ups DragEnd)
                ]



-- PORTS


{-| TAB PORTS
-}
port getTabs : () -> Cmd msg


port tabs : (List Tab -> msg) -> Sub msg


port tabScreenshot : (TabScreenshot -> msg) -> Sub msg


{-| LOCAL STORAGE PORTS
-}
port saveModel : Model -> Cmd msg


port getModel : () -> Cmd msg


port savedModel : (Maybe Model -> msg) -> Sub msg


port tabGroup : (TabGroup -> msg) -> Sub msg
