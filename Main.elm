port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Json.Decode as Decode
import Mouse exposing (Position)
import String.Extra exposing (ellipsis)


{-
   TODO: fix ability to change model without
   clearing local storage
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
    }


type alias GroupTitle =
    String


initTabGroup : Int -> String -> List Tab -> TabGroup
initTabGroup id title tabs =
    { id = id
    , title = title
    , tabs = tabs
    , position = Position 10 10
    , drag = Nothing
    , changingTitle = False
    }


blankTabGroup : Int -> TabGroup
blankTabGroup id =
    { id = id
    , title = "Add Title Here"
    , tabs = []
    , position = Position 10 10
    , drag = Nothing
    , changingTitle = False
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Tab =
    { id : Int
    , title : String
    , url : String
    , screenshot : Maybe String
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
    [ initTabGroup 1 "Group 1" [ Tab 1 "tab1" "www.tab1.com" Nothing ]
    , initTabGroup 2 "Group 2" [ Tab 2 "tab2" "www.tab2.com" Nothing ]
    ]



-- UPDATE


type Msg
    = GotTabs (List Tab)
    | GotTabGroup TabGroup
    | GotTabScreenshot TabScreenshot
    | GotSavedModel (Maybe Model)
    | StartGroupTitleEdit String
    | FinishGroupTitleEdit String
    | ChangeGroupTitle String String
    | DeleteTabGroup String
    | AddTabGroup
    | DragMsg ( GroupTitle, DragMsg )


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
                _ =
                    Debug.log "tab" "new tabs"

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

        DeleteTabGroup title ->
            let
                newTabGroups =
                    List.filter (\tg -> tg.title /= title) model.tabGroups

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

        ChangeGroupTitle oldTitle newTitle ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.title == oldTitle then
                                    { tabGroup | title = newTitle }
                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            ( newModel, saveModel newModel )

        FinishGroupTitleEdit title ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.title == title then
                                    { tabGroup | changingTitle = False }
                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            ( newModel, saveModel newModel )

        StartGroupTitleEdit title ->
            let
                newTabGroups =
                    model.tabGroups
                        |> List.map
                            (\tabGroup ->
                                if tabGroup.title == title then
                                    { tabGroup | changingTitle = True }
                                else
                                    tabGroup
                            )

                newModel =
                    { model | tabGroups = newTabGroups }
            in
            ( newModel, saveModel newModel )

        DragMsg ( groupTitle, dragMsg ) ->
            let
                newModel =
                    { model
                        | tabGroups =
                            List.map (updateTabGroup groupTitle dragMsg) model.tabGroups
                    }
            in
            ( newModel, saveModel newModel )


updateTabGroup : GroupTitle -> DragMsg -> TabGroup -> TabGroup
updateTabGroup title msg tabGroup =
    if title /= tabGroup.title then
        tabGroup
    else
        case msg of
            DragStart xy ->
                { tabGroup | drag = Just (Drag xy xy) }

            DragAt xy ->
                { tabGroup | drag = Maybe.map (\{ start } -> Drag start xy) tabGroup.drag }

            DragEnd _ ->
                { tabGroup | position = getPosition tabGroup, drag = Nothing }



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
    in
    div
        [ onMouseDown tabGroup
        , class "tabGroup"
        , style
            [ "padding" => "10px"
            , "cursor" => "move"
            , "width" => "600px"
            , "height" => "400px"
            , "border-radius" => "4px"
            , "position" => "absolute"
            , "left" => px realPosition.x
            , "top" => px realPosition.y
            ]
        ]
        (viewTabGroupDeleteButton tabGroup
            :: viewTabGroupTitle tabGroup
            :: List.map viewTab tabGroup.tabs
        )


viewTabGroupTitle : TabGroup -> Html Msg
viewTabGroupTitle tabGroup =
    if tabGroup.changingTitle then
        div []
            [ input
                [ onInput (ChangeGroupTitle tabGroup.title)
                , onEnter (FinishGroupTitleEdit tabGroup.title)
                , onBlur (FinishGroupTitleEdit tabGroup.title)
                , value tabGroup.title
                ]
                [ text tabGroup.title ]
            ]
    else
        div
            [ onClick (StartGroupTitleEdit tabGroup.title)
            , style [ ( "cursor", "pointer" ) ]
            ]
            [ text tabGroup.title ]


viewTabGroupDeleteButton : TabGroup -> Html Msg
viewTabGroupDeleteButton tabGroup =
    div
        [ onClick (DeleteTabGroup tabGroup.title)
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


onMouseDown : TabGroup -> Attribute Msg
onMouseDown tabGroup =
    on "mousedown" (Decode.map (\pos -> DragMsg ( tabGroup.title, DragStart pos )) Mouse.position)


viewTab : Tab -> Html Msg
viewTab tab =
    let
        screenshot =
            case tab.screenshot of
                Nothing ->
                    div
                        [ style
                            [ "width" => "100px"
                            , "height" => "80px"
                            , "background-color" => "white"
                            ]
                        ]
                        []

                Just screenshot ->
                    img
                        [ src screenshot
                        , style
                            [ "width" => "100px"
                            , "height" => "80px"
                            ]
                        ]
                        []
    in
    div
        [ style
            [ ( "float", "left" )
            , ( "width", "100px" )
            , ( "height", "100px" )
            , ( "margin", "10px" )
            ]
        ]
        [ screenshot
        , div [] [ text (ellipsis 20 tab.title) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            List.map tabGroupSub model.tabGroups
    in
    Sub.batch
        (savedModel GotSavedModel
            :: tabGroup GotTabGroup
            :: tabs GotTabs
            :: tabScreenshot GotTabScreenshot
            :: subs
        )


tabGroupSub : TabGroup -> Sub Msg
tabGroupSub tabGroup =
    case tabGroup.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Sub.map (\msg -> DragMsg ( tabGroup.title, msg )) (Mouse.moves DragAt)
                , Sub.map (\msg -> DragMsg ( tabGroup.title, msg )) (Mouse.ups DragEnd)
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
