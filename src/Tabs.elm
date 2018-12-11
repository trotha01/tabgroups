module Tabs exposing (Tab, TabID, Tabs, tabDecoder, tabEncode)

import Dict exposing (Dict)


type alias Tabs =
    Dict TabID Tab


type alias TabID =
    Int


type alias Tab =
    { title : String
    , url : String
    , screenshot : Maybe String
    , height : Int
    , width : Int
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Position =
    { x : Int
    , y : Int
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
