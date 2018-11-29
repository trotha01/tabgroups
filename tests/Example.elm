module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Tabs module"
        [ describe "Tab encoding and decoding"
            -- Nest as many descriptions as you like.
            [ fuzz tabFuzzer "encode-decode-encode" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)
            ]
        ]


tabFuzzer : Fuzzer Tab
tabFuzzer =
    Fuzzer.map5 Tab
        -- "title"
        Fuzzer.string
        -- "url"
        Fuzzer.string
        -- "screenshot"
        (Fuzzer.maybe Fizzer.string)
        --  "height"
        Fuzzer.int
        -- width
        Fuzzer.int
        -- "drag"
        (Fuzzer.maybe dragFuzzer)


dragFuzzer : Fuzzer Drag
dragFuzzer =
    Fuzzer.map2 Drag
        positionFuzzer
        positionFuzzer


positionFuzzer : Fuzzer Position
positionFuzzer =
    Fuzzer.map2 Position
        Fuzzer.int
        Fuzzer.int
