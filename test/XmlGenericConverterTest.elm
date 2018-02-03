module XmlGenericConverterTest exposing (..)

import Test exposing (..)
import Expect
import XmlGenericConverter as C


testPreparation : Test
testPreparation =
    describe "preparation of input tests"
        [ describe "tokenize: from (k,v) to ([tokens], v)"
            [ test "tokenize 1: empty" <|
                \_ -> testFromKvToTokensV ( "", "" ) ( [], "" )
            , test "tokenize 2: single value" <|
                \_ -> testFromKvToTokensV ( "", "a" ) ( [], "a" )
            , test "tokenize 3: single token" <|
                \_ -> testFromKvToTokensV ( "a", "" ) ( [ "a" ], "" )
            , test "tokenize 4: two tokens" <|
                \_ -> testFromKvToTokensV ( "a_b", "" ) ( [ "a", "b" ], "" )
            , test "tokenize 5: ignore empty tokens 1" <|
                \_ -> testFromKvToTokensV ( "_", "" ) ( [], "" )
            , test "tokenize 5: ignore empty tokens 2" <|
                \_ -> testFromKvToTokensV ( "_a___", "" ) ( [ "a" ], "" )
            ]
        , describe "removeNumericTokens"
            [ test "removeNumericTokens 1: empty" <|
                \_ -> testRemoveNumericTokens ( [], "" ) ( [], "" )
            , test "removeNumericTokens 2: a numberic token" <|
                \_ -> testRemoveNumericTokens ( [ "1" ], "" ) ( [], "" )
            , test "removeNumericTokens 3: value" <|
                \_ -> testRemoveNumericTokens ( [ "a" ], "a" ) ( [ "a" ], "a" )
            , test "removeNumericTokens 4: mixed values" <|
                \_ ->
                    testRemoveNumericTokens
                        ( [ "a", "1", "2", "b" ], "a" )
                        ( [ "a", "b" ], "a" )
            ]
        , todo "getPath: (['a', 'b', 'c'], 'v') -> (['a', 'b'], (Just 'c', 'v'))"
        ]


testFromKvToTokensV : C.KeyValue -> C.TokensValue -> Expect.Expectation
testFromKvToTokensV input expected =
    input
        |> C.tokenize
        |> Expect.equal expected


testRemoveNumericTokens : C.TokensValue -> C.TokensValue -> Expect.Expectation
testRemoveNumericTokens input expected =
    input
        |> C.removeNumericTokens
        |> Expect.equal expected
