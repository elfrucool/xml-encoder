module XmlGenericConverterTest exposing (..)

import Test exposing (..)
import Expect
import XmlGenericConverter as C
import XmlNode as X


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
        , describe "getPath: (['a', 'b', 'c'], 'v') -> (['a', 'b'], (Just 'c', 'v'))"
            [ test "getPath: ([], '') -> ([], (Nothing, ''))" <|
                \_ -> testGetPath ( [], "" ) ( [], ( Nothing, "" ) )
            , test "getPath: ([], 'a') -> ([], (Nothing, 'a'))" <|
                \_ -> testGetPath ( [], "a" ) ( [], ( Nothing, "a" ) )
            , test "getPath: (['x'], 'a') -> ([], (Just 'x', 'a'))" <|
                \_ -> testGetPath ( [ "x" ], "a" ) ( [], ( Just "x", "a" ) )
            , test "getPath: (['x', 'y'], 'a') -> (['x'], (Just 'y', 'a'))" <|
                \_ -> testGetPath ( [ "x", "y" ], "a" ) ( [ "x" ], ( Just "y", "a" ) )
            ]
        , describe "toNode: (['a', 'b'], (Just 'c', 'v')) -> (['a', 'b'], Just x'<c>v</c>')"
            [ test "toNode: ([], (Nothing, '')) -> ([], None)" <|
                \_ -> testToNode ( [], ( Nothing, "" ) ) ( [], C.None )
            , test "toNode: ([], (Just 'x', 'a')) -> ([], Element '<x>a</x>')" <|
                \_ ->
                    testToNode ( [], ( Just "x", "a" ) )
                        ( [], C.Element (X.element "x" [] [ X.text "a" ]) )
            , test "toNode: (['p'], (Just 'x', 'a')) -> (['p'], Element x'<x>a</x>')" <|
                \_ ->
                    testToNode ( [ "p" ], ( Just "x", "a" ) )
                        ( [ "p" ], C.Element (X.element "x" [] [ X.text "a" ]) )
            , test "toNode: (['p'], (Just '@x', 'a')) -> (['p'], Attribute 'x'='a')" <|
                \_ ->
                    testToNode ( [ "p" ], ( Just "@x", "a" ) )
                        ( [ "p" ], C.Attribute ( "x", "a" ) )
            ]
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


testGetPath : C.TokensValue -> C.PathFieldValue -> Expect.Expectation
testGetPath input expected =
    input
        |> C.getPath
        |> Expect.equal expected


testToNode : C.PathFieldValue -> C.PathNode -> Expect.Expectation
testToNode input expected =
    input
        |> C.toNode
        |> Expect.equal expected
