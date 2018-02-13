module XmlGenericConverterTest exposing (..)

import Test exposing (..)
import Expect
import XmlGenericConverter as C
import XmlNode as X


tokenizeScenarios : Test
tokenizeScenarios =
    describe "tokenize: from (k,v) to ([tokens], v)"
        [ test "tokenize 1: empty" <|
            \_ -> testTokenize ( "", "" ) ( [], "" )
        , test "tokenize 2: single value" <|
            \_ -> testTokenize ( "", "a" ) ( [], "a" )
        , test "tokenize 3: single token" <|
            \_ -> testTokenize ( "a", "" ) ( [ "a" ], "" )
        , test "tokenize 4: two tokens" <|
            \_ -> testTokenize ( "a_b", "" ) ( [ "a", "b" ], "" )
        , test "tokenize 5: ignore empty tokens 1" <|
            \_ -> testTokenize ( "_", "" ) ( [], "" )
        , test "tokenize 5: ignore empty tokens 2" <|
            \_ -> testTokenize ( "_a___", "" ) ( [ "a" ], "" )
        ]


removeNumericTokensScenarios : Test
removeNumericTokensScenarios =
    describe "removeNumericTokens"
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


getPathScenarios : Test
getPathScenarios =
    describe "getPath: (['a', 'b', 'c'], 'v') -> (['a', 'b'], (Just 'c', 'v'))"
        [ test "getPath: ([], '') -> ([], (Nothing, ''))" <|
            \_ -> testGetPath ( [], "" ) ( [], ( Nothing, "" ) )
        , test "getPath: ([], 'a') -> ([], (Nothing, 'a'))" <|
            \_ -> testGetPath ( [], "a" ) ( [], ( Nothing, "a" ) )
        , test "getPath: (['x'], 'a') -> ([], (Just 'x', 'a'))" <|
            \_ -> testGetPath ( [ "x" ], "a" ) ( [], ( Just "x", "a" ) )
        , test "getPath: (['x', 'y'], 'a') -> (['x'], (Just 'y', 'a'))" <|
            \_ -> testGetPath ( [ "x", "y" ], "a" ) ( [ "x" ], ( Just "y", "a" ) )
        ]


toNodeScenarios : Test
toNodeScenarios =
    describe "toNode: (['a','b'], (Just 'c','v')) -> (['a','b'], Element '<c>v</c>')"
        [ test "toNode: ([], (Nothing, '')) -> ([], None)" <|
            \_ -> testToNode ( [], ( Nothing, "" ) ) ( [], C.None )
        , test "toNode: ([], (Just 'x', 'a')) -> ([], Element '<x>a</x>')" <|
            \_ ->
                testToNode ( [], ( Just "x", "a" ) )
                    ( [], C.Element (X.element "x" [] [ X.text "a" ]) )
        , test "toNode: (['p'], (Just 'x', 'a')) -> (['p'], Element '<x>a</x>')" <|
            \_ ->
                testToNode ( [ "p" ], ( Just "x", "a" ) )
                    ( [ "p" ], C.Element (X.element "x" [] [ X.text "a" ]) )
        , test "toNode: (['p'], (Just '@x', 'a')) -> (['p'], Attribute 'x'='a')" <|
            \_ ->
                testToNode ( [ "p" ], ( Just "@x", "a" ) )
                    ( [ "p" ], C.Attribute ( "x", "a" ) )
        ]


convertSingleItemScenarios : Test
convertSingleItemScenarios =
    describe "convertSingleItem: (key,value) -> (Path, Element '<x>value</x>')"
        [ test "convertSingleItem: None" <|
            \_ -> testConvertSingleItem ( "1_2_3", "value" ) ( [], C.None )
        , test "convertSingleItem: element" <|
            \_ ->
                testConvertSingleItem ( "A_B", "v" )
                    ( [ "A" ], makeNodeElement "B" "v" )
        , test "convertSingleItem: attribute" <|
            \_ ->
                testConvertSingleItem ( "A_B_@c", "v" )
                    ( [ "A", "B" ], C.Attribute ( "c", "v" ) )
        ]


convertScenarios : Test
convertScenarios =
    describe "preparation of input tests"
        [ describe "convert: transform key-values to reduced Node"
            [ test "convert: attempt 1: func returns the root item" <|
                \_ -> testConvert alwaysPrevious (makeEmptyPathNode "ParentOfRoot")
            , test "convert: attempt 2: func returns the last item" <|
                \_ ->
                    testConvert alwaysNext <|
                        ( [ "Root" ], makeNodeElement "Node3" "node 3 value" )
            ]
        ]


testTokenize : C.KeyValue -> C.TokensValue -> Expect.Expectation
testTokenize input expected =
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


testConvertSingleItem : C.KeyValue -> C.PathNode -> Expect.Expectation
testConvertSingleItem input expected =
    input
        |> C.convertSingleItem
        |> Expect.equal expected


testConvert : C.ReduceFunc -> C.PathNode -> Expect.Expectation
testConvert reduceFunc expected =
    makeConvertInput
        |> C.convert (makeEmptyPathNode "ParentOfRoot") reduceFunc
        |> Expect.equal expected


alwaysPrevious : C.ReduceFunc
alwaysPrevious next previous =
    previous


alwaysNext : C.ReduceFunc
alwaysNext next previous =
    next


makeEmptyPathNode : String -> C.PathNode
makeEmptyPathNode name =
    ( [], makeNodeElementWithoutValue name )


makeNodeElementWithoutValue : String -> C.Node
makeNodeElementWithoutValue name =
    makeNodeElement name ""


makeNodeElement : String -> String -> C.Node
makeNodeElement name value =
    C.Element <| X.element name [] [ X.text value ]


makeConvertInput : List C.KeyValue
makeConvertInput =
    [ ( "Root_Node1", "node 1 value" )
    , ( "Root_Node2", "node 2 value" )
    , ( "", "filter out me" )
    , ( "Root_Node2_@attr", "node 2 attr value" )
    , ( "Root_Node3_1", "node 3 value" )
    , ( "", "filter out me" )
    ]
