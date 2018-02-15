module XmlGenericConverterTest exposing (..)

import Test exposing (..)
import Expect
import XmlGenericConverter as C
import XmlNode as X


-- TEST SUITES


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


addPathScenarios : Test
addPathScenarios =
    describe "addPath: (['a', 'b', 'c'], 'v') -> (['a', 'b'], (Just 'c', 'v'))"
        [ test "addPath: ([], '') -> ([], (Nothing, ''))" <|
            \_ -> testAddPath ( [], "" ) ( [], ( Nothing, "" ) )
        , test "addPath: ([], 'a') -> ([], (Nothing, 'a'))" <|
            \_ -> testAddPath ( [], "a" ) ( [], ( Nothing, "a" ) )
        , test "addPath: (['x'], 'a') -> ([], (Just 'x', 'a'))" <|
            \_ -> testAddPath ( [ "x" ], "a" ) ( [], ( Just "x", "a" ) )
        , test "addPath: (['x', 'y'], 'a') -> (['x'], (Just 'y', 'a'))" <|
            \_ -> testAddPath ( [ "x", "y" ], "a" ) ( [ "x" ], ( Just "y", "a" ) )
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
                    ( [ "A" ], C.makeNodeValueElement "B" "v" )
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
                        ( [ "Root" ], C.makeNodeValueElement "Node3" "node 3 value" )
            ]
        ]


isElementScenarios : Test
isElementScenarios =
    describe "isElement determines whether a Node is an elemement"
        [ test "isElement None --> False" <|
            \_ -> Expect.false "None is not an element" <| C.isElement C.None
        , test "isElement Element --> True" <|
            \_ ->
                Expect.true "Element is an element" <|
                    C.isElement <|
                        C.Element X.empty
        , test "isElement Attribute --> False" <|
            \_ ->
                Expect.false "Attribute is not an element" <|
                    C.isElement <|
                        C.Attribute ( "foo", "bar" )
        ]


getNodeScenarios : Test
getNodeScenarios =
    describe "getNodeScenarios"
        [ test "getNode None" <|
            \_ ->
                ( [], C.None )
                    |> C.getNode
                    |> Expect.equal C.None
        , test "getNode Attribute" <|
            \_ ->
                ( [], C.Attribute ( "attr", "value" ) )
                    |> C.getNode
                    |> Expect.equal (C.Attribute ( "attr", "value" ))
        , test "getNode Element" <|
            \_ ->
                ( [], C.Element (X.empty) )
                    |> C.getNode
                    |> Expect.equal (C.Element (X.empty))
        ]


getXmlNodeScenarios : Test
getXmlNodeScenarios =
    describe "getXmlNode scenarios"
        [ test "getXmlNode: None -> Nothing" <|
            \_ -> testGetXmlNode C.None Nothing
        , test "getXmlNode: Attribute -> Nothing" <|
            \_ ->
                testGetXmlNode (C.Attribute ( "A", "v" )) Nothing
        , test "getXmlNode: Element -> XmlNode" <|
            \_ ->
                testGetXmlNode (C.makeNodeValueElement "A" "v") <|
                    Just (X.element "A" [] [ X.text "v" ])
        ]


getAttributeScenarios : Test
getAttributeScenarios =
    describe "getAttribute scenarios"
        [ test "getAttribute None -> Nothing" <|
            \_ -> testGetAttribute ( [ "a", "b" ], C.None ) Nothing
        , test "getAttribute Attribute -> Just ('attr', 'value')" <|
            \_ ->
                testGetAttribute ( [ "a", "b" ], C.Attribute ( "attr", "value" ) ) <|
                    Just ( "attr", "value" )
        , test "getAttribute Element -> Nothing" <|
            \_ ->
                testGetAttribute ( [ "a", "b" ], C.Element X.empty ) <|
                    Nothing
        ]



-- TEST FUNCTIONS


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


testAddPath : C.TokensValue -> C.PathFieldValue -> Expect.Expectation
testAddPath input expected =
    input
        |> C.addPath
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


testGetXmlNode : C.Node -> Maybe X.XmlNode -> Expect.Expectation
testGetXmlNode input expected =
    input
        |> C.getXmlNode
        |> Expect.equal expected


testGetAttribute : C.PathNode -> Maybe ( String, String ) -> Expect.Expectation
testGetAttribute input expected =
    input
        |> C.getAttribute
        |> Expect.equal expected



-- HELPER FUNCTIONS


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
    C.makeNodeValueElement name ""


makeConvertInput : List C.KeyValue
makeConvertInput =
    [ ( "Root_Node1", "node 1 value" )
    , ( "Root_Node2", "node 2 value" )
    , ( "", "filter out me" )
    , ( "Root_Node2_@attr", "node 2 attr value" )
    , ( "Root_Node3_1", "node 3 value" )
    , ( "", "filter out me" )
    ]
