module XmlNodeTest exposing (xml)

import Test exposing (..)
import Fuzz as F
import Expect
import XmlNode as X


xml : Test
xml =
    describe "I can create nodes"
        [ describe "I can create text nodes"
            [ fuzz F.string "Creating text nodes" <|
                \text -> testTextNode text <| X.escape text
            ]
        , describe "I can create elements"
            [ test "I can create empty element" <|
                \_ -> testEmptyElement
            , test "I can create elements with name only" <|
                \_ -> testNameOnlyElements
            , test "I can create elements with attributes" <|
                \_ -> testElementsWithAttributes
            , test "Attribute values are xml excaped" <|
                \_ -> testElementsWithAttributesXmlEscaped
            , fuzz F.string "I can create elements with single text child" <|
                \text -> testElementsWithTextChild text <| X.escape text
            , test "I can create elements with single element child" <|
                \_ -> testElementsWithElementChild
            , test "I can create elements with multiple children" <|
                \_ -> testElementsWithMultipleChildren
            , test "I can create elements with children with children" <|
                \_ -> testNestedChildrenElements
            ]
        , describe "I can get new elements modifying old elements"
            [ test "I can set a name to an element" <|
                \_ -> testUpdateElementSetName
            , test "I can set attributes to an element" <|
                \_ -> testUpdateElementSetAttributes
            , test "I can add attributes to an element" <|
                \_ -> testUpdateElementAddAttributes
            , test "I can set children to an element" <|
                \_ -> testUpdateElementSetChildren
            , test "I can add children to an element" <|
                \_ -> testUpdateElementAddChildren
            , test "I can replace a children at a specific index" <|
                \_ -> testUpdateElementReplaceChildAt
            ]
        , describe "I can get data from nodes"
            [ test "I can get text from text element" <|
                \_ -> testGetText (X.text "a text") (Just "a text")
            , test "extracting text from an element returns nothing" <|
                \_ -> testGetText (X.empty) Nothing
            , test "I can get the element's name" <|
                \_ -> testGetElementName (X.element "Name" [] []) (Just "Name")
            , test "extracting element name from a text returns nothing" <|
                \_ -> testGetElementName (X.text "a text") Nothing
            , test "I can get element attributes" <|
                \_ ->
                    testGetAttributes
                        (X.element "Node" [ ( "attr", "value" ) ] [])
                        [ ( "attr", "value" ) ]
            , test "extracting element attributes from a text returns an empty list" <|
                \_ -> testGetAttributes (X.text "a text") []
            , test "I can get element children" <|
                \_ ->
                    testGetChildren
                        (X.element "node" [] [ X.text "child text" ])
                        [ X.text "child text" ]
            , test "extracting element children from a text returns an empty list" <|
                \_ -> testGetChildren (X.text "a text") []
            ]
        ]


testTextNode : String -> String -> Expect.Expectation
testTextNode text expected =
    text
        |> X.text
        |> X.toString 0
        |> Expect.equal expected


testEmptyElement : Expect.Expectation
testEmptyElement =
    X.empty
        |> X.toString 0
        |> Expect.equal "</>"


testNameOnlyElements : Expect.Expectation
testNameOnlyElements =
    X.element "name" [] []
        |> X.toString 0
        |> Expect.equal "<name/>"


testElementsWithAttributes : Expect.Expectation
testElementsWithAttributes =
    X.element "name" [ ( "attr1", "val1" ), ( "attr2", "val2" ) ] []
        |> X.toString 0
        |> Expect.equal "<name attr1=\"val1\" attr2=\"val2\"/>"


testElementsWithAttributesXmlEscaped : Expect.Expectation
testElementsWithAttributesXmlEscaped =
    X.element "name" [ ( "attr1", "value\"" ) ] []
        |> X.toString 0
        |> Expect.equal "<name attr1=\"value&quot;\"/>"


testElementsWithTextChild : String -> String -> Expect.Expectation
testElementsWithTextChild text expected =
    X.element "name" [] [ X.text text ]
        |> X.toString 0
        |> Expect.equal ("<name>" ++ expected ++ "</name>")


testElementsWithElementChild : Expect.Expectation
testElementsWithElementChild =
    X.element "Parent" [] [ X.element "Child" [] [] ]
        |> X.toString 0
        |> Expect.equal testElementsWithTextChild_expected


testElementsWithTextChild_expected : String
testElementsWithTextChild_expected =
    """
<Parent>
  <Child/>
</Parent>
  """ |> String.trim


testElementsWithMultipleChildren : Expect.Expectation
testElementsWithMultipleChildren =
    Expect.equal
        testElementsWithMultipleChildren_expected
    <|
        X.toString 0 <|
            X.element "Parent"
                []
                [ X.element "Child-1" [] []
                , X.text "hello"
                , X.element "Child-2" [] []
                ]


testElementsWithMultipleChildren_expected : String
testElementsWithMultipleChildren_expected =
    """
<Parent>
  <Child-1/>
  hello
  <Child-2/>
</Parent>
    """ |> String.trim


testNestedChildrenElements : Expect.Expectation
testNestedChildrenElements =
    Expect.equal testNestedChildrenElements_expected <|
        X.toString 0 <|
            X.element "Node"
                []
                [ X.element "Child"
                    []
                    [ X.element "GrandChild" [] [] ]
                , X.element "Child2"
                    [ ( "attr1", "value1" )
                    , ( "attr2", "value2" )
                    ]
                    [ X.text "Text" ]
                , X.element "Child3"
                    []
                    [ X.text "text here"
                    , X.element "GrandChild2" [] [ X.text "a text" ]
                    ]
                ]


testNestedChildrenElements_expected : String
testNestedChildrenElements_expected =
    """
<Node>
  <Child>
    <GrandChild/>
  </Child>
  <Child2 attr1="value1" attr2="value2">Text</Child2>
  <Child3>
    text here
    <GrandChild2>a text</GrandChild2>
  </Child3>
</Node>
      """ |> String.trim


testUpdateElementSetName : Expect.Expectation
testUpdateElementSetName =
    X.empty
        |> X.setName "Name"
        |> X.toString 0
        |> Expect.equal "<Name/>"


testUpdateElementSetAttributes : Expect.Expectation
testUpdateElementSetAttributes =
    X.empty
        |> X.setAttributes [ ( "attr", "value" ) ]
        |> X.toString 0
        |> Expect.equal "< attr=\"value\"/>"


testUpdateElementAddAttributes : Expect.Expectation
testUpdateElementAddAttributes =
    X.element "Node" [ ( "attr1", "value1" ) ] []
        |> X.addAttributes [ ( "attr2", "value2" ) ]
        |> X.toString 0
        |> Expect.equal "<Node attr1=\"value1\" attr2=\"value2\"/>"


testUpdateElementSetChildren : Expect.Expectation
testUpdateElementSetChildren =
    X.element "Node" [] []
        |> X.setChildren [ X.element "Foo" [] [] ]
        |> X.toString 0
        |> Expect.equal "<Node>\n  <Foo/>\n</Node>"


testUpdateElementAddChildren : Expect.Expectation
testUpdateElementAddChildren =
    X.element "Node" [] [ X.element "Foo" [] [] ]
        |> X.addChildren [ X.text "hello" ]
        |> X.toString 0
        |> Expect.equal "<Node>\n  <Foo/>\n  hello\n</Node>"


testUpdateElementReplaceChildAt : Expect.Expectation
testUpdateElementReplaceChildAt =
    let
        original =
            X.element "Node"
                []
                [ X.element "Foo" [] []
                , X.text "bar"
                , X.element "Baz" [] []
                ]

        expected =
            X.element "Node"
                []
                [ X.element "Foo" [] []
                , X.text "replaced"
                , X.element "Baz" [] []
                ]
    in
        original
            |> X.replaceChildAt 1 (X.text "replaced")
            |> Expect.equal expected


testGetText : X.XmlNode -> Maybe String -> Expect.Expectation
testGetText node expected =
    node
        |> X.getText
        |> Expect.equal expected


testGetElementName : X.XmlNode -> Maybe String -> Expect.Expectation
testGetElementName node expected =
    node
        |> X.getElementName
        |> Expect.equal expected


testGetAttributes : X.XmlNode -> List X.Attribute -> Expect.Expectation
testGetAttributes node expected =
    node
        |> X.getAttributes
        |> Expect.equal expected


testGetChildren : X.XmlNode -> List X.XmlNode -> Expect.Expectation
testGetChildren node expected =
    node
        |> X.getChildren
        |> Expect.equal expected
