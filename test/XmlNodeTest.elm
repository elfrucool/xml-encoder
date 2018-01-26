module XmlNodeTest exposing (xml)

import Test exposing (Test, describe, test, fuzz)
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
