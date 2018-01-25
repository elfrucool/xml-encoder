module XmlNodeTest exposing (xml)

import Test exposing (Test, describe, test, fuzz)
import Fuzz as F
import Expect
import XmlNode as X


xml : Test
xml =
    describe "I can create nodes"
        [ fuzz F.string "I can create text nodes" <|
            \text -> testTextNode text
        , describe "I can create elements"
            [ test "I can create empty element" <|
                \_ -> testEmptyElement
            , test "I can create elements with name only" <|
                \_ -> testNameOnlyElements
            , test "I can create elements with attributes" <|
                \_ -> testElementsWithAttributes
            , test "Attribute values are url encoded" <|
                \_ -> testElementsWithAttributesUrlEncoded
            , fuzz F.string "I can create elements with single text child" <|
                \text -> testElementsWithTextChild text
            , test "I can create elements with single element child" <|
                \_ -> testElementsWithElementChild
            , test "I can create elements with multiple children" <|
                \_ -> testElementsWithMultipleChildren
            , test "I can create elements with children with children" <|
                \_ -> testNestedChildrenElements
            ]
        , describe "I can get new elements modifying old elements"
            [ test "I can set a name to an element" <|
                \_ -> testUpdateElementName
            ]
        ]


testTextNode : String -> Expect.Expectation
testTextNode text =
    text
        |> X.text
        |> X.toString 0
        |> Expect.equal text


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


testElementsWithAttributesUrlEncoded : Expect.Expectation
testElementsWithAttributesUrlEncoded =
    X.element "name" [ ( "attr1", "value\"" ) ] []
        |> X.toString 0
        |> Expect.equal "<name attr1=\"value%22\"/>"


testElementsWithTextChild : String -> Expect.Expectation
testElementsWithTextChild text =
    X.element "name" [] [ X.text text ]
        |> X.toString 0
        |> Expect.equal ("<name>" ++ text ++ "</name>")


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


testUpdateElementName : Expect.Expectation
testUpdateElementName =
    X.empty
        |> X.setName "Name"
        |> X.toString 0
        |> Expect.equal "<Name/>"
