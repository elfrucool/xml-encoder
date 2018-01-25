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
            ]
        ]


testTextNode : String -> Expect.Expectation
testTextNode text =
    text
        |> X.text
        |> X.toString
        |> Expect.equal text


testEmptyElement : Expect.Expectation
testEmptyElement =
    X.empty
        |> X.toString
        |> Expect.equal "</>"


testNameOnlyElements : Expect.Expectation
testNameOnlyElements =
    X.element "name" [] []
        |> X.toString
        |> Expect.equal "<name/>"


testElementsWithAttributes : Expect.Expectation
testElementsWithAttributes =
    X.element "name" [ ( "attr1", "val1" ), ( "attr2", "val2" ) ] []
        |> X.toString
        |> Expect.equal "<name attr1=\"val1\" attr2=\"val2\"/>"


testElementsWithAttributesUrlEncoded : Expect.Expectation
testElementsWithAttributesUrlEncoded =
    X.element "name" [ ( "attr1", "value\"" ) ] []
        |> X.toString
        |> Expect.equal "<name attr1=\"value%22\"/>"


testElementsWithTextChild : String -> Expect.Expectation
testElementsWithTextChild text =
    X.element "name" [] [ X.text text ]
        |> X.toString
        |> Expect.equal ("<name>" ++ text ++ "</name>")


testElementsWithElementChild : Expect.Expectation
testElementsWithElementChild =
    X.element "Parent" [] [ X.element "Child" [] [] ]
        |> X.toString
        |> Expect.equal
            "<Parent>\n  <Child/>\n</Parent>"


testElementsWithMultipleChildren : Expect.Expectation
testElementsWithMultipleChildren =
    Expect.equal "<Parent>\n  <Child-1/>\n  hello\n  <Child-2/>\n</Parent>" <|
        X.toString <|
            X.element "Parent"
                []
                [ X.element "Child-1" [] []
                , X.text "hello"
                , X.element "Child-2" [] []
                ]
