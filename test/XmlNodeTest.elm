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
    X.element "name" []
        |> X.toString
        |> Expect.equal "<name/>"


testElementsWithAttributes : Expect.Expectation
testElementsWithAttributes =
    X.element "name" [ ( "attr1", "value1" ), ( "attr2", "value2" ) ]
        |> X.toString
        |> Expect.equal "<name attr1=\"value1\" attr2=\"value2\"/>"


testElementsWithAttributesUrlEncoded : Expect.Expectation
testElementsWithAttributesUrlEncoded =
    X.element "name" [ ( "attr1", "value\"" ) ]
        |> X.toString
        |> Expect.equal "<name attr1=\"value%22\"/>"
