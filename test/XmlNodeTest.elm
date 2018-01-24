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
        , test "I can create empty element" <|
            \_ -> testEmptyElement
        , test "I can create elements with name only" <|
            \_ -> testNameOnlyElements
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
    X.element "name"
        |> X.toString
        |> Expect.equal "<name/>"
