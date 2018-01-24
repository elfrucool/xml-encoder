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
        ]


testTextNode : String -> Expect.Expectation
testTextNode text =
    text
        |> X.text
        |> X.toString
        |> Expect.equal text
