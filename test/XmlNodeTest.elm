module XmlNodeTest exposing (..)

import Test exposing (..)
import Expect
import XmlNode as X


xml : Test
xml =
    describe "I can create nodes"
        [ test "I can create text nodes" <|
            \_ -> testTextNode
        ]


testTextNode : Expect.Expectation
testTextNode =
    "hello world"
        |> X.text
        |> X.toString
        |> Expect.equal "hello world"
