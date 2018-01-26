module XmlNode.UtilTest exposing (..)

import Expect
import Test exposing (Test, test, describe)
import XmlNode.Util as U


testEscape : Test
testEscape =
    describe "escape escapes & > and <"
        [ test "empty string" <|
            \_ -> Expect.equal "" <| U.escape ""
        , test "non escapable characters" <|
            \_ -> Expect.equal "a" <| U.escape "a"
        , test "replace & with &amp;" <|
            \_ -> Expect.equal "&amp;" <| U.escape "&"
        , test "replace < with &lt;" <|
            \_ -> Expect.equal "&lt;" <| U.escape "<"
        , test "replace > with &gt;" <|
            \_ -> Expect.equal "&gt;" <| U.escape ">"
        , test "complex scenario" <|
            \_ ->
                Expect.equal "&gt;hello&amp;&lt;&lt;bye" <|
                    U.escape ">hello&<<bye"
        ]


testEscapeAttribute : Test
testEscapeAttribute =
    describe "escapeAttribute escapes & > < and \""
        [ test "escaping \"" <|
            \_ -> Expect.equal "&quot;" <| U.escapeAttribute "\""
        , test "also same as `escape`" <|
            \_ ->
                Expect.equal "&amp;&lt;&gt;&quot;" <|
                    U.escapeAttribute "&<>\""
        ]
