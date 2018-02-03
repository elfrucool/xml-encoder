module XmlNode.Util exposing (escape, escapeAttribute, isNumeric)

import Char
import String.Extra as StringExtra


escape : String -> String
escape s =
    s
        |> StringExtra.replace "&" "&amp;"
        |> StringExtra.replace "<" "&lt;"
        |> StringExtra.replace ">" "&gt;"


escapeAttribute : String -> String
escapeAttribute s =
    s
        |> escape
        |> StringExtra.replace "\"" "&quot;"


isNumeric : String -> Bool
isNumeric s =
    case (String.uncons s) of
        Nothing ->
            False

        Just _ ->
            String.all Char.isDigit s
