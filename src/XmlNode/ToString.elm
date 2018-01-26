module XmlNode.ToString exposing (..)

import XmlNode.Types exposing (..)
import XmlNode.Util as U


toString : Int -> XmlNode -> String
toString indentation node =
    case node of
        Text string ->
            U.escape string

        Element element ->
            elementToString indentation element


elementToString : Int -> XmlElement -> String
elementToString indentation { name, attributes, children } =
    if List.isEmpty children then
        openElement name attributes ++ "/>"
    else
        startElement name attributes
            ++ childrenToString indentation children
            ++ closeElement name


childrenToString : Int -> List XmlNode -> String
childrenToString indentation nodes =
    case nodes of
        [] ->
            ""

        [ node ] ->
            case node of
                Text _ ->
                    toString indentation node

                Element _ ->
                    nextIndent indentation
                        ++ toString (indentation + 1) node
                        ++ indent indentation

        _ ->
            nodes
                |> List.map (toString (indentation + 1))
                |> String.join (nextIndent indentation)
                |> (\s -> nextIndent indentation ++ s ++ indent indentation)


startElement : String -> List Attribute -> String
startElement name attributes =
    openElement name attributes ++ ">"


openElement : String -> List Attribute -> String
openElement name attributes =
    "<" ++ name ++ attributesToString attributes


closeElement : String -> String
closeElement name =
    "</" ++ name ++ ">"


attributesToString : List Attribute -> String
attributesToString attributes =
    if List.isEmpty attributes then
        ""
    else
        attributes
            |> List.map singleAttributeToString
            |> String.join " "
            |> (++) " "


singleAttributeToString : Attribute -> String
singleAttributeToString ( key, value ) =
    key ++ "=\"" ++ (U.escapeAttribute value) ++ "\""


indent : Int -> String
indent indentation =
    "\n" ++ String.repeat indentation "  "


nextIndent : Int -> String
nextIndent indentation =
    indent (indentation + 1)
