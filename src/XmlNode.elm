module XmlNode exposing (XmlNode, text, toString, empty, element)

import Http exposing (encodeUri)


type alias Attribute =
    ( String, String )


type alias XmlElement =
    { name : String
    , attributes : List Attribute
    , children : List XmlNode
    }


type XmlNode
    = Text String
    | Element XmlElement


text : String -> XmlNode
text =
    Text


empty : XmlNode
empty =
    Element <| XmlElement "" [] []


element : String -> List Attribute -> List XmlNode -> XmlNode
element name attributes children =
    Element <| XmlElement name attributes children


toString : Int -> XmlNode -> String
toString indentation node =
    case node of
        Text string ->
            string

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
    key ++ "=\"" ++ (encodeUri value) ++ "\""


indent : Int -> String
indent indentation =
    "\n" ++ String.repeat indentation "  "


nextIndent : Int -> String
nextIndent indentation =
    indent (indentation + 1)
