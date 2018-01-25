module XmlNode exposing (text, toString, empty, element)

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
      -- | Element String (List Attribute)
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


toString : XmlNode -> String
toString node =
    case node of
        Text string ->
            string

        Element element ->
            elementToString element


elementToString : XmlElement -> String
elementToString { name, attributes, children } =
    if List.isEmpty children then
        openElement name attributes ++ "/>"
    else
        startElement name attributes
            ++ childrenToString children
            ++ closeElement name


childrenToString : List XmlNode -> String
childrenToString nodes =
    case nodes of
        [] ->
            ""

        [ node ] ->
            case node of
                Text _ ->
                    toString node

                Element _ ->
                    "\n  " ++ toString node ++ "\n"

        _ ->
            nodes
                |> List.map toString
                |> String.join "\n  "
                |> (\s -> "\n  " ++ s ++ "\n")


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
