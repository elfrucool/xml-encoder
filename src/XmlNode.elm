module XmlNode exposing (text, toString, empty, element)

import Http exposing (encodeUri)


type alias Attribute =
    ( String, String )


type alias XmlElement =
    { name : String
    , attributes : List Attribute
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
    Element <| XmlElement "" []


element : String -> List Attribute -> XmlNode
element name attributes =
    Element <| XmlElement name attributes


toString : XmlNode -> String
toString node =
    case node of
        Text string ->
            string

        Element { name, attributes } ->
            "<" ++ name ++ attributesToString attributes ++ "/>"


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
