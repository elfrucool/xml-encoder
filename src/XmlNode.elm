module XmlNode exposing (text, toString, empty, element)


type XmlNode
    = Text String
    | Element String


text : String -> XmlNode
text =
    Text


empty : XmlNode
empty =
    Element ""


element : String -> XmlNode
element name =
    Element name


toString : XmlNode -> String
toString node =
    case node of
        Text string ->
            string

        Element name ->
            "<" ++ name ++ "/>"
