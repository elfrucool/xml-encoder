module XmlNode exposing (text, toString)


type XmlNode
    = Text String


text : String -> XmlNode
text =
    Text


toString : XmlNode -> String
toString (Text string) =
    string
