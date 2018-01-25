module XmlNode.Types exposing (..)


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
