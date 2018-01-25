module XmlNode exposing (XmlNode, text, toString, empty, element, setName)

import XmlNode.Types as T
import XmlNode.Types exposing (..)
import XmlNode.ToString as S


type alias Attribute =
    T.Attribute


type alias XmlElement =
    T.XmlElement


type alias XmlNode =
    T.XmlNode


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
toString =
    S.toString


setName : String -> XmlNode -> XmlNode
setName name node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | name = name }
