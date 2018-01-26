module XmlNode
    exposing
        ( XmlNode
        , text
        , toString
        , empty
        , element
        , setName
        , setAttributes
        , addAttributes
        , setChildren
        , addChildren
        , escape
        , escapeAttribute
        )

import XmlNode.Types as T
import XmlNode.Types exposing (..)
import XmlNode.ToString as S
import XmlNode.Util as U


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


setAttributes : List Attribute -> XmlNode -> XmlNode
setAttributes attributes node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | attributes = attributes }


addAttributes : List Attribute -> XmlNode -> XmlNode
addAttributes attributes node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            setAttributes (xmlElement.attributes ++ attributes) node


setChildren : List XmlNode -> XmlNode -> XmlNode
setChildren children node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | children = children }


addChildren : List XmlNode -> XmlNode -> XmlNode
addChildren children node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            setChildren (xmlElement.children ++ children) node



-- UTIL FUNCTIONS


escape : String -> String
escape =
    U.escape


escapeAttribute : String -> String
escapeAttribute =
    U.escapeAttribute
