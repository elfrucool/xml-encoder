module XmlNode
    exposing
        ( -- types
          XmlNode
        , Attribute
          -- building
        , text
        , empty
        , element
          -- getting info
        , getText
        , getElementName
        , getAttributes
        , getChildren
          -- rendering
        , toString
          -- modifying info
        , setName
        , setAttributes
        , addAttributes
        , setChildren
        , addChildren
          -- util functions
        , escape
        , escapeAttribute
        )

import XmlNode.Types as T
import XmlNode.Types exposing (..)
import XmlNode.ToString as S
import XmlNode.Util as U


-- EXPORTING TYPES FROM ANOTHER MODULE


type alias Attribute =
    T.Attribute


type alias XmlElement =
    T.XmlElement


type alias XmlNode =
    T.XmlNode



-- BUILD FUNCTIONS


text : String -> XmlNode
text =
    Text


empty : XmlNode
empty =
    Element <| XmlElement "" [] []


element : String -> List Attribute -> List XmlNode -> XmlNode
element name attributes children =
    Element <| XmlElement name attributes children



-- GETTING INFO


getText : XmlNode -> Maybe String
getText node =
    case node of
        Text string ->
            Just string

        Element _ ->
            Nothing


getElementName : XmlNode -> Maybe String
getElementName node =
    case node of
        Text _ ->
            Nothing

        Element xmlElement ->
            Just xmlElement.name


getAttributes : XmlNode -> List Attribute
getAttributes node =
    case node of
        Text _ ->
            []

        Element xmlElement ->
            xmlElement.attributes


getChildren : XmlNode -> List XmlNode
getChildren node =
    case node of
        Text _ ->
            []

        Element xmlElement ->
            xmlElement.children



-- TO STRING FUNCTION


toString : Int -> XmlNode -> String
toString =
    S.toString



-- MODIFYING ELEMENTS FUNCTIONS


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
