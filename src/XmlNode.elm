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

{-| This module aims to represent XML nodes and have a nice
string representation of them.

# Types
@docs XmlNode, Attribute

# Building functions
@docs text, empty, element

# Getting info functions
@docs getText, getElementName, getAttributes, getChildren

# Rendering function
@docs toString

# Modifying info functions
@docs setName, setAttributes, addAttributes, setChildren, addChildren

# Utility functions
@docs escape, escapeAttribute

-}

import XmlNode.Types as T
import XmlNode.Types exposing (..)
import XmlNode.ToString as S
import XmlNode.Util as U


-- EXPORTING TYPES FROM ANOTHER MODULE


{-| Represents an Xml Attribute. It's an alias of ( String, String )
-}
type alias Attribute =
    T.Attribute


{-| Represents an element, it contains a name, a list of attributes and a list
of children nodes.

It is its definition:

      type alias XmlElement =
          { name : String
          , attributes : List Attribute
          , children : List XmlNode
          }
-}
type alias XmlElement =
    T.XmlElement


{-| It represents an XML node that could be either text or element.

It is its definition:

    type XmlNode
        = Text String
        | Element XmlElement
-}
type alias XmlNode =
    T.XmlNode



-- BUILD FUNCTIONS


{-| Builds a Text xml node.

    text "text"
-}
text : String -> XmlNode
text =
    Text


{-| Builds an empty element.

    empty
-}
empty : XmlNode
empty =
    Element <| XmlElement "" [] []


{-| Builds an element with name, attributes & children.

    element "name" [("attr", "value"), ...] [ text "child1", ... ]
-}
element : String -> List Attribute -> List XmlNode -> XmlNode
element name attributes children =
    Element <| XmlElement name attributes children



-- GETTING INFO


{-| Gets the text of a Text element.

    getText <| text "hello" -- returns "hello"
-}
getText : XmlNode -> Maybe String
getText node =
    case node of
        Text string ->
            Just string

        Element _ ->
            Nothing


{-| Gets the element name of an element.

    getElementName <| element "name" [] [] -- returns "name"
-}
getElementName : XmlNode -> Maybe String
getElementName node =
    case node of
        Text _ ->
            Nothing

        Element xmlElement ->
            Just xmlElement.name


{-| Gets the attribute list of an element.

    getAttributes <| element "n" [("attr", "value")] []

Example above will return `[("attr", "value")]`
-}
getAttributes : XmlNode -> List Attribute
getAttributes node =
    case node of
        Text _ ->
            []

        Element xmlElement ->
            xmlElement.attributes


{-| Gets the children list of an element.

    getChildren <| element "n" [] [ text "hello" ] -- returns [text "hello"]
-}
getChildren : XmlNode -> List XmlNode
getChildren node =
    case node of
        Text _ ->
            []

        Element xmlElement ->
            xmlElement.children



-- TO STRING FUNCTION


{-| Converts an Xml Node to its representation specifying the indent level.
It indents using two spaces always.

    toString 0 node
-}
toString : Int -> XmlNode -> String
toString =
    S.toString



-- MODIFYING ELEMENTS FUNCTIONS


{-| Replaces the element name of an XmlNode.
It does nothing if XmlNode is text.

    setName "newName" node
-}
setName : String -> XmlNode -> XmlNode
setName name node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | name = name }


{-| Replaces the attributes list of an XmlNode to the provided one,
it does nothing if XmlNode is Text.

    setdAttributes [("attr", "value"), ...] node
-}
setAttributes : List Attribute -> XmlNode -> XmlNode
setAttributes attributes node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | attributes = attributes }


{-| Adds an attributes list to an existing XmlNode, it does nothing if XmlNode
is Text.

    addAttributes [("attr", "value"), ...] node
-}
addAttributes : List Attribute -> XmlNode -> XmlNode
addAttributes attributes node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            setAttributes (xmlElement.attributes ++ attributes) node


{-| Replaces the children list of an XmlNode. It does nothing if XmlNode is
Text.

    setChildren [ text "new", ... ] node
-}
setChildren : List XmlNode -> XmlNode -> XmlNode
setChildren children node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            Element { xmlElement | children = children }


{-| Appends new childen nodes to an existing XmlNode.
It does nothing if XmlNode is Text.

    addChildren [ text "new", ... ] node
-}
addChildren : List XmlNode -> XmlNode -> XmlNode
addChildren children node =
    case node of
        Text _ ->
            node

        Element xmlElement ->
            setChildren (xmlElement.children ++ children) node



-- UTIL FUNCTIONS


{-| Escapes Xml special characters: <, > and &.

    escape "hello > world < ! &" -- becomes "hello &gt; world &lt; ! &amp"
-}
escape : String -> String
escape =
    U.escape


{-| Replaces Xml special characters including double quotes, this is suitable
to use in XML attribute names.

    escapeAttribute "\"<>&" -- becomes "&quot;&lt;&gt;&amp"
-}
escapeAttribute : String -> String
escapeAttribute =
    U.escapeAttribute
