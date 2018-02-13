module XmlGenericConverter
    exposing
        ( KeyValue
        , TokensValue
        , PathFieldValue
        , Node(..)
        , PathNode
        , ReduceFunc
        , tokenize
        , removeNumericTokens
        , getPath
        , toNode
        , convertSingleItem
        , convert
        , getXmlNode
        )

{-| This library aims to convert a key-value list to
an XmlNode using a reduce function based on convention.

# Types
@docs KeyValue, TokensValue, PathFieldValue, Node, PathNode, ReduceFunc

# Tranform functions
@docs tokenize, removeNumericTokens, getPath, toNode, convertSingleItem, convert

# Extract final value function
@docs getXmlNode

-}

import XmlNode.Util as U
import List.Extra as LE
import XmlNode as X


-- TYPES


{-| Represents a key-value pair. It is an alias of (String, String).
-}
type alias KeyValue =
    ( String, String )


{-| Represents a tokens associated with a value.

Its an alias of (List String, String)

-}
type alias TokensValue =
    ( List String, String )


{-| Represents the path of previous tokens and a key-value.
But the key may not be present.

Its an alias of (List String, (Maybe String, String))
-}
type alias PathFieldValue =
    ( List String, ( Maybe String, String ) )


type alias XmlNode =
    X.XmlNode


{-| Its a node thay may be an xml element, an xml attribute, or nothing.

It is its definition:

    type Node
        = None
        | Element XmlNode
        | Attribute ( String, String )
-}
type Node
    = None
    | Element XmlNode
    | Attribute ( String, String )


{-| Represents a Node with a path of parent values.
It's an alias of (List String, Node)
-}
type alias PathNode =
    ( List String, Node )


{-| Type signature of reduce functions in `convert` function.

Its an alias of PathNode -> PathNode -> PathNode.
-}
type alias ReduceFunc =
    PathNode -> PathNode -> PathNode



-- FUNCTIONS


{-| Converts a key value to a list of tokens and a value.
It uses the "_" string as token separator.

    tokenize ("A_B_C", "value") -- returns (["A", "B", "C"], "value")
-}
tokenize : KeyValue -> TokensValue
tokenize ( key, value ) =
    ( String.split "_" key
        |> List.filter (not << String.isEmpty)
    , value
    )


{-| Removes numeric tokens. Numeric tokens are used to make repetition of
elements when coming from JSON. but they have no meaning in XML.

    removeNumericTokens (["A", "1", "B"], "value")
    -- will return (["A", "B"], "value")
-}
removeNumericTokens : TokensValue -> TokensValue
removeNumericTokens ( tokens, value ) =
    ( List.filter (not << U.isNumeric) tokens
    , value
    )


{-| Transforms a token list with value to a tuple with parent values
and a key-value tuple, but the key may not be present.

    getPath (["A", "B"], "value") -- returns (["A"], (Just "B", "value"))

    getPath (["A"], "value") -- returns ([], (Just "A", "value"))

    getPath ([], "value") -- returns ([], (Nothing, "value"))
-}
getPath : TokensValue -> PathFieldValue
getPath ( tokens, value ) =
    let
        xs =
            Maybe.withDefault [] <| LE.init tokens

        x =
            LE.last tokens
    in
        ( xs, ( x, value ) )


{-| Converts a PathFieldValue to a path with node.
It takes into account the field name. If it is Nothing, then the node is None.
If the field name starts with '@', then it's an attribute.
Otherwise it's an element.

    toNode (["A"], (Just "B", "value")) -- returns (["A"], Element (...))

    toNode (["A"], (Nothing, "value")) -- returns (["A"], None)

    toNode (["A"], (Just "@a", "v")) -- returns (["A"], Attribute ("a", "v"))

Note in the last example the '@' was removed.
-}
toNode : PathFieldValue -> PathNode
toNode ( path, ( maybeField, value ) ) =
    case maybeField of
        Just field ->
            ( path, makeElement_ field value )

        Nothing ->
            ( [], None )


makeElement_ : String -> String -> Node
makeElement_ field value =
    if String.startsWith "@" field then
        Attribute ( String.dropLeft 1 field, value )
    else
        Element (X.element field [] [ X.text value ])


{-| Converts a single key-value tuple to a PathNode.

    convertSingleItem ("a_@b", "v") -- returns (["a"], Element ("b", "v"))
-}
convertSingleItem : KeyValue -> PathNode
convertSingleItem =
    tokenize
        >> removeNumericTokens
        >> getPath
        >> toNode


{-| Converts a list of key-value tuples to a list of PathNode elements using
a root element and a reduce function.

    convert ([], None) (\last first -> last) items

The example avobe will return the last item of the list converted to PathNode.
-}
convert : PathNode -> ReduceFunc -> List KeyValue -> PathNode
convert root reduceFunc =
    List.map convertSingleItem
        >> List.filter (\( _, n ) -> n /= None)
        >> List.foldl reduceFunc root


{-| Extracts the XmlNode from a PathNode, since it may be not present, then
it is wrapped in Maybe.

    getXmlNode (["a"], Element (X.element "element" [] []))

The example avobe will return Just (X.element "element" [] [])
-}
getXmlNode : PathNode -> Maybe X.XmlNode
getXmlNode ( _, node ) =
    case node of
        Element element ->
            Just element

        _ ->
            Nothing
