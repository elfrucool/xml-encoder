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

import XmlNode.Util as U
import List.Extra as LE
import XmlNode as X


type alias KeyValue =
    ( String, String )


type alias TokensValue =
    ( List String, String )


type alias PathFieldValue =
    ( List String, ( Maybe String, String ) )


type alias XmlNode =
    X.XmlNode


type Node
    = None
    | Element XmlNode
    | Attribute ( String, String )


type alias PathNode =
    ( List String, Node )


type alias ReduceFunc =
    PathNode -> PathNode -> PathNode


tokenize : KeyValue -> TokensValue
tokenize ( key, value ) =
    ( String.split "_" key
        |> List.filter (not << String.isEmpty)
    , value
    )


removeNumericTokens : TokensValue -> TokensValue
removeNumericTokens ( tokens, value ) =
    ( List.filter (not << U.isNumeric) tokens
    , value
    )


getPath : TokensValue -> PathFieldValue
getPath ( tokens, value ) =
    let
        xs =
            Maybe.withDefault [] <| LE.init tokens

        x =
            LE.last tokens
    in
        ( xs, ( x, value ) )


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


convertSingleItem : KeyValue -> PathNode
convertSingleItem =
    tokenize
        >> removeNumericTokens
        >> getPath
        >> toNode


convert : PathNode -> ReduceFunc -> List KeyValue -> PathNode
convert root reduceFunc =
    List.map convertSingleItem
        >> List.filter (\( _, n ) -> n /= None)
        >> List.foldl reduceFunc root


getXmlNode : PathNode -> Maybe X.XmlNode
getXmlNode ( _, node ) =
    case node of
        Element element ->
            Just element

        _ ->
            Nothing
