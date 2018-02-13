module XmlGenericConverter
    exposing
        ( KeyValue
        , TokensValue
        , PathFieldValue
        , Node(..)
        , PathNode
        , tokenize
        , removeNumericTokens
        , getPath
        , toNode
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
            ( path, makeElement field value )

        Nothing ->
            ( [], None )


makeElement : String -> String -> Node
makeElement field value =
    if String.startsWith "@" field then
        Attribute ( String.dropLeft 1 field, value )
    else
        Element (X.element field [] [ X.text value ])
