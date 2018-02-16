module XmlMerger exposing (merge)

{-| This library is for merging XmlGenericConverter.PathNode elements
to produce a reduced XmlGenericConverter.PathNode with appended values.

# Merge functions
@docs merge
-}

import XmlNode as X
import XmlGenericConverter as C
import Maybe.Extra as ME


{-| Merges a XmlGenericConverter.PathNode into another
XmlGenericConverter.PathNode either as a child or as an attribute depending
on its type.

    merge child parent
-}
merge : C.PathNode -> C.PathNode -> C.PathNode
merge curPathNode ( prevPath, prevNode ) =
    let
        prevXmlNode =
            C.getXmlNode prevNode
    in
        if C.isElement prevNode then
            ( [], maybeAppendNode curPathNode prevXmlNode )
        else
            ( [], C.None )


maybeAppendNode : C.PathNode -> Maybe X.XmlNode -> C.Node
maybeAppendNode curPathNode maybePrevXmlNode =
    maybeToNode <|
        ME.orElse maybePrevXmlNode <|
            Maybe.andThen
                (appendNode curPathNode)
                maybePrevXmlNode


appendNode : C.PathNode -> X.XmlNode -> Maybe X.XmlNode
appendNode ( curPath, curNode ) prevXmlNode =
    appendNodeToLeaf curNode prevXmlNode


appendNodeToLeaf : C.Node -> X.XmlNode -> Maybe X.XmlNode
appendNodeToLeaf curNode prevXmlNode =
    case curNode of
        C.None ->
            Nothing

        C.Element child ->
            Just <| X.addChildren [ child ] prevXmlNode

        C.Attribute attribute ->
            Just <| X.addAttributes [ attribute ] prevXmlNode


maybeToNode : Maybe X.XmlNode -> C.Node
maybeToNode maybeNode =
    case maybeNode of
        Just xmlNode ->
            C.Element xmlNode

        Nothing ->
            C.None
