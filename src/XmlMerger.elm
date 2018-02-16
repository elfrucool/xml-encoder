module XmlMerger exposing (..)

import XmlNode as X
import XmlGenericConverter as C
import Maybe.Extra as ME


merge : C.PathNode -> C.PathNode -> C.PathNode
merge ( curPath, curNode ) ( prevPath, prevNode ) =
    let
        prevXmlNode =
            C.getXmlNode prevNode
    in
        if C.isElement prevNode then
            ( [], maybeAppendNode curNode prevXmlNode )
        else
            ( [], C.None )


maybeAppendNode : C.Node -> Maybe X.XmlNode -> C.Node
maybeAppendNode curNode maybePrevXmlNode =
    maybeToNode <|
        ME.orElse maybePrevXmlNode <|
            Maybe.andThen
                (appendNode curNode)
                maybePrevXmlNode


appendNode : C.Node -> X.XmlNode -> Maybe X.XmlNode
appendNode curNode prevXmlNode =
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