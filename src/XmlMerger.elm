module XmlMerger exposing (..)

import XmlNode as X
import XmlGenericConverter as C
import Maybe.Extra as ME


merge : C.PathNode -> C.PathNode -> C.PathNode
merge ( curPath, curNode ) ( prevPath, prevNode ) =
    let
        prevXmlNode =
            C.getXmlNode prevNode

        curXmlNode =
            (C.getXmlNode curNode)
    in
        if C.isElement prevNode then
            ( [], appendElement curXmlNode prevXmlNode )
        else
            ( [], C.None )


appendElement : Maybe X.XmlNode -> Maybe X.XmlNode -> C.Node
appendElement curXmlNode prevXmlNode =
    maybeToNode <|
        ME.orElse prevXmlNode <|
            Maybe.map2
                (\n c -> X.addChildren [ c ] n)
                curXmlNode
                prevXmlNode


maybeToNode : Maybe X.XmlNode -> C.Node
maybeToNode maybeNode =
    case maybeNode of
        Just xmlNode ->
            C.Element xmlNode

        Nothing ->
            C.None
