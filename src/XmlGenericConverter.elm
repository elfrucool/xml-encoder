module XmlGenericConverter exposing (..)

import XmlNode.Util as U


type alias KeyValue =
    ( String, String )


type alias TokensValue =
    ( List String, String )


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
