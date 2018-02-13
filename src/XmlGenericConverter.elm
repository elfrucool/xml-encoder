module XmlGenericConverter exposing (..)

import XmlNode.Util as U
import List.Extra as LE


type alias KeyValue =
    ( String, String )


type alias TokensValue =
    ( List String, String )


type alias PathFieldValue =
    ( List String, ( Maybe String, String ) )


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
