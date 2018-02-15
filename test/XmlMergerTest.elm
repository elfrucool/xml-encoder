module XmlMergerTest exposing (..)

import Test exposing (..)
import Expect
import XmlNode as X
import XmlGenericConverter as C
import XmlMerger as M


mergeScenarios : Test
mergeScenarios =
    describe "merge scenarios"
        [ test "merge 1: empty path, actual=none + previous=none" <|
            \_ -> testMerge ( [], C.None ) ( [], C.None ) ( [], C.None )
        , test "merge 2: empty path, actual=element + previous=element" <|
            \_ ->
                testMerge
                    ( [], C.Element (X.element "root" [] []) )
                    ( [], C.makeNodeValueElement "element" "value" )
                    ( []
                    , C.Element <|
                        X.element "root"
                            []
                            [ X.element "element" [] [ X.text "value" ] ]
                    )
        , test "merge 3: empty path, actual=none + previous=element" <|
            \_ ->
                testMerge
                    ( [], C.None )
                    ( [], C.Element (X.element "root" [] []) )
                    ( [], C.Element (X.element "root" [] []) )
        ]


testMerge : C.PathNode -> C.PathNode -> C.PathNode -> Expect.Expectation
testMerge actual previous expected =
    previous
        |> M.merge actual
        |> Expect.equal expected
