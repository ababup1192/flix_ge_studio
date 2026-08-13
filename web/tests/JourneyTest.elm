module JourneyTest exposing (suite)

{-| /journey/changes の読み取り。知らせ 1 件の形(name/ver/前/今のパス)が
契約どおりに起きることだけ見る(見せ方はテストしない)。
-}

import Expect
import Journey
import Json.Decode as D
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Journey.changesDecoder"
        [ test "baking・seen・changes(name/ver/before/after)を読み取る" <|
            \() ->
                """{"baking":true,"seen":false,"changes":[{"name":"title.png","ver":3,"before":"reference/archive/title.v3.png","after":"reference/title.png"}]}"""
                    |> D.decodeString Journey.changesDecoder
                    |> Expect.equal
                        (Ok
                            { baking = True
                            , seen = False
                            , changes =
                                [ { name = "title.png"
                                  , ver = 3
                                  , before = "reference/archive/title.v3.png"
                                  , after = "reference/title.png"
                                  }
                                ]
                            }
                        )
        ]
