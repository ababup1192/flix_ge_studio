module SkeletonTest exposing (suite)

{-| 新しいファイルの骨格と、名前 → パスの決まり。

固定するのは「旗が明示されること」(宣言された欄が全部書かれる)と、
「打った名前がどこへ落ちるか」の 2 点。字下げの見た目は検査しない。

-}

import Expect
import FileVerbs
import Schema
import Skeleton
import Test exposing (Test, describe, test)


{-| trigger 風の宣言: default 持ち(once)と default 無し(on / says)、
一覧(catalog / list)と単一値(field)を 1 つずつ踏む。
-}
schema : Maybe Schema.Schema
schema =
    """
{
  "version": 2,
  "sections": {
    "meta": { "kind": "record", "fields": {
      "on":   { "type": {"enum": ["enter", "step"]} },
      "once": { "type": "bool", "default": true },
      "says": { "type": {"list": "text"} } } },
    "rooms":    { "kind": "list", "fields": { "x": { "type": "int" } } },
    "monsters": { "kind": "catalog", "fields": { "hp": { "type": "int" } } },
    "gravity":  { "kind": "value", "type": "float", "default": 9.8 }
  }
}
"""
        |> Schema.decodeString
        |> Result.toMaybe


suite : Test
suite =
    describe "Skeleton — 新しいファイルの骨格"
        [ test "宣言された欄は全部書く(default があれば default、無ければ型の空値)" <|
            \_ ->
                Skeleton.docText schema
                    |> String.replace "\n" ""
                    |> String.replace " " ""
                    |> Expect.equal
                        ("""{"version":2,"meta":{"on":"enter","once":true,"says":[]},"""
                            ++ """"rooms":[],"monsters":{},"gravity":9.8}"""
                        )
        , test "スキーマが無ければ空の入れ物だけ(旗の立てようがない)" <|
            \_ ->
                Skeleton.docText Nothing
                    |> String.trim
                    |> Expect.equal "{}"
        , test "pathFor: \"*\" を名前で埋める。置き場・拡張子を打っても同じ 1 本に落ちる" <|
            \_ ->
                [ Skeleton.pathFor "assets/*.map.json" "cave"
                , Skeleton.pathFor "assets/*.map.json" "  cave.map.json  "
                , Skeleton.pathFor "assets/*.map.json" "other/cave"

                -- "*" の無い宣言は 1 本しか置けない(名前は使わない)
                , Skeleton.pathFor "hitbox.json" "cave"
                ]
                    |> Expect.equal
                        [ "assets/cave.map.json"
                        , "assets/cave.map.json"
                        , "assets/cave.map.json"
                        , "hitbox.json"
                        ]
        , test "bareNameOf: 既存パスから宣言の飾りを外す(複製・改名の初期値)" <|
            \_ ->
                Skeleton.bareNameOf "assets/*.map.json" "assets/cave.map.json"
                    |> Expect.equal "cave"
        , describe "動詞の断り(サーバへ行く前に止める)"
            [ test "空の名前・既にある名前は理由つきで断る" <|
                \_ ->
                    let
                        dialog text =
                            { kind =
                                FileVerbs.NewFile
                                    { groupId = "maps"
                                    , groupLabel = "マップ"
                                    , pattern = "assets/*.map.json"
                                    , schemaPath = Nothing
                                    }
                            , text = text
                            , error = Nothing
                            }

                        existing =
                            [ "assets/cave.map.json" ]
                    in
                    [ FileVerbs.problem existing (dialog "  ")
                    , FileVerbs.problem existing (dialog "cave")
                    , FileVerbs.problem existing (dialog "hall")
                    ]
                        |> Expect.equal
                            [ Just "名前が空です"
                            , Just "\"assets/cave.map.json\" は既にあります"
                            , Nothing
                            ]
            , test "その場の名前変更: 空と重複は断り、元と同じ名前は通す(やめたのと同じ)" <|
                \_ ->
                    let
                        renaming text =
                            { pattern = "assets/*.map.json", path = "assets/cave.map.json", text = text }

                        existing =
                            [ "assets/cave.map.json", "assets/hall.map.json" ]
                    in
                    [ FileVerbs.renameProblem existing (renaming " ")
                    , FileVerbs.renameProblem existing (renaming "hall")
                    , FileVerbs.renameProblem existing (renaming "cave")
                    , FileVerbs.renameProblem existing (renaming "cellar")
                    ]
                        |> Expect.equal
                            [ Just "名前が空です"
                            , Just "\"assets/hall.map.json\" は既にあります"
                            , Nothing
                            , Nothing
                            ]
            , test "消すのは名前を聞かないので、断る理由も無い" <|
                \_ ->
                    FileVerbs.problem [ "assets/cave.map.json" ] (FileVerbs.forDelete "assets/cave.map.json")
                        |> Expect.equal Nothing
            ]
        ]
