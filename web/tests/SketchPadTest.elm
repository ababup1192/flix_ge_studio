module SketchPadTest exposing (suite)

{-| ラフ塗りの純ロジックだけを検査する: コードの自動割り振り・sketch.json の
往復・依頼文の一節づくり・依頼文への差し込み・一筆の undo。
見た目(チップの並び・色の見え方)はテストしない。
-}

import Expect
import SketchPad
import Test exposing (Test, describe, test)


{-| 2x2 を 1 マスだけ塗った最小の模型。

    W .        W = 壁(ひとこと付き)
    . .

-}
paintedModel : SketchPad.Model
paintedModel =
    let
        base =
            SketchPad.init
    in
    { base
        | size = { w = 2, h = 2 }
        , legend =
            [ { char = 'W', name = "壁", fill = "#8a6d3b", desc = "崩れかけた石壁" }
            , { char = 'F', name = "床", fill = "#d9cfb8", desc = "" }
            ]
        , rows = [ "W.", ".." ]
        , note = "左が入り口"
        , name = "stage2"
    }


{-| サーバの依頼文の形(必ず【やること】の行を持つ)を模した最小の下書き。 -}
promptFixture : String
promptFixture =
    String.join "\n"
        [ "あなたはこのゲームを広げる係です。"
        , "対象プロジェクト: demo"
        , ""
        , "【やること】"
        , "1. 場面を足す"
        ]


suite : Test
suite =
    describe "SketchPad"
        [ describe "nextChar(コードの自動割り振り)"
            [ test "空きの先頭 'A' から割り振る" <|
                \_ -> SketchPad.nextChar [] |> Expect.equal (Just 'A')
            , test "使用中を飛ばして次の空きを返す" <|
                \_ -> SketchPad.nextChar [ 'A', 'B', 'D' ] |> Expect.equal (Just 'C')
            , test "全部使い切ったら Nothing(追加を断る)" <|
                \_ ->
                    SketchPad.nextChar (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                        |> Expect.equal Nothing
            ]
        , describe "encode / decode(sketch.json の往復)"
            [ test "書いた JSON を読み戻すと同じ絵とラベルに戻る" <|
                \_ ->
                    SketchPad.decode (SketchPad.encode paintedModel)
                        |> Maybe.map (\m -> ( m.rows, m.legend, ( m.size, m.note, m.preset ) ))
                        |> Expect.equal
                            (Just
                                ( paintedModel.rows
                                , paintedModel.legend
                                , ( paintedModel.size, paintedModel.note, paintedModel.preset )
                                )
                            )
            , test "壊れた JSON は Nothing" <|
                \_ -> SketchPad.decode "{ こわれてる" |> Expect.equal Nothing
            ]
        , describe "promptSection(依頼文の一節)"
            [ test "何も塗らず補足も空なら Nothing(依頼文は今まで通り)" <|
                \_ -> SketchPad.promptSection SketchPad.init |> Expect.equal Nothing
            , test "塗りがあれば、凡例(ひとこと付き)・マス目・補足・原本パスが 1 節にまとまる" <|
                \_ ->
                    SketchPad.promptSection paintedModel
                        |> Expect.equal
                            (Just
                                (String.join "\n"
                                    [ "## 画面のラフ（2x2、1文字=1マス。塗りから自動生成）"
                                    , "凡例: W=壁（崩れかけた石壁）  .=空き"
                                    , "W."
                                    , ".."
                                    , "補足: 左が入り口"
                                    , "凡例のかっこ内は意図です。ラフなのでマス単位の忠実さは不要です。"
                                    , "原本: draft/sketch/stage2.sketch.json"
                                    ]
                                )
                            )
            ]
        , describe "spliceInto(依頼文への差し込み)"
            [ test "【やること】の直前(説明の直後)に入る" <|
                \_ ->
                    SketchPad.spliceInto "## 画面のラフ" promptFixture
                        |> Expect.equal
                            (String.join "\n"
                                [ "あなたはこのゲームを広げる係です。"
                                , "対象プロジェクト: demo"
                                , ""
                                , "## 画面のラフ"
                                , ""
                                , "【やること】"
                                , "1. 場面を足す"
                                ]
                            )
            , test "【やること】が無い文でも末尾に足して依頼を壊さない" <|
                \_ ->
                    SketchPad.spliceInto "## 画面のラフ" "ただの文"
                        |> Expect.equal "ただの文\n\n## 画面のラフ"
            ]
        , describe "update(一筆と undo)"
            [ test "押して→なぞって→離す、で 2 マス塗れて undo は一筆 1 本" <|
                \_ ->
                    let
                        ( afterStroke, _ ) =
                            stepAll
                                [ SketchPad.CellDown ( 0, 0 )
                                , SketchPad.CellEntered ( 1, 0 )
                                , SketchPad.StrokeEnded
                                ]
                                { paintedModel | rows = [ "..", ".." ], active = 'W' }
                    in
                    ( afterStroke.rows, List.length afterStroke.undo )
                        |> Expect.equal ( [ "WW", ".." ], 1 )
            , test "戻すと一筆まるごと塗る前に戻る" <|
                \_ ->
                    let
                        ( afterUndo, _ ) =
                            stepAll
                                [ SketchPad.CellDown ( 0, 0 )
                                , SketchPad.CellEntered ( 1, 0 )
                                , SketchPad.StrokeEnded
                                , SketchPad.UndoClicked
                                ]
                                { paintedModel | rows = [ "..", ".." ], active = 'W' }
                    in
                    ( afterUndo.rows, List.length afterUndo.undo )
                        |> Expect.equal ( [ "..", ".." ], 0 )
            ]
        ]


{-| Msg を順に流す(Out は最後のものだけ返す)。 -}
stepAll : List SketchPad.Msg -> SketchPad.Model -> ( SketchPad.Model, SketchPad.Out )
stepAll msgs model =
    List.foldl (\msg ( m, _ ) -> SketchPad.update msg m) ( model, SketchPad.OutNone ) msgs
