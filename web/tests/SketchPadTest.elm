module SketchPadTest exposing (suite)

{-| ラフ塗りの純ロジックだけを検査する: コードの自動割り振り・sketch.json の
往復・依頼文の一節づくり・依頼文への差し込み・一筆の undo。
見た目(チップの並び・色の見え方)はテストしない。
-}

import Expect
import SketchPad
import Test exposing (Test, describe, test)


{-| 6x4 の左上に W・右下に F を塗った模型(リサイズの検査用)。

    W . . . . .
    . . . . . .
    . . . . . .
    . . . . . F

-}
cornerModel : SketchPad.Model
cornerModel =
    { paintedModel
        | size = { w = 6, h = 4 }
        , rows = [ "W.....", "......", "......", ".....F" ]
    }


{-| 角のつまみを (dx, dy) だけ引く 1 ドラッグ。 -}
dragCorner : ( Float, Float ) -> SketchPad.Model -> SketchPad.Model
dragCorner ( dx, dy ) model =
    stepAll
        [ SketchPad.ResizeStarted SketchPad.CornerEdge { x = 100, y = 100 }
        , SketchPad.ResizeMoved { x = 100 + dx, y = 100 + dy }
        , SketchPad.ResizeEnded
        ]
        model
        |> Tuple.first


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
                                    [ "## 画面のラフ（カメラ: 見下ろし、2x2、1文字=1マス。塗りから自動生成）"
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
        , describe "リサイズ(はみ出した塗りの保持と復元)"
            [ test "角を左上へ 2 マスぶん引くと 4x3 になり、右下の F は見えなくなる" <|
                \_ ->
                    let
                        shrunk =
                            dragCorner ( -40, -20 ) cornerModel
                    in
                    ( shrunk.size, shrunk.rows )
                        |> Expect.equal ( { w = 4, h = 3 }, [ "W...", "....", "...." ] )
            , test "縮めた後の保存 JSON と依頼文は、見えている範囲だけを載せる" <|
                \_ ->
                    let
                        shrunk =
                            dragCorner ( -40, -20 ) cornerModel

                        section =
                            SketchPad.promptSection shrunk |> Maybe.withDefault ""
                    in
                    ( String.contains ".....F" (SketchPad.encode shrunk)
                    , String.contains "F=床" section
                    , String.contains "4x3" section
                    )
                        |> Expect.equal ( False, False, True )
            , test "縮めてから同じぶん広げ直すと F が戻る" <|
                \_ ->
                    let
                        restored =
                            cornerModel
                                |> dragCorner ( -40, -20 )
                                |> dragCorner ( 40, 20 )
                    in
                    ( restored.size, restored.rows )
                        |> Expect.equal ( { w = 6, h = 4 }, cornerModel.rows )
            , test "縮めた後にプリセットを替えると、退避していた塗りも捨てられ、広げても蘇らない" <|
                \_ ->
                    let
                        after =
                            cornerModel
                                |> dragCorner ( -40, -20 )
                                |> stepAll [ SketchPad.PresetPicked "map" ]
                                |> Tuple.first
                                |> dragCorner ( 40, 20 )
                    in
                    (after.rows ++ after.hidden)
                        |> List.any (\row -> String.contains "F" row || String.contains "W" row)
                        |> Expect.equal False
            , test "縮める→戻す→消す→縮める→広げる、で消した塗りは退避から戻らない" <|
                \_ ->
                    let
                        after =
                            cornerModel
                                |> dragCorner ( -40, -20 )
                                |> stepAll [ SketchPad.UndoClicked ]
                                |> Tuple.first
                                |> stepAll
                                    [ SketchPad.ToolPicked SketchPad.Eraser
                                    , SketchPad.CellDown ( 5, 3 )
                                    , SketchPad.StrokeEnded
                                    ]
                                |> Tuple.first
                                |> dragCorner ( -40, -20 )
                                |> dragCorner ( 40, 20 )
                    in
                    ( after.rows |> List.any (String.contains "F")
                    , after.rows |> List.head |> Maybe.map (String.startsWith "W")
                    )
                        |> Expect.equal ( False, Just True )
            ]
        , describe "リサイズ(スナップと軸の独立)"
            [ test "右のつまみを +45px 引くと round で 2 列増え、行は変わらない" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ResizeStarted SketchPad.RightEdge { x = 100, y = 100 }
                                , SketchPad.ResizeMoved { x = 145, y = 999 }
                                , SketchPad.ResizeEnded
                                ]
                                cornerModel
                    in
                    after.size |> Expect.equal { w = 8, h = 4 }
            , test "下のつまみを +45px 引くと 2 行増え、列は変わらない" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ResizeStarted SketchPad.BottomEdge { x = 100, y = 100 }
                                , SketchPad.ResizeMoved { x = 999, y = 145 }
                                , SketchPad.ResizeEnded
                                ]
                                cornerModel
                    in
                    after.size |> Expect.equal { w = 6, h = 6 }
            ]
        , describe "リサイズ(上限・下限)"
            [ test "どれだけ引いても 48x32 で止まる" <|
                \_ ->
                    (dragCorner ( 10000, 10000 ) cornerModel).size
                        |> Expect.equal { w = 48, h = 32 }
            , test "どれだけ縮めても 4x3 で止まる" <|
                \_ ->
                    (dragCorner ( -10000, -10000 ) cornerModel).size
                        |> Expect.equal { w = 4, h = 3 }
            ]
        , describe "リサイズ(undo)"
            [ test "1 ドラッグで undo は 1 本" <|
                \_ ->
                    dragCorner ( -40, -20 ) cornerModel
                        |> .undo
                        |> List.length
                        |> Expect.equal 1
            , test "戻すとサイズと塗りが対で元に戻る" <|
                \_ ->
                    let
                        ( after, _ ) =
                            dragCorner ( -40, -20 ) cornerModel
                                |> stepAll [ SketchPad.UndoClicked ]
                    in
                    ( after.size, after.rows )
                        |> Expect.equal ( cornerModel.size, cornerModel.rows )
            ]
        , describe "カメラ(保存と読み戻し)"
            [ test "encode → decode でカメラが保たれる" <|
                \_ ->
                    SketchPad.decode (SketchPad.encode { paintedModel | camera = SketchPad.SideView })
                        |> Maybe.map .camera
                        |> Expect.equal (Just SketchPad.SideView)
            , test "camera の欄が無い旧 JSON はプリセットの初期カメラに倒す" <|
                \_ ->
                    SketchPad.decode """{"version":1,"kind":"sketch","preset":"map","size":{"w":2,"h":2},"legend":[],"rows":["..",".."],"note":""}"""
                        |> Maybe.map .camera
                        |> Expect.equal (Just SketchPad.TopDown)
            , test "知らないカメラ名も読める(fail-open で初期カメラに倒す)" <|
                \_ ->
                    SketchPad.decode """{"version":1,"kind":"sketch","preset":"map","size":{"w":2,"h":2},"camera":"fisheye","legend":[],"rows":["..",".."],"note":""}"""
                        |> Maybe.map .camera
                        |> Expect.equal (Just SketchPad.TopDown)
            ]
        , describe "カメラ(プリセット連動と選び直し)"
            [ test "風景プリセットを選ぶとサイドビューで始まる" <|
                \_ ->
                    stepAll [ SketchPad.PresetPicked "scenery" ] SketchPad.init
                        |> Tuple.first
                        |> .camera
                        |> Expect.equal SketchPad.SideView
            , test "プリセットの初期カメラは後から選び直せる" <|
                \_ ->
                    stepAll
                        [ SketchPad.PresetPicked "scenery"
                        , SketchPad.CameraPicked SketchPad.FirstPerson
                        ]
                        SketchPad.init
                        |> Tuple.first
                        |> .camera
                        |> Expect.equal SketchPad.FirstPerson
            ]
        , describe "カメラ(依頼文への反映)"
            [ test "見出しにカメラとサイズが出る" <|
                \_ ->
                    SketchPad.promptSection { paintedModel | camera = SketchPad.SideView }
                        |> Maybe.withDefault ""
                        |> String.contains "カメラ: サイドビュー、2x2"
                        |> Expect.equal True
            ]
        ]


{-| Msg を順に流す(Out は最後のものだけ返す)。 -}
stepAll : List SketchPad.Msg -> SketchPad.Model -> ( SketchPad.Model, SketchPad.Out )
stepAll msgs model =
    List.foldl (\msg ( m, _ ) -> SketchPad.update msg m) ( model, SketchPad.OutNone ) msgs
