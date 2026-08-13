module SketchPadTest exposing (suite)

{-| ラフ塗りの純ロジックだけを検査する: コードの自動割り振り・sketch.json の
往復・依頼文の一節づくり・依頼文への差し込み・一筆の undo。
見た目(チップの並び・色の見え方)はテストしない。
-}

import Expect
import Json.Decode as D
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
    withRows [ "W.....", "......", "......", ".....F" ] { paintedModel | size = { w = 6, h = 4 } }


{-| いま塗っているレイヤーの絵を差し替える(レイヤー 1 枚だけの模型になる)。 -}
withRows : List String -> SketchPad.Model -> SketchPad.Model
withRows rows model =
    { model
        | layers = [ { name = "レイヤー1", rows = rows, hidden = [], visible = True } ]
        , layerIndex = 0
    }


{-| いま塗っているレイヤー 1 枚。 -}
activeSheet : SketchPad.Model -> SketchPad.LayerSheet
activeSheet model =
    model.layers
        |> List.drop model.layerIndex
        |> List.head
        |> Maybe.withDefault { name = "", rows = [], hidden = [], visible = True }


{-| 角のつまみを (dx, dy) だけ引く 1 ドラッグ。 -}
dragCorner : ( Float, Float ) -> SketchPad.Model -> SketchPad.Model
dragCorner ( dx, dy ) model =
    dragEdge SketchPad.CornerEdge ( dx, dy ) model


{-| 好きなつまみを (dx, dy) だけ引く 1 ドラッグ。 -}
dragEdge : SketchPad.Edge -> ( Float, Float ) -> SketchPad.Model -> SketchPad.Model
dragEdge edge ( dx, dy ) model =
    stepAll
        [ SketchPad.ResizeStarted edge { x = 100, y = 100 }
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
    withRows [ "W.", ".." ]
        { base
            | size = { w = 2, h = 2 }
            , legend =
                [ { char = 'W', name = "壁", fill = "#8a6d3b", desc = "崩れかけた石壁" }
                , { char = 'F', name = "床", fill = "#d9cfb8", desc = "" }
                ]
            , note = "左が入り口"
            , name = "stage2"
        }


{-| paintedModel を 3 枚のレイヤーに塗り分けた模型(並びは奥から手前)。

    1枚目 = F(床) 左下 / 2枚目 = W(壁) 左上 / 3枚目 = F(床) 右上

-}
layeredModel : SketchPad.Model
layeredModel =
    { paintedModel
        | layers =
            [ { name = "1枚目", rows = [ "..", "F." ], hidden = [], visible = True }
            , { name = "2枚目", rows = [ "W.", ".." ], hidden = [], visible = True }
            , { name = "3枚目", rows = [ ".F", ".." ], hidden = [], visible = True }
            ]
        , layerIndex = 1
    }


{-| レイヤーを持たない頃に保存されたラフ(比較の窓が読み戻す形)。 -}
oldSketchJson : String
oldSketchJson =
    """{"version":1,"kind":"sketch","preset":"map","size":{"w":2,"h":2},"camera":"top-down","fidelity":1,"legend":[{"char":"W","name":"壁","fill":"#8a6d3b","desc":"崩れかけた石壁"},{"char":"F","name":"床","fill":"#d9cfb8"}],"rows":["W.",".."],"note":"左が入り口"}"""


{-| id で奥・主役・手前を指していた頃に保存されたラフ。 -}
idLayeredSketchJson : String
idLayeredSketchJson =
    """{"version":1,"kind":"sketch","preset":"map","size":{"w":2,"h":2},"camera":"top-down","fidelity":1,"legend":[{"char":"W","name":"壁","fill":"#8a6d3b","desc":"崩れかけた石壁"},{"char":"F","name":"床","fill":"#d9cfb8"}],"rows":["WF","F."],"layers":[{"id":"back","rows":["..","F."]},{"id":"main","rows":["W.",".."]},{"id":"front","rows":[".F",".."]}],"note":"左が入り口"}"""


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
                        |> Maybe.map (\m -> ( SketchPad.activeRows m, m.legend, ( m.size, m.note, m.preset ) ))
                        |> Expect.equal
                            (Just
                                ( SketchPad.activeRows paintedModel
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
                                    [ "## 画面のラフ（カメラ: 見下ろし、2x2、再現度: 雰囲気再現、1文字=1マス。塗りから自動生成）"
                                    , "凡例: W=壁（崩れかけた石壁）  .=空き"
                                    , "W."
                                    , ".."
                                    , "補足: 左が入り口"
                                    , "凡例のかっこ内は意図です。ラフなのでマス単位の忠実さは不要です。雰囲気が伝われば十分です。"
                                    , "原本: draft/sketch/stage2/v1.json"
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
                                (withRows [ "..", ".." ] { paintedModel | active = 'W' })
                    in
                    ( SketchPad.activeRows afterStroke, List.length afterStroke.undo )
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
                                (withRows [ "..", ".." ] { paintedModel | active = 'W' })
                    in
                    ( SketchPad.activeRows afterUndo, List.length afterUndo.undo )
                        |> Expect.equal ( [ "..", ".." ], 0 )
            ]
        , describe "リサイズ(はみ出した塗りの保持と復元)"
            [ test "角を左上へ 2 マスぶん引くと 4x3 になり、右下の F は見えなくなる" <|
                \_ ->
                    let
                        shrunk =
                            dragCorner ( -40, -20 ) cornerModel
                    in
                    ( shrunk.size, SketchPad.activeRows shrunk )
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
                    ( restored.size, SketchPad.activeRows restored )
                        |> Expect.equal ( { w = 6, h = 4 }, SketchPad.activeRows cornerModel )
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
                    (SketchPad.activeRows after ++ (activeSheet after).hidden)
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
                    ( SketchPad.activeRows after |> List.any (String.contains "F")
                    , SketchPad.activeRows after |> List.head |> Maybe.map (String.startsWith "W")
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
        , describe "リサイズ(左・上のつまみ)"
            [ test "左のつまみを左へ 45px 引くと 2 列が左に増え、見えていた塗りは動かない" <|
                \_ ->
                    let
                        after =
                            dragEdge SketchPad.LeftEdge ( -45, 0 ) cornerModel
                    in
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal
                            ( { w = 8, h = 4 }
                            , [ "..W.....", "........", "........", ".......F" ]
                            )
            , test "上のつまみを上へ 45px 引くと 2 行が上に増える" <|
                \_ ->
                    let
                        after =
                            dragEdge SketchPad.TopEdge ( 0, -45 ) cornerModel
                    in
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal
                            ( { w = 6, h = 6 }
                            , [ "......", "......", "W.....", "......", "......", ".....F" ]
                            )
            , test "左上に縮めて離してから広げ直すと、見えていた塗りは元の位置に戻る(ドラッグをまたいだ列と行は戻らない)" <|
                \_ ->
                    let
                        after =
                            cornerModel
                                |> dragEdge SketchPad.TopLeftEdge ( 40, 20 )
                                |> dragEdge SketchPad.TopLeftEdge ( -40, -20 )
                    in
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal
                            ( { w = 6, h = 4 }
                            , [ "......", "......", "......", ".....F" ]
                            )
            , test "左つまみは 1 ドラッグ内なら縮めすぎても戻せる(離すまで列は失われない)" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ResizeStarted SketchPad.LeftEdge { x = 100, y = 100 }
                                , SketchPad.ResizeMoved { x = 145, y = 100 }
                                , SketchPad.ResizeMoved { x = 100, y = 100 }
                                , SketchPad.ResizeEnded
                                ]
                                cornerModel
                    in
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal ( { w = 6, h = 4 }, SketchPad.activeRows cornerModel )
            , test "右へ縮める→左へ縮める→undo→右へ広げる、で退避の塗りも元の位置に戻る(hidden も undo の対)" <|
                \_ ->
                    let
                        after =
                            cornerModel
                                |> dragEdge SketchPad.RightEdge ( -25, 0 )
                                |> dragEdge SketchPad.LeftEdge ( 25, 0 )
                                |> stepAll [ SketchPad.UndoClicked ]
                                |> Tuple.first
                                |> dragEdge SketchPad.RightEdge ( 25, 0 )
                    in
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal ( { w = 6, h = 4 }, SketchPad.activeRows cornerModel )
            ]
        , describe "リサイズ(上限・下限)"
            [ test "どれだけ引いても 75x75 で止まる" <|
                \_ ->
                    (dragCorner ( 10000, 10000 ) cornerModel).size
                        |> Expect.equal { w = 75, h = 75 }
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
                    ( after.size, SketchPad.activeRows after )
                        |> Expect.equal ( cornerModel.size, SketchPad.activeRows cornerModel )
            ]
        , describe "形の道具(直線・矩形・楕円)"
            [ test "直線: 押して→引いて→離す、で対角 3 マスが塗れて undo は 1 本" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ToolPicked SketchPad.Line
                                , SketchPad.CellDown ( 0, 0 )
                                , SketchPad.CellEntered ( 2, 2 )
                                , SketchPad.StrokeEnded
                                ]
                                (withRows blankRows cornerModel)
                    in
                    ( SketchPad.activeRows after, List.length after.undo )
                        |> Expect.equal ( [ "W.....", ".W....", "..W...", "......" ], 1 )
            , test "直線: undo 1 本で引く前に戻る" <|
                \_ ->
                    stepAll
                        [ SketchPad.ToolPicked SketchPad.Line
                        , SketchPad.CellDown ( 0, 0 )
                        , SketchPad.CellEntered ( 2, 2 )
                        , SketchPad.StrokeEnded
                        , SketchPad.UndoClicked
                        ]
                        cornerModel
                        |> Tuple.first
                        |> SketchPad.activeRows
                        |> Expect.equal (SketchPad.activeRows cornerModel)
            , test "ドラッグ途中(離す前)は rows を触らない(プレビューは実データと別)" <|
                \_ ->
                    stepAll
                        [ SketchPad.ToolPicked SketchPad.Line
                        , SketchPad.CellDown ( 0, 0 )
                        , SketchPad.CellEntered ( 2, 2 )
                        ]
                        (withRows blankRows cornerModel)
                        |> Tuple.first
                        |> SketchPad.activeRows
                        |> Expect.equal blankRows
            , test "矩形: (0,0)-(3,2) で枠だけ塗れて中は空きのまま" <|
                \_ ->
                    stepAll
                        [ SketchPad.ToolPicked SketchPad.Rect
                        , SketchPad.CellDown ( 0, 0 )
                        , SketchPad.CellEntered ( 3, 2 )
                        , SketchPad.StrokeEnded
                        ]
                        (withRows blankRows cornerModel)
                        |> Tuple.first
                        |> SketchPad.activeRows
                        |> Expect.equal [ "WWWW..", "W..W..", "WWWW..", "......" ]
            , test "楕円: 確定で始点の列と終点の列にマスが塗られ、undo で戻る" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ToolPicked SketchPad.Ellipse
                                , SketchPad.CellDown ( 0, 0 )
                                , SketchPad.CellEntered ( 4, 2 )
                                , SketchPad.StrokeEnded
                                ]
                                (withRows blankRows cornerModel)

                        ( undone, _ ) =
                            stepAll [ SketchPad.UndoClicked ] after
                    in
                    ( SketchPad.activeRows after |> List.any (\row -> String.slice 0 1 row == "W")
                    , SketchPad.activeRows after |> List.any (\row -> String.slice 4 5 row == "W")
                    , SketchPad.activeRows undone
                    )
                        |> Expect.equal ( True, True, blankRows )
            ]
        , describe "ラベルの削除"
            [ test "削除で凡例から消え、塗ってあったマスは空きに戻り、選択は残りの先頭に移る" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ChipPicked 'F'
                                , SketchPad.ChipPicked 'F'
                                , SketchPad.ChipDeleted
                                ]
                                (withRows [ "WF", ".." ] paintedModel)
                    in
                    ( List.map .char after.legend, SketchPad.activeRows after, ( after.active, after.editing ) )
                        |> Expect.equal ( [ 'W' ], [ "W.", ".." ], ( 'W', Nothing ) )
            , test "削除は退避(hidden)からも消える — 縮めて隠した塗りも広げ直しで復活しない" <|
                \_ ->
                    cornerModel
                        |> dragCorner ( -40, -20 )
                        |> stepAll
                            [ SketchPad.ChipPicked 'F'
                            , SketchPad.ChipPicked 'F'
                            , SketchPad.ChipDeleted
                            ]
                        |> Tuple.first
                        |> dragCorner ( 40, 20 )
                        |> SketchPad.activeRows
                        |> List.any (String.contains "F")
                        |> Expect.equal False
            , test "undo で塗りと凡例が対で戻る" <|
                \_ ->
                    let
                        ( after, _ ) =
                            stepAll
                                [ SketchPad.ChipPicked 'F'
                                , SketchPad.ChipPicked 'F'
                                , SketchPad.ChipDeleted
                                , SketchPad.UndoClicked
                                ]
                                (withRows [ "WF", ".." ] paintedModel)
                    in
                    ( List.map .char after.legend, SketchPad.activeRows after )
                        |> Expect.equal ( [ 'W', 'F' ], [ "WF", ".." ] )
            ]
        , describe "マスの大きさ"
            [ test "32 までは入力どおり" <|
                \_ ->
                    stepAll [ SketchPad.CellSizeEdited "32" ] cornerModel
                        |> Tuple.first
                        |> .cellPx
                        |> Expect.equal 32
            , test "0 は下限 10 に丸める" <|
                \_ ->
                    stepAll [ SketchPad.CellSizeEdited "0" ] cornerModel
                        |> Tuple.first
                        |> .cellPx
                        |> Expect.equal 10
            , test "数字でない入力は無視する" <|
                \_ ->
                    stepAll [ SketchPad.CellSizeEdited "abc" ] cornerModel
                        |> Tuple.first
                        |> .cellPx
                        |> Expect.equal 20
            , test "マスを 10px にすると、右つまみ +45px は round(45/10)=5 列増える(スナップが追随)" <|
                \_ ->
                    stepAll [ SketchPad.CellSizeEdited "10" ] cornerModel
                        |> Tuple.first
                        |> dragEdge SketchPad.RightEdge ( 45, 0 )
                        |> .size
                        |> Expect.equal { w = 11, h = 4 }
            ]
        , describe "再現度"
            [ test "encode → decode で再現度が保たれる" <|
                \_ ->
                    SketchPad.decode (SketchPad.encode { paintedModel | fidelity = 3 })
                        |> Maybe.map .fidelity
                        |> Expect.equal (Just 3)
            , test "fidelity 欄の無い旧 JSON は 1(雰囲気再現)に倒す" <|
                \_ ->
                    SketchPad.decode """{"version":1,"kind":"sketch","preset":"map","size":{"w":2,"h":2},"legend":[],"rows":["..",".."],"note":""}"""
                        |> Maybe.map .fidelity
                        |> Expect.equal (Just 1)
            , test "段階 1: 見出しに段階名、末尾に雰囲気でよい旨が出る" <|
                \_ ->
                    let
                        section =
                            SketchPad.promptSection { paintedModel | fidelity = 1 }
                                |> Maybe.withDefault ""
                    in
                    ( String.contains "再現度: 雰囲気再現" section
                    , String.contains "雰囲気が伝われば十分です。" section
                    )
                        |> Expect.equal ( True, True )
            , test "段階 4: 見出しに段階名、末尾にマス単位で忠実の注文が出る" <|
                \_ ->
                    let
                        section =
                            SketchPad.promptSection { paintedModel | fidelity = 4 }
                                |> Maybe.withDefault ""
                    in
                    ( String.contains "再現度: 忠実再現" section
                    , String.contains "マス単位でできるだけ忠実に再現してください。" section
                    )
                        |> Expect.equal ( True, True )
            , test "スライダー入力は 1〜4 に丸め、数字でない入力は無視する" <|
                \_ ->
                    ( stepAll [ SketchPad.FidelityEdited "9" ] cornerModel |> Tuple.first |> .fidelity
                    , stepAll [ SketchPad.FidelityEdited "0" ] cornerModel |> Tuple.first |> .fidelity
                    , stepAll [ SketchPad.FidelityEdited "abc" ] { cornerModel | fidelity = 2 } |> Tuple.first |> .fidelity
                    )
                        |> Expect.equal ( 4, 1, 2 )
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
            , test "画角なしも encode → decode で保たれる" <|
                \_ ->
                    SketchPad.decode (SketchPad.encode { paintedModel | camera = SketchPad.NoAngle })
                        |> Maybe.map .camera
                        |> Expect.equal (Just SketchPad.NoAngle)
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
            , test "ドット絵プリセットは画角なしで始まる" <|
                \_ ->
                    stepAll [ SketchPad.PresetPicked "dot" ] SketchPad.init
                        |> Tuple.first
                        |> .camera
                        |> Expect.equal SketchPad.NoAngle
            , test "ドット絵プリセットは面と材質のラベル 8 枚で始まる" <|
                \_ ->
                    stepAll [ SketchPad.PresetPicked "dot" ] SketchPad.init
                        |> Tuple.first
                        |> .legend
                        |> List.map .name
                        |> Expect.equal [ "輪郭", "ハイライト", "影", "肌", "布", "木", "石", "金属" ]
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
            , test "ドット絵のラフは見出しと単位が絵向きになる" <|
                \_ ->
                    SketchPad.promptSection { paintedModel | preset = "dot", camera = SketchPad.NoAngle }
                        |> Maybe.withDefault ""
                        |> String.contains "## ドット絵のラフ（カメラ: 画角なし、2x2、再現度: 雰囲気再現、1文字=1ドット"
                        |> Expect.equal True
            ]
        , describe "sketchPath(次のバージョン番号)"
            [ test "一覧が無ければ v1" <|
                \_ ->
                    SketchPad.sketchPath paintedModel
                        |> Expect.equal "draft/sketch/stage2/v1.json"
            , test "既存バージョン [3,2,1] があれば最大+1の v4" <|
                \_ ->
                    { paintedModel | sketches = [ { name = "stage2", versions = [ 3, 2, 1 ] } ] }
                        |> SketchPad.sketchPath
                        |> Expect.equal "draft/sketch/stage2/v4.json"
            , test "別名のラフのバージョン番号には釣られない" <|
                \_ ->
                    { paintedModel | sketches = [ { name = "other", versions = [ 9 ] } ] }
                        |> SketchPad.sketchPath
                        |> Expect.equal "draft/sketch/stage2/v1.json"
            ]
        , describe "保存直後の場所(絵を触るまで動かない)"
            [ test "保存が受かった直後は同じ場所を指し続ける" <|
                \_ ->
                    let
                        flying =
                            stepAll [ SketchPad.SaveClicked ] paintedModel |> Tuple.first

                        done =
                            SketchPad.saved flying
                    in
                    SketchPad.sketchPath done |> Expect.equal (SketchPad.sketchPath flying)
            , test "保存後に絵を触ると次のバージョンへ動く" <|
                \_ ->
                    let
                        done =
                            stepAll [ SketchPad.SaveClicked ] paintedModel
                                |> Tuple.first
                                |> SketchPad.saved

                        touched =
                            stepAll [ SketchPad.CellDown ( 1, 1 ) ] done |> Tuple.first
                    in
                    SketchPad.sketchPath touched |> Expect.equal "draft/sketch/stage2/v2.json"
            , test "保存後に絵はそのままで名前だけ打ち替えると、前の名前のファイルを指さず新しい名前の v1 へ動く" <|
                \_ ->
                    -- NameEdited は絵を触らせずとも save を SaveIdle へ戻すので、name だけ
                    -- 直に書き換えて sketchPath 自体の名前一致チェックを検査する
                    -- (content が一致するだけでは古い名前の場所を指し続けてしまう回帰の検査)
                    let
                        done =
                            stepAll [ SketchPad.SaveClicked ] paintedModel
                                |> Tuple.first
                                |> SketchPad.saved

                        renamed =
                            { done | name = "stage3" }
                    in
                    SketchPad.sketchPath renamed |> Expect.equal "draft/sketch/stage3/v1.json"
            ]
        , describe "レイヤーの増減と並べ替え"
            [ test "新しいラフのレイヤーは 1 枚だけ" <|
                \_ ->
                    ( List.map .name SketchPad.init.layers, SketchPad.init.layerIndex )
                        |> Expect.equal ( [ "レイヤー1" ], 0 )
            , test "追加すると一番手前に足され、そのまま選ばれる" <|
                \_ ->
                    stepAll [ SketchPad.LayerAdded ] paintedModel
                        |> Tuple.first
                        |> (\m -> ( List.map .name m.layers, m.layerIndex ))
                        |> Expect.equal ( [ "レイヤー1", "レイヤー2" ], 1 )
            , test "3 枚まで増やせる" <|
                \_ ->
                    stepAll [ SketchPad.LayerAdded, SketchPad.LayerAdded ] paintedModel
                        |> Tuple.first
                        |> .layers
                        |> List.length
                        |> Expect.equal 3
            , test "4 枚目は増やせず、断りを伝える" <|
                \_ ->
                    stepAll [ SketchPad.LayerAdded, SketchPad.LayerAdded, SketchPad.LayerAdded ] paintedModel
                        |> Tuple.second
                        |> Expect.equal (SketchPad.OutToast "レイヤーはこれ以上増やせません")
            , test "上限まで増やしても 3 枚を超えない" <|
                \_ ->
                    stepAll [ SketchPad.LayerAdded, SketchPad.LayerAdded, SketchPad.LayerAdded ] paintedModel
                        |> Tuple.first
                        |> .layers
                        |> List.length
                        |> Expect.equal 3
            , test "選んでいるレイヤーを削除すると、そのレイヤーの絵ごと消える" <|
                \_ ->
                    stepAll [ SketchPad.LayerDeleted ] layeredModel
                        |> Tuple.first
                        |> (\m -> ( List.map .name m.layers, m.layerIndex ))
                        |> Expect.equal ( [ "1枚目", "3枚目" ], 1 )
            , test "残り 1 枚のときは削除できず、断りを伝える" <|
                \_ ->
                    stepAll [ SketchPad.LayerDeleted ] paintedModel
                        |> Tuple.second
                        |> Expect.equal (SketchPad.OutToast "最後の 1 枚は削除できません")
            , test "残り 1 枚のときの削除は絵を 1 マスも変えない" <|
                \_ ->
                    stepAll [ SketchPad.LayerDeleted ] paintedModel
                        |> Tuple.first
                        |> SketchPad.activeRows
                        |> Expect.equal [ "W.", ".." ]
            , test "手前へ動かすと重なり順が入れ替わり、選んだままになる" <|
                \_ ->
                    stepAll [ SketchPad.LayerRaised ] layeredModel
                        |> Tuple.first
                        |> (\m -> ( List.map .name m.layers, m.layerIndex ))
                        |> Expect.equal ( [ "1枚目", "3枚目", "2枚目" ], 2 )
            , test "奥へ動かすと重なり順が入れ替わる" <|
                \_ ->
                    stepAll [ SketchPad.LayerLowered ] layeredModel
                        |> Tuple.first
                        |> (\m -> List.map .name m.layers)
                        |> Expect.equal [ "2枚目", "1枚目", "3枚目" ]
            , test "一番手前のレイヤーはさらに手前へ動かせない" <|
                \_ ->
                    stepAll [ SketchPad.LayerPicked 2, SketchPad.LayerRaised ] layeredModel
                        |> Tuple.first
                        |> (\m -> ( List.map .name m.layers, m.layerIndex ))
                        |> Expect.equal ( [ "1枚目", "2枚目", "3枚目" ], 2 )
            , test "名前はその場で打ち替えられる" <|
                \_ ->
                    stepAll [ SketchPad.LayerRenamed 0 "地面" ] layeredModel
                        |> Tuple.first
                        |> (\m -> List.map .name m.layers)
                        |> Expect.equal [ "地面", "2枚目", "3枚目" ]
            ]
        , describe "レイヤーの重なりと表示・非表示"
            [ test "レイヤーの欄が無い古いラフは、1 枚として読める" <|
                \_ ->
                    SketchPad.decode oldSketchJson
                        |> Maybe.map (\m -> ( List.map .rows m.layers, List.map .name m.layers, m.layerIndex ))
                        |> Expect.equal (Just ( [ [ "W.", ".." ] ], [ "レイヤー1" ], 0 ))
            , test "id で奥・主役・手前を指していた頃のラフは、3 枚のレイヤーとして読める" <|
                \_ ->
                    SketchPad.decode idLayeredSketchJson
                        |> Maybe.map (\m -> List.map (\sheet -> ( sheet.name, sheet.rows )) m.layers)
                        |> Expect.equal
                            (Just
                                [ ( "奥", [ "..", "F." ] )
                                , ( "主役", [ "W.", ".." ] )
                                , ( "手前", [ ".F", ".." ] )
                                ]
                            )
            , test "1 枚で描いたラフの保存物にレイヤーの欄は出ない" <|
                \_ ->
                    SketchPad.encode paintedModel
                        |> String.contains "\"layers\""
                        |> Expect.equal False
            , test "レイヤーに分けて描いたら保存物にレイヤーの欄が出る" <|
                \_ ->
                    SketchPad.encode layeredModel
                        |> String.contains "\"layers\""
                        |> Expect.equal True
            , test "3 枚のラフは読み書きしてもレイヤーごとの絵と名前が変わらない" <|
                \_ ->
                    SketchPad.decode (SketchPad.encode layeredModel)
                        |> Maybe.map (\m -> List.map (\sheet -> ( sheet.name, sheet.rows )) m.layers)
                        |> Expect.equal
                            (Just
                                [ ( "1枚目", [ "..", "F." ] )
                                , ( "2枚目", [ "W.", ".." ] )
                                , ( "3枚目", [ ".F", ".." ] )
                                ]
                            )
            , test "非表示の印も読み書きで残る" <|
                \_ ->
                    stepAll [ SketchPad.LayerVisibilityToggled 2 ] layeredModel
                        |> Tuple.first
                        |> SketchPad.encode
                        |> SketchPad.decode
                        |> Maybe.map (\m -> List.map .visible m.layers)
                        |> Expect.equal (Just [ True, True, False ])
            , test "保存物の rows はレイヤーを畳んだ 1 枚(手前のレイヤーが勝つ)" <|
                \_ ->
                    SketchPad.flatRows layeredModel |> Expect.equal [ "WF", "F." ]
            , test "非表示のレイヤーは畳んだ絵に入らない" <|
                \_ ->
                    stepAll [ SketchPad.LayerVisibilityToggled 2 ] layeredModel
                        |> Tuple.first
                        |> SketchPad.flatRows
                        |> Expect.equal [ "W.", "F." ]
            , test "塗るのは選んでいるレイヤーだけ(ほかは変わらない)" <|
                \_ ->
                    stepAll [ SketchPad.CellDown ( 1, 1 ), SketchPad.StrokeEnded ] layeredModel
                        |> Tuple.first
                        |> SketchPad.layerRows 0
                        |> Expect.equal [ "..", "F." ]
            , test "広げると塗っていないレイヤーも一緒に広がる" <|
                \_ ->
                    dragEdge SketchPad.RightEdge ( 40, 0 ) layeredModel
                        |> SketchPad.layerRows 0
                        |> Expect.equal [ "....", "F..." ]
            , test "戻すと、塗っていないレイヤーの寸法替えも一緒に戻る" <|
                \_ ->
                    stepAll [ SketchPad.UndoClicked ] (dragEdge SketchPad.RightEdge ( 40, 0 ) layeredModel)
                        |> Tuple.first
                        |> SketchPad.layerRows 0
                        |> Expect.equal [ "..", "F." ]
            , test "ラベルを消すと、ほかのレイヤーで塗ったマスも消える" <|
                \_ ->
                    stepAll [ SketchPad.ChipDeleted ] { layeredModel | editing = Just 'F' }
                        |> Tuple.first
                        |> SketchPad.layerRows 0
                        |> Expect.equal [ "..", ".." ]
            , test "レイヤーに分けたラフの依頼文は、レイヤーごとにマス目を 1 枚ずつ書く" <|
                \_ ->
                    SketchPad.promptSection layeredModel
                        |> Expect.equal
                            (Just
                                (String.join "\n"
                                    [ "## 画面のラフ（カメラ: 見下ろし、2x2、再現度: 雰囲気再現、1文字=1マス。塗りから自動生成）"
                                    , "凡例: W=壁（崩れかけた石壁）  F=床  .=空き"
                                    , "レイヤー: あとに書いたものほど手前に重なります。レイヤーごとに 1 枚ずつ書きます。"
                                    , "### 1枚目"
                                    , ".."
                                    , "F."
                                    , "### 2枚目"
                                    , "W."
                                    , ".."
                                    , "### 3枚目"
                                    , ".F"
                                    , ".."
                                    , "補足: 左が入り口"
                                    , "凡例のかっこ内は意図です。ラフなのでマス単位の忠実さは不要です。雰囲気が伝われば十分です。"
                                    , "原本: draft/sketch/stage2/v1.json"
                                    ]
                                )
                            )
            , test "レイヤーを増やしただけ(塗ったのは 1 枚)の依頼文は、1 枚で描いたときと同じ" <|
                \_ ->
                    stepAll [ SketchPad.LayerAdded ] paintedModel
                        |> Tuple.first
                        |> SketchPad.promptSection
                        |> Expect.equal (SketchPad.promptSection paintedModel)
            ]
        , describe "listDecoder(GET /sketch/list)"
            [ test "サーバの応答形を読める" <|
                \_ ->
                    D.decodeString SketchPad.listDecoder
                        """{"ok":true,"sketches":[{"name":"stage2","versions":[3,2,1]},{"name":"title","versions":[1]}]}"""
                        |> Expect.equal
                            (Ok
                                [ { name = "stage2", versions = [ 3, 2, 1 ] }
                                , { name = "title", versions = [ 1 ] }
                                ]
                            )
            ]
        ]


{-| cornerModel と同じ 6x4 の何も塗っていないマス目。 -}
blankRows : List String
blankRows =
    List.repeat 4 "......"


{-| Msg を順に流す(Out は最後のものだけ返す)。 -}
stepAll : List SketchPad.Msg -> SketchPad.Model -> ( SketchPad.Model, SketchPad.Out )
stepAll msgs model =
    List.foldl (\msg ( m, _ ) -> SketchPad.update msg m) ( model, SketchPad.OutNone ) msgs
