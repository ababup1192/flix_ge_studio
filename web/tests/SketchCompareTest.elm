module SketchCompareTest exposing (suite)

{-| ラフと見比べる窓の規則のテスト。

守るのは規則だけ: 選んだ物の残り方と、一覧が変わったときの倒れ方、
読みに行く置き場の綴り、読めない中身の着地。見た目は焼かない。

-}

import Expect
import SketchCompare
import SketchPad
import Test exposing (Test, describe, test)


sketches : List SketchPad.Sketch
sketches =
    [ { name = "title", versions = [ 1, 3, 2 ] }
    , { name = "battle", versions = [ 1 ] }
    ]


{-| 一覧が両方届いた状態から始める。 -}
ready : SketchCompare.Model
ready =
    SketchCompare.init
        |> SketchCompare.open
        |> SketchCompare.withScenes [ "title.png", "battle.png" ]
        |> SketchCompare.withSketches sketches


suite : Test
suite =
    describe "SketchCompare"
        [ describe "選ぶ"
            [ test "ラフを選ぶと一番新しいバージョンから見る" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.selectedVersion
                        |> Expect.equal (Just 3)
            , test "選んだラフの置き場はバージョンつきの綴り" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.selectVersion 2
                        |> SketchCompare.pathOf
                        |> Expect.equal (Just "draft/sketch/title/v2.json")
            , test "ラフを選んだ直後は中身を読みに行く" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "battle"
                        |> SketchCompare.pendingPath
                        |> Expect.equal (Just "draft/sketch/battle/v1.json")
            , test "中身が届いたら読みに行かない" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "battle"
                        |> SketchCompare.loaded sample
                        |> SketchCompare.pendingPath
                        |> Expect.equal Nothing
            , test "バージョンは新しい順に並べる" <|
                \_ ->
                    ready
                        |> SketchCompare.versionsOf "title"
                        |> Expect.equal [ 3, 2, 1 ]
            , test "知らないラフのバージョンは空" <|
                \_ ->
                    ready
                        |> SketchCompare.versionsOf "unknown"
                        |> Expect.equal []
            ]
        , describe "一覧が変わったとき"
            [ test "選んでいた絵が消えたら選び直しへ戻す" <|
                \_ ->
                    ready
                        |> SketchCompare.selectScene "title.png"
                        |> SketchCompare.withScenes [ "battle.png" ]
                        |> SketchCompare.selectedScene
                        |> Expect.equal Nothing
            , test "選んでいた絵が残っていればそのまま" <|
                \_ ->
                    ready
                        |> SketchCompare.selectScene "title.png"
                        |> SketchCompare.withScenes [ "title.png" ]
                        |> SketchCompare.selectedScene
                        |> Expect.equal (Just "title.png")
            , test "選んでいたラフが消えたら選び直しへ戻す" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.withSketches [ { name = "battle", versions = [ 1 ] } ]
                        |> SketchCompare.selectedSketch
                        |> Expect.equal Nothing
            , test "選んでいたバージョンだけ消えたら最新へ寄せる" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.selectVersion 1
                        |> SketchCompare.withSketches [ { name = "title", versions = [ 2, 3 ] } ]
                        |> SketchCompare.selectedVersion
                        |> Expect.equal (Just 3)
            , test "最新へ寄せたら中身も読み直す" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.selectVersion 1
                        |> SketchCompare.loaded sample
                        |> SketchCompare.withSketches [ { name = "title", versions = [ 2, 3 ] } ]
                        |> SketchCompare.pendingPath
                        |> Expect.equal (Just "draft/sketch/title/v3.json")
            , test "バージョンが変わっていなければ読み直さない" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "title"
                        |> SketchCompare.loaded sample
                        |> SketchCompare.withSketches sketches
                        |> SketchCompare.pendingPath
                        |> Expect.equal Nothing
            , test "ラフが 1 本も無ければ入口を出さない" <|
                \_ ->
                    SketchCompare.init
                        |> SketchCompare.hasSketches
                        |> Expect.equal False
            ]
        , describe "ラフの中身"
            [ test "読めた中身はマスの並びとして持つ" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "battle"
                        |> SketchCompare.loaded sample
                        |> SketchCompare.draftOf
                        |> draftRows
                        |> Expect.equal (Just [ "AA..", "..A.", "...." ])
            , test "壊れた中身は空の窓でなく理由になる" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "battle"
                        |> SketchCompare.loaded "{"
                        |> SketchCompare.draftOf
                        |> isBroken
                        |> Expect.equal True
            , test "読みに行って失敗したら理由になる" <|
                \_ ->
                    ready
                        |> SketchCompare.selectSketch "battle"
                        |> SketchCompare.loadFailed "HTTP 404"
                        |> SketchCompare.draftOf
                        |> isBroken
                        |> Expect.equal True
            ]
        , describe "見比べ方"
            [ test "重ねの透過は数として読む" <|
                \_ ->
                    SketchCompare.init
                        |> SketchCompare.setOpacity "0.25"
                        |> .opacity
                        |> Expect.within (Expect.Absolute 0.0001) 0.25
            , test "数でない透過は今の濃さのまま" <|
                \_ ->
                    SketchCompare.init
                        |> SketchCompare.setOpacity "あ"
                        |> .opacity
                        |> Expect.within (Expect.Absolute 0.0001) 0.5
            , test "閉じても選んだ物は残る" <|
                \_ ->
                    ready
                        |> SketchCompare.selectScene "title.png"
                        |> SketchCompare.close
                        |> SketchCompare.selectedScene
                        |> Expect.equal (Just "title.png")
            ]
        ]


{-| SketchPad.encode と同じ形の 4x3 のラフ。 -}
sample : String
sample =
    """
    { "version": 1
    , "kind": "sketch"
    , "preset": "map"
    , "size": { "w": 4, "h": 3 }
    , "camera": "topDown"
    , "fidelity": 1
    , "legend": [ { "char": "A", "name": "壁", "fill": "#884422" } ]
    , "rows": [ "AA..", "..A.", "...." ]
    , "note": ""
    }
    """


draftRows : SketchCompare.Draft -> Maybe (List String)
draftRows draft =
    case draft of
        SketchCompare.DraftReady pad ->
            Just pad.rows

        _ ->
            Nothing


isBroken : SketchCompare.Draft -> Bool
isBroken draft =
    case draft of
        SketchCompare.DraftBroken _ ->
            True

        _ ->
            False
