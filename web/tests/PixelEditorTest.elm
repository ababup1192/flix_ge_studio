module PixelEditorTest exposing (suite)

{-| ドット絵のクイック編集のルールだけを検査する: rows の書き換え・バケツの連結・
スポイト・legend からのパレット導出。見た目(色の見え方・描画タイミング)は
テストしない。
-}

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import PixelEditor
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


{-| legend は hex とテーマ名の混在。壊れたフレーム(行の長さ不揃い)と壊れた絵
(空 rows)・コメント名は黙って除かれる、が fixture の狙い。
-}
docFixture : Maybe PixelEditor.Doc
docFixture =
    """
{
  "version": 1,
  "legend": { "a": "#102030", "h": "hair" },
  "sprites": {
    "//": "コメントは絵ではない",
    "hero": {
      "note": "3×3",
      "frames": {
        "idle": [ "aa.", "ah.", "..a" ],
        "ragged": [ "aa", "a" ]
      }
    },
    "broken": { "frames": { "idle": [] } }
  }
}
"""
        |> D.decodeString D.value
        |> Result.withDefault E.null
        |> PixelEditor.fromDoc


rows : List String
rows =
    [ "aa.", "ah.", "..a" ]


suite : Test
suite =
    describe "PixelEditor — ルール(塗り・バケツ・スポイト・パレット導出)"
        [ test "塗り — (x,y) の 1 セルだけ書き換わる。範囲外は無傷" <|
            \_ ->
                ( PixelEditor.paintAt 'h' ( 2, 0 ) rows
                , PixelEditor.paintAt 'h' ( 5, 1 ) rows
                )
                    |> Expect.equal ( [ "aah", "ah.", "..a" ], rows )
        , test "バケツ — つながった同色だけ変わる(離れ小島は残る)" <|
            \_ ->
                PixelEditor.floodAt 'h' ( 0, 0 ) rows
                    |> Expect.equal [ "hh.", "hh.", "..a" ]
        , test "スポイト — セルの文字を引く(透明='.'・範囲外は Nothing)" <|
            \_ ->
                ( PixelEditor.pickAt ( 1, 1 ) rows
                , PixelEditor.pickAt ( 2, 0 ) rows
                , PixelEditor.pickAt ( 9, 9 ) rows
                )
                    |> Expect.equal ( Just 'h', Just '.', Nothing )
        , test "legend→パレット — 並びのまま・hex は素通し・透明は実データの '.'。壊れた絵とフレームは除く" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        ( PixelEditor.palette noColors doc
                            |> List.map (\sw -> ( sw.ch, sw.name, sw.css == sw.name ))
                        , PixelEditor.transparentChar doc
                        , doc.sprites |> List.map (\s -> ( s.name, List.map Tuple.first s.frames ))
                        )
                            |> Expect.equal
                                ( [ ( 'a', "#102030", True ), ( 'h', "hair", False ) ]
                                , '.'
                                , [ ( "hero", [ "idle" ] ) ]
                                )
        , test "legend→パレット — サーバの解決表があれば実色が勝ち、無いキーは仮色に倒れる" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        PixelEditor.palette { table = Dict.fromList [ ( "hair", "#ffc95e" ) ], unresolved = Set.empty } doc
                            |> List.map (\sw -> ( sw.ch, sw.css ))
                            |> Expect.equal
                                -- hex 値の 'a' は素通し、意味色キー 'h' は解決表の実色
                                [ ( 'a', "#102030" ), ( 'h', "#ffc95e" ) ]
        , test "legend→パレット — 実色に解けない色は印(仮色の目印)が付く。hex 素通しと解決済みは付かない" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        ( PixelEditor.palette noColors doc |> List.map .guessed
                        , PixelEditor.palette { table = Dict.fromList [ ( "hair", "#ffc95e" ) ], unresolved = Set.empty } doc
                            |> List.map .guessed
                        , PixelEditor.palette { table = Dict.empty, unresolved = Set.fromList [ "hair" ] } doc
                            |> List.map .guessed
                        )
                            |> Expect.equal
                                -- 'a' は "#102030"(素通しなので印なし)、'h' は解決の有無で変わる
                                ( [ False, True ], [ False, False ], [ False, True ] )
        , test "パレットの見た目 — 未解決の色だけに「?」が付き、意味の説明も出る" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        PixelEditor.view noColors doc PixelEditor.init
                            |> Query.fromHtml
                            |> Expect.all
                                [ -- 印が付くのは 'h'(=hair)だけ。'a' は hex 素通しなので付かない
                                  Query.findAll [ Selector.class "px-guessed" ] >> Query.count (Expect.equal 1)
                                , Query.has [ Selector.text "? の色はテーマに無く、仮の色で描いています。ゲームが場面ごとに決めている色(味方と敵で塗り分ける等)かもしれません。テーマに足すとゲームと同じ色になります。" ]
                                ]
        , test "パレットの見た目 — 全部解決していれば印も説明も出ない" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        PixelEditor.view { table = Dict.fromList [ ( "hair", "#ffc95e" ) ], unresolved = Set.empty } doc PixelEditor.init
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.class "px-guessed" ]
                            |> Query.count (Expect.equal 0)
        , describe "絵の見出し(group)"
            [ test "同じ頭を持つ絵が 2 枚以上あれば、その頭が見出しになる" <|
                \_ ->
                    groupsFrom """{ "legend": { "a": "#000" }, "sprites": {
                        "tile_grass_a": { "frames": { "i": ["a"] } },
                        "tile_grass_b": { "frames": { "i": ["a"] } },
                        "house_wall":   { "frames": { "i": ["a"] } },
                        "house_roof":   { "frames": { "i": ["a"] } } } }"""
                        |> Expect.equal [ ( "tile", 2 ), ( "house", 2 ) ]
            , test "束ねる相手が居ない絵は見出しを持たない(「その他」を作らない)" <|
                \_ ->
                    groupsFrom """{ "legend": { "a": "#000" }, "sprites": {
                        "hero":     { "frames": { "i": ["a"] } },
                        "villager": { "frames": { "i": ["a"] } },
                        "herb":     { "frames": { "i": ["a"] } } } }"""
                        |> Expect.equal [ ( "", 3 ) ]
            , test "JSON に書いた group が名前より勝つ" <|
                \_ ->
                    groupsFrom """{ "legend": { "a": "#000" }, "sprites": {
                        "tile_grass_a": { "group": "地面", "frames": { "i": ["a"] } },
                        "tile_grass_b": { "group": "地面", "frames": { "i": ["a"] } } } }"""
                        |> Expect.equal [ ( "地面", 2 ) ]
            , test "頭を共有しない絵と共有する絵が混ざっても取り違えない" <|
                \_ ->
                    groupsFrom """{ "legend": { "a": "#000" }, "sprites": {
                        "tile_grass_a": { "frames": { "i": ["a"] } },
                        "lamp":         { "frames": { "i": ["a"] } },
                        "tile_grass_b": { "frames": { "i": ["a"] } },
                        "fence":        { "frames": { "i": ["a"] } } } }"""
                        |> Expect.equal [ ( "tile", 2 ), ( "", 2 ) ]
            ]
        , describe "形の道具(直線・矩形・楕円)"
            [ test "直線 — 斜め 45 度は 1 セルずつ階段になる" <|
                \_ ->
                    PixelEditor.lineCells ( 0, 0 ) ( 3, 3 )
                        |> Expect.equal [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
            , test "直線 — 押した所と離した所が同じなら 1 セル" <|
                \_ ->
                    PixelEditor.lineCells ( 2, 5 ) ( 2, 5 )
                        |> Expect.equal [ ( 2, 5 ) ]
            , test "直線 — 引く向きが逆でも同じセルを通る" <|
                \_ ->
                    PixelEditor.lineCells ( 5, 1 ) ( 0, 3 )
                        |> List.sort
                        |> Expect.equal (PixelEditor.lineCells ( 0, 3 ) ( 5, 1 ) |> List.sort)
            , test "矩形 — 枠だけで中は空く(4×3 なら 10 セル)" <|
                \_ ->
                    PixelEditor.rectCells ( 0, 0 ) ( 3, 2 )
                        |> List.length
                        |> Expect.equal 10
            , test "矩形 — 4 隅が入る" <|
                \_ ->
                    PixelEditor.rectCells ( 1, 1 ) ( 4, 3 )
                        |> Set.fromList
                        |> (\cells ->
                                [ ( 1, 1 ), ( 4, 1 ), ( 1, 3 ), ( 4, 3 ) ]
                                    |> List.all (\c -> Set.member c cells)
                                    |> Expect.equal True
                           )
            , test "矩形 — 中心は塗らない" <|
                \_ ->
                    PixelEditor.rectCells ( 0, 0 ) ( 4, 4 )
                        |> List.member ( 2, 2 )
                        |> Expect.equal False
            , test "楕円 — 上下左右の 4 点が入る" <|
                \_ ->
                    PixelEditor.ellipseCells ( 0, 0 ) ( 6, 4 )
                        |> Set.fromList
                        |> (\cells ->
                                [ ( 0, 2 ), ( 6, 2 ), ( 3, 0 ), ( 3, 4 ) ]
                                    |> List.all (\c -> Set.member c cells)
                                    |> Expect.equal True
                           )
            , test "楕円 — 中心は塗らない(枠線だけ)" <|
                \_ ->
                    PixelEditor.ellipseCells ( 0, 0 ) ( 8, 8 )
                        |> List.member ( 4, 4 )
                        |> Expect.equal False
            , test "楕円 — 潰れて幅か高さが無いときは直線に倒す" <|
                \_ ->
                    PixelEditor.ellipseCells ( 1, 2 ) ( 5, 2 )
                        |> Expect.equal (PixelEditor.lineCells ( 1, 2 ) ( 5, 2 ))
            , test "まとめ塗り — 指定したセルだけが変わる" <|
                \_ ->
                    PixelEditor.paintCells 'x' [ ( 0, 0 ), ( 2, 1 ) ] [ "...", "...", "..." ]
                        |> Expect.equal [ "x..", "..x", "..." ]
            , test "まとめ塗り — 絵の外のセルは黙って捨てる" <|
                \_ ->
                    PixelEditor.paintCells 'x' [ ( 9, 0 ), ( 0, 9 ), ( -1, 0 ) ] [ "..", ".." ]
                        |> Expect.equal [ "..", ".." ]
            , test "まとめ塗り — 触らない行は同じ値のまま" <|
                \_ ->
                    PixelEditor.paintCells 'x' [ ( 0, 1 ) ] [ "ab", "cd", "ef" ]
                        |> Expect.equal [ "ab", "xd", "ef" ]
            ]
        ]


{-| 見出しごとの絵の枚数。並びは *.sprite.json に出てくる順。 -}
groupsFrom : String -> List ( String, Int )
groupsFrom text =
    text
        |> D.decodeString D.value
        |> Result.withDefault E.null
        |> PixelEditor.fromDoc
        |> Maybe.map
            (PixelEditor.groupsOf >> List.map (\( name, sprites ) -> ( name, List.length sprites )))
        |> Maybe.withDefault []


{-| 解決表が何も無い状態(応答が届く前)。 -}
noColors : PixelEditor.Colors
noColors =
    { table = Dict.empty, unresolved = Set.empty }
