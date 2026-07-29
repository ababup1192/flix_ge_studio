module MapEditorTest exposing (suite)

{-| マップの手直しのルールだけを検査する: 文書の読み取り(x,y 持ちの機械検出)・
広げる・配置の移動・地形パレットの fail-open。見た目(色の見え方・印の描画)は
テストしない。
-}

import Expect
import Json.Decode as D
import Json.Encode as E
import MapEditor
import Test exposing (Test, describe, test)


{-| rpg.map.json と同じ骨格の縮小版。villagers は x,y 以外のフィールド持ち
(追加不可)、herbs は x,y だけ(追加可)、start/door は単体、goal / 空配列は
配置ではない、が fixture の狙い。
-}
mapJson : String
mapJson =
    """
{
  "title": "t",
  "start": { "x": 2, "y": 1 },
  "rows": [ "##,#", "#..#", "####" ],
  "villagers": [ { "id": "a", "x": 1, "y": 1, "line": "..." } ],
  "door": { "x": 3, "y": 1, "promise": "a", "need": 2 },
  "herbs": [ { "x": 1, "y": 2 }, { "x": 2, "y": 2 } ],
  "empty": [],
  "goal": 5
}
"""


parse : String -> D.Value
parse text =
    D.decodeString D.value text |> Result.withDefault E.null


mapDoc : Maybe MapEditor.Doc
mapDoc =
    MapEditor.fromDoc [] Nothing (parse mapJson)


{-| kaidan の triggers と同じ形: マスを見ない行(on:enter)と、マスを踏む行が
1 つの配列に混ざる。
-}
mixedJson : String
mixedJson =
    """
{
  "rows": [ "....", "...." ],
  "triggers": [
    { "on": "enter", "once": true, "says": [ "…" ] },
    { "on": "step", "x": 1, "y": 1, "says": [ "…" ] },
    { "on": "step", "x": 2, "y": 0, "says": [ "…" ] }
  ]
}
"""


mixedDoc : Maybe MapEditor.Doc
mixedDoc =
    MapEditor.fromDoc [] Nothing (parse mixedJson)


terrainJson : String
terrainJson =
    """
{ "entries": [
    { "char": "#", "name": "石垣", "fill": "#334455" },
    { "char": ",", "name": "畑", "fill": "@field" }
] }
"""


suite : Test
suite =
    describe "MapEditor — ルール(読み取り・広げる・移動・パレット導出)"
        [ test "読み取り — トップレベルの x,y 持ちだけが配置になる(単体と配列、足せるかの区別。空配列・数値は拾わない)" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        doc.groups
                            |> List.map (\g -> ( g.key, kindShape g.kind ))
                            |> Expect.equal
                                -- villagers はスキーマのキーに挙げていないので足せない
                                [ ( "start", ( "single", 1, "-" ) )
                                , ( "villagers", ( "many", 1, "no" ) )
                                , ( "door", ( "single", 1, "-" ) )
                                , ( "herbs", ( "many", 2, "xyOnly" ) )
                                ]
        , test "読み取り — x,y の無い行が混ざっても捨てない。印は元の配列の添字を覚え、置けない行は件数だけ" <|
            \_ ->
                let
                    shape doc =
                        doc |> Maybe.map (.groups >> List.map (\g -> ( g.key, marks g.kind )))

                    -- マスを見ない行だけの配列(kaidan の musicroom)も件数として残る
                    enterOnly =
                        MapEditor.fromDoc [] Nothing
                            (parse """{ "rows": [ "..", ".." ], "triggers": [ { "on": "enter", "says": [ "…" ] } ] }""")
                in
                ( shape mixedDoc, shape enterOnly )
                    |> Expect.equal
                        ( Just [ ( "triggers", ( [ ( 1, ( 1, 1 ) ), ( 2, ( 2, 0 ) ) ], 1 ) ) ]
                        , Just [ ( "triggers", ( [], 1 ) ) ]
                        )
        , test "移動(混在) — 印を動かすと、詰めた順ではなく元の配列の添字で書き戻す" <|
            \_ ->
                case mixedDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        let
                            ( m1, _ ) =
                                MapEditor.update doc (MapEditor.PlaceChosen "triggers") MapEditor.init

                            -- 先頭の印(元の添字 1)を選び、別のマスへ
                            ( m2, _ ) =
                                MapEditor.update doc (MapEditor.CellPressed 1 1 0) m1
                        in
                        MapEditor.update doc (MapEditor.CellPressed 3 1 0) m2
                            |> Tuple.second
                            |> Expect.equal
                                (MapEditor.Edited
                                    (MapEditor.PointMoved { key = "triggers", index = Just 1, x = 3, y = 1 })
                                )
        , test "追加(雛形) — スキーマ宣言のある配列は空きマスのクリックで足せる。宣言が無ければ足さない" <|
            \_ ->
                let
                    clickEmpty doc =
                        let
                            ( m1, _ ) =
                                MapEditor.update doc (MapEditor.PlaceChosen "triggers") MapEditor.init
                        in
                        MapEditor.update doc (MapEditor.CellPressed 3 1 0) m1

                    withSchema =
                        MapEditor.fromDoc [ { key = "triggers", room = True } ] Nothing (parse mixedJson) |> Maybe.map clickEmpty

                    withoutSchema =
                        mixedDoc |> Maybe.map clickEmpty
                in
                ( withSchema |> Maybe.map Tuple.second
                  -- 生まれた行(元の配列の末尾 = 添字 3)を選んでおく
                , withSchema |> Maybe.andThen (Tuple.first >> .picked)
                , withoutSchema |> Maybe.map Tuple.second
                )
                    |> Expect.equal
                        ( Just
                            (MapEditor.Edited
                                (MapEditor.PointAdded { key = "triggers", x = 3, y = 1, fromSchema = True })
                            )
                        , Just ( "triggers", Just 3 )
                        , Just (MapEditor.Noticed "動かしたい印をクリックで選んでください")
                        )
        , test "部屋の行 — 一覧の見出しは値から作り、選ぶと選択に乗る。追加は既に部屋の行があるかを添える" <|
            \_ ->
                case MapEditor.fromDoc [ { key = "triggers", room = True } ] Nothing (parse mixedJson) of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        let
                            ( m1, _ ) =
                                MapEditor.update doc (MapEditor.OffRowChosen "triggers" 0) MapEditor.init

                            ( _, added ) =
                                MapEditor.update doc (MapEditor.RoomRowPressed "triggers") m1
                        in
                        ( doc.groups |> List.concatMap (\g -> offRows g.kind)
                        , MapEditor.selectedRow m1
                        , added
                        )
                            |> Expect.equal
                                ( [ ( 0, "enter — …" ) ]
                                , Just ( "triggers", Just 0 )
                                , MapEditor.Edited (MapEditor.RoomRowAdded { key = "triggers", hadRoom = True })
                                )
        , test "追加(x,y だけの配列) — 従来どおり {x,y} の行を足し、続けて置けるよう選ばない" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        let
                            ( m1, _ ) =
                                MapEditor.update doc (MapEditor.PlaceChosen "herbs") MapEditor.init

                            ( m2, out ) =
                                MapEditor.update doc (MapEditor.CellPressed 2 1 0) m1
                        in
                        ( out, m2.picked )
                            |> Expect.equal
                                ( MapEditor.Edited
                                    (MapEditor.PointAdded { key = "herbs", x = 2, y = 1, fromSchema = False })
                                , Nothing
                                )
        , test "広げる — + で全行が「一番長い行+1」に揃い、元の中身は無傷・新セルは既定の文字" <|
            \_ ->
                let
                    ragged =
                        [ "##,#", "#.", "###" ]

                    widest =
                        ragged |> List.map String.length |> List.maximum |> Maybe.withDefault 0

                    out =
                        MapEditor.addColumn ragged
                in
                Expect.all
                    [ \o -> o |> List.map String.length |> Expect.equal (List.map (\_ -> widest + 1) ragged)
                    , \o ->
                        List.map2 (\before after -> String.left (String.length before) after) ragged o
                            |> Expect.equal ragged
                    , \o ->
                        List.map2 (\before after -> String.dropLeft (String.length before) after) ragged o
                            |> List.concatMap String.toList
                            |> List.filter (\c -> c /= MapEditor.defaultChar)
                            |> Expect.equal []
                    ]
                    out
        , test "移動 — 印をクリックで選び、別セルをクリックすると x,y だけの編集になる" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        let
                            firstHerb =
                                doc.groups
                                    |> List.filterMap
                                        (\g ->
                                            if g.key == "herbs" then
                                                List.head (points g.kind)

                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.withDefault ( -1, -1 )

                            ( m1, _ ) =
                                MapEditor.update doc (MapEditor.PlaceChosen "herbs") MapEditor.init

                            ( m2, afterSelect ) =
                                MapEditor.update doc
                                    (MapEditor.CellPressed (Tuple.first firstHerb) (Tuple.second firstHerb) 0)
                                    m1

                            ( _, afterMove ) =
                                MapEditor.update doc (MapEditor.CellPressed 3 2 0) m2
                        in
                        ( afterSelect, afterMove )
                            |> Expect.equal
                                ( MapEditor.Silent
                                , MapEditor.Edited
                                    (MapEditor.PointMoved { key = "herbs", index = Just 0, x = 3, y = 2 })
                                )
        , test "地形パレット — terrain Doc が無ければ rows の文字+'.'、あれば entries の name と fill(hex は素通し・@キーは仮色)" <|
            \_ ->
                let
                    without =
                        MapEditor.fromDoc [] Nothing (parse mapJson)
                            |> Maybe.map (.terrain >> List.map .ch)

                    withDoc =
                        MapEditor.fromDoc [] (Just (parse terrainJson)) (parse mapJson)
                            |> Maybe.map (.terrain >> List.map (\sw -> ( sw.ch, sw.name, sw.css == "#334455" )))
                in
                ( without, withDoc )
                    |> Expect.equal
                        ( Just [ '#', ',', '.' ]
                        , Just [ ( '#', "石垣", True ), ( ',', "畑", False ) ]
                        )
        , test "レイヤー — パレットは選択中レイヤーの中身だけ(地形は文字、配置は JSON キー)" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        ( MapEditor.paletteChips MapEditor.TerrainLayer doc
                        , MapEditor.paletteChips MapEditor.PlaceLayer doc
                        )
                            |> Expect.equal
                                ( doc.terrain |> List.map (.ch >> String.fromChar)
                                , [ "start", "villagers", "door", "herbs" ]
                                )
        , test "レイヤー — 淡さは 選択中=1・非選択=0.45・👁 オフ=0(オフのレイヤーは描かない)" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        let
                            -- 配置を選択(→ 地形は非選択)、地形の 👁 をオフにする
                            ( selected, _ ) =
                                MapEditor.update doc (MapEditor.LayerChosen MapEditor.PlaceLayer) MapEditor.init

                            ( m, _ ) =
                                MapEditor.update doc (MapEditor.LayerToggled MapEditor.TerrainLayer) selected
                        in
                        Expect.all
                            [ \_ -> MapEditor.layerAlpha MapEditor.PlaceLayer m |> Expect.within (Expect.Absolute 0.001) 1
                            , \_ -> MapEditor.layerAlpha MapEditor.TerrainLayer m |> Expect.within (Expect.Absolute 0.001) 0

                            -- 👁 を戻すと非選択の淡さ
                            , \_ ->
                                MapEditor.layerAlpha MapEditor.TerrainLayer selected |> Expect.within (Expect.Absolute 0.001) 0.45
                            ]
                            ()
        , test "文字格子の判定 — トップに rows があれば開ける。ドット絵(sprites.frames の下)は掛からない" <|
            \_ ->
                let
                    course =
                        MapEditor.fromDoc [] Nothing (parse """{ "version": 1, "rows": [ "..##..", "..##.." ] }""")

                    sprite =
                        MapEditor.fromDoc [] Nothing
                            (parse """{ "sprites": { "hero": { "frames": { "idle": [ "..ii..", ".iiii." ] } } } }""")

                    emptyRows =
                        MapEditor.fromDoc [] Nothing (parse """{ "rows": [] }""")
                in
                Expect.all
                    [ \_ -> course |> Maybe.map (.rows >> List.length) |> Expect.equal (Just 2)
                    , \_ -> sprite |> Expect.equal Nothing
                    , \_ -> emptyRows |> Expect.equal Nothing
                    ]
                    ()
        , test "地形パレット — 仮色は並び順から色相を離して導く(少数の候補どうしが必ず見分けられる)。'.' は無彩色の暗色" <|
            \_ ->
                let
                    swatches =
                        MapEditor.fromDoc [] Nothing (parse """{ "rows": [ "#,~*", "...." ] }""")
                            |> Maybe.map .terrain
                            |> Maybe.withDefault []

                    ( defaults, others ) =
                        List.partition (\sw -> sw.ch == MapEditor.defaultChar) swatches

                    hues =
                        others |> List.filterMap (.css >> hslHue)

                    minGap =
                        pairs hues
                            |> List.map (\( a, b ) -> min (abs (a - b)) (360 - abs (a - b)))
                            |> List.minimum
                            |> Maybe.withDefault 0
                in
                Expect.all
                    [ -- 仮色は全員 hsl で導かれている
                      \_ -> List.length hues |> Expect.equal (List.length others)

                    -- どの 2 色も色相が大きく離れる(ハッシュ由来の偶然の近さがない)
                    , \_ -> minGap |> Expect.atLeast 50

                    -- '.'(既定の文字)は無彩色(彩度 0%)の暗色
                    , \_ -> defaults |> List.map (.css >> String.contains " 0% ") |> Expect.equal [ True ]
                    ]
                    ()
        ]


{-| "hsl(H S% L%)" の H。hsl でなければ Nothing。 -}
hslHue : String -> Maybe Int
hslHue css =
    if String.startsWith "hsl(" css then
        css
            |> String.dropLeft 4
            |> String.words
            |> List.head
            |> Maybe.andThen String.toInt

    else
        Nothing


pairs : List a -> List ( a, a )
pairs items =
    case items of
        x :: rest ->
            List.map (Tuple.pair x) rest ++ pairs rest

        [] ->
            []


kindShape : MapEditor.GroupKind -> ( String, Int, String )
kindShape kind =
    case kind of
        MapEditor.Single _ ->
            ( "single", 1, "-" )

        MapEditor.Many many ->
            ( "many", List.length many.points, addLabel many.add )


addLabel : MapEditor.AddKind -> String
addLabel add =
    case add of
        MapEditor.XyOnly ->
            "xyOnly"

        MapEditor.FromSchema ->
            "schema"

        MapEditor.NoAdd ->
            "no"


{-| 印(元の添字つき)と、マスに置かれていない行の数。
-}
marks : MapEditor.GroupKind -> ( List ( Int, ( Int, Int ) ), Int )
marks kind =
    case kind of
        MapEditor.Single point ->
            ( [ ( 0, point ) ], 0 )

        MapEditor.Many many ->
            ( many.points |> List.map (\mark -> ( mark.index, mark.at )), List.length many.offRows )


{-| マスを見ない行(添字と見出し)。 -}
offRows : MapEditor.GroupKind -> List ( Int, String )
offRows kind =
    case kind of
        MapEditor.Single _ ->
            []

        MapEditor.Many many ->
            many.offRows |> List.map (\row -> ( row.index, row.summary ))


points : MapEditor.GroupKind -> List ( Int, Int )
points kind =
    case kind of
        MapEditor.Single p ->
            [ p ]

        MapEditor.Many many ->
            List.map .at many.points
