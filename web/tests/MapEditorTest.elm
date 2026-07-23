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
    MapEditor.fromDoc Nothing (parse mapJson)


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
        [ test "読み取り — トップレベルの x,y 持ちだけが配置になる(単体と配列、xyOnly の区別。空配列・数値は拾わない)" <|
            \_ ->
                case mapDoc of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        doc.groups
                            |> List.map (\g -> ( g.key, kindShape g.kind ))
                            |> Expect.equal
                                [ ( "start", ( "single", 1, False ) )
                                , ( "villagers", ( "many", 1, False ) )
                                , ( "door", ( "single", 1, False ) )
                                , ( "herbs", ( "many", 2, True ) )
                                ]
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
                        MapEditor.fromDoc Nothing (parse mapJson)
                            |> Maybe.map (.terrain >> List.map .ch)

                    withDoc =
                        MapEditor.fromDoc (Just (parse terrainJson)) (parse mapJson)
                            |> Maybe.map (.terrain >> List.map (\sw -> ( sw.ch, sw.name, sw.css == "#334455" )))
                in
                ( without, withDoc )
                    |> Expect.equal
                        ( Just [ '#', ',', '.' ]
                        , Just [ ( '#', "石垣", True ), ( ',', "畑", False ) ]
                        )
        , test "地形パレット — 仮色は並び順から色相を離して導く(少数の候補どうしが必ず見分けられる)。'.' は無彩色の暗色" <|
            \_ ->
                let
                    swatches =
                        MapEditor.fromDoc Nothing (parse """{ "rows": [ "#,~*", "...." ] }""")
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


kindShape : MapEditor.GroupKind -> ( String, Int, Bool )
kindShape kind =
    case kind of
        MapEditor.Single _ ->
            ( "single", 1, False )

        MapEditor.Many many ->
            ( "many", List.length many.points, many.xyOnly )


points : MapEditor.GroupKind -> List ( Int, Int )
points kind =
    case kind of
        MapEditor.Single p ->
            [ p ]

        MapEditor.Many many ->
            many.points
