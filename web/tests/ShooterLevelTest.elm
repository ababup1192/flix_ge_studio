module ShooterLevelTest exposing (suite)

{-| ShooterLevel プラグインの純関数(preview の items 導出・hitTest)を fixture で固定する。
サーバへ送る JSON の形が契約なので、生成した Value をデコーダで覗いて具体値と比べる。
-}

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Plugins.ShooterLevel as ShooterLevel
import Test exposing (Test, describe, test)


{-| 3 種の敵が 1 体ずつ。items は [text, line, spawn0, spawn1, spawn2] の並びになる。 -}
threeKindsFixture : String
threeKindsFixture =
    """
    {
      "meta": { "scrollSpeed": 60 },
      "routes": { "mid": { "type": "straight", "speed": 70 } },
      "spawns": [
        { "atX": 200,  "kind": "popcorn", "y": 120, "route": "mid" },
        { "atX": 800,  "kind": "turret",  "y": 212, "route": "mid" },
        { "atX": 1000, "kind": "dome",    "y": 90,  "route": "mid" }
      ]
    }
    """


countFixture : String
countFixture =
    """
    {
      "meta": { "scrollSpeed": 60 },
      "spawns": [
        { "atX": 650, "kind": "popcorn", "y": 120, "route": "mid", "count": 3, "intervalSec": 0.35 }
      ]
    }
    """


smallFixture : String
smallFixture =
    """
    {
      "meta": { "scrollSpeed": 45.5 },
      "spawns": [
        { "atX": 100, "kind": "popcorn", "y": 120, "route": "mid" }
      ]
    }
    """


previewOf : String -> D.Value
previewOf text =
    D.decodeString D.value text
        |> Result.withDefault E.null
        |> ShooterLevel.preview


{-| items の i 番目を、テストで見たい面だけの record に落とす。 -}
itemAt : Int -> D.Decoder a -> D.Value -> Result D.Error a
itemAt i dec made =
    D.decodeValue (D.field "items" (D.index i dec)) made


shapeDecoder : D.Decoder { id : Maybe String, type_ : String, color : String, alpha : Maybe Float }
shapeDecoder =
    D.map4 (\id type_ color alpha -> { id = id, type_ = type_, color = color, alpha = alpha })
        (D.oneOf [ D.field "id" (D.map Just D.string), D.succeed Nothing ])
        (D.field "type" D.string)
        (D.field "color" D.string)
        (D.oneOf [ D.field "alpha" (D.map Just D.float), D.succeed Nothing ])


rect : Float -> Float -> Float -> Float -> { x : Float, y : Float, w : Float, h : Float }
rect x y w h =
    { x = x, y = y, w = w, h = h }


suite : Test
suite =
    describe "Plugins.ShooterLevel"
        [ describe "preview — items の導出"
            [ test "design は最大 atX + 80 と高さ 240 に伸びる" <|
                \_ ->
                    D.decodeValue (D.field "design" (D.map2 Tuple.pair (D.field "w" D.float) (D.field "h" D.float)))
                        (previewOf threeKindsFixture)
                        |> Expect.equal (Ok ( 1080, 240 ))
            , test "atX が小さくても design の幅は最低 320" <|
                \_ ->
                    D.decodeValue (D.at [ "design", "w" ] D.float) (previewOf smallFixture)
                        |> Expect.equal (Ok 320)
            , test "先頭に scrollSpeed テキスト・次に y=226 の地面線" <|
                \_ ->
                    Result.map2 Tuple.pair
                        (itemAt 0
                            (D.map2 Tuple.pair (D.field "type" D.string) (D.field "text" D.string))
                            (previewOf threeKindsFixture)
                        )
                        (itemAt 1
                            (D.map4 (\t y1 y2 c -> ( t, ( y1, y2 ), c ))
                                (D.field "type" D.string)
                                (D.field "y1" D.float)
                                (D.field "y2" D.float)
                                (D.field "color" D.string)
                            )
                            (previewOf threeKindsFixture)
                        )
                        |> Expect.equal
                            (Ok
                                ( ( "text", "scrollSpeed 60" )
                                , ( "line", ( 226, 226 ), "#5a6570" )
                                )
                            )
            , test "popcorn は水色の円 r4・id spawns/0" <|
                \_ ->
                    Result.map2 Tuple.pair
                        (itemAt 2 shapeDecoder (previewOf threeKindsFixture))
                        (itemAt 2
                            (D.map3 (\x y r -> ( x, y, r ))
                                (D.field "x" D.float)
                                (D.field "y" D.float)
                                (D.field "r" D.float)
                            )
                            (previewOf threeKindsFixture)
                        )
                        |> Expect.equal
                            (Ok
                                ( { id = Just "spawns/0", type_ = "circle", color = "#56b6d2", alpha = Nothing }
                                , ( 200, 120, 4 )
                                )
                            )
            , test "turret は橙の 8x8 box(点が中心に見える左上へずらす)・id spawns/1" <|
                \_ ->
                    Result.map2 Tuple.pair
                        (itemAt 3 shapeDecoder (previewOf threeKindsFixture))
                        (itemAt 3
                            (D.map4 (\x y w h -> ( ( x, y ), ( w, h ) ))
                                (D.field "x" D.float)
                                (D.field "y" D.float)
                                (D.field "w" D.float)
                                (D.field "h" D.float)
                            )
                            (previewOf threeKindsFixture)
                        )
                        |> Expect.equal
                            (Ok
                                ( { id = Just "spawns/1", type_ = "box", color = "#e8845a", alpha = Nothing }
                                , ( ( 796, 208 ), ( 8, 8 ) )
                                )
                            )
            , test "dome は黄の円 r5・id spawns/2" <|
                \_ ->
                    Result.map2 Tuple.pair
                        (itemAt 4 shapeDecoder (previewOf threeKindsFixture))
                        (itemAt 4 (D.field "r" D.float) (previewOf threeKindsFixture))
                        |> Expect.equal
                            (Ok
                                ( { id = Just "spawns/2", type_ = "circle", color = "#c9a227", alpha = Nothing }
                                , 5
                                )
                            )
            , test "count 3 は本体の後ろに id 無しの残像 2 個(alpha 0.35・x は 6px ずつ左)" <|
                \_ ->
                    Result.map3 (\a b c -> ( a, b, c ))
                        (itemAt 2 (D.map2 Tuple.pair shapeDecoder (D.field "x" D.float)) (previewOf countFixture))
                        (itemAt 3 (D.map2 Tuple.pair shapeDecoder (D.field "x" D.float)) (previewOf countFixture))
                        (itemAt 4 (D.map2 Tuple.pair shapeDecoder (D.field "x" D.float)) (previewOf countFixture))
                        |> Expect.equal
                            (Ok
                                ( ( { id = Just "spawns/0", type_ = "circle", color = "#56b6d2", alpha = Nothing }, 650 )
                                , ( { id = Nothing, type_ = "circle", color = "#56b6d2", alpha = Just 0.35 }, 644 )
                                , ( { id = Nothing, type_ = "circle", color = "#56b6d2", alpha = Just 0.35 }, 638 )
                                )
                            )
            ]
        , describe "hitTest — rects 上のクリック判定"
            [ test "矩形の中をクリックしたらその spawn" <|
                \_ ->
                    ShooterLevel.hitTest
                        (Dict.fromList [ ( "spawns/2", rect 196 116 8 8 ) ])
                        { x = 200, y = 120 }
                        |> Expect.equal (Just 2)
            , test "複数の矩形に含まれたら添字の小さい方" <|
                \_ ->
                    ShooterLevel.hitTest
                        (Dict.fromList
                            [ ( "spawns/3", rect 198 118 8 8 )
                            , ( "spawns/1", rect 196 116 8 8 )
                            ]
                        )
                        { x = 201, y = 121 }
                        |> Expect.equal (Just 1)
            , test "外れていても近く(12px 以内)なら一番近い spawn" <|
                \_ ->
                    ShooterLevel.hitTest
                        (Dict.fromList
                            [ ( "spawns/0", rect 196 116 8 8 )
                            , ( "spawns/1", rect 296 116 8 8 )
                            ]
                        )
                        { x = 210, y = 120 }
                        |> Expect.equal (Just 0)
            , test "どの矩形からも遠い余白クリックは何も選ばない" <|
                \_ ->
                    ShooterLevel.hitTest
                        (Dict.fromList [ ( "spawns/0", rect 196 116 8 8 ) ])
                        { x = 300, y = 30 }
                        |> Expect.equal Nothing
            , test "spawns/ 以外の id(将来の別レイヤ)は判定に混ざらない" <|
                \_ ->
                    ShooterLevel.hitTest
                        (Dict.fromList [ ( "routes/0", rect 196 116 8 8 ) ])
                        { x = 200, y = 120 }
                        |> Expect.equal Nothing
            ]
        , describe "ドラッグ — 換算・clamp・確定編集の導出"
            [ test "表示 px の移動量を ratio 倍で design 座標へ換算して掴んだ中心に足す" <|
                \_ ->
                    ShooterLevel.dragPoint
                        { center = { x = 200, y = 120 }, ratio = 2, designH = 240 }
                        { dx = 30, dy = -10 }
                        |> Expect.equal { x = 260, y = 100 }
            , test "atX は 0 未満・y は盤面(0〜designH)の外に出ない" <|
                \_ ->
                    ShooterLevel.dragPoint
                        { center = { x = 10, y = 230 }, ratio = 1, designH = 240 }
                        { dx = -50, dy = 100 }
                        |> Expect.equal { x = 0, y = 240 }
            , test "確定時は atX → y の順で round した整数の 2 本" <|
                \_ ->
                    ShooterLevel.dragEdits 240 { x = 259.6, y = 119.4 }
                        |> Expect.equal
                            [ { field = "atX", value = 260 }
                            , { field = "y", value = 119 }
                            ]
            , test "確定時も clamp してから round する" <|
                \_ ->
                    ShooterLevel.dragEdits 240 { x = -5, y = 300 }
                        |> Expect.equal
                            [ { field = "atX", value = 0 }
                            , { field = "y", value = 240 }
                            ]
            ]
        ]
