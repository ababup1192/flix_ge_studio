module PixelEditorTest exposing (suite)

{-| ドット絵の手直しのルールだけを検査する: rows の書き換え・バケツの連結・
スポイト・legend からのパレット導出。見た目(色の見え方・描画タイミング)は
テストしない。
-}

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import PixelEditor
import Test exposing (Test, describe, test)


{-| legend は hex とテーマ名の混在。壊れたコマ(行の長さ不揃い)と壊れた絵
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
        , test "legend→パレット — 並びのまま・hex は素通し・透明は実データの '.'。壊れた絵とコマは除く" <|
            \_ ->
                case docFixture of
                    Nothing ->
                        Expect.fail "fixture が読めるべき"

                    Just doc ->
                        ( PixelEditor.palette Dict.empty doc
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
                        PixelEditor.palette (Dict.fromList [ ( "hair", "#ffc95e" ) ]) doc
                            |> List.map (\sw -> ( sw.ch, sw.css ))
                            |> Expect.equal
                                -- hex 値の 'a' は素通し、意味色キー 'h' は解決表の実色
                                [ ( 'a', "#102030" ), ( 'h', "#ffc95e" ) ]
        ]
