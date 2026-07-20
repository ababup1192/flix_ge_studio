module PluginsTest exposing (suite)

{-| プラグインレジストリの解決とフォールバック、重ね合わせ幾何を固定する。
「plugin 宣言が無いファイルではプレビュー要求が出ない」は、Main がプレビューの
入口にしている forPath が Nothing を返すことに等しいので、その形で pin する。
-}

import Api
import Expect
import Json.Decode as D
import Json.Encode as E
import Plugins
import Test exposing (Test, describe, test)


levelGroup : Api.ResourceGroup
levelGroup =
    { id = "level"
    , title = Just "レベル"
    , plugin = Just "shooterLevel"
    , files = [ { path = "assets/level.json", schema = Just "assets/level.schema.json" } ]
    }


{-| plugin 宣言なしのグループ(自動フォームだけで扱うリソース)。 -}
plainGroup : Api.ResourceGroup
plainGroup =
    { id = "misc"
    , title = Nothing
    , plugin = Nothing
    , files = [ { path = "assets/player.hitbox.json", schema = Nothing } ]
    }


{-| 宣言はあるがエディタがまだ実装を持たない plugin id。 -}
futureGroup : Api.ResourceGroup
futureGroup =
    { id = "board"
    , title = Nothing
    , plugin = Just "puzzleBoard"
    , files = [ { path = "assets/board.json", schema = Nothing } ]
    }


groups : List Api.ResourceGroup
groups =
    [ levelGroup, plainGroup, futureGroup ]


rect : Float -> Float -> Float -> Float -> Api.PreviewRect
rect x y w h =
    { x = x, y = y, w = w, h = h }


suite : Test
suite =
    describe "Plugins"
        [ describe "find — plugin id の解決"
            [ test "shooterLevel は登録済み" <|
                \_ ->
                    Plugins.find "shooterLevel"
                        |> Maybe.map .id
                        |> Expect.equal (Just "shooterLevel")
            , test "知らない id は Nothing(エディタは静かにフォームだけへ倒れる)" <|
                \_ ->
                    Plugins.find "puzzleBoard"
                        |> Maybe.map .id
                        |> Expect.equal Nothing
            ]
        , describe "forPath — 開いたファイル → プラグイン"
            [ test "宣言グループの files に居るパスはそのグループの plugin に届く" <|
                \_ ->
                    Plugins.forPath groups "assets/level.json"
                        |> Maybe.map .id
                        |> Expect.equal (Just "shooterLevel")
            , test "plugin 宣言の無いグループのファイルは Nothing = プレビュー要求は出ない" <|
                \_ ->
                    Plugins.forPath groups "assets/player.hitbox.json"
                        |> Maybe.map .id
                        |> Expect.equal Nothing
            , test "どのグループにも居ないファイルは Nothing" <|
                \_ ->
                    Plugins.forPath groups "assets/theme.palette.json"
                        |> Maybe.map .id
                        |> Expect.equal Nothing
            , test "宣言されていても未実装の plugin id なら Nothing(宣言先行に耐える)" <|
                \_ ->
                    Plugins.forPath groups "assets/board.json"
                        |> Maybe.map .id
                        |> Expect.equal Nothing
            ]
        , describe "束ねたプラグイン経由の導出"
            [ test "解決したプラグインの preview はパース済み文書から items を導ける" <|
                \_ ->
                    Plugins.forPath groups "assets/level.json"
                        |> Maybe.andThen (\p -> p.preview (E.object [ ( "spawns", E.list identity [] ) ]))
                        |> Maybe.andThen (D.decodeValue (D.at [ "design", "w" ] D.float) >> Result.toMaybe)
                        |> Expect.equal (Just 320)
            ]
        , describe "重ね合わせの幾何(プラグイン共通)"
            [ test "rect → CSS % はクリック換算の逆(表示幅を読まず比率で重ねる)" <|
                \_ ->
                    Plugins.rectPercent { w = 320, h = 240 } (rect 16 12 8 12)
                        |> Expect.equal { left = 5, top = 5, width = 2.5, height = 5 }
            , test "掴み所は design px で四方に広がる" <|
                \_ ->
                    Plugins.inflate 6 (rect 196 116 8 8)
                        |> Expect.equal (rect 190 110 20 20)
            ]
        ]
