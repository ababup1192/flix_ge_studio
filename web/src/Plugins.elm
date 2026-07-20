module Plugins exposing
    ( Plugin
    , Point
    , find
    , forPath
    , inflate
    , plugins
    , rectPercent
    )

{-| 盤面プレビューを持つリソース種のレジストリ。

/resources が配る plugin id(文字列)と、エディタが持つ実装(関数の束)を
ここで突き合わせる。id が引けないリソースはプレビュー無し(自動フォームだけ)に
静かに倒れる — 宣言が先行してもエディタが壊れないように。

-}

import Api
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Plugins.ShooterLevel as ShooterLevel


type alias Point =
    { x : Float, y : Float }


{-| プラグイン 1 件 = 関数の束。Elm には存在型が無いので、種ごとの実装を
後から差し込む口は「同じ形の record を並べる」が現実解。
各プラグイン側(ShooterLevel 等)はこの alias を import せず同じ形を書く —
レジストリが実装を並べる都合で import が逆向きになり、循環してしまうため。

  - preview: パース済み文書 → /preview/items リクエスト(描けない文書は Nothing)
  - sectionKey: 盤面クリック・ドラッグが選ぶ表のセクション(hitTest の添字の意味)
  - dragRects: サーバが返した rects から掴める (添字, 矩形) だけを取り出す
  - dragPoint: 表示 px の移動量 → design 座標(clamp 込み)
  - dragEdits: ドラッグ確定で正本へ流す編集列

-}
type alias Plugin =
    { id : String
    , sectionKey : String
    , preview : D.Value -> Maybe E.Value
    , hitTest : Dict String Api.PreviewRect -> Point -> Maybe Int
    , dragRects : Dict String Api.PreviewRect -> List ( Int, Api.PreviewRect )
    , dragPoint : { center : Point, ratio : Float, designH : Float } -> { dx : Float, dy : Float } -> Point
    , dragEdits : Float -> Point -> List { field : String, value : Int }
    }


plugins : List Plugin
plugins =
    [ ShooterLevel.plugin ]


find : String -> Maybe Plugin
find id =
    plugins
        |> List.filter (\p -> p.id == id)
        |> List.head


{-| 開いたファイルのプラグインを /resources のグループから引く。
pattern の再解釈はしない(glob の解釈をサーバと二重に持たない)—
グループの files に並んだ実パスとの一致だけで決める。
-}
forPath : List Api.ResourceGroup -> String -> Maybe Plugin
forPath groups path =
    groups
        |> List.filter (\g -> List.any (\f -> f.path == path) g.files)
        |> List.head
        |> Maybe.andThen .plugin
        |> Maybe.andThen find



-- プラグインに依らない重ね合わせの幾何(選択枠・掴み所の位置決め)


{-| design 座標の矩形 → img に重ねる div の CSS %(クリック換算の逆)。
px でなく % なのは、Elm の view から img の表示幅が読めないため —
img は width:100% の等比表示なので、比率ならレイアウトを読まずに常に重なる。
-}
rectPercent : Api.Design -> Api.PreviewRect -> { left : Float, top : Float, width : Float, height : Float }
rectPercent design r =
    { left = r.x / max 1 design.w * 100
    , top = r.y / max 1 design.h * 100
    , width = r.w / max 1 design.w * 100
    , height = r.h / max 1 design.h * 100
    }


{-| 点の矩形(4〜8px)はそのままでは掴むのに小さすぎるので、当たり範囲だけ
design px で広げる(絵の点の大きさは変えない)。
-}
inflate : Float -> Api.PreviewRect -> Api.PreviewRect
inflate pad r =
    { x = r.x - pad, y = r.y - pad, w = r.w + pad * 2, h = r.h + pad * 2 }
