module Plugins.ShooterLevel exposing
    ( Point
    , dragEdits
    , dragPoint
    , hitTest
    , plugin
    , preview
    , spawnRects
    )

{-| ShooterLevel(level.json)の盤面プレビュー導出。

絵はサーバ(POST /preview/items)が焼く。ここが持つのは
「文書 → 描画アイテム列」の純関数と、サーバが返した rects 上のクリック・
ドラッグの座標計算だけ — Elm 側に描画コードを持たないため(エンジンが唯一のレンダラ)。

-}

import Api
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


{-| レジストリ(Plugins.plugins)へ渡す束。id は project.json の "plugin" に
書かれる公開名。型アノテーションが Plugins.Plugin の写しなのは、alias を import すると
Plugins(実装一覧を持つ側)⇄ ここ の循環になるため — record は構造的に一致すれば
同じ型なので、写しでもコンパイラが食い違いを検出してくれる。
preview を Just 固定にできるのは、壊れた JSON は呼び側がパースの段で弾いていて、
パースさえ通れば空文書にも枠(最低 320px)を描く方針のため。
-}
plugin :
    { id : String
    , sectionKey : String
    , preview : D.Value -> Maybe E.Value
    , hitTest : Dict String Api.PreviewRect -> Point -> Maybe Int
    , dragRects : Dict String Api.PreviewRect -> List ( Int, Api.PreviewRect )
    , dragPoint : { center : Point, ratio : Float, designH : Float } -> { dx : Float, dy : Float } -> Point
    , dragEdits : Float -> Point -> List { field : String, value : Int }
    }
plugin =
    { id = "shooterLevel"
    , sectionKey = "spawns"
    , preview = \doc -> Just (preview doc)
    , hitTest = hitTest
    , dragRects = spawnRects
    , dragPoint = dragPoint
    , dragEdits = dragEdits
    }



-- 文書 → /preview/items リクエスト


{-| level.json(パース済み)から items リクエストを導く。
横軸 = atX(スクロール軸を 1 枚に伸ばす)・縦軸 = y・敵種で形と色を変える。
routes の軌道は描かない(第 2 弾)。
-}
preview : D.Value -> E.Value
preview doc =
    let
        spawns =
            decodeSpawns doc

        w =
            designWidth spawns

        -- scrollSpeed が書かれていない文書に既定値の絵を出すと
        -- 「書いてある」と誤解させるので、その時はテキストごと出さない
        headline =
            case D.decodeValue (D.at [ "meta", "scrollSpeed" ] D.float) doc of
                Ok n ->
                    [ textItem ("scrollSpeed " ++ String.fromFloat n) ]

                Err _ ->
                    []
    in
    E.object
        [ ( "design", E.object [ ( "w", E.float w ), ( "h", E.float 240 ) ] )
        , ( "items"
          , E.list identity
                (headline ++ groundItem w :: List.concatMap spawnItems spawns)
          )
        ]


type alias Spawn =
    { index : Int, atX : Float, y : Float, kind : String, count : Int }


{-| 書きかけの行(atX 未記入等)は黙って飛ばす — 編集途中でプレビュー全体を
消さないため。id の添字は文書の並びで振るので、飛ばしても他の行の添字はずれない。
-}
decodeSpawns : D.Value -> List Spawn
decodeSpawns doc =
    D.decodeValue (D.field "spawns" (D.list D.value)) doc
        |> Result.withDefault []
        |> List.indexedMap decodeSpawn
        |> List.filterMap identity


decodeSpawn : Int -> D.Value -> Maybe Spawn
decodeSpawn i raw =
    D.decodeValue
        (D.map4 (\atX y kind count -> { index = i, atX = atX, y = y, kind = kind, count = count })
            (D.field "atX" D.float)
            (D.field "y" D.float)
            (D.field "kind" D.string)
            (D.oneOf [ D.field "count" D.int, D.succeed 1 ])
        )
        raw
        |> Result.toMaybe


{-| 右端の spawn がふちに張り付かないよう +80 の余白。spawn が無い文書でも
枠は出したいので最低 320。
-}
designWidth : List Spawn -> Float
designWidth spawns =
    spawns
        |> List.map .atX
        |> List.maximum
        |> Maybe.withDefault 0
        |> (\maxAtX -> max 320 (maxAtX + 80))


spawnItems : Spawn -> List E.Value
spawnItems spawn =
    bodyItem spawn :: List.map (echoItem spawn) (List.range 1 (spawn.count - 1))


{-| 未知の敵種(enum の書きかけ)でも点を消すと選択の足場が無くなるので、
灰色の円として id 付きのまま出す。
-}
bodyItem : Spawn -> E.Value
bodyItem spawn =
    case spawn.kind of
        "popcorn" ->
            circleItem { x = spawn.atX, y = spawn.y, r = 4, color = "#56b6d2", id = Just spawn.index, alpha = Nothing }

        "turret" ->
            boxItem { x = spawn.atX, y = spawn.y, size = 8, color = "#e8845a", id = Just spawn.index, alpha = Nothing }

        "dome" ->
            circleItem { x = spawn.atX, y = spawn.y, r = 5, color = "#c9a227", id = Just spawn.index, alpha = Nothing }

        _ ->
            circleItem { x = spawn.atX, y = spawn.y, r = 4, color = "#8a90a0", id = Just spawn.index, alpha = Nothing }


{-| count>1 の残像。id を付けないのは、クリックで選ぶ対象は本体 1 つで足りる
(残像を選べても同じ行に行くだけ)ため。
-}
echoItem : Spawn -> Int -> E.Value
echoItem spawn j =
    let
        x =
            spawn.atX - 6 * toFloat j
    in
    case spawn.kind of
        "turret" ->
            boxItem { x = x, y = spawn.y, size = 6, color = "#e8845a", id = Nothing, alpha = Just 0.35 }

        "dome" ->
            circleItem { x = x, y = spawn.y, r = 4, color = "#c9a227", id = Nothing, alpha = Just 0.35 }

        _ ->
            circleItem { x = x, y = spawn.y, r = 3, color = "#56b6d2", id = Nothing, alpha = Just 0.35 }


textItem : String -> E.Value
textItem content =
    E.object
        [ ( "type", E.string "text" )
        , ( "x", E.float 6 )
        , ( "y", E.float 6 )
        , ( "size", E.float 10 )
        , ( "text", E.string content )
        , ( "color", E.string "#8a90a0" )
        ]


groundItem : Float -> E.Value
groundItem w =
    E.object
        [ ( "type", E.string "line" )
        , ( "x1", E.float 0 )
        , ( "y1", E.float 226 )
        , ( "x2", E.float w )
        , ( "y2", E.float 226 )
        , ( "color", E.string "#5a6570" )
        ]


circleItem : { x : Float, y : Float, r : Float, color : String, id : Maybe Int, alpha : Maybe Float } -> E.Value
circleItem c =
    E.object
        (List.filterMap identity
            [ c.id |> Maybe.map idField
            , Just ( "type", E.string "circle" )
            , Just ( "x", E.float c.x )
            , Just ( "y", E.float c.y )
            , Just ( "r", E.float c.r )
            , Just ( "color", E.string c.color )
            , c.alpha |> Maybe.map (\a -> ( "alpha", E.float a ))
            ]
        )


{-| box は左上基準の契約なので、そのまま置くと中心基準の円の敵と半サイズずれて
並ぶ。点(atX, y)が中心に見えるようずらして渡す。
-}
boxItem : { x : Float, y : Float, size : Float, color : String, id : Maybe Int, alpha : Maybe Float } -> E.Value
boxItem b =
    E.object
        (List.filterMap identity
            [ b.id |> Maybe.map idField
            , Just ( "type", E.string "box" )
            , Just ( "x", E.float (b.x - b.size / 2) )
            , Just ( "y", E.float (b.y - b.size / 2) )
            , Just ( "w", E.float b.size )
            , Just ( "h", E.float b.size )
            , Just ( "color", E.string b.color )
            , b.alpha |> Maybe.map (\a -> ( "alpha", E.float a ))
            ]
        )


idField : Int -> ( String, E.Value )
idField i =
    ( "id", E.string ("spawns/" ++ String.fromInt i) )



-- rects 上のクリック判定


{-| クリック位置(design 座標)から spawn の添字へ。矩形に含まれる物を優先し、
複数含むなら添字の小さい方。含む物が無ければ中心が一番近い物 —
点が 4〜8px と小さくぴったり踏むのは難しいため。
-}
hitTest : Dict String Api.PreviewRect -> { x : Float, y : Float } -> Maybe Int
hitTest rects point =
    let
        candidates =
            spawnRects rects

        containing =
            candidates
                |> List.filter (\( _, r ) -> contains point r)
                |> List.map Tuple.first
                |> List.minimum
    in
    case containing of
        Just i ->
            Just i

        Nothing ->
            candidates
                -- (距離, 添字) の昇順 = 同距離なら添字小、を List.sort の辞書順に任せる
                |> List.map (\( i, r ) -> ( centerDistance point r, i ))
                |> List.filter (\( d, _ ) -> d <= nearLimit)
                |> List.sort
                |> List.head
                |> Maybe.map Tuple.second


{-| どこをクリックしても何かが選ばれると余白クリック(選択解除のつもり)が
誤操作になるので、この距離より遠い near ヒットは拾わない。
-}
nearLimit : Float
nearLimit =
    12


{-| rects から spawn の (添字, 矩形) だけを取り出す。routes 等の別レイヤ id が
将来混ざっても、掴み所・クリック判定に紛れ込ませないため。
-}
spawnRects : Dict String Api.PreviewRect -> List ( Int, Api.PreviewRect )
spawnRects rects =
    Dict.toList rects
        |> List.filterMap (\( key, r ) -> spawnIndex key |> Maybe.map (\i -> ( i, r )))


spawnIndex : String -> Maybe Int
spawnIndex key =
    case String.split "/" key of
        [ "spawns", n ] ->
            String.toInt n

        _ ->
            Nothing


contains : { x : Float, y : Float } -> Api.PreviewRect -> Bool
contains point r =
    (r.x <= point.x)
        && (point.x <= r.x + r.w)
        && (r.y <= point.y)
        && (point.y <= r.y + r.h)


centerDistance : { x : Float, y : Float } -> Api.PreviewRect -> Float
centerDistance point r =
    let
        dx =
            point.x - (r.x + r.w / 2)

        dy =
            point.y - (r.y + r.h / 2)
    in
    sqrt (dx * dx + dy * dy)



-- ドラッグ(表示座標⇔design 座標の換算と、確定時に出す編集列)


type alias Point =
    { x : Float, y : Float }


{-| ドラッグ中の点(design 座標)。掴んだ点の中心に、mousedown からの表示 px の
移動量を ratio(design px / 表示 px)で換算して足す。offsetX を追いかけないのは、
mousemove が文書全体の購読で届く(target が img とは限らず offset の基準が
揺れる)ため — 移動量方式ならどこで動いても基準が壊れない。
-}
dragPoint : { center : Point, ratio : Float, designH : Float } -> { dx : Float, dy : Float } -> Point
dragPoint spec delta =
    clampPoint spec.designH
        { x = spec.center.x + delta.dx * spec.ratio
        , y = spec.center.y + delta.dy * spec.ratio
        }


{-| atX に負(スクロール開始より手前)を書かせない・y は盤面(0〜design.h)の中だけ。 -}
clampPoint : Float -> Point -> Point
clampPoint designH point =
    { x = max 0 point.x, y = clamp 0 designH point.y }


{-| mouseup で元データへ流す編集列。round は docEdit の isInt=true と対 —
ドラッグ由来の端数を JSON に書かないため。
-}
dragEdits : Float -> Point -> List { field : String, value : Int }
dragEdits designH point =
    let
        p =
            clampPoint designH point
    in
    [ { field = "atX", value = round p.x }
    , { field = "y", value = round p.y }
    ]
