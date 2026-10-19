module TalkGraph exposing
    ( Edge(..)
    , Graph
    , Placed
    , boxH
    , boxW
    , build
    , view
    )

{-| 会話のノードの図(公式の Yarn Spinner for VS Code の Graph View に近い形)。

  - 箱 = ノード 1 つ(このファイルのノードだけ)。押すとその台本の行へ。
  - 矢印 = `<<jump>>`(実線)と、選択肢の中の `<<jump>>`(点線。選んだ時の行き先)。
  - 別のファイルのノードへの jump は、小さな丸で終わる線と行き先の名前(VS Code と同じ見せ方)。
  - 置き場所: このファイルの全部のノードがヘッダに `position: x,y` を持っていればそれを使う。
    1 つでも無ければ、入りの無いノード(無ければ最初のノード)から jump を辿った段で左から右へ並べる。

並べ方と線は build(純粋)が決め、view は描くだけ。

-}

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import TalkTree exposing (LinkKind(..), TreeNode)


boxW : Float
boxW =
    150


boxH : Float
boxH =
    36


gapX : Float
gapX =
    70


gapY : Float
gapY =
    24


{-| 置いた箱(左上の座標)。 -}
type alias Placed =
    { node : TreeNode, x : Float, y : Float }


{-| 線: このファイルの箱どうし(元, 先, 種類) / 別のファイルのノードへ(元, 先の名前, 種類)。 -}
type Edge
    = Between String String LinkKind
    | Outside String String LinkKind


type alias Graph =
    { placed : List Placed, edges : List Edge, width : Float, height : Float }


{-| file のノードの図を組む。 -}
build : String -> List TreeNode -> Graph
build file all =
    let
        nodes =
            all |> List.filter (\n -> n.file == file) |> List.sortBy .line

        titles =
            List.map .title nodes

        placed =
            case List.map .position nodes |> allJust of
                Just positions ->
                    fromPositions nodes positions

                Nothing ->
                    layered nodes

        edges =
            nodes
                |> List.concatMap
                    (\n ->
                        n.links
                            |> List.map
                                (\l ->
                                    if List.member l.target titles then
                                        Between n.title l.target l.kind

                                    else
                                        Outside n.title l.target l.kind
                                )
                    )
                |> dedupe
    in
    { placed = placed
    , edges = edges
    , width = (placed |> List.map .x |> List.maximum |> Maybe.withDefault 0) + boxW + gapX + 80
    -- 下に回る戻りの線の分も空けておく
    , height = (placed |> List.map .y |> List.maximum |> Maybe.withDefault 0) + boxH + gapY + 60
    }


allJust : List (Maybe a) -> Maybe (List a)
allJust list =
    if List.isEmpty list then
        Nothing

    else
        List.foldr (\m acc -> Maybe.map2 (::) m acc) (Just []) list


dedupe : List a -> List a
dedupe list =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        list


{-| ヘッダの位置をそのまま使う(いちばん左上を 0, 0 に寄せる)。 -}
fromPositions : List TreeNode -> List ( Float, Float ) -> List Placed
fromPositions nodes positions =
    let
        minX =
            positions |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0

        minY =
            positions |> List.map Tuple.second |> List.minimum |> Maybe.withDefault 0
    in
    List.map2 (\n ( x, y ) -> { node = n, x = x - minX, y = y - minY }) nodes positions


{-| 段で並べる: 入りの無いノード(無ければ最初のノード)を 0 段にして、jump を辿って 1 段ずつ右へ。
辿り着かないノードは 0 段の下に足す。同じ段の中は台本に書いた順。
-}
layered : List TreeNode -> List Placed
layered nodes =
    let
        titles =
            List.map .title nodes

        targets n =
            n.links |> List.map .target |> List.filter (\t -> List.member t titles && t /= n.title)

        incoming =
            List.concatMap targets nodes

        roots =
            case nodes |> List.filter (\n -> not (List.member n.title incoming)) of
                [] ->
                    List.take 1 nodes

                some ->
                    some

        depths =
            walk nodes (List.map (\r -> ( r.title, 0 )) roots) (List.map .title roots)

        depthOf n =
            depths |> List.filter (\( t, _ ) -> t == n.title) |> List.head |> Maybe.map Tuple.second

        maxDepth =
            depths |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0

        -- 辿り着かないノードは 0 段
        columnOf n =
            depthOf n |> Maybe.withDefault 0
    in
    List.range 0 maxDepth
        |> List.concatMap
            (\d ->
                nodes
                    |> List.filter (\n -> columnOf n == d)
                    |> List.indexedMap
                        (\row n -> { node = n, x = toFloat d * (boxW + gapX), y = toFloat row * (boxH + gapY) })
            )


{-| 幅優先で段を決める(seen = 段の決まったノード、frontier = 今の段のノード)。 -}
walk : List TreeNode -> List ( String, Int ) -> List String -> List ( String, Int )
walk nodes seen frontier =
    if List.isEmpty frontier then
        seen

    else
        let
            depthOf t =
                seen |> List.filter (\( s, _ ) -> s == t) |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault 0

            next =
                frontier
                    |> List.concatMap
                        (\t ->
                            nodes
                                |> List.filter (\n -> n.title == t)
                                |> List.concatMap (\n -> List.map (\l -> ( l.target, depthOf t + 1 )) n.links)
                        )
                    |> List.filter (\( t, _ ) -> List.any (\n -> n.title == t) nodes)
                    |> List.filter (\( t, _ ) -> not (List.any (\( s, _ ) -> s == t) seen))
                    |> dedupeBy Tuple.first
        in
        walk nodes (seen ++ next) (List.map Tuple.first next)


dedupeBy : (a -> b) -> List a -> List a
dedupeBy key list =
    List.foldl
        (\x acc ->
            if List.any (\y -> key y == key x) acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        list



-- 画面


{-| 図を描く。here = カーソルのあるノード(強く見せる)、onPick = 箱を押した時。 -}
view : { here : Maybe String, onPick : TreeNode -> msg } -> Graph -> Html msg
view config graph =
    let
        find title =
            graph.placed |> List.filter (\p -> p.node.title == title) |> List.head

        -- 戻る線が通る高さ(いちばん下の箱の下)
        floor =
            (graph.placed |> List.map .y |> List.maximum |> Maybe.withDefault 0) + boxH

        dash kind =
            case kind of
                OptionLink ->
                    [ SA.strokeDasharray "4 3" ]

                JumpLink ->
                    []

        edge e =
            case e of
                Between from to kind ->
                    case ( find from, find to ) of
                        ( Just a, Just b ) ->
                            [ Svg.path
                                ([ SA.d (edgePath floor a b)
                                 , SA.fill "none"
                                 , SA.stroke "currentColor"
                                 , SA.strokeOpacity "0.55"
                                 , SA.markerEnd "url(#talk-arrow)"
                                 ]
                                    ++ dash kind
                                )
                                []
                            ]

                        _ ->
                            []

                Outside from to kind ->
                    case find from of
                        Just a ->
                            let
                                ( x1, y1 ) =
                                    ( a.x + boxW, a.y + boxH / 2 + 8 )
                            in
                            [ Svg.line ([ SA.x1 (num x1), SA.y1 (num y1), SA.x2 (num (x1 + 28)), SA.y2 (num y1), SA.stroke "currentColor", SA.strokeOpacity "0.55" ] ++ dash kind) []
                            , Svg.circle [ SA.cx (num (x1 + 31)), SA.cy (num y1), SA.r "3", SA.fill "none", SA.stroke "currentColor" ] []
                            , Svg.text_ [ SA.x (num (x1 + 37)), SA.y (num (y1 + 3)), SA.fontSize "10", SA.fill "currentColor", SA.fillOpacity "0.7" ] [ Svg.text to ]
                            ]

                        Nothing ->
                            []

        box p =
            Svg.g
                [ SA.class "talk-graph-node cursor-pointer"
                , SE.onClick (config.onPick p.node)
                ]
                [ Svg.rect
                    [ SA.x (num p.x)
                    , SA.y (num p.y)
                    , SA.width (num boxW)
                    , SA.height (num boxH)
                    , SA.rx "4"
                    -- 箱は塗りつぶす(後ろを通る線を透かさない)
                    , SA.style
                        (if config.here == Just p.node.title then
                            "fill: rgb(58 60 112)"

                         else
                            "fill: var(--color-panel)"
                        )
                    , SA.stroke "currentColor"
                    , SA.strokeOpacity "0.4"
                    ]
                    []
                , Svg.text_ [ SA.x (num (p.x + 8)), SA.y (num (p.y + 15)), SA.fontSize "12", SA.fill "currentColor", SA.fontFamily "monospace" ] [ Svg.text p.node.title ]
                , Svg.text_ [ SA.x (num (p.x + 8)), SA.y (num (p.y + 29)), SA.fontSize "9", SA.fill "currentColor", SA.fillOpacity "0.55" ] [ Svg.text (String.fromInt p.node.line ++ " 行目") ]
                ]
    in
    Svg.svg
        [ SA.class "talk-graph block text-ink"
        , SA.width (num (graph.width + gapX))
        , SA.height (num (graph.height + 20))
        , SA.viewBox (num (-gapX / 2 - 10) ++ " -10 " ++ num (graph.width + gapX) ++ " " ++ num (graph.height + 20))
        ]
        (Svg.defs []
            [ Svg.marker [ SA.id "talk-arrow", SA.viewBox "0 0 8 8", SA.refX "8", SA.refY "4", SA.markerWidth "7", SA.markerHeight "7", SA.orient "auto" ]
                [ Svg.path [ SA.d "M0 0 L8 4 L0 8 z", SA.fill "currentColor", SA.fillOpacity "0.7" ] [] ]
            ]
            :: List.concatMap edge graph.edges
            ++ List.map box graph.placed
        )


{-| 線の道。先が右にあれば元の右辺から先の左辺へ曲線で。左か同じ段(戻る jump)なら、箱を横切らないよう
段と段の間の通り道を使う: 元の右の通り道を下りて図のいちばん下(floor)の下を左へ渡り、
先の左の通り道を上って先の左辺へ入る。
-}
edgePath : Float -> Placed -> Placed -> String
edgePath floor a b =
    if b.x > a.x then
        let
            ( x1, y1 ) =
                ( a.x + boxW, a.y + boxH / 2 )

            ( x2, y2 ) =
                ( b.x, b.y + boxH / 2 )
        in
        "M" ++ num x1 ++ " " ++ num y1 ++ " C" ++ num (x1 + 40) ++ " " ++ num y1 ++ " " ++ num (x2 - 40) ++ " " ++ num y2 ++ " " ++ num x2 ++ " " ++ num y2

    else
        let
            ( x1, y1 ) =
                ( a.x + boxW, a.y + boxH / 2 + 6 )

            ( x2, y2 ) =
                ( b.x, b.y + boxH / 2 + 6 )

            right =
                x1 + gapX / 2 - 6

            left =
                x2 - gapX / 2 + 6

            bottom =
                floor + 16
        in
        "M" ++ num x1 ++ " " ++ num y1
            ++ " L" ++ num right ++ " " ++ num y1
            ++ " L" ++ num right ++ " " ++ num bottom
            ++ " L" ++ num left ++ " " ++ num bottom
            ++ " L" ++ num left ++ " " ++ num y2
            ++ " L" ++ num x2 ++ " " ++ num y2


num : Float -> String
num =
    String.fromFloat
