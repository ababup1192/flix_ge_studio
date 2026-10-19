module TalkGraphTest exposing (suite)

{-| ノードの図の並べ方と線(純粋な build)。 -}

import Expect
import TalkGraph exposing (Edge(..))
import TalkTree exposing (LinkKind(..), TreeNode)
import Test exposing (Test, describe, test)


node : String -> Int -> Maybe ( Float, Float ) -> List ( String, LinkKind ) -> TreeNode
node title line position links =
    { title = title
    , file = "a.yarn"
    , line = line
    , position = position
    , links = List.map (\( t, k ) -> { target = t, kind = k, line = line + 1 }) links
    }


-- start → (選択肢) shop / → end、shop → end、end → other.yarn の far
nodes : List TreeNode
nodes =
    [ node "start" 1 Nothing [ ( "shop", OptionLink ), ( "end", JumpLink ) ]
    , node "shop" 10 Nothing [ ( "end", JumpLink ) ]
    , node "end" 20 Nothing [ ( "far", JumpLink ) ]
    , { title = "far", file = "b.yarn", line = 1, position = Nothing, links = [] }
    ]


suite : Test
suite =
    describe "TalkGraph"
        [ test "position が無ければ、入りの無いノードから jump を辿った段で左から右へ並べる" <|
            \_ ->
                TalkGraph.build "a.yarn" nodes
                    |> .placed
                    |> List.map (\p -> ( p.node.title, p.x > 0, p.y ))
                    |> Expect.equal [ ( "start", False, 0 ), ( "shop", True, 0 ), ( "end", True, 0 + TalkGraph.boxH + 24 ) ]
        , test "線はこのファイルの箱どうしと、別のファイルへの丸(同じ線は 1 本)" <|
            \_ ->
                TalkGraph.build "a.yarn" (nodes ++ [ node "loop" 30 Nothing [ ( "end", JumpLink ), ( "end", JumpLink ) ] ])
                    |> .edges
                    |> Expect.equal
                        [ Between "start" "shop" OptionLink
                        , Between "start" "end" JumpLink
                        , Between "shop" "end" JumpLink
                        , Outside "end" "far" JumpLink
                        , Between "loop" "end" JumpLink
                        ]
        , test "全部のノードが position: を持っていればそれを使う(左上を 0, 0 に寄せる)" <|
            \_ ->
                TalkGraph.build "a.yarn"
                    [ node "start" 1 (Just ( 100, 50 )) []
                    , node "shop" 10 (Just ( 300, -50 )) []
                    ]
                    |> .placed
                    |> List.map (\p -> ( p.node.title, p.x, p.y ))
                    |> Expect.equal [ ( "start", 0, 100 ), ( "shop", 200, 0 ) ]
        , test "どれも入りがある輪だけなら、最初のノードから並べる" <|
            \_ ->
                TalkGraph.build "a.yarn" [ node "a" 1 Nothing [ ( "b", JumpLink ) ], node "b" 5 Nothing [ ( "a", JumpLink ) ] ]
                    |> .placed
                    |> List.map (\p -> ( p.node.title, p.x == 0 ))
                    |> Expect.equal [ ( "a", True ), ( "b", False ) ]
        ]
