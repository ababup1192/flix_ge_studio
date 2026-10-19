module TalkTreeTest exposing (suite)

{-| 組んだ会話の木(dialogue.json)の読み: 変数の一覧とノードの図の元。 -}

import Expect
import Json.Decode as D
import TalkTree exposing (LinkKind(..), Value(..))
import Test exposing (Test, describe, test)


tree : String
tree =
    """{"version":2,
        "variables":[{"name":"gold","value":0,"file":"a.yarn","line":3},
                     {"name":"met","value":false,"file":"a.yarn","line":4},
                     {"name":"who","value":"girl","file":"a.yarn","line":5}],
        "nodes":[
          {"title":"start","file":"a.yarn","line":1,"headers":[{"key":"position","value":"10, -20"}],
           "body":[{"kind":"line","line":6,"text":"やあ"},
                   {"kind":"options","line":7,"options":[
                      {"line":7,"text":"行く","body":[{"kind":"jump","line":8,"node":"shop"}]},
                      {"line":9,"text":"帰る","body":[]}]},
                   {"kind":"if","line":10,"clauses":[{"line":10,"body":[{"kind":"jump","line":11,"node":"secret"}]}]},
                   {"kind":"jump","line":12,"node":"end"}]},
          {"title":"shop","file":"a.yarn","line":20,"headers":[],"body":[]}]}"""


suite : Test
suite =
    describe "TalkTree"
        [ test "変数は名前・初めの値(数・真偽・字)・書いた行" <|
            \_ ->
                D.decodeString TalkTree.decoder tree
                    |> Result.map (.variables >> List.map (\v -> ( v.name, v.value )))
                    |> Expect.equal (Ok [ ( "gold", Number 0 ), ( "met", Flag False ), ( "who", Text "girl" ) ])
        , test "ノードの矢印は本文の jump・選択肢の中の jump・if の中の jump(深く見る)" <|
            \_ ->
                D.decodeString TalkTree.decoder tree
                    |> Result.map (.nodes >> List.map (\n -> ( n.title, List.map (\l -> ( l.target, l.kind )) n.links )))
                    |> Expect.equal
                        (Ok
                            [ ( "start", [ ( "shop", OptionLink ), ( "secret", JumpLink ), ( "end", JumpLink ) ] )
                            , ( "shop", [] )
                            ]
                        )
        , test "ヘッダの position: を位置に読む(無ければ Nothing)" <|
            \_ ->
                D.decodeString TalkTree.decoder tree
                    |> Result.map (.nodes >> List.map .position)
                    |> Expect.equal (Ok [ Just ( 10, -20 ), Nothing ])
        , test "値の字は始まりの引数と同じ書き方" <|
            \_ ->
                List.map TalkTree.valueText [ Number 1.5, Number 3, Flag True, Text "girl" ]
                    |> Expect.equal [ "1.5", "3", "true", "girl" ]
        , test "木はシナリオと同じフォルダの dialogue.json" <|
            \_ ->
                TalkTree.treePathOf "assets/talk/opening.yarn" |> Expect.equal "assets/talk/dialogue.json"
        ]
