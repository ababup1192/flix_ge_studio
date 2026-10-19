module TalkRunTest exposing (suite)

{-| 読み進める欄の流れ(TalkRun): 台詞・選択肢・<<set>>・<<if>>・{式}・visited・jump・stop。 -}

import Dict
import Expect
import Json.Decode as D
import TalkRun exposing (Entry(..), Status(..))
import TalkTree exposing (Value(..))
import Test exposing (Test, describe, test)


{-| Gate: 切符の数で分かれ、選択肢(行末の if で 1 つ選べない)を出し、選ぶと Shop へ jump。
Shop: 切符を 1 減らして {式} で言い、visited("Gate") を見て止まる。
-}
program : TalkRun.Program
program =
    D.decodeString TalkRun.programDecoder
        """{"version":2,
            "variables":[{"name":"$tickets","value":2,"file":"a.yarn","line":1},{"name":"$vip","value":false,"file":"a.yarn","line":2}],
            "nodes":[
              {"title":"Gate","body":[
                 {"kind":"if","line":3,"clauses":[
                    {"line":3,"condition":{"kind":">","args":[{"kind":"variable","name":"$tickets"},{"kind":"value","value":0}]},
                     "body":[{"kind":"line","line":4,"speaker":"guard","text":"通っていい"}]},
                    {"line":5,"body":[{"kind":"line","line":6,"speaker":"guard","text":"切符が無い"}]}]},
                 {"kind":"command","line":7,"name":"wait","args":["1"]},
                 {"kind":"options","line":8,"options":[
                    {"line":8,"text":"店へ","body":[{"kind":"jump","line":9,"node":"Shop"}]},
                    {"line":10,"text":"VIP 室","condition":{"kind":"variable","name":"$vip"},"body":[]},
                    {"line":11,"text":"帰る","body":[{"kind":"stop","line":12}]}]}]},
              {"title":"Shop","body":[
                 {"kind":"set","line":20,"name":"$tickets","expr":{"kind":"-","args":[{"kind":"variable","name":"$tickets"},{"kind":"value","value":1}]}},
                 {"kind":"line","line":21,"speaker":"clerk","text":"残り {$tickets} 枚",
                  "parts":[{"text":"残り "},{"expr":{"kind":"variable","name":"$tickets"}},{"text":" 枚"}]},
                 {"kind":"if","line":22,"clauses":[{"line":22,"condition":{"kind":"visited","name":"Gate"},
                    "body":[{"kind":"line","line":23,"speaker":"clerk","text":"門から来たね"}]}]},
                 {"kind":"jump","line":24,"node":"Nowhere"}]}]}"""
        |> Result.withDefault TalkRun.emptyProgram


said : TalkRun.Reading -> List String
said reading =
    reading.log
        |> List.filterMap
            (\e ->
                case e of
                    Said s ->
                        Just s.text

                    _ ->
                        Nothing
            )


suite : Test
suite =
    describe "TalkRun"
        [ test "選択肢で止まり、条件が偽の選択肢は選べないものとして並ぶ" <|
            \_ ->
                let
                    r =
                        TalkRun.start program Dict.empty "Gate"
                in
                ( said r, r.status )
                    |> Expect.equal
                        ( [ "通っていい" ]
                        , Choosing
                            [ { line = 8, text = "店へ", available = True }
                            , { line = 10, text = "VIP 室", available = False }
                            , { line = 11, text = "帰る", available = True }
                            ]
                        )
        , test "変数の一覧の値で始めると、その枝を流す(選べなかった選択肢も選べる)" <|
            \_ ->
                let
                    r =
                        TalkRun.start program (TalkRun.overridesFrom program (Dict.fromList [ ( "$tickets", "0" ), ( "$vip", "true" ) ])) "Gate"
                in
                ( said r
                , case r.status of
                    Choosing choices ->
                        List.map .available choices

                    _ ->
                        []
                )
                    |> Expect.equal ( [ "切符が無い" ], [ True, True, True ] )
        , test "選ぶと続きへ: <<set>> と {式} と visited(門を出た数)を解き、知らないノードへの jump で理由を出す" <|
            \_ ->
                let
                    r =
                        TalkRun.start program Dict.empty "Gate" |> TalkRun.choose program 0
                in
                ( said r, TalkRun.valueOf "$tickets" r, r.status )
                    |> Expect.equal ( [ "通っていい", "残り 1 枚", "門から来たね" ], Just (Number 1), Broken "ノード Nowhere がありません" )
        , test "選べない番号は何も変えない・<<stop>> で終わる" <|
            \_ ->
                let
                    r =
                        TalkRun.start program Dict.empty "Gate"
                in
                ( TalkRun.choose program 1 r == r, (TalkRun.choose program 2 r).status )
                    |> Expect.equal ( True, Ended )
        , test "コマンドは流さずに欄に出す" <|
            \_ ->
                TalkRun.start program Dict.empty "Gate"
                    |> .log
                    |> List.member (Commanded { line = 7, text = "<<wait 1>>" })
                    |> Expect.equal True
        ]
