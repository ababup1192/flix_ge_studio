module TalkSearchTest exposing (suite)

{-| 会話のシナリオを字で探す決まりのテスト(行・文字位置・ノードの引き方と、次 / 前の回り方)。 -}

import Expect
import TalkSearch
import Test exposing (Test, describe, test)


body : String
body =
    "title: op\n---\ngirl: Hello\nme: hello hello\n===\ntitle: end\n---\ngirl: bye\n==="


suite : Test
suite =
    describe "TalkSearch"
        [ test "大文字と小文字を区別せず、行と文字位置とノードを返す" <|
            \_ ->
                TalkSearch.inText "hello" "a.yarn" body
                    |> List.map (\h -> ( h.line, ( h.from, h.to ), h.node ))
                    |> Expect.equal
                        [ ( 3, ( 20, 25 ), Just "op" )
                        , ( 4, ( 30, 35 ), Just "op" )
                        , ( 4, ( 36, 41 ), Just "op" )
                        ]
        , test "文字位置は本文の字をそのまま指す" <|
            \_ ->
                TalkSearch.inText "bye" "a.yarn" body
                    |> List.map (\h -> String.slice h.from h.to body)
                    |> Expect.equal [ "bye" ]
        , test "=== の後はノードの外(次の title: から次のノード)" <|
            \_ ->
                TalkSearch.inText "title" "a.yarn" body
                    |> List.map .node
                    |> Expect.equal [ Just "op", Just "end" ]
        , test "空の字では探さない" <|
            \_ -> TalkSearch.inText "" "a.yarn" body |> Expect.equal []
        , test "横断ではファイルの並びのまま並べ、どのファイルかを持つ" <|
            \_ ->
                TalkSearch.across "bye" [ ( "a.yarn", body ), ( "b.yarn", "x\nbye bye" ) ]
                    |> List.map (\h -> ( h.file, h.line ))
                    |> Expect.equal [ ( "a.yarn", 8 ), ( "b.yarn", 2 ), ( "b.yarn", 2 ) ]
        , test "カーソルの行から下の一番近い所。下に無ければ先頭へ回る" <|
            \_ ->
                let
                    hits =
                        TalkSearch.inText "hello" "a.yarn" body
                in
                ( TalkSearch.firstFrom 4 hits, TalkSearch.firstFrom 9 hits ) |> Expect.equal ( 1, 0 )
        , test "次・前は端で反対の端へ回る" <|
            \_ ->
                let
                    hits =
                        TalkSearch.inText "hello" "a.yarn" body
                in
                ( TalkSearch.step 1 2 hits, TalkSearch.step -1 0 hits, TalkSearch.step 1 0 [] ) |> Expect.equal ( 0, 2, 0 )
        ]
