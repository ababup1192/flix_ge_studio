module EditHistoryTest exposing (suite)

{-| 元に戻す / やり直すの骨組み。

固定するのは 2 つだけ: 「どの手の逆がどの編集になるか」と「積む・戻す・やり直す・
切るで履歴がどう動くか」。Payload は E.Value を含む(== に掛けられない)ので、
比較は 1 行の文字列に畳んでから行う。

-}

import Edit exposing (Op(..), Payload, Seg(..))
import EditHistory
import Expect
import Json.Encode as E
import Test exposing (Test, describe, test)


payloadText : Payload -> String
payloadText payload =
    opName payload.op ++ " " ++ Edit.pathKey payload.path ++ " = " ++ E.encode 0 payload.value


{-| 手の中身(ファイルをまたぐ列)を 1 行に。単一ファイルの手は 1 件だけ並ぶ。 -}
stepsText : List EditHistory.Step -> String
stepsText steps =
    steps
        |> List.map (\step -> step.file ++ ": " ++ payloadText step.payload)
        |> String.join " / "


opName : Op -> String
opName op =
    case op of
        SetOp ->
            "set"

        AppendOp ->
            "append"

        RemoveOp ->
            "remove"

        BatchSetOp ->
            "batch"


set : List Seg -> E.Value -> Payload
set path value =
    { op = SetOp, path = path, value = value, isInt = False }


append : List Seg -> E.Value -> Payload
append path value =
    { op = AppendOp, path = path, value = value, isInt = False }


remove : List Seg -> Payload
remove path =
    { op = RemoveOp, path = path, value = E.null, isInt = False }


spawn : Int -> E.Value
spawn atX =
    E.object [ ( "atX", E.int atX ) ]


pushed : String -> Maybe String -> Payload -> EditHistory.Before -> EditHistory.History -> EditHistory.History
pushed file group payload before =
    EditHistory.push
        { file = file, label = "編集", group = group, payload = payload, before = before }


suite : Test
suite =
    describe "EditHistory — 元に戻すの骨組み"
        [ describe "逆操作の導出"
            [ test "set(元の値あり)の逆は、その場所へ旧値を書く set" <|
                \() ->
                    EditHistory.inverse
                        (set [ KeySeg "meta", KeySeg "scrollSpeed" ] (E.int 72))
                        (EditHistory.Value (Just (E.int 60)))
                        |> Maybe.map payloadText
                        |> Expect.equal (Just "set meta/scrollSpeed = 60")
            , test "set(キーが無かった)の逆は remove — 書く前の「無い」状態へ戻す" <|
                \() ->
                    EditHistory.inverse
                        (set [ KeySeg "meta", KeySeg "tint" ] (E.string "#fff"))
                        (EditHistory.Value Nothing)
                        |> Maybe.map payloadText
                        |> Expect.equal (Just "remove meta/tint = null")
            , test "append の逆は、足す前の長さを添字にした remove(末尾 1 件だけ消す)" <|
                \() ->
                    EditHistory.inverse
                        (append [ KeySeg "spawns" ] (spawn 400))
                        (EditHistory.Array [ spawn 100, spawn 200 ])
                        |> Maybe.map payloadText
                        |> Expect.equal (Just "remove spawns/2 = null")
            , test "remove(キー)の逆は、そのキーへ旧値を書く set" <|
                \() ->
                    EditHistory.inverse
                        (remove [ KeySeg "routes", KeySeg "slow" ])
                        (EditHistory.Value (Just (E.object [ ( "speed", E.int 60 ) ])))
                        |> Maybe.map payloadText
                        |> Expect.equal (Just """set routes/slow = {"speed":60}""")
            , test "remove(配列の途中)の逆は、その配列を丸ごと書き戻す set" <|
                \() ->
                    EditHistory.inverse
                        (remove [ KeySeg "spawns", IdxSeg 1 ])
                        (EditHistory.Array [ spawn 100, spawn 200, spawn 300 ])
                        |> Maybe.map payloadText
                        |> Expect.equal (Just """set spawns = [{"atX":100},{"atX":200},{"atX":300}]""")
            , test "batch の逆は、同じ場所を旧値へ戻す batch(旧値の無い場所は null)" <|
                \() ->
                    EditHistory.inverse
                        { op = BatchSetOp, path = [ KeySeg "drops" ], value = E.list identity [], isInt = False }
                        (EditHistory.Batch
                            [ ( [ KeySeg "drops", KeySeg "coin" ], Just (E.int 70) )
                            , ( [ KeySeg "drops", KeySeg "gem" ], Nothing )
                            ]
                        )
                        |> Maybe.map payloadText
                        |> Expect.equal
                            (Just
                                ("batch drops = "
                                    ++ """[{"op":"set","path":["drops","coin"],"value":70,"intField":false},"""
                                    ++ """{"op":"set","path":["drops","gem"],"value":null,"intField":false}]"""
                                )
                            )
            , test "組み合わせの合わない旧値(append にキーの旧値など)は逆を作らない" <|
                \() ->
                    [ EditHistory.inverse (append [ KeySeg "spawns" ] (spawn 1)) (EditHistory.Value Nothing)
                    , EditHistory.inverse (remove [ KeySeg "spawns", IdxSeg 0 ]) (EditHistory.Value Nothing)

                    -- 末尾が添字でない remove に配列の旧値は噛み合わない
                    , EditHistory.inverse (remove [ KeySeg "spawns" ]) (EditHistory.Array [])
                    ]
                        |> List.map (Maybe.map payloadText)
                        |> Expect.equal [ Nothing, Nothing, Nothing ]
            ]
        , describe "積む・戻す・やり直す"
            [ test "積んだ順に戻り、戻した手はやり直しへ回る" <|
                \() ->
                    let
                        history =
                            EditHistory.empty
                                |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                                |> pushed "level.json" Nothing (set [ KeySeg "b" ] (E.int 20)) (EditHistory.Value (Just (E.int 10)))
                    in
                    case EditHistory.undo history of
                        Just ( first, afterFirst ) ->
                            case EditHistory.undo afterFirst of
                                Just ( second, afterSecond ) ->
                                    ( List.map stepsText [ first, second ]
                                    , EditHistory.depth afterSecond
                                    , EditHistory.undo afterSecond |> Maybe.map (\_ -> "まだ戻せる")
                                    )
                                        |> Expect.equal
                                            ( [ "level.json: set b = 10", "level.json: set a = 1" ], ( 0, 2 ), Nothing )

                                Nothing ->
                                    Expect.fail "2 手目が戻せない"

                        Nothing ->
                            Expect.fail "1 手目が戻せない"
            , test "やり直しは、戻したのと同じ手をもう一度流す" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> EditHistory.undo
                        |> Maybe.andThen (\( _, h ) -> EditHistory.redo h)
                        |> Maybe.map (\( steps, h ) -> ( stepsText steps, EditHistory.depth h ))
                        |> Expect.equal (Just ( "level.json: set a = 2", ( 1, 0 ) ))
            , test "戻した後に新しい手を積むと、やり直しの先は消える(枝分かれを持たない)" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> EditHistory.undo
                        |> Maybe.map Tuple.second
                        |> Maybe.map (pushed "level.json" Nothing (set [ KeySeg "c" ] (E.int 9)) (EditHistory.Value Nothing))
                        |> Maybe.map EditHistory.depth
                        |> Expect.equal (Just ( 1, 0 ))
            , test "同じ group の連続は 1 手に畳む(戻り先は最初の値)" <|
                \() ->
                    let
                        drag n old =
                            pushed "level.json" (Just "meta/scrollSpeed") (set [ KeySeg "speed" ] (E.int n)) (EditHistory.Value (Just (E.int old)))

                        history =
                            EditHistory.empty |> drag 61 60 |> drag 62 61 |> drag 63 62
                    in
                    ( EditHistory.depth history
                    , EditHistory.undo history |> Maybe.map (Tuple.first >> stepsText)
                    )
                        |> Expect.equal ( ( 1, 0 ), Just "level.json: set speed = 60" )
            , test "group が違えば別の手(畳まない)" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" (Just "a") (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> pushed "level.json" (Just "b") (set [ KeySeg "b" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> EditHistory.depth
                        |> Expect.equal ( 2, 0 )
            , test "逆を作れない手は積まず、それまでの履歴も切る(間違って戻さない)" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> pushed "level.json" Nothing (remove [ KeySeg "spawns", IdxSeg 0 ]) (EditHistory.Value Nothing)
                        |> (\h -> ( EditHistory.depth h, EditHistory.canUndo h ))
                        |> Expect.equal ( ( 0, 0 ), False )
            , test "別のファイルの手が来たら、前のファイルの履歴は捨てる" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> pushed "sprites.json" Nothing (set [ KeySeg "b" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> (\h -> ( h.file, EditHistory.depth h ))
                        |> Expect.equal ( Just "sprites.json", ( 1, 0 ) )
            , test "外から元データが入れ替わったら、戻すもやり直すも全部捨てる" <|
                \() ->
                    EditHistory.empty
                        |> pushed "level.json" Nothing (set [ KeySeg "a" ] (E.int 2)) (EditHistory.Value (Just (E.int 1)))
                        |> EditHistory.undo
                        |> Maybe.map Tuple.second
                        |> Maybe.map EditHistory.cutOnExternalChange
                        |> Maybe.map (\h -> ( EditHistory.depth h, EditHistory.canUndo h, EditHistory.canRedo h ))
                        |> Expect.equal (Just ( ( 0, 0 ), False, False ))
            ]
        , describe "ファイルをまたぐ 1 手(横断置換)"
            [ test "1 手で全ファイルぶんが戻る(戻す順は当てた順の逆)" <|
                \() ->
                    let
                        step file path old new =
                            { file = file
                            , payload = set [ KeySeg path ] (E.string new)
                            , before = EditHistory.Value (Just (E.string old))
                            }

                        history =
                            EditHistory.empty
                                |> EditHistory.pushCross
                                    { file = "a.json"
                                    , label = "置換"
                                    , steps =
                                        [ step "a.json" "title" "旧" "新"
                                        , step "b.json" "name" "旧" "新"
                                        ]
                                    }
                    in
                    ( EditHistory.depth history
                    , EditHistory.undo history |> Maybe.map (Tuple.first >> stepsText)
                    )
                        |> Expect.equal
                            ( ( 1, 0 )
                            , Just "b.json: set name = \"旧\" / a.json: set title = \"旧\""
                            )
            , test "やり直すと、当てた順のまま同じ編集が並ぶ" <|
                \() ->
                    EditHistory.empty
                        |> EditHistory.pushCross
                            { file = "a.json"
                            , label = "置換"
                            , steps =
                                [ { file = "a.json", payload = set [ KeySeg "t" ] (E.string "新"), before = EditHistory.Value (Just (E.string "旧")) }
                                , { file = "b.json", payload = set [ KeySeg "t" ] (E.string "新"), before = EditHistory.Value (Just (E.string "旧")) }
                                ]
                            }
                        |> EditHistory.undo
                        |> Maybe.andThen (\( _, h ) -> EditHistory.redo h)
                        |> Maybe.map (Tuple.first >> stepsText)
                        |> Expect.equal (Just "a.json: set t = \"新\" / b.json: set t = \"新\"")
            , test "逆を組めない中身が混ざった横断手は積まない(履歴ごと切る)" <|
                \() ->
                    EditHistory.empty
                        |> pushed "a.json" Nothing (set [ KeySeg "x" ] (E.int 1)) (EditHistory.Value (Just (E.int 0)))
                        |> EditHistory.pushCross
                            { file = "a.json"
                            , label = "置換"
                            , steps =
                                [ { file = "a.json", payload = remove [ KeySeg "s", IdxSeg 0 ], before = EditHistory.Value Nothing } ]
                            }
                        |> EditHistory.depth
                        |> Expect.equal ( 0, 0 )
            ]
        , test "batchPaths — BatchSet の中身から、旧値を控えるべき場所が読める" <|
            \() ->
                EditHistory.batchPaths
                    (E.list identity
                        [ E.object
                            [ ( "op", E.string "set" )
                            , ( "path", E.list identity [ E.string "drops", E.string "coin" ] )
                            , ( "value", E.int 70 )
                            , ( "intField", E.bool False )
                            ]
                        , E.object
                            [ ( "op", E.string "set" )
                            , ( "path", E.list identity [ E.string "spawns", E.int 2 ] )
                            , ( "value", E.int 1 )
                            , ( "intField", E.bool False )
                            ]
                        ]
                    )
                    |> List.map Edit.pathKey
                    |> Expect.equal [ "drops/coin", "spawns/2" ]
        ]
