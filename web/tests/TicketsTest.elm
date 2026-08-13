module TicketsTest exposing (suite)

{-| アノテーションチケットの規則のテスト。

守るのは規則だけ: 依頼文が一言とチケット内ファイルの場所を運ぶこと、
一言が空のうちは報告できないこと、書きかけの保存はフォーカスが外れた時だけ起きること。
パネルの見た目の演出は焼かない。

-}

import Expect
import Test exposing (Test, describe, test)
import Tickets


sample : Tickets.Ticket
sample =
    { id = "20260812-140312_my_game_f3412"
    , title = "アノテーション: my_game — frame 3412(2026-08-12 14:03)"
    , comment = ""
    , hasShot = True
    , isSketch = False
    }


{-| ラフと見比べて切った 1 枚(印だけが違う)。 -}
sketchSample : Tickets.Ticket
sketchSample =
    { sample | id = "20260812-140312_title_png_sketch", isSketch = True }


{-| 一覧が届いた状態から始める。 -}
loaded : Tickets.Model
loaded =
    Tickets.gotList [ sample ] Tickets.init


suite : Test
suite =
    describe "Tickets"
        [ describe "buildTicketPrompt"
            [ test "一言と、チケット内の README・スクショ・world.json の場所を運ぶ" <|
                \_ ->
                    let
                        prompt =
                            Tickets.buildTicketPrompt
                                { id = sample.id
                                , comment = "敵がジャンプ台の上に湧いて理不尽"
                                , isSketch = False
                                }
                    in
                    Expect.equal
                        [ True, True, True, True ]
                        [ String.contains "ひとこと: 敵がジャンプ台の上に湧いて理不尽" prompt
                        , String.contains ("debug/annotations/" ++ sample.id ++ "/README.md") prompt
                        , String.contains ("debug/annotations/" ++ sample.id ++ "/highlighted.png") prompt
                        , String.contains ("debug/annotations/" ++ sample.id ++ "/world.json") prompt
                        ]
            , test "直し先の判断は AI に委ねる一文を必ず載せる" <|
                \_ ->
                    Tickets.buildTicketPrompt { id = sample.id, comment = "一言", isSketch = False }
                        |> String.contains "直し先はあなたが選んでください"
                        |> Expect.equal True
            , test "ラフと見比べて切った 1 枚は、遊んだ時の手がかりでなく見比べた 2 枚を指す" <|
                \_ ->
                    let
                        prompt =
                            Tickets.buildTicketPrompt
                                { id = sketchSample.id, comment = "空が明るすぎる", isSketch = True }
                    in
                    Expect.equal
                        [ True, True, False ]
                        [ String.contains "見た目の直し" prompt
                        , String.contains ("debug/annotations/" ++ sketchSample.id ++ "/screenshot.png") prompt
                        , String.contains "world.json" prompt
                        ]
            ]
        , describe "報告のゲート"
            [ test "一言が空のままの報告は何も起こさない(依頼文を作らない)" <|
                \_ ->
                    Tickets.update (Tickets.ReportClicked sample) loaded
                        |> Tuple.second
                        |> Expect.equal Tickets.OutNone
            , test "一言を書いてから報告すると、その一言入りの依頼文コピーを頼む" <|
                \_ ->
                    let
                        ( edited, _ ) =
                            Tickets.update (Tickets.CommentEdited sample.id "コインが取れない") loaded
                    in
                    Tickets.update (Tickets.ReportClicked sample) edited
                        |> Tuple.second
                        |> Expect.equal
                            (Tickets.OutCopyPrompt
                                (Tickets.buildTicketPrompt
                                    { id = sample.id, comment = "コインが取れない", isSketch = False }
                                )
                            )
            ]
        , describe "一言の保存"
            [ test "フォーカスが外れた時、書きかけが保存済みと違えば保存を頼む(前後の空白は落とす)" <|
                \_ ->
                    let
                        ( edited, _ ) =
                            Tickets.update (Tickets.CommentEdited sample.id "  コインが取れない  ") loaded
                    in
                    Tickets.update (Tickets.CommentBlurred sample) edited
                        |> Tuple.second
                        |> Expect.equal
                            (Tickets.OutSaveComment { id = sample.id, comment = "コインが取れない" })
            , test "書きかけが保存済みと同じなら、フォーカスが外れても何も頼まない" <|
                \_ ->
                    Tickets.update (Tickets.CommentBlurred sample) loaded
                        |> Tuple.second
                        |> Expect.equal Tickets.OutNone
            ]
        , describe "一覧の取り直し"
            [ test "保存が一覧に映ったら書きかけは捨て、まだ違う書きかけは残す" <|
                \_ ->
                    let
                        savedTicket =
                            { sample | comment = "コインが取れない" }

                        ( edited, _ ) =
                            Tickets.update (Tickets.CommentEdited sample.id "コインが取れない") loaded

                        refreshed =
                            Tickets.gotList [ savedTicket ] edited
                    in
                    Tickets.update (Tickets.CommentBlurred savedTicket) refreshed
                        |> Tuple.second
                        |> Expect.equal Tickets.OutNone
            ]
        ]
