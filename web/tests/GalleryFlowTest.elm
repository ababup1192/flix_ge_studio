module GalleryFlowTest exposing (suite)

{-| ギャラリーの焼き・祝福の「規則」のテスト。

見た目(板の文言・並び)は焼かない — ここで守るのは、
祝福の歩きで何が選ばれ何が送られるか、ログ取りをいつ回すか、という規則だけ。

-}

import Dict
import Expect
import Gallery
import Json.Decode as D
import Progress
import Test exposing (Test, describe, test)


{-| diff 判定を流し込んだ初期状態(DIFF 2 枚: a.png, b.png / OK 1 枚)。 -}
withDiff : Gallery.Model
withDiff =
    Gallery.init
        |> Gallery.gotDiff
            (Dict.fromList
                [ ( "a.png", Gallery.StatusDiff )
                , ( "b.png", Gallery.StatusDiff )
                , ( "ok.png", Gallery.StatusOk )
                ]
            )


step : Gallery.Msg -> ( Gallery.Model, Gallery.Out ) -> ( Gallery.Model, Gallery.Out )
step msg ( model, _ ) =
    Gallery.update msg model


begin : Gallery.Model -> ( Gallery.Model, Gallery.Out )
begin model =
    ( model, Gallery.OutNone )


suite : Test
suite =
    describe "ギャラリーの焼きと祝福"
        [ describe "祝福の歩き(選び)"
            [ test "全部祝福すると DIFF の名前が全部送られる" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BlessOpened
                        |> step Gallery.BlessAccepted
                        |> step Gallery.BlessAccepted
                        |> Tuple.second
                        |> Expect.equal (Gallery.OutBless [ "a.png", "b.png" ])
            , test "保留した絵は送られない" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BlessOpened
                        |> step Gallery.BlessHeld
                        |> step Gallery.BlessAccepted
                        |> Tuple.second
                        |> Expect.equal (Gallery.OutBless [ "b.png" ])
            , test "全部保留なら何も送らない" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BlessOpened
                        |> step Gallery.BlessHeld
                        |> step Gallery.BlessHeld
                        |> Tuple.second
                        |> Expect.equal Gallery.OutNone
            , test "途中でやめると選んだ分も送らない" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BlessOpened
                        |> step Gallery.BlessAccepted
                        |> step Gallery.BlessQuit
                        |> Tuple.second
                        |> Expect.equal Gallery.OutNone
            , test "DIFF が無ければ歩きは始まらない" <|
                \_ ->
                    begin Gallery.init
                        |> step Gallery.BlessOpened
                        |> Tuple.first
                        |> .bless
                        |> Expect.equal Nothing
            , test "祝福が済むと golden のキャッシュ避けが進む" <|
                \_ ->
                    Gallery.blessDone withDiff
                        |> .goldenBust
                        |> Expect.equal (withDiff.goldenBust + 1)
            ]
        , describe "焼きとログ取り(いつ回すか)"
            [ test "焼く押下で選んだ的が送られる" <|
                \_ ->
                    begin withDiff
                        |> step (Gallery.TargetChosen Gallery.TargetPrologue)
                        |> step Gallery.BakeClicked
                        |> Tuple.second
                        |> Expect.equal (Gallery.OutBake Gallery.TargetPrologue)
            , test "202 待ちの間はまだ回さない(前の走りの残りログと混ぜない)" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BakeClicked
                        |> Tuple.first
                        |> Gallery.isPolling
                        |> Expect.equal False
            , test "受理されたら回し始める" <|
                \_ ->
                    begin withDiff
                        |> step Gallery.BakeClicked
                        |> Tuple.first
                        |> Gallery.bakeAccepted
                        |> Gallery.isPolling
                        |> Expect.equal True
            , test "走っている間の二度目の押下は送られない" <|
                \_ ->
                    begin (Gallery.bakeAccepted withDiff)
                        |> step Gallery.BakeClicked
                        |> Tuple.second
                        |> Expect.equal Gallery.OutNone
            , test "exit 0 で止まり、焼き上がり(True)を告げる" <|
                \_ ->
                    let
                        ( model, finished ) =
                            Gallery.gotRunnerLog
                                { running = False, exitCode = Just 0, lines = [ "done" ] }
                                (Gallery.bakeAccepted withDiff)
                    in
                    ( Gallery.isPolling model, finished )
                        |> Expect.equal ( False, Just True )
            , test "exit 非 0 で止まり、失敗(False)を告げる" <|
                \_ ->
                    let
                        ( model, finished ) =
                            Gallery.gotRunnerLog
                                { running = False, exitCode = Just 2, lines = [ "boom" ] }
                                (Gallery.bakeAccepted withDiff)
                    in
                    ( Gallery.isPolling model, finished )
                        |> Expect.equal ( False, Just False )
            , test "走行中のログは回し続ける(終わりを告げない)" <|
                \_ ->
                    let
                        ( model, finished ) =
                            Gallery.gotRunnerLog
                                { running = True, exitCode = Nothing, lines = [ "…" ] }
                                (Gallery.bakeAccepted withDiff)
                    in
                    ( Gallery.isPolling model, finished )
                        |> Expect.equal ( True, Nothing )
            , test "走っていない時に届いた古いログは無視する" <|
                \_ ->
                    Gallery.gotRunnerLog
                        { running = False, exitCode = Just 0, lines = [] }
                        withDiff
                        |> Expect.equal ( withDiff, Nothing )
            , test "404 の焼き口は『準備中』に倒れて止まる" <|
                \_ ->
                    let
                        model =
                            Gallery.bakeFailed "HTTP 404: /gallery/bake" withDiff
                    in
                    ( model.runner, Gallery.isPolling model )
                        |> Expect.equal ( Gallery.RunnerUnavailable, False )
            ]
        , describe "焼きログの進捗ミニパネル(展開の規則)"
            [ test "既定ではログ全文は開かない" <|
                \_ ->
                    (Gallery.bakeAccepted withDiff).logExpanded
                        |> Expect.equal False
            , test "⤢ の切り替えで全文が開く" <|
                \_ ->
                    begin (Gallery.bakeAccepted withDiff)
                        |> step Gallery.LogToggled
                        |> Tuple.first
                        |> .logExpanded
                        |> Expect.equal True
            , test "失敗の時だけは自動で全文が開く(隠さない)" <|
                \_ ->
                    Gallery.gotRunnerLog
                        { running = False, exitCode = Just 2, lines = [ "boom" ] }
                        (Gallery.bakeAccepted withDiff)
                        |> Tuple.first
                        |> .logExpanded
                        |> Expect.equal True
            , test "lines が増えたら畳んだ抜粋は末尾に追随する(先頭で止まらない)" <|
                \_ ->
                    ( Progress.excerpt [ "a", "b", "c" ]
                    , Progress.excerpt [ "a", "b", "c", "d", "e" ]
                    )
                        |> Expect.equal ( [ "a", "b", "c" ], [ "c", "d", "e" ] )
            ]
        , describe "JSON 橋渡し(runnerLog)"
            [ test "壊れた JSON でも既定値に倒れる" <|
                \_ ->
                    D.decodeString Gallery.runnerLogDecoder "{}"
                        |> Expect.equal (Ok { running = False, exitCode = Nothing, lines = [] })
            , test "exitCode null は未終了として読む" <|
                \_ ->
                    D.decodeString Gallery.runnerLogDecoder
                        """{"running":true,"target":"bake","exitCode":null,"lines":["a"]}"""
                        |> Expect.equal (Ok { running = True, exitCode = Nothing, lines = [ "a" ] })
            ]
        ]
