module EngineUpdateFlowTest exposing (suite)

{-| engine の差し替えの帯: 出る条件・押した後の姿・終わり方。

出るのは「新しいバージョンが出ていて、この環境で押せる」ときだけ。
押したら進み具合になり、終わったらバージョンを引き直す。

-}

import EngineUpdate
import Expect
import Json.Encode as E
import Main
import Test exposing (Test, describe, test)


response : String -> List ( String, E.Value ) -> E.Value
response kind body =
    E.object
        [ ( "id", E.int 1 )
        , ( "kind", E.string kind )
        , ( "ok", E.bool True )
        , ( "body", E.object body )
        ]


failure : String -> String -> E.Value
failure kind message =
    E.object
        [ ( "id", E.int 1 )
        , ( "kind", E.string kind )
        , ( "ok", E.bool False )
        , ( "body", E.object [ ( "message", E.string message ) ] )
        ]


offered : Main.Model
offered =
    let
        ( m0, _ ) =
            Main.init ()

        ( m1, _ ) =
            Main.update
                (Main.GotApiResponse
                    (response "engineUpdateCheck"
                        [ ( "ok", E.bool True )
                        , ( "current", E.string "0.31.0" )
                        , ( "available", E.bool True )
                        , ( "version", E.string "0.32.0" )
                        , ( "updatable", E.bool True )
                        ]
                    )
                )
                m0
    in
    m1


suite : Test
suite =
    describe "engine の差し替えの帯"
        [ test "新しいバージョンの知らせで押せる姿になる" <|
            \() ->
                ( offered.engineUpdate.step, EngineUpdate.pendingVersion offered.engineUpdate )
                    |> Expect.equal ( EngineUpdate.Offered, Just "0.32.0" )
        , test "押せない環境 (Windows・engine のリポで開発中) では押させない" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    ( m1, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineUpdateCheck"
                                    [ ( "available", E.bool True )
                                    , ( "version", E.string "0.32.0" )
                                    , ( "updatable", E.bool False )
                                    ]
                                )
                            )
                            m0
                in
                EngineUpdate.pendingVersion m1.engineUpdate
                    |> Expect.equal Nothing
        , test "押すと進み具合の姿になり、二度押しの口が閉じる" <|
            \() ->
                let
                    ( after, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered
                in
                ( after.engineUpdate.step, EngineUpdate.pendingVersion after.engineUpdate )
                    |> Expect.equal ( EngineUpdate.Working, Nothing )
        , test "走っている間はログを引き続け、終わったら止まる" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered

                    ( done, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineUpdateLog"
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 0 )
                                    , ( "lines", E.list E.string [ "[update] engine 0.32.0 に切り替えました" ] )
                                    ]
                                )
                            )
                            working
                in
                ( EngineUpdate.isPolling working.engineUpdate
                , EngineUpdate.isPolling done.engineUpdate
                , done.engineUpdate.step
                )
                    |> Expect.equal ( True, False, EngineUpdate.Finished )
        , test "終わりの知らせは最後の行を残す (ゲームの追随が要るかがそこに出る)" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered

                    ( done, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineUpdateLog"
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 0 )
                                    , ( "lines"
                                      , E.list E.string
                                            [ "[update] engine 0.32.0 に切り替えました"
                                            , "[update] 開いているゲームは追随が要ります (直下の UPDATE_PLAN.md)"
                                            ]
                                      )
                                    ]
                                )
                            )
                            working
                in
                done.engineUpdate.note
                    |> Expect.equal "[update] 開いているゲームは追随が要ります (直下の UPDATE_PLAN.md)"
        , test "始まった気配が無いまま引き続けたら止める (サーバが立ち上がり直した後)" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered

                    empty =
                        Main.GotApiResponse
                            (response "engineUpdateLog"
                                [ ( "running", E.bool False ), ( "lines", E.list E.string [] ) ]
                            )

                    after =
                        List.foldl (\msg m -> Tuple.first (Main.update msg m))
                            working
                            (List.repeat 11 empty)
                in
                ( after.engineUpdate.step, EngineUpdate.isPolling after.engineUpdate )
                    |> Expect.equal ( EngineUpdate.Failed, False )
        , test "始める口が断られたら理由をそのまま出す (走っている最中の 409 も含む)" <|
            \() ->
                let
                    ( after, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (failure "engineUpdateStart"
                                    "HTTP 409: /engine/update — engine を差し替えている最中です"
                                )
                            )
                            offered
                in
                ( after.engineUpdate.step, String.contains "差し替えている最中" after.engineUpdate.note )
                    |> Expect.equal ( EngineUpdate.Failed, True )
        , test "見に行く口が無いサーバでは帯を出さない" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    ( after, _ ) =
                        Main.update
                            (Main.GotApiResponse (failure "engineUpdateCheck" "HTTP 404: /engine/update/check"))
                            m0
                in
                after.engineUpdate.step
                    |> Expect.equal EngineUpdate.Idle
        , test "差し替えに失敗したら理由の姿で止まる" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered

                    ( failed, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineUpdateLog"
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 1 )
                                    , ( "lines", E.list E.string [ "!! 照合の相手が揃っていません" ] )
                                    ]
                                )
                            )
                            working
                in
                ( failed.engineUpdate.step, failed.engineUpdate.note )
                    |> Expect.equal ( EngineUpdate.Failed, "!! 照合の相手が揃っていません" )
        , test "engine は最新でも、ゲームが古ければそろえるボタンを出す" <|
            \() ->
                ( behindGame.engineUpdate.gameLag
                , EngineUpdate.canUpgradeGame behindGame.engineUpdate
                )
                    |> Expect.equal ( Just "0.31.0", True )
        , test "ゲームが追いついていればボタンを出さない" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    ( after, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineGameCheck"
                                    [ ( "engine", E.string "0.32.0" )
                                    , ( "game", E.string "0.32.0" )
                                    , ( "behind", E.bool False )
                                    ]
                                )
                            )
                            m0
                in
                EngineUpdate.canUpgradeGame after.engineUpdate
                    |> Expect.equal False
        , test "engine を差し替えた後でも、次に開いたゲームが古ければボタンが出る" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update (Main.EngineUpdateClicked "0.32.0") offered

                    ( finished, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineUpdateLog"
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 0 )
                                    , ( "lines", E.list E.string [ "[update] engine 0.32.0 に切り替えました" ] )
                                    ]
                                )
                            )
                            working

                    ( after, _ ) =
                        Main.update
                            (Main.GotApiResponse
                                (response "engineGameCheck"
                                    [ ( "engine", E.string "0.32.0" )
                                    , ( "game", E.string "0.31.0" )
                                    , ( "behind", E.bool True )
                                    ]
                                )
                            )
                            finished
                in
                ( after.engineUpdate.step, EngineUpdate.canUpgradeGame after.engineUpdate )
                    |> Expect.equal ( EngineUpdate.Finished, True )
        , test "そろえるを押すと進み具合になり、同じログ口を引き始める" <|
            \() ->
                let
                    ( working, _ ) =
                        Main.update Main.EngineGameUpgradeClicked behindGame
                in
                ( working.engineUpdate.step, EngineUpdate.isPolling working.engineUpdate )
                    |> Expect.equal ( EngineUpdate.Working, True )
        ]


{-| engine は最新で、開いているゲームだけが古い姿。
-}
behindGame : Main.Model
behindGame =
    let
        ( m0, _ ) =
            Main.init ()

        ( m1, _ ) =
            Main.update
                (Main.GotApiResponse
                    (response "engineGameCheck"
                        [ ( "engine", E.string "0.32.0" )
                        , ( "game", E.string "0.31.0" )
                        , ( "behind", E.bool True )
                        ]
                    )
                )
                m0
    in
    m1
