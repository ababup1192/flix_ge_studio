module MiniPlayerTest exposing (suite)

{-| ミニプレイヤーの「どの場面を映すか」の規則(純関数)と、
プロジェクトをまたぐ混線の防ぎ(URL の区別・切り替え時の初期化)。

チップの並びやオーバーレイ等の見た目は検査しない — 演出は目視の仕事。
サーバ(Changes)は新しい変化を列の末尾へ積む契約なので、自動は末尾を追う。

-}

import Expect
import Journey
import Json.Encode as E
import Main
import SceneView
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


change : String -> Int -> Journey.Change
change name ver =
    { name = name
    , ver = ver
    , before = "golden/archive/" ++ name
    , after = "golden/" ++ name
    }


{-| 下段の見た目だけを見たいので、押した先は問わない。 -}
handlers : SceneView.Handlers ()
handlers =
    { onToggle = ()
    , onScene = \_ -> ()
    , onZoomOpen = \_ -> ()
    , onZoomClosed = ()
    , onStart = ()
    , onStop = ()
    }


openWith : { running : Bool, starting : Bool, failed : Bool } -> SceneView.State
openWith flags =
    { open = True
    , swapNotice = False
    , scenes = []
    , pin = Nothing
    , changes = []
    , baking = False
    , zoom = Nothing
    , refresh = 0
    , serverBase = ""
    , root = "/Users/me/Desktop/flix_ge_village"
    , running = flags.running
    , starting = flags.starting
    , failed = flags.failed
    }


suite : Test
suite =
    describe "ミニプレイヤーの場面選び"
        [ test "自動は知らせの最新(列の末尾)の場面を追う" <|
            \() ->
                SceneView.shownScene
                    { pin = Nothing
                    , changes = [ change "forest.png" 1, change "title.png" 2 ]
                    , scenes = [ "forest.png", "title.png", "cave.png" ]
                    }
                    |> Expect.equal (Just "title.png")
        , test "ピン留め中は知らせが進んでも動かない" <|
            \() ->
                SceneView.shownScene
                    { pin = Just "cave.png"
                    , changes = [ change "forest.png" 1, change "title.png" 2 ]
                    , scenes = [ "forest.png", "title.png", "cave.png" ]
                    }
                    |> Expect.equal (Just "cave.png")
        , test "絵の URL はプロジェクトごとに異なる(定番名 title.png でもキャッシュが混ざらない)" <|
            \() ->
                SceneView.galleryImageUrl "" "/Users/me/Desktop/flix_ge_village" "golden" "title.png"
                    |> Expect.notEqual
                        (SceneView.galleryImageUrl "" "/Users/me/Desktop/flix_ge_dungeon" "golden" "title.png")
        , test "プロジェクト切り替えでピン・場面一覧・知らせが初期化される" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    before =
                        { m0
                            | miniPin = Just "title.png"
                            , miniScenes = [ "title.png", "forest.png" ]
                            , miniChanges = [ change "title.png" 3 ]
                        }

                    switched =
                        E.object
                            [ ( "id", E.int 9 )
                            , ( "kind", E.string "selectProject" )
                            , ( "ok", E.bool True )
                            , ( "body"
                              , E.object
                                    [ ( "ok", E.bool True )
                                    , ( "dir", E.string "/Users/me/Desktop/flix_ge_dungeon" )
                                    , ( "title", E.string "ダンジョン" )
                                    , ( "design", E.object [ ( "w", E.float 320 ), ( "h", E.float 240 ) ] )
                                    ]
                              )
                            ]

                    ( after, _ ) =
                        Main.update (Main.GotApiResponse switched) before
                in
                ( after.miniPin, after.miniScenes, after.miniChanges )
                    |> Expect.equal ( Nothing, [], [] )
        , test "保存応答の baking:true でその場でスピナー状態になる(ポーリングを待たない)" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    saved =
                        E.object
                            [ ( "id", E.int 6 )
                            , ( "kind", E.string "putFile" )
                            , ( "ok", E.bool True )
                            , ( "body"
                              , E.object
                                    [ ( "ok", E.bool True )
                                    , ( "mtime", E.int 999 )
                                    , ( "baking", E.bool True )
                                    ]
                              )
                            ]

                    ( after, _ ) =
                        Main.update (Main.GotApiResponse saved) { m0 | putReq = Just 6 }
                in
                after.changesBaking |> Expect.equal True
        , test "起動にしくじると、下段は黙って『▶ 起動する』に戻らず理由を出す" <|
            \() ->
                SceneView.view handlers (openWith { running = False, starting = False, failed = True })
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.text "起動できませんでした" ]
                        , Query.has [ Selector.text "↻ もう一度起動する" ]
                        , Query.hasNot [ Selector.text "▶ 起動する" ]
                        ]
        , test "しくじっていなければ下段はいつもの『▶ 起動する』" <|
            \() ->
                SceneView.view handlers (openWith { running = False, starting = False, failed = False })
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.text "▶ 起動する" ]
                        , Query.hasNot [ Selector.text "起動できませんでした" ]
                        ]
        ]
