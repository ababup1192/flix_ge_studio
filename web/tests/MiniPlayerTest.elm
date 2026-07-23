module MiniPlayerTest exposing (suite)

{-| ミニプレイヤーの「どの場面を映すか」の規則(純関数)。

チップの並びやオーバーレイ等の見た目は検査しない — 演出は目視の仕事。
サーバ(Changes)は新しい変化を列の末尾へ積む契約なので、自動は末尾を追う。

-}

import Expect
import Journey
import Main
import Test exposing (Test, describe, test)


change : String -> Int -> Journey.Change
change name ver =
    { name = name
    , ver = ver
    , before = "golden/archive/" ++ name
    , after = "golden/" ++ name
    }


suite : Test
suite =
    describe "ミニプレイヤーの場面選び"
        [ test "自動は知らせの最新(列の末尾)の場面を追う" <|
            \() ->
                Main.miniShownScene
                    { pin = Nothing
                    , changes = [ change "forest.png" 1, change "title.png" 2 ]
                    , scenes = [ "forest.png", "title.png", "cave.png" ]
                    }
                    |> Expect.equal (Just "title.png")
        , test "ピン留め中は知らせが進んでも動かない" <|
            \() ->
                Main.miniShownScene
                    { pin = Just "cave.png"
                    , changes = [ change "forest.png" 1, change "title.png" 2 ]
                    , scenes = [ "forest.png", "title.png", "cave.png" ]
                    }
                    |> Expect.equal (Just "cave.png")
        ]
