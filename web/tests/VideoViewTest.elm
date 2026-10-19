module VideoViewTest exposing (suite)

{-| 動画の面の規則のテスト。

守るのは規則だけ: 道具の答えの読み分け(使えない・この Mac に置き場が無い・撮れていない)、
音の表の読みと同じコマに 2 回の数え、流し直しを二重に頼まないこと、音の表を 1 回だけ取りに行くこと。
カードの見た目は焼かない。

-}

import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import VideoView


decode : String -> Result D.Error VideoView.Listing
decode =
    D.decodeString VideoView.listDecoder


listed : VideoView.Model
listed =
    case decode """{"available":true,"here":true,"videos":[{"name":"clock","state":"ok","flow":"e2e/clock.flow","sfx":"debug/video/clock/sfx.txt","fps":30},{"name":"talk","state":"stuck"}]}""" of
        Ok listing ->
            VideoView.gotList listing VideoView.init

        Err _ ->
            VideoView.init


suite : Test
suite =
    describe "VideoView"
        [ test "道具が使えない答えは理由つきの Unavailable" <|
            \_ ->
                decode """{"available":false,"reason":"0.49.0 から"}"""
                    |> Expect.equal (Ok (VideoView.Unavailable "0.49.0 から"))
        , test "置き場の無い Mac は here = False(「この Mac では撮っていません」の元)" <|
            \_ ->
                decode """{"available":true,"here":false,"videos":[]}"""
                    |> Expect.equal (Ok (VideoView.Listed { here = False, videos = [] }))
        , test "running のまま make が居ない記録は Stuck として読む" <|
            \_ ->
                case decode """{"available":true,"here":true,"videos":[{"name":"a","state":"stuck"}]}""" of
                    Ok (VideoView.Listed { videos }) ->
                        List.map .state videos |> Expect.equal [ VideoView.Stuck ]

                    _ ->
                        Expect.fail "読めない"
        , test "知らない state は記録なし(撮れていないとは言わない)" <|
            \_ ->
                case decode """{"available":true,"videos":[{"name":"a","state":"???"}]}""" of
                    Ok (VideoView.Listed { videos }) ->
                        List.map .state videos |> Expect.equal [ VideoView.NoRecord ]

                    _ ->
                        Expect.fail "読めない"
        , test "sfx.txt はコマ番号と名前の並び(# の行と空行は飛ばす)" <|
            \_ ->
                VideoView.parseSfx "# fps 30\n12 hit\n\n30 swish loud\n"
                    |> Expect.equal [ ( 12, "hit" ), ( 30, "swish" ) ]
        , test "同じコマに同じ音が 2 回鳴ったら 1 つの印に数 2" <|
            \_ ->
                VideoView.marksOf [ ( 12, "hit" ), ( 12, "hit" ), ( 30, "swish" ) ]
                    |> Expect.equal [ ( 12, "hit", 2 ), ( 30, "swish", 1 ) ]
        , test "流している最中にもう 1 回押しても頼み直さない" <|
            \_ ->
                let
                    ( once, _ ) =
                        VideoView.update (VideoView.RerunClicked "clock") listed

                    ( _, out ) =
                        VideoView.update (VideoView.RerunClicked "clock") once
                in
                out |> Expect.equal VideoView.OutNone
        , test "流し直しが断られたらカードに理由を残す" <|
            \_ ->
                let
                    ( running, _ ) =
                        VideoView.update (VideoView.RerunClicked "clock") listed
                in
                VideoView.gotRerun "clock" (Err "ゲームが起動していません") running
                    |> .cards
                    |> Expect.equal (Dict.singleton "clock" (VideoView.RerunFailed "ゲームが起動していません"))
        , test "再生を開いた時、音の表はまだ無ければ 1 回だけ取りに行く" <|
            \_ ->
                let
                    ( opened, first ) =
                        VideoView.update (VideoView.CardClicked "clock") listed

                    ( _, second ) =
                        VideoView.update (VideoView.CardClicked "clock") (VideoView.gotSfx "clock" "1 a" opened)
                in
                ( first, second ) |> Expect.equal ( VideoView.OutFetchSfx "clock" "debug/video/clock/sfx.txt", VideoView.OutNone )
        ]

