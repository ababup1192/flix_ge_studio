module TimelineTest exposing (suite)

{-| タイムライン(固定トラック)の宣言の読み・ドラッグ → 値の変換・表示ならしを
固定する。宣言の JSON は battle.schema.json の実物の形をそのまま貼る —
契約が変わったらここが割れる。
-}

import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Widgets.Timeline as Timeline


{-| battle.schema.json の turnCue.widget の実物(短い label に置き換えず貼る)。
-}
turnCueWidget : Maybe D.Value
turnCueWidget =
    widgetOf """
      { "timeline": {
          "totalSeconds": { "multiply": ["beatSeconds", "turnBeatScale"], "to": "turnBeatScale" },
          "phases": [
            { "label": "ターン表示",   "to": "turnCounterEnd", "description": "ターン数が回る" },
            { "label": "ウェイト", "to": "countdownStart", "wait": true },
            { "label": "カウントダウン", "to": "countdownEnd" },
            { "label": "ウェイト", "to": "enterFieldStart", "wait": true },
            { "label": "場に出る",   "to": "end" } ] } }
    """


{-| battle.schema.json の fxTiming.widget の実物。
-}
fxTimingWidget : Maybe D.Value
fxTimingWidget =
    widgetOf """
      { "clips": {
          "totalSeconds": { "multiply": ["beatSeconds"], "to": "beatSeconds" },
          "items": [
            { "label": "玉が対象へ飛ぶ", "length": "orbFlightRatio", "restLabel": "炸裂" },
            { "label": "踏み込み", "length": "lungeRatio", "capSeconds": "lungeMaxSeconds" } ] } }
    """


widgetOf : String -> Maybe D.Value
widgetOf raw =
    D.decodeString D.value raw |> Result.toMaybe


specs : List Timeline.TrackSpec
specs =
    Timeline.specsFrom [ ( "turnCue", turnCueWidget ), ( "fxTiming", fxTimingWidget ) ]


{-| internet\_dungeon.battle.json の実データの値。
-}
config : Timeline.Config
config =
    { specs = specs
    , values =
        Dict.fromList
            [ ( "beatSeconds", 0.58 )
            , ( "turnBeatScale", 2.2 )
            , ( "turnCue.turnCounterEnd", 0.26 )
            , ( "turnCue.countdownStart", 0.36 )
            , ( "turnCue.countdownEnd", 0.6 )
            , ( "turnCue.enterFieldStart", 0.68 )
            , ( "fxTiming.orbFlightRatio", 0.65 )
            , ( "fxTiming.lungeRatio", 0.45 )
            , ( "fxTiming.lungeMaxSeconds", 0.26 )
            ]
    , fields =
        Dict.fromList
            [ ( "turnCue.countdownEnd", { min = Just 0, max = Just 1, step = Just 0.01, default = Just 0.6 } )
            , ( "turnBeatScale", { min = Just 1, max = Just 5, step = Nothing, default = Just 2.2 } )
            ]
    , labels = Dict.empty
    }


{-| Out を比較できる形に(浮動小数は 1/1000 目盛りの整数へ)。
-}
outKey : Timeline.Out -> ( List String, Int )
outKey out =
    case out of
        Timeline.Silent ->
            ( [], 0 )

        Timeline.Edited edit ->
            ( edit.path, round (edit.value * 1000) )

        -- ズームは文書を書かない(スクロール合わせの頼み事だけ)
        Timeline.Zoomed zoomed ->
            ( [ "zoom" ], round (zoomed.ratio * 1000) )

        Timeline.ZoomReset ->
            ( [ "zoom-reset" ], 0 )


{-| ハンドルを掴んで(Pressed)、位置 fx まで動かした(Moved)ときに出る Out。
押しただけでは書かないので、書き戻しは必ずこの 2 手で起きる。
-}
dragTo : Timeline.Handle -> Float -> ( List String, Int )
dragTo handle fx =
    Timeline.update config (Timeline.Pressed handle { fx = fx, fy = 0 }) Timeline.init
        |> Tuple.first
        |> Timeline.update config (Timeline.Moved { fx = fx, fy = 0 })
        |> Tuple.second
        |> outKey


suite : Test
suite =
    describe "Widgets.Timeline — 宣言の読みとドラッグの変換"
        [ test "turnCue 宣言はフェーズ 5 本・ウェイト 2 本・掴める境目 4 本に読める" <|
            \_ ->
                (case specs of
                    (Timeline.PhaseTrack track) :: _ ->
                        ( List.length track.phases
                        , ( track.phases |> List.filter .wait |> List.length
                          , track.phases |> List.filterMap .to
                          )
                        , ( track.total.multiply, track.total.to )
                        )

                    _ ->
                        ( 0, ( 0, [] ), ( [], Nothing ) )
                )
                    |> Expect.equal
                        ( 5
                        , ( 2, [ "turnCounterEnd", "countdownStart", "countdownEnd", "enterFieldStart" ] )
                        , ( [ "beatSeconds", "turnBeatScale" ], Just "turnBeatScale" )
                        )
        , test "fxTiming 宣言は上限秒とレストの有無ごとクリップに読める" <|
            \_ ->
                (case specs of
                    [ _, Timeline.ClipTrack track ] ->
                        track.items
                            |> List.map (\c -> ( c.length, c.capSeconds, c.restLabel ))

                    _ ->
                        []
                )
                    |> Expect.equal
                        [ ( "orbFlightRatio", Nothing, Just "炸裂" )
                        , ( "lungeRatio", Just "lungeMaxSeconds", Nothing )
                        ]
        , test "読めない宣言は無視する(素の文字列・空 object・途中の end)" <|
            \_ ->
                Timeline.specsFrom
                    [ ( "a", widgetOf "\"sfx\"" )
                    , ( "b", widgetOf "{}" )
                    , ( "c"
                      , widgetOf """
                          { "timeline": {
                              "totalSeconds": { "multiply": ["beatSeconds"] },
                              "phases": [
                                { "label": "x", "to": "end" },
                                { "label": "y", "to": "countdownEnd" } ] } }
                        """
                      )
                    , ( "d", Nothing )
                    ]
                    |> Expect.equal []
        , test "総尺は multiply の積(実データで 0.58 × 2.2)・物差しは +15% の余白つき" <|
            \_ ->
                ( specs |> List.map (\s -> round (Timeline.totalSecondsOf config s * 1000))
                , round (Timeline.rulerSecondsOf config * 1000)
                )
                    -- turnCue 1.276s / fxTiming 0.58s → 物差し 1.276 × 1.15
                    |> Expect.equal ( [ 1276, 580 ], 1467 )
        , test "境目のドラッグ: 物差しの位置 → トラックの割合 → step 0.01 へ丸めて書く" <|
            \_ ->
                -- fx 0.43 × 1.4674s = 0.631s → ÷1.276s = 0.4945 → 0.49
                dragTo (Timeline.Boundary { section = "turnCue", target = "countdownEnd" }) 0.43
                    |> Expect.equal ( [ "turnCue", "countdownEnd" ], 490 )
        , test "境目のドラッグは schema の範囲で clamp する(右端いっぱい → 1.0)" <|
            \_ ->
                -- fx 1.0 は割合 1.15 相当だが、max 1.0 で止まる
                dragTo (Timeline.Boundary { section = "turnCue", target = "countdownEnd" }) 1.0
                    |> Expect.equal ( [ "turnCue", "countdownEnd" ], 1000 )
        , test "丸めた結果が今の値と同じなら書かない(step が洪水のスロットルになる)" <|
            \_ ->
                -- fx 0.5217 × 1.4674s ÷ 1.276s = 0.5999… → 0.60 = 今の値
                dragTo (Timeline.Boundary { section = "turnCue", target = "countdownEnd" }) 0.5217
                    |> Expect.equal ( [], 0 )
        , test "帯全体の右端: 秒 → turnBeatScale の逆算(総尺 ÷ beatSeconds)" <|
            \_ ->
                -- fx 1.0 = 物差しの右端 1.4674s → ÷0.58 = 2.53
                dragTo (Timeline.TotalEnd { target = "turnBeatScale" }) 1.0
                    |> Expect.equal ( [ "turnBeatScale" ], 2530 )

        -- 「1 ビート」の線のドラッグ = beatSeconds の直書き
        -- (multiply が beatSeconds 1 つなので、逆算は秒がそのまま値になる)
        , test "1 ビートの線のドラッグ: 秒がそのまま beatSeconds になる" <|
            \_ ->
                -- fx 0.5 × 1.4674s = 0.7337s → step 既定 0.01 で 0.73
                dragTo (Timeline.TotalEnd { target = "beatSeconds" }) 0.5
                    |> Expect.equal ( [ "beatSeconds" ], 730 )
        , test "pushForward は 0..1 に収めてから前へ押し出す(表示専用)" <|
            \_ ->
                Timeline.pushForward [ 0.26, 0.7, 0.2, 1.4 ]
                    |> Expect.equal [ 0.26, 0.7, 0.7, 1.0 ]
        , test "クリップの長さは length × ビート秒と上限秒の短い方" <|
            \_ ->
                -- 踏み込み: 0.45 × 0.58 = 0.261s だが上限 0.26s で切られる
                Timeline.clipSpan config
                    { section = "fxTiming", beatSeconds = 0.58 }
                    { label = "踏み込み", length = "lungeRatio", capSeconds = Just "lungeMaxSeconds", restLabel = Nothing }
                    |> Maybe.map (\span -> ( round (span.seconds * 1000), span.capped ))
                    |> Expect.equal (Just ( 260, True ))
        , test "文書に無い欄は schema の default へ倒れる" <|
            \_ ->
                Timeline.valueOf
                    { config | values = Dict.remove "turnCue.countdownEnd" config.values }
                    "turnCue.countdownEnd"
                    |> Expect.equal (Just 0.6)

        -- ズーム: 倍率は無段階に掛かるが、1(全体が収まる)より縮めない。
        -- ×1.3 のあと ×0.1 を掛けても 1 で止まる
        , test "ズームは掛け算で効き、1 未満へは縮まない" <|
            \_ ->
                let
                    ( zoomedIn, _ ) =
                        Timeline.update config (Timeline.ZoomBy { fx = 0.5, fy = 0 } 1.3) Timeline.init

                    ( clamped, _ ) =
                        Timeline.update config (Timeline.ZoomBy { fx = 0.5, fy = 0 } 0.1) zoomedIn
                in
                ( zoomedIn.zoom, clamped.zoom ) |> Expect.equal ( 1.3, 1 )

        -- 全体へ戻る: 倍率が 1 に戻り、スクロールを左端へ戻す頼み事が出る
        , test "ZoomHome は倍率 1 + スクロールのリセットを頼む" <|
            \_ ->
                let
                    ( zoomedIn, _ ) =
                        Timeline.update config (Timeline.ZoomBy { fx = 0.5, fy = 0 } 2) Timeline.init

                    ( home, out ) =
                        Timeline.update config Timeline.ZoomHome zoomedIn
                in
                ( home.zoom, outKey out ) |> Expect.equal ( 1, ( [ "zoom-reset" ], 0 ) )
        ]
