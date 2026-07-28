module SfxEditor exposing
    ( Config
    , Model
    , Msg
    , Out(..)
    , Shape
    , edges
    , init
    , isDragging
    , shapeDecoder
    , forget
    , shapeLoaded
    , startingChanged
    , update
    , view
    , wanted
    )

{-| 効果音のつまみを、絵を見ながら触る画面。

正本はいつも文書(\*.sfxtune.json)の数値で、ここは「絵の上の操作 → 数値 1 つの
書き換え」に写すだけ。生の標本は編集しない — 音を作るのはゲーム側なので、
エディタが波形を直接いじれても保存する場所が無い。

絵はサーバの GET /sfx/shape が返す「焼き上がった WAV の実測」を出す。
予測で描くと、まだ焼かれていない値の絵を本物のように見せてしまう。

掴める所は 2 つだけ:

  - 波形に重なった帯の右の縁 … 左が立ち上がり(attack)・右が余韻(release)
  - 音色の面 … 横が こもる↔刺さる(bright)・縦が 低い↔高い(pitch)

残りのつまみは親のフォーム(数値)に任せる。掴みどころを増やすほど
「どれを触ればいいか」が分からなくなるため。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Json.Decode as D
import Set exposing (Set)
import Svg
import Svg.Attributes as SA


{-| 焼き上がった音の実測。peaks は波形の包絡(0〜1)、bands は帯×時刻(0〜1)で
先頭の行が高い帯。
-}
type alias Shape =
    { name : String
    , ms : Float
    , peak : Float
    , bandLo : Float
    , bandHi : Float
    , peaks : List Float
    , bands : List (List Float)
    }


type alias Model =
    { shapes : Dict String Shape
    , asked : Set String

    -- 絵を取った時のつまみの値。いまの値との比から「仮の絵」を導く
    -- (焼き直すまで実物は変わらないので、これが無いと操作しても絵が動かない)
    , calib : Dict String (Dict String Float)
    , looping : Maybe String
    , drag : Maybe Drag

    -- 焼き係を立ち上げ中(初回だけ 1〜2 分)。この間は音が出ない
    , starting : Bool
    }


type alias Drag =
    { sound : String
    , handle : Handle
    }


type Handle
    = HeadEdge
    | TailEdge
    | Tone


{-| 描くのに要る材料。values は文書のいまの値(欠けていれば既定)。
-}
type alias Config =
    { sound : String
    , label : String
    , values : Dict String Float
    }


type Msg
    = Pressed String Handle Point
    | Moved Point
    | Released
    | PlayPressed String
    | LoopToggled String
    | VolumeInput String String


type alias Point =
    { fx : Float, fy : Float }


type Out
    = Silent
    | Edited { field : String, value : Float }
    | Play { name : String, loop : Bool }
    | Stop


init : Model
init =
    { shapes = Dict.empty
    , asked = Set.empty
    , calib = Dict.empty
    , looping = Nothing
    , drag = Nothing
    , starting = False
    }


{-| まだ取りに行っていない音の名前(親が GET /sfx/shape を投げる材料)。 -}
wanted : String -> Model -> Maybe String
wanted sound model =
    if Dict.member sound model.shapes || Set.member sound model.asked then
        Nothing

    else
        Just sound


{-| いま掴んで動かしている最中か。
掴んでいる間は焼き直さない — 途中で焼くと、実測から導く縁が指の下で飛び、
音も動かすたびに鳴って何を聴いているのか分からなくなる。
-}
isDragging : Model -> Bool
isDragging model =
    model.drag /= Nothing


{-| 焼き係の立ち上げ待ちかどうかを差し替える。 -}
startingChanged : Bool -> Model -> Model
startingChanged starting model =
    { model | starting = starting }


{-| 取り直せるように印を消す(焼き直したあとに使う)。 -}
forget : String -> Model -> Model
forget sound model =
    { model | asked = Set.remove sound model.asked }


{-| 取りに行った印を付ける(応答の前でも二重に投げない)。
values は「この絵を焼いた時のつまみ」として控える。
-}
shapeLoaded : String -> Maybe Shape -> Dict String Float -> Model -> Model
shapeLoaded sound shape values model =
    { model
        | asked = Set.insert sound model.asked
        , shapes =
            case shape of
                Just s ->
                    Dict.insert sound s model.shapes

                Nothing ->
                    model.shapes
        , calib =
            case shape of
                Just _ ->
                    Dict.insert sound values model.calib

                Nothing ->
                    model.calib
    }


{-| 絵を取った時のつまみ。控えが無ければいまの値(= 差が無い扱い)。 -}
calibOf : Config -> Model -> Dict String Float
calibOf config model =
    Dict.get config.sound model.calib |> Maybe.withDefault config.values


{-| 焼いた時から動かしたか。動かしていれば絵は「仮」。 -}
drifted : Config -> Model -> Bool
drifted config model =
    let
        base =
            calibOf config model
    in
    [ "attack", "release", "volume", "pitch", "bright", "speed", "room" ]
        |> List.any
            (\field ->
                abs (Dict.get field config.values |> Maybe.withDefault 1)
                    - abs (Dict.get field base |> Maybe.withDefault 1)
                    |> abs
                    |> (\d -> d > 0.001)
            )


{-| 画面に出す帯の縁。

アタックもリリースも**時間 (ms)** なので、縁の位置はそのまま時間から決まる
(掴んだ場所がそのまま値になる)。0 のときだけ「録音のまま」なので、
実測した縁 — 頭 = いちばん大きい所、尾 = 最大の 1 割を最後に下回る所 — を出す。

-}
shownEdges : Config -> Model -> Shape -> { head : Float, tail : Float }
shownEdges config model shape =
    let
        _ =
            model

        total =
            max 1 shape.ms

        measured =
            edges shape

        atk =
            valueOf config "attack" 0

        rel =
            valueOf config "release" 0

        head =
            if atk <= 0 then
                measured.head

            else
                clamp 0.01 0.95 (atk / total)

        tail =
            if rel <= 0 then
                max measured.tail (head + 0.01)

            else
                clamp (head + 0.01) 1 ((atk + rel) / total)
    in
    { head = head, tail = tail }


{-| 画面に出す音の大きさ(波形の高さ)。音量を動かせばその場で高さも変わる。 -}
shownLevel : Config -> Model -> Shape -> Float
shownLevel config model shape =
    let
        base =
            Dict.get "volume" (calibOf config model) |> Maybe.withDefault 1

        now =
            Dict.get "volume" config.values |> Maybe.withDefault 1
    in
    if base <= 0.0001 then
        shape.peak

    else
        clamp 0 1 (shape.peak * now / base)


shapeDecoder : D.Decoder Shape
shapeDecoder =
    D.map7 Shape
        (D.field "name" D.string)
        (D.field "ms" D.float)
        (D.field "peak" D.float)
        (D.field "bandLo" D.float)
        (D.field "bandHi" D.float)
        (D.field "peaks" (D.list D.float))
        (D.field "bands" (D.list (D.list D.float)))



-- ── 実測から「帯の縁」を導く ────────────────────────────


{-| 波形のどこまでが立ち上がりで、どこまでが余韻か(0〜1 の割合)。

頭 = いちばん大きい所、尾 = 最大の 1 割を最後に下回る所。予測でなく実測から
導くので、焼き直すたびに縁も自然に付いてくる。

-}
edges : Shape -> { head : Float, tail : Float }
edges shape =
    let
        arr =
            shape.peaks

        n =
            List.length arr

        top =
            List.foldl max 0 arr

        indexed =
            List.indexedMap Tuple.pair arr

        headIdx =
            indexed
                |> List.foldl
                    (\( i, v ) best ->
                        if v >= top && best < 0 then
                            i

                        else
                            best
                    )
                    -1

        tailIdx =
            indexed
                |> List.foldl
                    (\( i, v ) best ->
                        if v >= top * 0.1 then
                            i

                        else
                            best
                    )
                    0

        denom =
            toFloat (max 1 (n - 1))
    in
    { head = toFloat (max 0 headIdx) / denom
    , tail = clamp 0.02 1 (toFloat tailIdx / denom)
    }



-- ── 更新 ────────────────────────────────────────────────


update : Config -> Msg -> Model -> ( Model, Out )
update config msg model =
    case msg of
        Pressed sound handle point ->
            let
                m1 =
                    { model | drag = Just { sound = sound, handle = handle } }
            in
            case handle of
                Tone ->
                    ( m1, toneEdit config point )

                _ ->
                    ( m1, Silent )

        Moved point ->
            case model.drag of
                Nothing ->
                    ( model, Silent )

                Just drag ->
                    case drag.handle of
                        Tone ->
                            ( model, toneEdit config point )

                        HeadEdge ->
                            ( model, timeEdit config model "attack" point )

                        TailEdge ->
                            ( model, timeEdit config model "release" point )

        Released ->
            ( { model | drag = Nothing }
            , if model.drag == Nothing || model.looping /= Nothing then
                Silent

              else
                Play { name = config.sound, loop = False }
            )

        PlayPressed sound ->
            ( model, Play { name = sound, loop = False } )

        LoopToggled sound ->
            if model.looping == Just sound then
                ( { model | looping = Nothing }, Stop )

            else
                ( { model | looping = Just sound }
                , Play { name = sound, loop = True }
                )

        VolumeInput _ raw ->
            case String.toFloat raw of
                Just v ->
                    ( model, Edited { field = "volume", value = clamp 0 1 v } )

                Nothing ->
                    ( model, Silent )


{-| 面の位置 → 明るさと高さ。横は 0.2〜3.0、縦は 0.4〜2.5(スキーマの範囲と同じ)。
1 回の操作で 2 つの値が動くので、書き換えは 2 本に分けず bright を返して
pitch は次の Moved で拾う…とはせず、ここでは横を主に扱い縦は同時に返せないため、
面のドラッグは bright と pitch を交互でなく「どちらも書く」必要がある。
親が 1 件ずつ受けられるよう、ここでは差の大きいほうだけを返す。
-}
toneEdit : Config -> Point -> Out
toneEdit config point =
    let
        wantBright =
            0.2 + clamp 0 1 point.fx * 2.8

        wantPitch =
            0.4 + (1 - clamp 0 1 point.fy) * 2.1

        nowBright =
            valueOf config "bright" 1

        nowPitch =
            valueOf config "pitch" 1

        dBright =
            abs (wantBright - nowBright) / 2.8

        dPitch =
            abs (wantPitch - nowPitch) / 2.1
    in
    if dBright < 0.002 && dPitch < 0.002 then
        Silent

    else if dBright >= dPitch then
        Edited { field = "bright", value = round3 wantBright }

    else
        Edited { field = "pitch", value = round3 wantPitch }


{-| 帯の縁 → アタック / リリース (どちらも ms)。

掴んだ場所がそのまま時間になる。アタックは頭から縁までの長さ、
リリースはアタックの縁からそこまでの長さ。

-}
timeEdit : Config -> Model -> String -> Point -> Out
timeEdit config model field point =
    case Dict.get config.sound model.shapes of
        Nothing ->
            Silent

        Just shape ->
            let
                total =
                    max 1 shape.ms

                ms =
                    clamp 0 total (point.fx * total)

                current =
                    valueOf config field 0

                next =
                    if field == "attack" then
                        clamp 0 120 ms

                    else
                        clamp 0 600 (ms - valueOf config "attack" 0)
            in
            if abs (next - current) < 0.5 then
                Silent

            else
                Edited { field = field, value = round1 next }


valueOf : Config -> String -> Float -> Float
valueOf config field fallback =
    Dict.get field config.values |> Maybe.withDefault fallback


round3 : Float -> Float
round3 v =
    toFloat (Basics.round (v * 1000)) / 1000


{-| 時間は 0.1ms 刻みで足りる（それ以下は耳に出ない）。 -}
round1 : Float -> Float
round1 v =
    toFloat (Basics.round (v * 10)) / 10



-- ── 見た目 ──────────────────────────────────────────────


view : Config -> Model -> Html Msg
view config model =
    let
        shape =
            Dict.get config.sound model.shapes
    in
    div [ HA.class "sfx-editor" ]
        [ viewToolbar config model shape
        , div [ HA.class "sfx-body" ]
            [ div [ HA.class "sfx-graphs" ]
                [ viewWave config model shape
                , Html.Lazy.lazy viewSpec shape
                ]
            , viewPad config
            ]
        ]


viewToolbar : Config -> Model -> Maybe Shape -> Html Msg
viewToolbar config model shape =
    let
        looping =
            model.looping == Just config.sound
    in
    div [ HA.class "sfx-toolbar" ]
        [ button
            [ HA.class "sfx-btn sfx-play"
            , HA.type_ "button"
            , HA.disabled model.starting
            , HA.title "いまのつまみで焼き直して鳴らします"
            , HE.onClick (PlayPressed config.sound)
            ]
            [ text "▶ 試聴" ]
        , button
            [ HA.class "sfx-btn"
            , HA.type_ "button"
            , HA.disabled model.starting
            , HA.attribute "aria-pressed"
                (if looping then
                    "true"

                 else
                    "false"
                )
            , HE.onClick (LoopToggled config.sound)
            ]
            [ text "ループ" ]
        , span [ HA.class "sfx-vol" ]
            [ span [] [ text "音量" ]
            , input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "1"
                , HA.step "0.01"
                , HA.value (String.fromFloat (valueOf config "volume" 1))
                , HE.onInput (VolumeInput config.sound)
                ]
                []
            ]
        , div [ HA.class "sfx-readout" ]
            (case shape of
                Just s ->
                    let
                        e =
                            shownEdges config model s
                    in
                    [ span [] [ text "全体 ", Html.b [] [ text (msText (s.ms * e.tail)) ] ]
                    , span [] [ text "アタック ", Html.b [] [ text (msText (s.ms * e.head)) ] ]
                    , span [] [ text "大きさ ", Html.b [] [ text (pctText (shownLevel config model s)) ] ]
                    , if drifted config model then
                        span [ HA.class "sfx-flag" ] [ text "仮の絵（保存して焼き直すと本物に）" ]

                      else
                        text ""
                    ]

                Nothing ->
                    [ span [] [ text "まだ焼かれていません" ] ]
            )
        , if model.starting then
            span [ HA.class "sfx-flag" ]
                [ text "音の準備をしています…（初回だけ 1〜2 分）" ]

          else
            text ""
        ]


msText : Float -> String
msText ms =
    String.fromInt (Basics.round ms) ++ "ms"


pctText : Float -> String
pctText v =
    String.fromInt (Basics.round (v * 100)) ++ "%"


{-| 波形と、掴める 2 本の帯。

字は SVG に置かない — 器いっぱいに引き伸ばす描き方(preserveAspectRatio="none")では
字まで横に潰れるため、字と目盛りは HTML で重ねる。

-}
viewWave : Config -> Model -> Maybe Shape -> Html Msg
viewWave config model shape =
    let
        e =
            shape
                |> Maybe.map (shownEdges config model)
                |> Maybe.withDefault { head = 0.12, tail = 0.6 }

        bars =
            case shape of
                Just s ->
                    let
                        n =
                            max 1 (List.length s.peaks)

                        bw =
                            100 / toFloat n
                    in
                    s.peaks
                        |> List.indexedMap
                            (\i v ->
                                let
                                    -- 小さい音でも形が読めるように、高さは大きさに
                                    -- そのまま比例させず底上げする(数字は読み出しに出る)
                                    bh =
                                        v * (0.4 + 0.6 * shownLevel config model s) * 47
                                in
                                Svg.rect
                                    [ SA.x (String.fromFloat (toFloat i * bw))
                                    , SA.y (String.fromFloat (50 - bh))
                                    , SA.width (String.fromFloat bw)
                                    , SA.height (String.fromFloat (bh * 2))
                                    , SA.class "sfx-bar"
                                    ]
                                    []
                            )

                Nothing ->
                    []

        zone x0 x1 extra =
            Svg.rect
                [ SA.x (String.fromFloat (x0 * 100))
                , SA.y "0"
                , SA.width (String.fromFloat ((x1 - x0) * 100))
                , SA.height "100"
                , SA.class ("sfx-zone" ++ extra)
                ]
                []
    in
    div
        ([ HA.classList
            [ ( "sfx-wave", True )
            , ( "sfx-provisional", drifted config model )
            ]
         , onPointerDown (pickHandle config.sound e)
         ]
            ++ dragAttrs model
        )
        [ Svg.svg
            [ SA.viewBox "0 0 100 100"
            , SA.preserveAspectRatio "none"
            , SA.class "sfx-svg"
            ]
            (zone 0 e.head "" :: zone e.head e.tail " sfx-zone-tail" :: bars)
        , grip e.head "アタック"
        , grip e.tail "リリース"
        ]


{-| 掴む縁 1 本。線と丸と札を HTML で重ねる(字が潰れないように)。 -}
grip : Float -> String -> Html Msg
grip fx label =
    div [ HA.class "sfx-grip", HA.style "left" (String.fromFloat (fx * 100) ++ "%") ]
        [ div [ HA.class "sfx-grip-dot" ] []
        , div [ HA.class "sfx-grip-label" ] [ text label ]
        ]


{-| 押した位置がどちらの縁に近いかで、掴む物を決める。 -}
pickHandle : String -> { head : Float, tail : Float } -> Point -> Msg
pickHandle sound e point =
    if abs (point.fx - e.head) <= abs (point.fx - e.tail) then
        Pressed sound HeadEdge point

    else
        Pressed sound TailEdge point


{-| 帯ごとの音量。縦が周波数(下が低い)。目盛りは実際の Hz。 -}
viewSpec : Maybe Shape -> Html Msg
viewSpec shape =
    let
        cells =
            case shape of
                Just s ->
                    let
                        rowCount =
                            max 1 (List.length s.bands)

                        colCount =
                            s.bands |> List.head |> Maybe.map List.length |> Maybe.withDefault 1

                        cw =
                            100 / toFloat (max 1 colCount)

                        ch =
                            100 / toFloat rowCount
                    in
                    s.bands
                        |> List.indexedMap
                            (\r row ->
                                row
                                    |> List.indexedMap
                                        (\c v ->
                                            if v < 0.02 then
                                                Svg.text ""

                                            else
                                                Svg.rect
                                                    [ SA.x (String.fromFloat (toFloat c * cw))
                                                    , SA.y (String.fromFloat (toFloat r * ch))
                                                    , SA.width (String.fromFloat cw)
                                                    , SA.height (String.fromFloat ch)
                                                    , SA.class "sfx-cell"
                                                    , SA.opacity (String.fromFloat (v ^ 0.7))
                                                    ]
                                                    []
                                        )
                            )
                        |> List.concat

                Nothing ->
                    []

        ticks =
            case shape of
                Just s ->
                    [ 500, 1000, 2000, 5000 ]
                        |> List.filter (\f -> f > s.bandLo && f < s.bandHi)
                        |> List.map
                            (\f ->
                                div
                                    [ HA.class "sfx-tick"
                                    , HA.style "bottom"
                                        (String.fromFloat (logUnit s.bandLo s.bandHi f * 100) ++ "%")
                                    ]
                                    [ span [] [ text (hzText f) ] ]
                            )

                Nothing ->
                    []
    in
    div [ HA.class "sfx-spec-row" ]
        [ div [ HA.class "sfx-axis-y" ]
            [ span [] [ text "高い" ], span [] [ text "低い" ] ]
        , div [ HA.class "sfx-spec" ]
            (Svg.svg
                [ SA.viewBox "0 0 100 100"
                , SA.preserveAspectRatio "none"
                , SA.class "sfx-svg"
                ]
                cells
                :: ticks
            )
        ]


hzText : Float -> String
hzText f =
    if f >= 1000 then
        String.fromInt (Basics.round (f / 1000)) ++ "k"

    else
        String.fromInt (Basics.round f)


logUnit : Float -> Float -> Float -> Float
logUnit lo hi f =
    if hi <= lo then
        0

    else
        clamp 0 1 (logBase 2 (f / lo) / logBase 2 (hi / lo))


{-| 音色の面。横が こもる↔刺さる、縦が 低い↔高い。 -}
viewPad : Config -> Html Msg
viewPad config =
    let
        fx =
            clamp 0 1 ((valueOf config "bright" 1 - 0.2) / 2.8)

        fy =
            clamp 0 1 (1 - (valueOf config "pitch" 1 - 0.4) / 2.1)
    in
    div [ HA.class "sfx-pad-block" ]
        [ div [ HA.class "sfx-pad-title" ] [ text "音色" ]
        , div [ HA.class "sfx-pad-row" ]
            [ div [ HA.class "sfx-axis-y" ]
                [ span [] [ text "高い" ], span [] [ text "低い" ] ]
            , div
                [ HA.class "sfx-pad"
                , onPointerDown (Pressed config.sound Tone)
                , HE.on "pointermove" (D.map Moved pointDecoder)
                , HE.on "pointerup" (D.succeed Released)
                , HE.on "pointercancel" (D.succeed Released)
                ]
                [ div
                    [ HA.class "sfx-puck"
                    , HA.style "left" (String.fromFloat (fx * 100) ++ "%")
                    , HA.style "top" (String.fromFloat (fy * 100) ++ "%")
                    ]
                    []
                ]
            ]
        , div [ HA.class "sfx-axis-x" ]
            [ span [] [ text "こもる" ], span [] [ text "刺さる" ] ]
        ]


dragAttrs : Model -> List (Html.Attribute Msg)
dragAttrs model =
    if model.drag == Nothing then
        [ HE.on "pointerup" (D.succeed Released) ]

    else
        [ HE.on "pointermove" (D.map Moved pointDecoder)
        , HE.on "pointerup" (D.succeed Released)
        , HE.on "pointercancel" (D.succeed Released)
        ]


onPointerDown : (Point -> Msg) -> Html.Attribute Msg
onPointerDown toMsg =
    HE.on "pointerdown" (D.map toMsg pointDecoder)


{-| 押した場所を「その要素の中の割合」で読む。要素の実寸は currentTarget から
取れるので、画面幅が変わっても同じ意味になる。
-}
pointDecoder : D.Decoder Point
pointDecoder =
    D.map4 (\x y w h -> { fx = safeDiv x w, fy = safeDiv y h })
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)
        (D.at [ "currentTarget", "clientWidth" ] D.float)
        (D.at [ "currentTarget", "clientHeight" ] D.float)


safeDiv : Float -> Float -> Float
safeDiv a b =
    if b <= 0 then
        0

    else
        clamp 0 1 (a / b)
