module Waveform exposing
    ( Handlers
    , Model
    , Selection
    , clear
    , cursor
    , dragTo
    , init
    , playFrom
    , playSpan
    , playheadId
    , pressAt
    , release
    , selection
    , view
    )

{-| 波形の帯: 再生位置(プレイヘッド)・つまみ出し(範囲選択)・その場送り(シーク)。

範囲は**秒**で持つ。目盛りや器の幅で持つと、器の大きさが変わった時に選んだ場所が
ずれる。秒で持てば、同じ範囲を別の画面(将来の素材切り出し = 開始と長さの指定)へ
そのまま渡せる。

プレイヘッドの線は**この部品が置くだけ**で、動かすのは JS(rAF で
AudioContext.currentTime から算出)。1 音 200ms の効果音でも毎フレーム Elm へ
返すと全画面の描き直しになるため、Elm が持つのは「どこから鳴らすか」の意図だけ。

-}

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Svg
import Svg.Attributes as SA


{-| 選んだ範囲(秒)。from < to は組み立て側で必ず整える。 -}
type alias Selection =
    { from : Float, to : Float }


type alias Model =
    { -- 停止中の開始位置(秒)。クリックで動く
      cursor : Float

    -- 選んだ範囲(Nothing = 全体)
    , selection : Maybe Selection

    -- ドラッグ中の起点(秒)。離すまで範囲は確定しない
    , anchor : Maybe Float
    }


init : Model
init =
    { cursor = 0, selection = Nothing, anchor = Nothing }


cursor : Model -> Float
cursor model =
    model.cursor


selection : Model -> Maybe Selection
selection model =
    model.selection


{-| 押した瞬間: そこを開始位置にして、範囲の起点にする(まだ範囲は作らない —
ただのクリックで範囲が生まれると、シークのたびに選択が付いて回る)。
-}
pressAt : Float -> Model -> Model
pressAt seconds model =
    { model | cursor = max 0 seconds, anchor = Just (max 0 seconds), selection = Nothing }


{-| 掴んだまま動かした: 起点から今の場所までが範囲。
つまみ幅に満たない動き(1ms 未満)は範囲にしない — 手の震えで選択が付かないように。
-}
dragTo : Float -> Model -> Model
dragTo seconds model =
    case model.anchor of
        Just from ->
            let
                to =
                    max 0 seconds
            in
            if abs (to - from) < 0.001 then
                { model | selection = Nothing }

            else
                { model | selection = Just { from = min from to, to = max from to } }

        Nothing ->
            model


release : Model -> Model
release model =
    { model | anchor = Nothing }


{-| 選択を解く(範囲の外のクリック・Esc)。開始位置は残す。 -}
clear : Model -> Model
clear model =
    { model | selection = Nothing, anchor = Nothing }


{-| 次に鳴らす開始位置(秒)。範囲があればその頭から。 -}
playFrom : Model -> Float
playFrom model =
    case model.selection of
        Just span ->
            span.from

        Nothing ->
            model.cursor


{-| 鳴らす長さ(秒)。範囲が無ければ Nothing = 最後まで。 -}
playSpan : Model -> Maybe Float
playSpan model =
    model.selection |> Maybe.map (\span -> span.to - span.from)


{-| プレイヘッドの線の照合キー。JS はこの印を持つ線を全部動かす —
波形とピアノロールに同じ照合キーを渡せば、1 回の再生で両方の線が走る。
-}
playheadId : String -> String
playheadId key =
    "playhead-" ++ key


type alias Handlers msg =
    { onPress : Float -> msg
    , onDrag : Float -> msg
    , onRelease : msg
    }


{-| 波形 1 枚。peaks は 0〜1 の包絡(サーバの実測をそのまま使う)。
duration は全体の秒数で、クリック位置の秒はここから割り出す。
-}
view : Handlers msg -> { key : String, peaks : List Float, duration : Float } -> Model -> Html msg
view handlers spec model =
    div
        [ HA.class "waveform relative h-24 w-full cursor-text overflow-hidden rounded border border-edge bg-well"
        , onPointer "mousedown" (\ratio -> handlers.onPress (ratio * spec.duration))
        , onPointer "mousemove" (\ratio -> handlers.onDrag (ratio * spec.duration))
        , HE.onMouseUp handlers.onRelease
        , HE.onMouseLeave handlers.onRelease
        ]
        [ viewPeaks spec.peaks
        , viewSelection spec.duration model.selection
        , viewCursor spec.duration model.cursor
        , div
            [ HA.class "waveform-playhead"
            , HA.attribute "data-playhead" (playheadId spec.key)
            , HA.style "display" "none"
            ]
            []
        ]


{-| 包絡を上下対称の帯で。器いっぱいに引き伸ばす(preserveAspectRatio none)—
横に潰れて困る字はここに置かない。
-}
viewPeaks : List Float -> Html msg
viewPeaks peaks =
    let
        count =
            max 1 (List.length peaks)

        bar index value =
            let
                x =
                    toFloat index / toFloat count * 100

                h =
                    max 0.5 (min 1 value * 100)
            in
            Svg.rect
                [ SA.x (String.fromFloat x ++ "%")
                , SA.y (String.fromFloat ((100 - h) / 2) ++ "%")
                , SA.width (String.fromFloat (100 / toFloat count) ++ "%")
                , SA.height (String.fromFloat h ++ "%")
                , SA.class "waveform-bar"
                ]
                []
    in
    Svg.svg
        [ SA.class "absolute inset-0 h-full w-full"
        , SA.preserveAspectRatio "none"
        ]
        (List.indexedMap bar peaks)


viewSelection : Float -> Maybe Selection -> Html msg
viewSelection duration span =
    case span of
        Just s ->
            div
                [ HA.class "waveform-selection pointer-events-none absolute top-0 bottom-0 bg-accent/25 ring-1 ring-accent/60"
                , HA.style "left" (percent duration s.from)
                , HA.style "width" (percent duration (s.to - s.from))
                ]
                []

        Nothing ->
            text ""


viewCursor : Float -> Float -> Html msg
viewCursor duration at =
    div
        [ HA.class "waveform-cursor pointer-events-none absolute top-0 bottom-0 w-px bg-accent"
        , HA.style "left" (percent duration at)
        ]
        []


percent : Float -> Float -> String
percent duration seconds =
    if duration <= 0 then
        "0%"

    else
        String.fromFloat (clamp 0 100 (seconds / duration * 100)) ++ "%"


{-| 器の中の押した割合(0〜1)。器の幅は event から読む — 幅を Model に
覚えさせると、ペインを動かすたびに古い幅で秒を割り出してしまう。
-}
onPointer : String -> (Float -> msg) -> Html.Attribute msg
onPointer name toMsg =
    HE.on name
        (D.map2 (\x w -> toMsg (x / max 1 w))
            (D.field "offsetX" D.float)
            (D.at [ "currentTarget", "clientWidth" ] D.float)
        )
