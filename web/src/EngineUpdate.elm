module EngineUpdate exposing
    ( Check
    , State
    , Step(..)
    , checkDecoder
    , gotCheck
    , gotLog
    , init
    , isPolling
    , logDecoder
    , pendingVersion
    , started
    , startFailed
    , unavailable
    , view
    )

{-| 「engine の新しいバージョンが出ています」の帯とボタン。

Studio は engine を `.app` の外(`~/.flix_ge_studio/engines/<バージョン>/`)へ置いて
いるので、アプリごと入れ直さなくても engine だけ新しくできる。落とす・照合する・
展開するの中身はサーバが engine の道具(`fge fetch-engine`)を走らせて行い、
ここはその進み具合を見せて、押す 1 回を受けるだけ。

開いた瞬間に自動では上げない — バージョン上げは絵と挙動まで変わるので、
押すのは人。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。

-}

import Html exposing (Html, button, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


{-| 帯の今の姿。

  - Idle — 出す物が無い(見に行っていない・最新・見に行けなかった)
  - Offered — 新しいバージョンが出ている
  - Working — 落として組み立てている最中
  - Finished — 切り替わった
  - Failed — 差し替えられなかった(理由は note)

-}
type Step
    = Idle
    | Offered
    | Working
    | Finished
    | Failed


type alias State =
    { step : Step
    , latest : Maybe String
    , updatable : Bool
    , note : String

    -- 走っているはずなのに始まった気配が無い回数。サーバが差し替えの最中に
    -- 立ち上がり直すと「走っていない・終わってもいない」が返り続けるため
    , idleTicks : Int
    }


init : State
init =
    { step = Idle, latest = Nothing, updatable = False, note = "", idleTicks = 0 }


{-| GET /engine/update/check の中身。
-}
type alias Check =
    { available : Bool
    , version : Maybe String
    , updatable : Bool
    }


checkDecoder : D.Decoder Check
checkDecoder =
    D.map3 Check
        (D.oneOf [ D.field "available" D.bool, D.succeed False ])
        (D.oneOf [ D.field "version" (D.nullable D.string), D.succeed Nothing ])
        -- WhyNot: 既定を True にしない — この印を持たない古いサーバで
        -- 押せるボタンを出すと、無い口を叩いて理由の出ない失敗になる
        (D.oneOf [ D.field "updatable" D.bool, D.succeed False ])


gotCheck : Check -> State -> State
gotCheck check state =
    case ( check.available, check.version ) of
        ( True, Just version ) ->
            { state | step = Offered, latest = Just version, updatable = check.updatable }

        _ ->
            { state | step = Idle, latest = Nothing }


{-| 押したときに投げるバージョン。押せる姿でなければ何も投げない。
-}
pendingVersion : State -> Maybe String
pendingVersion state =
    if state.step == Offered && state.updatable then
        state.latest

    else
        Nothing


{-| 202(受理)。ここからログを引き始める。
-}
started : State -> State
started state =
    { state | step = Working, note = "engine を迎えに行っています", idleTicks = 0 }


startFailed : String -> State -> State
startFailed message state =
    { state | step = Failed, note = message }


{-| 口が無い・落ちた。回し続けても仕方ないので帯ごと引っ込める。
-}
unavailable : State -> State
unavailable state =
    { state | step = Idle, note = "" }


type alias Log =
    { running : Bool
    , exitCode : Maybe Int
    , lines : List String
    }


logDecoder : D.Decoder Log
logDecoder =
    D.map3 Log
        (D.oneOf [ D.field "running" D.bool, D.succeed False ])
        (D.oneOf [ D.field "exitCode" (D.map Just D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "lines" (D.list D.string), D.succeed [] ])


{-| ログ 1 回ぶんを流し込む。切り替わったときだけ True を返す
(呼ぶ側は engine のバージョンを引き直す)。
-}
gotLog : Log -> State -> ( State, Bool )
gotLog log state =
    if log.running then
        ( { state | step = Working, note = lastLine log.lines state.note, idleTicks = 0 }, False )

    else
        case log.exitCode of
            Just 0 ->
                -- WhyNot: 決まり文句で終わらせない — 最後の行には、開いているゲームが
                -- 追随を要るかどうか (人が次に動く唯一の手がかり) が載っている。
                ( { state | step = Finished, note = lastLine log.lines (switchedNote state) }, True )

            Just _ ->
                ( { state | step = Failed, note = lastLine log.lines "engine を差し替えられませんでした" }, False )

            Nothing ->
                -- WhyNot: まだ始まっていない(空のログ)をすぐ失敗にしない — 202 の直後の
                -- 1 回目はここに来る。ただし数え続けて、いつまでも始まらないなら止める
                -- (サーバが差し替えの最中に立ち上がり直すと永久にここへ来る)。
                let
                    ticks =
                        state.idleTicks + 1
                in
                if ticks > 10 then
                    ( { state | step = Failed, note = "差し替えの進み具合を追えなくなりました" }, False )

                else
                    ( { state | idleTicks = ticks }, False )


switchedNote : State -> String
switchedNote state =
    case state.latest of
        Just version ->
            "engine " ++ version ++ " に切り替えました"

        Nothing ->
            "engine を切り替えました"


lastLine : List String -> String -> String
lastLine lines fallback =
    List.foldl (\line acc -> ifBlank acc line) fallback lines


ifBlank : String -> String -> String
ifBlank fallback line =
    if String.trim line == "" then
        fallback

    else
        line


{-| ログを引き続けるのは走っている間だけ。
-}
isPolling : State -> Bool
isPolling state =
    state.step == Working



-- 画面


view : { onUpdate : String -> msg } -> State -> Html msg
view handlers state =
    case state.step of
        Idle ->
            text ""

        Offered ->
            case ( state.latest, state.updatable ) of
                ( Just version, True ) ->
                    span [ HA.class "flex shrink-0 items-center gap-1" ]
                        [ span [ HA.class "text-[10px] text-muted tabular-nums" ]
                            [ text ("engine " ++ version ++ " が出ています") ]
                        , button
                            [ HA.class "btn btn-mini"
                            , HA.title "engine だけを新しくします(開いているゲームはそのまま走り続けます)"
                            , HE.onClick (handlers.onUpdate version)
                            ]
                            [ text "上げる" ]
                        ]

                ( Just version, False ) ->
                    span [ HA.class "shrink-0 text-[10px] text-muted tabular-nums" ]
                        [ text ("engine " ++ version ++ " が出ています — Studio ごと入れ直してください") ]

                _ ->
                    text ""

        Working ->
            span [ HA.class "shrink-0 text-[10px] text-muted" ]
                [ text ("engine を差し替え中 — " ++ state.note) ]

        Finished ->
            span [ HA.class "shrink-0 text-[10px] text-muted" ] [ text state.note ]

        Failed ->
            span
                [ HA.class "shrink-0 text-[10px] text-warn"
                , HA.title state.note
                ]
                [ text "engine を差し替えられませんでした" ]
