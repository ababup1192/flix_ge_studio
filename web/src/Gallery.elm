module Gallery exposing
    ( BakeTarget(..)
    , Listing
    , Model
    , Msg(..)
    , Out(..)
    , Runner(..)
    , RunnerLog
    , Status(..)
    , bakeAccepted
    , bakeFailed
    , blessDone
    , blessEntryLabel
    , diffCount
    , diffDecoder
    , failed
    , gotDiff
    , gotList
    , gotRunnerLog
    , gotTargets
    , init
    , isPolling
    , listingDecoder
    , runnerLogDecoder
    , runnerStopped
    , showsTargetChooser
    , targetString
    , targetsDecoder
    , update
    , view
    )

{-| ギャラリー画面。焼いた絵(gallery/)と正解(golden/)の突き合わせを眺め、
ワンクリックで焼き(bake)、差分を 1 枚ずつ見比べて祝福(bless)する。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。
焼き・祝福の「送りたい事」は update の戻り値 Out で Main へ返す —
Journey が行き先 Nav を値で返すのと同じ流儀。
一覧(/gallery/list)と判定(/gallery/diff)は別々の応答なので別々に受け、
片方だけ届いた状態でも描ける形にする。エンドポイント未実装のサーバでは
「準備中」の 1 枚に倒す(落とさない)。

画像・音の URL は base(接続先サーバ)+ クエリで組む — vite dev(別オリジン)
でも .app(同一オリジン = base 空)でも同じ式で通すため。

-}

import Dict exposing (Dict)
import Html exposing (Html, audio, button, div, img, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Progress
import Url


type alias Item =
    { name : String
    , mtime : Int
    }


type alias Listing =
    { gallery : List Item
    , golden : List Item
    , debug : List Item
    , sounds : List Item
    }


type Status
    = StatusOk
    | StatusDiff
    | MissingGolden
    | MissingGallery


{-| 焼きの的。サーバの文字列(bake|gallery-prologue|gallery-sounds)と 1:1。 -}
type BakeTarget
    = TargetBake
    | TargetPrologue
    | TargetSounds


targetString : BakeTarget -> String
targetString target =
    case target of
        TargetBake ->
            "bake"

        TargetPrologue ->
            "gallery-prologue"

        TargetSounds ->
            "gallery-sounds"


{-| 焼きの進み。Starting = POST を投げて 202 待ち(この間もボタンは無効)。
Failed だけログの尻尾を残して見せる(成功は静かなトーストで済ませ、板は畳む)。
-}
type Runner
    = RunnerIdle
    | RunnerStarting
    | RunnerRunning { lines : List String }
    | RunnerFailed { exitCode : Int, lines : List String }
      -- サーバがまだ焼き口を持たない(404)。fail-open で「準備中」に倒す
    | RunnerUnavailable


{-| 祝福の見比べ歩き。remaining の先頭がいま見せている 1 枚。
total は始めた時の枚数(進み表示 k/N 用)。
-}
type alias Bless =
    { remaining : List String
    , chosen : List String
    , total : Int
    }


type alias Model =
    { listing : Maybe Listing

    -- name → 判定。Nothing = /gallery/diff 未着(バッジ無しで絵だけ出す)
    , diff : Maybe (Dict String Status)
    , error : Maybe String

    -- 差分の見比べ(golden と gallery を並べる)を開いている絵の名前
    , compare : Maybe String
    , bakeTarget : BakeTarget

    -- このプロジェクトが持つ焼きの的(GET /gallery/targets)。
    -- Nothing = 未着・未実装(fail-open で 3 択全部を見せる)
    , targets : Maybe (List BakeTarget)
    , runner : Runner
    , bless : Maybe Bless

    -- 祝福で golden が入れ替わった回数。golden の img URL に ?t= で付けて
    -- ブラウザの古いキャッシュ絵を避ける
    , goldenBust : Int

    -- 焼きログの全文展開(進捗ミニパネル)。既定は畳み、失敗時だけ自動で開く
    , logExpanded : Bool
    }


init : Model
init =
    { listing = Nothing
    , diff = Nothing
    , error = Nothing
    , compare = Nothing
    , bakeTarget = TargetBake
    , targets = Nothing
    , runner = RunnerIdle
    , bless = Nothing
    , goldenBust = 0
    , logExpanded = False
    }


gotList : Listing -> Model -> Model
gotList listing model =
    { model | listing = Just listing, error = Nothing }


gotDiff : Dict String Status -> Model -> Model
gotDiff diff model =
    { model | diff = Just diff }


{-| GET /gallery/targets の流し込み。選択中の的が消えたら先頭に倒す
(空リストは「情報なし」と同じ扱い — 3 択のまま)。
-}
gotTargets : List BakeTarget -> Model -> Model
gotTargets targets model =
    if List.isEmpty targets then
        { model | targets = Nothing }

    else
        { model
            | targets = Just targets
            , bakeTarget =
                if List.member model.bakeTarget targets then
                    model.bakeTarget

                else
                    List.head targets |> Maybe.withDefault TargetBake
        }


failed : String -> Model -> Model
failed message model =
    -- 一覧が既にあるなら画面は生かす(判定だけ欠けた状態)
    if model.listing == Nothing then
        { model | error = Just message }

    else
        model


type Msg
    = CompareOpened String
    | CompareClosed
    | TargetChosen BakeTarget
    | BakeClicked
    | LogClosed
    | LogToggled
    | BlessOpened
    | BlessAccepted
    | BlessHeld
    | BlessQuit


{-| サーバへ送りたい事。封筒の発行は Main の仕事なので値で返すだけ
(Journey の Nav と同じ流儀)。
-}
type Out
    = OutNone
    | OutBake BakeTarget
    | OutBless (List String)


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        CompareOpened name ->
            ( { model | compare = Just name }, OutNone )

        CompareClosed ->
            ( { model | compare = Nothing }, OutNone )

        TargetChosen target ->
            ( { model | bakeTarget = target }, OutNone )

        BakeClicked ->
            -- 走っている間は押せない(view でも disabled だが、規則はここが正)
            if isBusy model.runner then
                ( model, OutNone )

            else
                -- 新しい走りはログ展開を畳み直す(前回の失敗展開を持ち越さない)
                ( { model | runner = RunnerStarting, logExpanded = False }, OutBake model.bakeTarget )

        LogClosed ->
            ( { model | runner = RunnerIdle }, OutNone )

        LogToggled ->
            ( { model | logExpanded = not model.logExpanded }, OutNone )

        BlessOpened ->
            let
                names =
                    blessNames model
            in
            if List.isEmpty names then
                ( model, OutNone )

            else
                ( { model | bless = Just { remaining = names, chosen = [], total = List.length names }, compare = Nothing }
                , OutNone
                )

        BlessAccepted ->
            blessStep True model

        BlessHeld ->
            blessStep False model

        BlessQuit ->
            -- やめる = それまで選んだ分も送らない(祝福は最後まで歩いた時だけ)
            ( { model | bless = Nothing }, OutNone )


{-| 1 枚進める。keep なら選び袋へ。最後の 1 枚を捌いたら、選び袋が
空でない時だけ祝福を送る(全部保留なら何も送らず静かに閉じる)。
-}
blessStep : Bool -> Model -> ( Model, Out )
blessStep keep model =
    case model.bless of
        Nothing ->
            ( model, OutNone )

        Just bless ->
            case bless.remaining of
                [] ->
                    ( { model | bless = Nothing }, OutNone )

                current :: rest ->
                    let
                        chosen =
                            if keep then
                                bless.chosen ++ [ current ]

                            else
                                bless.chosen
                    in
                    if List.isEmpty rest then
                        ( { model | bless = Nothing }
                        , if List.isEmpty chosen then
                            OutNone

                          else
                            OutBless chosen
                        )

                    else
                        ( { model | bless = Just { bless | remaining = rest, chosen = chosen } }, OutNone )


isBusy : Runner -> Bool
isBusy runner =
    case runner of
        RunnerStarting ->
            True

        RunnerRunning _ ->
            True

        _ ->
            False


{-| /runner/log を回すべきか。Starting(202 待ち)では回さない —
前の走りの残りログ(running=false)を今回の結果と取り違えないため。
-}
isPolling : Model -> Bool
isPolling model =
    case model.runner of
        RunnerRunning _ ->
            True

        _ ->
            False


{-| 祝福の歩きに乗る名前(名前順で安定)。DIFF に加えて「正解がまだ無い」
(missingGolden)も含む — 産まれたてのゲームは全部が missingGolden で、
最初のお手本(golden)を作る入り口がここになる。
-}
blessNames : Model -> List String
blessNames model =
    model.diff
        |> Maybe.withDefault Dict.empty
        |> Dict.filter (\_ status -> status == StatusDiff || status == MissingGolden)
        |> Dict.keys


diffCount : Model -> Int
diffCount model =
    List.length (blessNames model)


{-| 歩きの全部が「正解がまだ無い」か(産まれたて)。入り口の文言が変わる。 -}
allMissingGolden : Model -> Bool
allMissingGolden model =
    let
        statuses =
            model.diff
                |> Maybe.withDefault Dict.empty
                |> Dict.values
                |> List.filter (\s -> s == StatusDiff || s == MissingGolden)
    in
    not (List.isEmpty statuses) && List.all ((==) MissingGolden) statuses


{-| 祝福の歩きの入り口ボタンの文言(歩く物が無ければ Nothing)。
産まれたて(全部 missingGolden)は「最初のお手本にする」に変わる。
-}
blessEntryLabel : Model -> Maybe String
blessEntryLabel model =
    let
        count =
            diffCount model
    in
    if count == 0 then
        Nothing

    else if allMissingGolden model then
        Just ("最初のお手本にする(" ++ String.fromInt count ++ "件)")

    else
        Just ("差分を確認して祝福する(" ++ String.fromInt count ++ "件)")



-- サーバ応答の流し込み(封筒の受領は Main、意味づけはここ)


{-| POST /gallery/bake が受かった(202)。409(既に走っている)も同じ扱いで
Running に入る — 次のログ取得が本当の姿を教えてくれる。
-}
bakeAccepted : Model -> Model
bakeAccepted model =
    { model | runner = RunnerRunning { lines = [] } }


{-| POST /gallery/bake の失敗。404 はサーバが古いだけなので「準備中」に倒す。 -}
bakeFailed : String -> Model -> Model
bakeFailed message model =
    if String.contains "404" message then
        { model | runner = RunnerUnavailable }

    else
        { model | runner = RunnerIdle }


type alias RunnerLog =
    { running : Bool
    , exitCode : Maybe Int
    , lines : List String
    }


{-| GET /runner/log の流し込み。戻りの Maybe Bool は「いま焼き終わった
(True = 成功)」— Main はこれを合図に一覧・判定・旅路を取り直す。
走っていない時に届いた古い応答は無視する。
-}
gotRunnerLog : RunnerLog -> Model -> ( Model, Maybe Bool )
gotRunnerLog log model =
    case model.runner of
        RunnerRunning _ ->
            if log.running then
                ( { model | runner = RunnerRunning { lines = log.lines } }, Nothing )

            else
                let
                    exit =
                        Maybe.withDefault 1 log.exitCode
                in
                if exit == 0 then
                    -- 成功は板を畳んで静かに祝う(トーストと差分の案内は別口)
                    ( { model | runner = RunnerIdle }, Just True )

                else
                    -- 失敗の時だけはログを隠さない(自動で全文展開)
                    ( { model | runner = RunnerFailed { exitCode = exit, lines = log.lines }, logExpanded = True }
                    , Just False
                    )

        _ ->
            ( model, Nothing )


{-| /runner/log 自体が失敗した(404 等)。回し続けても仕方ないので止める。 -}
runnerStopped : Model -> Model
runnerStopped model =
    { model | runner = RunnerUnavailable }


{-| 祝福が済んだ。golden が入れ替わったので img のキャッシュ避けを進める。
差分の取り直しは Main の仕事。
-}
blessDone : Model -> Model
blessDone model =
    { model | goldenBust = model.goldenBust + 1 }



-- デコーダ


listingDecoder : D.Decoder Listing
listingDecoder =
    D.map4 Listing
        (section "gallery")
        (section "golden")
        (section "debug")
        (section "sounds")


{-| どの節も欠けを失敗にしない — sounds を持たない古いサーバでも一覧は出す。 -}
section : String -> D.Decoder (List Item)
section name =
    D.oneOf [ D.field name (D.list itemDecoder), D.succeed [] ]


itemDecoder : D.Decoder Item
itemDecoder =
    D.map2 Item
        (D.field "name" D.string)
        (D.oneOf [ D.field "mtime" D.int, D.succeed 0 ])


diffDecoder : D.Decoder (Dict String Status)
diffDecoder =
    D.field "items"
        (D.list
            (D.map2 Tuple.pair
                (D.field "name" D.string)
                (D.field "status" (D.map statusFrom D.string))
            )
        )
        |> D.map Dict.fromList


{-| GET /gallery/targets — {"targets":[...]}。知らない文字列は捨てる
(欠け・壊れは Main が握りつぶし、3 択のままに倒す)。
-}
targetsDecoder : D.Decoder (List BakeTarget)
targetsDecoder =
    D.field "targets" (D.list D.string)
        |> D.map (List.filterMap targetFrom)


targetFrom : String -> Maybe BakeTarget
targetFrom raw =
    case raw of
        "bake" ->
            Just TargetBake

        "gallery-prologue" ->
            Just TargetPrologue

        "gallery-sounds" ->
            Just TargetSounds

        _ ->
            Nothing


runnerLogDecoder : D.Decoder RunnerLog
runnerLogDecoder =
    D.map3 RunnerLog
        (D.oneOf [ D.field "running" D.bool, D.succeed False ])
        (D.oneOf [ D.field "exitCode" (D.map Just D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "lines" (D.list D.string), D.succeed [] ])


statusFrom : String -> Status
statusFrom raw =
    case raw of
        "ok" ->
            StatusOk

        "diff" ->
            StatusDiff

        "missingGallery" ->
            MissingGallery

        _ ->
            -- 知らない判定は「正解がまだ無い」扱い(赤で騒がない)
            MissingGolden



-- 画面


imageUrl : String -> String -> String -> String
imageUrl base dir name =
    base ++ "/gallery/image?dir=" ++ dir ++ "&name=" ++ Url.percentEncode name


{-| golden だけキャッシュ避け付き。祝福で中身が入れ替わっても URL が同じだと
ブラウザが古い正解を見せ続けるため。
-}
goldenUrl : String -> Model -> String -> String
goldenUrl base model name =
    imageUrl base "golden" name
        ++ (if model.goldenBust > 0 then
                "&t=" ++ String.fromInt model.goldenBust

            else
                ""
           )


soundUrl : String -> String -> String
soundUrl base name =
    base ++ "/gallery/sound?name=" ++ Url.percentEncode name


view : String -> Model -> Html Msg
view base model =
    div [ HA.class "gallery min-h-0 flex-1 overflow-y-auto px-5 py-4" ]
        (case ( model.listing, model.error ) of
            ( Nothing, Just _ ) ->
                [ div [ HA.class "rounded-lg border border-edge bg-panel p-5" ]
                    [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "準備中" ]
                    , div [ HA.class "mt-1.5 text-xs text-ink-soft" ]
                        [ text "ギャラリーはまだ使えません。絵を焼くとここに並びます。" ]
                    ]
                ]

            ( Nothing, Nothing ) ->
                [ div [ HA.class "text-xs text-ink-soft" ] [ text "読み込み中…" ] ]

            ( Just listing, _ ) ->
                case model.bless of
                    Just bless ->
                        -- 祝福の歩き中は 1 枚に集中させる(格子は出さない)
                        viewBless base model bless

                    Nothing ->
                        List.concat
                            [ viewToolbar model
                            , viewRunner model
                            , viewBlessNudge model
                            , viewCompare base model
                            , viewGrid base listing model.diff
                            , viewSheets base listing
                            , viewSounds base listing
                            ]
        )


{-| このプロジェクトが持つ焼きの的。未着・未実装(Nothing)は 3 択全部(fail-open)。 -}
availableTargets : Model -> List BakeTarget
availableTargets model =
    Maybe.withDefault [ TargetBake, TargetPrologue, TargetSounds ] model.targets


{-| 的の選び(セグメント)を出すか。的が 1 つしか無ければ選ぶ意味がない
(焼くボタンだけ)。
-}
showsTargetChooser : Model -> Bool
showsTargetChooser model =
    List.length (availableTargets model) > 1


{-| 焼きの操作列。的の選び(プロジェクトが持つ的だけ)と火の点けボタン。
走行中は押せない。
-}
viewToolbar : Model -> List (Html Msg)
viewToolbar model =
    let
        busy =
            isBusy model.runner

        targetLabel target =
            case target of
                TargetBake ->
                    "ぜんぶ"

                TargetPrologue ->
                    "序章の絵"

                TargetSounds ->
                    "音"

        targetButton target =
            button
                [ HA.classList
                    [ ( "gallery-seg", True )
                    , ( "gallery-seg-active", model.bakeTarget == target )
                    ]
                , HA.disabled busy
                , HE.onClick (TargetChosen target)
                ]
                [ text (targetLabel target) ]
    in
    [ div [ HA.class "gallery-toolbar mb-4 flex flex-wrap items-center gap-3" ]
        [ if showsTargetChooser model then
            div [ HA.class "gallery-segs flex items-center overflow-hidden rounded border border-edge" ]
                (List.map targetButton (availableTargets model))

          else
            text ""
        , button
            [ HA.class "btn btn-primary"
            , HA.disabled busy
            , HE.onClick BakeClicked
            ]
            [ text
                (if busy then
                    "🔥 焼いています…"

                 else
                    "🔥 ギャラリーを焼く"
                )
            ]
        ]
    ]


{-| 焼きの進み・結果の板。成功時は出ない(Idle に畳まれ、トーストで祝う)。 -}
viewRunner : Model -> List (Html Msg)
viewRunner model =
    case model.runner of
        RunnerIdle ->
            []

        RunnerStarting ->
            [ div [ HA.class "gallery-runner mb-4 rounded-lg border border-edge bg-panel p-3 text-xs text-ink-soft" ]
                [ text "焼き始めています…" ]
            ]

        RunnerRunning info ->
            [ div [ HA.class "gallery-runner mb-4" ]
                [ Progress.view
                    { message = "🔥 焼いています…"
                    , lines = info.lines
                    , failed = False
                    , expanded = model.logExpanded
                    , onToggle = LogToggled
                    }
                ]
            ]

        RunnerFailed info ->
            [ div [ HA.class "gallery-runner mb-4" ]
                [ Progress.view
                    { message = "失敗しました。ログを見てください(手動の make も使えます)"
                    , lines = info.lines
                    , failed = True
                    , expanded = model.logExpanded
                    , onToggle = LogToggled
                    }
                , div [ HA.class "mt-1.5 flex justify-end" ]
                    [ button [ HA.class "btn btn-mini", HE.onClick LogClosed ] [ text "閉じる" ] ]
                ]
            ]

        RunnerUnavailable ->
            [ div [ HA.class "gallery-runner mb-4 rounded-lg border border-edge bg-panel p-3 text-xs text-ink-soft" ]
                [ text "準備中 — このサーバはまだ焼き口を持っていません。手元の make bake をどうぞ。" ]
            ]


{-| 差分・お手本待ちがある時の誘い。祝福の歩きへの入り口。 -}
viewBlessNudge : Model -> List (Html Msg)
viewBlessNudge model =
    case blessEntryLabel model of
        Nothing ->
            []

        Just label ->
            let
                count =
                    diffCount model

                message =
                    if allMissingGolden model then
                        "お手本がまだない絵が " ++ String.fromInt count ++ " 件あります。見て、お手本を作りましょう"

                    else
                        "差分が " ++ String.fromInt count ++ " 件あります。見比べて祝福しましょう"
            in
            [ div [ HA.class "gallery-nudge mb-4 flex flex-wrap items-center gap-3 rounded-lg border border-warn/40 bg-warn/10 px-4 py-3" ]
                [ span [ HA.class "text-xs text-ink" ] [ text message ]
                , span [ HA.class "flex-1" ] []
                , button [ HA.class "btn btn-primary", HE.onClick BlessOpened ]
                    [ text label ]
                ]
            ]


{-| 祝福の歩き。1 枚ずつ、正解と今回を並べて選ばせる。 -}
viewBless : String -> Model -> Bless -> List (Html Msg)
viewBless base model bless =
    case bless.remaining of
        [] ->
            []

        current :: _ ->
            let
                position =
                    bless.total - List.length bless.remaining + 1

                -- 正解がまだ無い絵は見比べる相手がいない — 今回の絵だけを見せて
                -- 「この絵をお手本にしますか」と聞く(POST は同じ祝福)
                missingGolden =
                    (model.diff |> Maybe.andThen (Dict.get current)) == Just MissingGolden
            in
            [ div [ HA.class "gallery-bless rounded-lg border border-accent/50 bg-panel p-4" ]
                [ div [ HA.class "mb-3 flex items-center gap-2" ]
                    [ span [ HA.class "badge bg-accent/20 text-accent" ]
                        [ text (String.fromInt position ++ " / " ++ String.fromInt bless.total) ]
                    , span [ HA.class "min-w-0 flex-1 truncate font-mono text-xs text-ink", HA.title current ]
                        [ text current ]
                    ]
                , if missingGolden then
                    div []
                        [ div [ HA.class "mb-2 text-xs text-ink-soft" ]
                            [ text "まだお手本がありません — この絵をお手本にしますか" ]
                        , div [ HA.class "flex flex-wrap gap-4" ]
                            [ comparePane "今回 (gallery)" (imageUrl base "gallery" current) ]
                        ]

                  else
                    div [ HA.class "flex flex-wrap gap-4" ]
                        [ comparePane "正解 (golden)" (goldenUrl base model current)
                        , comparePane "今回 (gallery)" (imageUrl base "gallery" current)
                        ]
                , div [ HA.class "mt-4 flex items-center gap-3" ]
                    [ button [ HA.class "btn btn-primary", HE.onClick BlessAccepted ]
                        [ text
                            (if missingGolden then
                                "✓ お手本にする"

                             else
                                "✓ 祝福する"
                            )
                        ]
                    , button [ HA.class "btn", HE.onClick BlessHeld ] [ text "保留" ]
                    , span [ HA.class "flex-1" ] []
                    , button [ HA.class "btn btn-ghost", HE.onClick BlessQuit ] [ text "やめる" ]
                    ]
                , if List.isEmpty bless.chosen then
                    text ""

                  else
                    div [ HA.class "mt-2 text-[11px] text-ink-faint" ]
                        [ text ("選んだ祝福: " ++ String.fromInt (List.length bless.chosen) ++ " 件(最後まで歩くと送ります)") ]
                ]
            ]


{-| 差分の見比べ(正解と今回を横並び)。DIFF の絵をクリックした時だけ出す。 -}
viewCompare : String -> Model -> List (Html Msg)
viewCompare base model =
    case model.compare of
        Nothing ->
            []

        Just name ->
            [ div [ HA.class "gallery-compare mb-5 rounded-lg border border-danger/50 bg-panel p-4" ]
                [ div [ HA.class "mb-3 flex items-center gap-2" ]
                    [ span [ HA.class "text-xs font-semibold text-ink" ] [ text ("見比べ: " ++ name) ]
                    , span [ HA.class "flex-1" ] []
                    , button [ HA.class "btn btn-mini", HE.onClick CompareClosed ] [ text "閉じる" ]
                    ]
                , div [ HA.class "flex flex-wrap gap-4" ]
                    [ comparePane "正解 (golden)" (goldenUrl base model name)
                    , comparePane "今回 (gallery)" (imageUrl base "gallery" name)
                    ]
                ]
            ]


comparePane : String -> String -> Html msg
comparePane label url =
    div [ HA.class "min-w-0 flex-1" ]
        [ div [ HA.class "mb-1 text-[11px] text-ink-faint" ] [ text label ]
        , img [ HA.class "gallery-shot w-full rounded border border-edge bg-well", HA.src url, HA.alt label ] []
        ]


viewGrid : String -> Listing -> Maybe (Dict String Status) -> List (Html Msg)
viewGrid base listing diff =
    let
        -- 「今回は焼かれなかったが正解はある」絵も見せたいので、一覧と判定の名前を合わせる
        names =
            List.map .name listing.gallery
                ++ (diff
                        |> Maybe.map (\d -> Dict.keys d |> List.filter (\n -> not (List.any (\g -> g.name == n) listing.gallery)))
                        |> Maybe.withDefault []
                   )
    in
    if List.isEmpty names then
        [ heading "焼いた絵"
        , div [ HA.class "mb-6 text-xs text-ink-soft" ] [ text "まだ絵がありません。焼くとここに並びます。" ]
        ]

    else
        [ heading "焼いた絵"
        , div [ HA.class "gallery-grid mb-6 grid grid-cols-[repeat(auto-fill,minmax(160px,1fr))] gap-3" ]
            (List.map (cell base diff) names)
        ]


cell : String -> Maybe (Dict String Status) -> String -> Html Msg
cell base diff name =
    let
        status =
            diff |> Maybe.andThen (Dict.get name)

        clickable =
            status == Just StatusDiff

        picture =
            case status of
                Just MissingGallery ->
                    div [ HA.class "flex aspect-square items-center justify-center bg-well text-[11px] text-ink-faint" ]
                        [ text "今回は焼かれていません" ]

                _ ->
                    img
                        [ HA.class "gallery-shot block w-full bg-well"
                        , HA.src (imageUrl base "gallery" name)
                        , HA.alt name
                        , HA.attribute "loading" "lazy"
                        ]
                        []
    in
    div
        (HA.classList
            [ ( "gallery-cell overflow-hidden rounded border border-edge bg-panel", True )
            , ( "cursor-pointer hover:border-danger/70", clickable )
            ]
            :: (if clickable then
                    [ HE.onClick (CompareOpened name), HA.title "クリックで正解と見比べる" ]

                else
                    []
               )
        )
        [ picture
        , div [ HA.class "flex items-center gap-1.5 px-2 py-1.5" ]
            [ span [ HA.class "min-w-0 flex-1 truncate font-mono text-[10px] text-ink-soft", HA.title name ] [ text name ]
            , badge status
            ]
        ]


badge : Maybe Status -> Html msg
badge status =
    case status of
        Just StatusOk ->
            span [ HA.class "badge shrink-0 bg-ok/15 text-ok" ] [ text "OK" ]

        Just StatusDiff ->
            span [ HA.class "badge shrink-0 bg-danger/15 text-danger" ] [ text "DIFF" ]

        Just MissingGolden ->
            span [ HA.class "badge shrink-0 bg-warn/15 text-warn" ] [ text "golden無し" ]

        Just MissingGallery ->
            span [ HA.class "badge shrink-0 bg-warn/15 text-warn" ] [ text "gallery無し" ]

        Nothing ->
            text ""


{-| debug/ の見取り図(全体・音・曲のまとめ画像)。ある物だけ出す。 -}
viewSheets : String -> Listing -> List (Html msg)
viewSheets base listing =
    let
        sheets =
            [ ( "all.png", "全体" ), ( "sounds.png", "音" ), ( "music.png", "曲" ) ]
                |> List.filter (\( name, _ ) -> List.any (\d -> d.name == name) listing.debug)
    in
    if List.isEmpty sheets then
        []

    else
        [ heading "見取り図 (debug)"
        , div [ HA.class "mb-6 flex flex-col gap-4" ]
            (sheets
                |> List.map
                    (\( name, label ) ->
                        div []
                            [ div [ HA.class "mb-1 text-[11px] text-ink-faint" ] [ text (label ++ " — " ++ name) ]
                            , img
                                [ HA.class "gallery-shot w-full max-w-3xl rounded border border-edge bg-well"
                                , HA.src (imageUrl base "debug" name)
                                , HA.alt name
                                , HA.attribute "loading" "lazy"
                                ]
                                []
                            ]
                    )
            )
        ]


viewSounds : String -> Listing -> List (Html msg)
viewSounds base listing =
    let
        wavs =
            listing.sounds |> List.filter (\s -> String.endsWith ".wav" s.name)
    in
    if List.isEmpty wavs then
        []

    else
        [ heading "音 (debug/sounds)"
        , div [ HA.class "mb-6 grid grid-cols-[repeat(auto-fill,minmax(240px,1fr))] gap-2" ]
            (wavs
                |> List.map
                    (\s ->
                        div [ HA.class "rounded border border-edge bg-panel px-2 py-1.5" ]
                            [ div [ HA.class "mb-1 truncate font-mono text-[10px] text-ink-soft", HA.title s.name ] [ text s.name ]
                            , audio [ HA.controls True, HA.class "w-full", HA.src (soundUrl base s.name) ] []
                            ]
                    )
            )
        ]


heading : String -> Html msg
heading label =
    div [ HA.class "mb-2 text-[11px] font-semibold text-ink-faint" ] [ text label ]
