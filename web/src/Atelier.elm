module Atelier exposing
    ( Candidates
    , Launch(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Out(..)
    , Phase(..)
    , PreviewState(..)
    , candidatesDecoder
    , candidatesFailed
    , gameLogDecoder
    , gameStartFailed
    , gameStarted
    , gotCandidates
    , gotGameLog
    , gotGameStatus
    , gotRunnerLines
    , hasCandidates
    , init
    , isBaking
    , isLaunchPolling
    , lightboxOpen
    , lightboxShownFile
    , needsTick
    , previewState
    , promoteFailed
    , promoted
    , showPicks
    , statusDecoder
    , update
    , view
    )

{-| アトリエの「候補えらび」— 生成された見た目候補をゲームへ装着(swap)する。

アトリエタブは 2 モード:
候補が 1 件以上あれば「候補えらび」が既定(この view が画面の主役 —
Doc エディタもそのタブ群も描かない)、無ければ従来の「そうこ」(Doc エディタ)。
どちらを描くかは Main が showPicks で判定し、リンク(OpenStorehouse /
OpenPicks)で行き来する。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。
送りたい事は update の戻り値 Out で Main へ返す(Gallery / Journey と同じ流儀)。
エンドポイント未実装のサーバでは候補モードに入らない(fail-open — 既存の
そうこ(Doc エディタ)はそのまま生きる)。候補ゼロのスロットはサーバが
返さない契約だが、来ても捨てる(防御 — ファイル名の羅列で画面を埋めない)。

装着の瞬間はこの画面の見せ場。成功はオーバーレイで大きく祝い、
旧版の退避先を必ず添える — 戻せると分かっていれば装着は怖くない。

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Progress
import Url


type alias Candidate =
    { file : String
    , note : Maybe String
    , isPrev : Bool
    , mtime : Int
    , previewReady : Bool
    }


type alias Slot =
    { slot : String
    , entityId : Maybe String
    , candidates : List Candidate
    , currentPreviewReady : Bool
    }


type alias Loose =
    { file : String
    , reason : String
    }


type alias Candidates =
    { slots : List Slot
    , loose : List Loose

    -- サーバがプレビューを焼いている最中か(自動で make atelier-preview が走る)
    , baking : Bool
    }


{-| Unavailable = サーバがまだ /atelier/candidates を持たない(404 等)。
候補ゾーンを出さないだけで、そうこ(エディタ)は普通に使える。
-}
type Data
    = Loading
    | Unavailable
    | Ready Candidates


type alias Selection =
    { slot : String
    , file : String
    }


{-| 装着(Swap)か巻き戻し(Rollback)か。オーバーレイの文言が変わるだけで、
サーバへの頼み事はどちらも同じ promote(戻すのも「前の版を装着する」)。
-}
type Mode
    = Swap
    | Rollback


{-| オーバーレイの段。Placed(装着しました)→ 1 秒 → Settled(反映・退避の報せ)。 -}
type Phase
    = Placed
    | Settled


{-| プレビュー拡大(lightbox)。140px そこそこの札ではドット絵の良し悪しは
判らない — クリックで原寸級に開く。compareWith はスロットの「いまの見た目」の
プレビューキー(file=<slot>)— 候補のカードだけ持ち、同じ場所で A/B に
切り替えて見比べる(いまの見た目のカード自身には比べる相手がない)。
-}
type alias Lightbox =
    { file : String
    , note : Maybe String
    , compareWith : Maybe String

    -- 「いまの見た目」側を映しているか(A/B トグル)
    , showingCurrent : Bool
    }


type alias Overlay =
    { mode : Mode
    , retired : Maybe String
    , phase : Phase

    -- 装着した瞬間にゲームが走っていたか(Settled の祝い文言用)
    , gameWasRunning : Bool
    }


{-| ゲーム起動(make debug)の進み。Flix のコンパイルは 1 分近くかかるので、
押した瞬間からボタンを畳み、ログを流して「起きている事」を見せる。
Starting の間だけ /game/log と /game/status を回す(bake の isPolling と同じ流儀)。
-}
type Launch
    = LaunchIdle
    | LaunchStarting { lines : List String }
    | LaunchRunning
    | LaunchFailed { exitCode : Int, lines : List String }


{-| GET /game/log(/runner/log と同じ形)。 -}
type alias GameLog =
    { running : Bool
    , exitCode : Maybe Int
    , lines : List String
    }


type alias Model =
    { data : Data
    , selected : Maybe Selection

    -- Nothing = /game/status 未着(不明)。不明のうちは装着前に起動の案内を挟む
    , gameRunning : Maybe Bool

    -- 「ゲームが起きていません」の案内カードを開いているか
    , gate : Bool

    -- 400 の理由(サーバの日本語文言)をボタンの近くに出す
    , promoteError : Maybe String

    -- promote が飛んでいる間の印(二度押し防止・オーバーレイの文言選び)
    , pending : Maybe Mode
    , overlay : Maybe Overlay

    -- 候補があっても「そうこ(Doc エディタ)を開く」を選んだ印
    , storehouse : Bool

    -- ゲーム起動(make debug)の進み
    , launch : Launch

    -- 起動ログの全文展開(進捗ミニパネル)。既定は畳み、失敗時だけ自動で開く
    , launchLogExpanded : Bool

    -- プレビュー焼きのログ末尾(/runner/log — gallery の焼きと共用の走者)
    , bakeLines : List String

    -- 焼きの進捗ミニパネルの全文展開。焼いていない時は
    -- 「作れませんでした」の『ログ』リンクでパネルごと開く印を兼ねる
    , bakeLogExpanded : Bool

    -- プレビュー画像のキャッシュ避け。候補の中身が変わった取得でだけ進む
    , bust : Int

    -- プレビューの拡大表示(開いている間だけ Just)
    , lightbox : Maybe Lightbox
    }


init : Model
init =
    { data = Loading
    , selected = Nothing
    , gameRunning = Nothing
    , gate = False
    , promoteError = Nothing
    , pending = Nothing
    , overlay = Nothing
    , storehouse = False
    , launch = LaunchIdle
    , launchLogExpanded = False
    , bakeLines = []
    , bakeLogExpanded = False
    , bust = 0
    , lightbox = Nothing
    }


type Msg
    = CandidateClicked String String
    | SwapClicked
    | StartGameClicked
    | PromoteAnywayClicked
    | RollbackClicked String String
    | OverlayTick
    | OverlayClosed
    | OpenStorehouse
    | OpenPicks
    | LaunchLogToggled
    | BakeLogToggled
    | PreviewClicked { file : String, note : Maybe String, compareWith : Maybe String }
    | LightboxClosed
    | CompareToggled
    | NoOp


{-| サーバへ送りたい事(封筒の発行は Main)。OutClosed はオーバーレイを
閉じた合図 — Main が候補と旅路を取り直し、焼きへの誘いを出す。
-}
type Out
    = OutNone
    | OutPromote { candidate : String, slot : String }
    | OutStartGame
    | OutClosed


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        CandidateClicked slot file ->
            -- 選び直しで前の失敗文言と起動案内は畳む(古い理由を残さない)
            ( { model
                | selected = Just { slot = slot, file = file }
                , promoteError = Nothing
                , gate = False
              }
            , OutNone
            )

        SwapClicked ->
            case ( model.selected, model.pending ) of
                ( Just sel, Nothing ) ->
                    if model.gameRunning == Just True then
                        promote Swap sel model

                    else
                        -- 起きていない(または不明)。装着は今すぐできるが、
                        -- 変化をその場で見るには起動が要る — 両方を差し出す
                        ( { model | gate = True }, OutNone )

                _ ->
                    ( model, OutNone )

        StartGameClicked ->
            -- 押した瞬間に「起動しています…」へ(連打の芽を最初から摘む)。
            -- 走っているかは楽観で決めず、/game/status のポーリングが教えてくれる
            case model.launch of
                LaunchStarting _ ->
                    ( model, OutNone )

                LaunchRunning ->
                    ( model, OutNone )

                _ ->
                    -- ログ展開は畳み直す(前回の失敗展開を持ち越さない)
                    ( { model | launch = LaunchStarting { lines = [] }, launchLogExpanded = False }
                    , OutStartGame
                    )

        PromoteAnywayClicked ->
            case ( model.selected, model.pending ) of
                ( Just sel, Nothing ) ->
                    promote Swap sel model

                _ ->
                    ( model, OutNone )

        RollbackClicked slot file ->
            -- 戻すのは怖くない操作なので、起動の案内も選択も挟まず直行する
            if model.pending == Nothing then
                promote Rollback { slot = slot, file = file } model

            else
                ( model, OutNone )

        OverlayTick ->
            ( { model
                | overlay = Maybe.map (\o -> { o | phase = Settled }) model.overlay
              }
            , OutNone
            )

        OverlayClosed ->
            ( { model | overlay = Nothing }, OutClosed )

        OpenStorehouse ->
            ( { model | storehouse = True }, OutNone )

        OpenPicks ->
            ( { model | storehouse = False }, OutNone )

        LaunchLogToggled ->
            ( { model | launchLogExpanded = not model.launchLogExpanded }, OutNone )

        BakeLogToggled ->
            ( { model | bakeLogExpanded = not model.bakeLogExpanded }, OutNone )

        PreviewClicked info ->
            -- 拡大を開くだけ — 選択(selected)には触れない。
            -- カードを選ぶのと絵を確かめるのは別の動詞
            ( { model
                | lightbox =
                    Just
                        { file = info.file
                        , note = info.note
                        , compareWith = info.compareWith
                        , showingCurrent = False
                        }
              }
            , OutNone
            )

        LightboxClosed ->
            ( { model | lightbox = Nothing }, OutNone )

        CompareToggled ->
            ( { model
                | lightbox =
                    Maybe.map (\lb -> { lb | showingCurrent = not lb.showingCurrent }) model.lightbox
              }
            , OutNone
            )

        NoOp ->
            ( model, OutNone )


promote : Mode -> Selection -> Model -> ( Model, Out )
promote mode sel model =
    ( { model | pending = Just mode, gate = False, promoteError = Nothing }
    , OutPromote { candidate = sel.file, slot = sel.slot }
    )



-- サーバ応答の流し込み(封筒の受領は Main、意味づけはここ)


{-| GET /atelier/candidates。焼き待ちの間は 2 秒毎に取り直すので、
選択は「同じファイルがまだ居るなら」保つ(装着や退避で消えていたら畳む)。
候補ゼロのスロットは捨てる(サーバは返さない契約だが、来ても描かない防御)。
キャッシュ避け(bust)は中身が変わった取得でだけ進める —
焼き上がりでプレビューが差し替わった瞬間だけ img を取り直させる。
-}
gotCandidates : Candidates -> Model -> Model
gotCandidates candidates model =
    let
        fresh =
            { candidates
                | slots = List.filter (\slot -> not (List.isEmpty slot.candidates)) candidates.slots
            }

        changed =
            case model.data of
                Ready old ->
                    old /= fresh

                _ ->
                    True

        stillThere sel =
            List.any
                (\slot ->
                    slot.slot == sel.slot && List.any (\c -> c.file == sel.file) slot.candidates
                )
                fresh.slots
    in
    { model
        | data = Ready fresh
        , selected = Maybe.andThen (\sel -> ifTrue (stillThere sel) sel) model.selected
        , bust =
            if changed then
                model.bust + 1

            else
                model.bust
    }


ifTrue : Bool -> a -> Maybe a
ifTrue cond value =
    if cond then
        Just value

    else
        Nothing


{-| /atelier/candidates の失敗。旧サーバ(404)も読めない応答も同じ —
候補ゾーンを出さないだけ(そうこは生きる)。
-}
candidatesFailed : Model -> Model
candidatesFailed model =
    { model | data = Unavailable }


{-| GET /game/status。起動待ち(Starting)の間に running=true が来たら
起動完了 — パネルを緑の帯に畳み、ポーリングを止める。
-}
gotGameStatus : Bool -> Model -> Model
gotGameStatus running model =
    { model
        | gameRunning = Just running
        , launch =
            case ( model.launch, running ) of
                ( LaunchStarting _, True ) ->
                    LaunchRunning

                ( launch, _ ) ->
                    launch
    }


{-| POST /game/start が受かった(202。409 = すでに起動中も realApi が ok に
均している)。どちらも「起動処理は走っている」— 本当の姿はポーリングが教える。
-}
gameStarted : Model -> Model
gameStarted model =
    case model.launch of
        LaunchStarting _ ->
            model

        LaunchRunning ->
            model

        _ ->
            { model | launch = LaunchStarting { lines = [] } }


{-| POST /game/start 自体の失敗(404 等)。ボタンを戻す(文言は Main のトースト)。 -}
gameStartFailed : Model -> Model
gameStartFailed model =
    { model | launch = LaunchIdle }


{-| GET /game/log の流し込み。起動待ちの間だけ意味を持つ(古い応答は無視)。
make debug が異常終了したらログの尻尾ごと失敗表示へ。
-}
gotGameLog : GameLog -> Model -> Model
gotGameLog log model =
    case model.launch of
        LaunchStarting _ ->
            if log.running then
                { model | launch = LaunchStarting { lines = log.lines } }

            else
                case log.exitCode of
                    Just code ->
                        if code == 0 then
                            -- 静かに終わった(窓を閉じた等)。ボタンを戻すだけ
                            { model | launch = LaunchIdle }

                        else
                            -- 失敗の時だけはログを隠さない(自動で全文展開)
                            { model
                                | launch = LaunchFailed { exitCode = code, lines = log.lines }
                                , launchLogExpanded = True
                            }

                    Nothing ->
                        -- まだ始まっていない(ログ口が空)。回し続ける
                        model

        _ ->
            model


{-| サーバがプレビューを焼いている最中か。Main はこれを見て
/atelier/candidates(2 秒)と /runner/log(bake と同じ口)を回す。
-}
isBaking : Model -> Bool
isBaking model =
    case model.data of
        Ready candidates ->
            candidates.baking

        _ ->
            False


{-| /runner/log の行の流し込み(走者は gallery の焼きと共用 —
封筒の受領と Gallery への配達は Main、こちらは進捗パネルの材料に写すだけ)。
-}
gotRunnerLines : List String -> Model -> Model
gotRunnerLines lines model =
    { model | bakeLines = lines }


{-| カードのプレビュー領域に何を描くか(ready と baking から決まる規則)。
焼きが自動なので、未完はまず「焼いています…」。焼きが止まってもまだ無いなら
失敗(ログへの小さな導線を出す)。
-}
type PreviewState
    = PreviewImage
    | PreviewBaking
    | PreviewFailed


previewState : { ready : Bool, baking : Bool } -> PreviewState
previewState { ready, baking } =
    if ready then
        PreviewImage

    else if baking then
        PreviewBaking

    else
        PreviewFailed


{-| 起動ログ・状態のポーリングを回すべきか(bake の isPolling と同じ流儀)。 -}
isLaunchPolling : Model -> Bool
isLaunchPolling model =
    case model.launch of
        LaunchStarting _ ->
            True

        _ ->
            False


{-| POST /atelier/promote 成功。選択を畳んでオーバーレイの祝いへ。 -}
promoted : Maybe String -> Model -> Model
promoted retired model =
    { model
        | overlay =
            Just
                { mode = Maybe.withDefault Swap model.pending
                , retired = retired
                , phase = Placed
                , gameWasRunning = model.gameRunning == Just True
                }
        , selected = Nothing
        , pending = Nothing
        , gate = False
    }


{-| POST /atelier/promote の失敗(400 等)。サーバの日本語の理由だけを
ボタンの近くに出す — 生のエラー行は見せない。
-}
promoteFailed : String -> Model -> Model
promoteFailed message model =
    { model | pending = Nothing, promoteError = Just (cleanReason message) }


{-| JS 橋の "Error: HTTP 400: … — 理由" から理由だけを取り出す。
区切りが見つからない時は全文(何も出ないよりまし)。
-}
cleanReason : String -> String
cleanReason message =
    case String.split " — " message of
        _ :: rest ->
            if List.isEmpty rest then
                message

            else
                String.join " — " rest

        [] ->
            message


{-| プレビュー拡大が開いているか。Main が Esc の購読を生かす条件。 -}
lightboxOpen : Model -> Bool
lightboxOpen model =
    model.lightbox /= Nothing


{-| 拡大に今映っているプレビューのキー(A/B トグルの規則)。
showingCurrent なら「いまの見た目」(file=<slot>)、比べる相手が無ければ常に候補。
-}
lightboxShownFile : Model -> Maybe String
lightboxShownFile model =
    Maybe.map
        (\lb ->
            if lb.showingCurrent then
                Maybe.withDefault lb.file lb.compareWith

            else
                lb.file
        )
        model.lightbox


{-| オーバーレイの段送り(Placed → 1 秒 → Settled)を待っているか。
Main の subscriptions がこれを見て 1 秒後の OverlayTick を流す。
-}
needsTick : Model -> Bool
needsTick model =
    case model.overlay of
        Just overlay ->
            overlay.phase == Placed

        _ ->
            False



-- デコーダ


candidatesDecoder : D.Decoder Candidates
candidatesDecoder =
    D.map3 Candidates
        (D.oneOf [ D.field "slots" (D.list slotDecoder), D.succeed [] ])
        (D.oneOf [ D.field "loose" (D.list looseDecoder), D.succeed [] ])
        -- 旧サーバには無いフィールド。欠けは「焼いていない」に倒す(fail-open)
        (D.oneOf [ D.field "baking" D.bool, D.succeed False ])


slotDecoder : D.Decoder Slot
slotDecoder =
    D.map4 Slot
        (D.field "slot" D.string)
        (D.oneOf [ D.field "entityId" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "candidates" (D.list candidateDecoder), D.succeed [] ])
        (D.oneOf [ D.field "currentPreviewReady" D.bool, D.succeed False ])


candidateDecoder : D.Decoder Candidate
candidateDecoder =
    D.map5 Candidate
        (D.field "file" D.string)
        (D.oneOf [ D.field "note" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "isPrev" D.bool, D.succeed False ])
        (D.oneOf [ D.field "mtime" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "previewReady" D.bool, D.succeed False ])


looseDecoder : D.Decoder Loose
looseDecoder =
    D.map2 Loose
        (D.field "file" D.string)
        (D.oneOf [ D.field "reason" D.string, D.succeed "" ])


{-| GET /game/status → running。壊れた応答は「走っていない」に倒す
(起動の案内が出るだけで、装着自体は妨げない)。
-}
statusDecoder : D.Decoder Bool
statusDecoder =
    D.oneOf [ D.field "running" D.bool, D.succeed False ]


{-| GET /game/log(/runner/log と同じ形)。欠けは既定値に倒す。 -}
gameLogDecoder : D.Decoder GameLog
gameLogDecoder =
    D.map3 GameLog
        (D.oneOf [ D.field "running" D.bool, D.succeed False ])
        (D.oneOf [ D.field "exitCode" (D.map Just D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "lines" (D.list D.string), D.succeed [] ])


{-| 候補(1 件以上)が届いているか。アトリエタブのモード分けの材料。 -}
hasCandidates : Model -> Bool
hasCandidates model =
    case model.data of
        Ready candidates ->
            not (List.isEmpty candidates.slots)

        _ ->
            False


{-| アトリエタブで「候補えらび」を主役に描くべきか。候補が 1 件以上ある時の
既定で、「そうこを開く」を選んでいる間だけ従来のエディタへ譲る。
-}
showPicks : Model -> Bool
showPicks model =
    hasCandidates model && not model.storehouse



-- 画面


{-| 候補えらびモードの画面(Main が showPicks の時だけこれを主役に描く —
Doc エディタもそのタブ群もここには出ない)。1 画面に収まる密度を保つ:
スロットは実際には 1〜数個で、カードの直下に装着ボタンが必ず見える。
-}
view : String -> Model -> Html Msg
view base model =
    case model.data of
        Ready candidates ->
            div [ HA.class "atelier-picks min-h-0 flex-1 overflow-y-auto px-6 py-6" ]
                [ div [ HA.class "mx-auto w-full max-w-3xl" ]
                    (List.concat
                        [ [ div [ HA.class "mb-5" ]
                                [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "🎨 候補えらび" ]
                                , div [ HA.class "mt-1 text-[11px] text-ink-soft" ]
                                    [ text "新しい見た目の候補です。カードを選んで、ゲームに装着しましょう" ]
                                ]
                          ]
                        , viewBakePanel model candidates
                        , List.map (viewSlot base model candidates.baking) candidates.slots
                        , viewActions model
                        , viewLoose candidates.loose
                        , [ div [ HA.class "mt-8 border-t border-edge pt-3" ]
                                [ button [ HA.class "btn btn-ghost", HE.onClick OpenStorehouse ]
                                    [ text "🗄 そうこ(Doc エディタ)を開く" ]
                                ]
                          ]
                        ]
                    )
                , case model.overlay of
                    Just overlay ->
                        viewOverlay overlay

                    Nothing ->
                        text ""
                , case model.lightbox of
                    Just lightbox ->
                        viewLightbox base model.bust lightbox

                    Nothing ->
                        text ""
                ]

        _ ->
            text ""


{-| 焼きの進捗ミニパネル(ゾーンに 1 枚 — カード毎には出さない)。
焼いている間は常に出す。焼きが止まってもプレビューが欠けている時は
「作れませんでした」の『ログ』リンクから同じパネルを開ける(失敗の赤)。
-}
viewBakePanel : Model -> Candidates -> List (Html Msg)
viewBakePanel model candidates =
    if candidates.baking then
        [ div [ HA.class "atelier-bake mb-4" ]
            [ Progress.view
                { message = "プレビューを焼いています… 焼き上がったカードから順に映ります"
                , lines = model.bakeLines
                , failed = False
                , expanded = model.bakeLogExpanded
                , onToggle = BakeLogToggled
                }
            ]
        ]

    else if model.bakeLogExpanded && anyPreviewMissing candidates then
        [ div [ HA.class "atelier-bake mb-4" ]
            [ Progress.view
                { message = "プレビューが作れませんでした。ログを確認してください"
                , lines = model.bakeLines
                , failed = True
                , expanded = True
                , onToggle = BakeLogToggled
                }
            ]
        ]

    else
        []


anyPreviewMissing : Candidates -> Bool
anyPreviewMissing candidates =
    List.any
        (\slot ->
            not slot.currentPreviewReady
                || List.any (\c -> not c.previewReady) slot.candidates
        )
        candidates.slots


viewSlot : String -> Model -> Bool -> Slot -> Html Msg
viewSlot base model baking slot =
    div [ HA.class "atelier-slot mb-4" ]
        [ div [ HA.class "mb-1.5 flex items-center gap-2" ]
            [ span [ HA.class "font-mono text-[11px] text-ink-soft" ] [ text slot.slot ]
            , case slot.entityId of
                Just entity ->
                    span [ HA.class "badge" ] [ text entity ]

                Nothing ->
                    text ""
            ]
        , div [ HA.class "flex flex-wrap gap-2" ]
            (viewCurrentCard base model baking slot
                :: List.map (viewCard base model baking slot.slot) slot.candidates
            )
        ]


{-| 「いまの見た目」の参照カード — 装着中の姿を並べて見比べる基準。
押せない(候補ではない)ので、枠も落ち着かせる。
-}
viewCurrentCard : String -> Model -> Bool -> Slot -> Html Msg
viewCurrentCard base model baking slot =
    div [ HA.class "atelier-card atelier-card-current w-56 rounded-lg border border-edge/60 bg-panel p-3 opacity-80" ]
        [ viewPreview base
            model.bust
            { file = slot.slot, note = Nothing, compareWith = Nothing }
            (previewState { ready = slot.currentPreviewReady, baking = baking })
        , div [ HA.class "mt-2 flex items-center gap-1.5" ]
            [ span [ HA.class "badge shrink-0" ] [ text "いまの見た目" ]
            , span [ HA.class "min-w-0 flex-1 truncate font-mono text-[10px] text-ink-faint", HA.title slot.slot ]
                [ text (baseName slot.slot) ]
            ]
        ]


{-| カード上段のプレビュー領域(高さ固定 — 焼き待ちでもカードが跳ねない)。
絵が映っている時はクリックで拡大(lightbox)— カードの選択とは別の動詞なので
伝播は止める。note はカードの説明文、compareWith は「いまの見た目」の
プレビューキー(候補カードだけ Just — 拡大の中で A/B に切り替える)。
-}
viewPreview : String -> Int -> { file : String, note : Maybe String, compareWith : Maybe String } -> PreviewState -> Html Msg
viewPreview base bust info state =
    div
        (HA.class "atelier-preview relative flex h-[180px] w-full items-center justify-center overflow-hidden rounded bg-black/50"
            :: (case state of
                    PreviewImage ->
                        [ HA.class "cursor-zoom-in", stopClick (PreviewClicked info) ]

                    _ ->
                        []
               )
        )
        (case state of
            PreviewImage ->
                [ Html.img
                    [ HA.src (previewUrl base bust info.file)
                    , HA.alt ""
                    , HA.class "max-h-full max-w-full object-contain"
                    , HA.style "image-rendering" "pixelated"
                    ]
                    []
                , span
                    [ HA.class "pointer-events-none absolute right-1.5 top-1.5 text-[11px] opacity-50"
                    , HA.attribute "aria-hidden" "true"
                    ]
                    [ text "🔍" ]
                ]

            PreviewBaking ->
                [ div [ HA.class "flex flex-col items-center gap-1.5" ]
                    [ span [ HA.class "progress-spinner", HA.attribute "aria-hidden" "true" ] []
                    , span [ HA.class "text-[10px] text-ink-faint" ] [ text "プレビューを焼いています…" ]
                    ]
                ]

            PreviewFailed ->
                [ div [ HA.class "flex flex-col items-center gap-1" ]
                    [ span [ HA.class "text-[10px] text-ink-faint" ] [ text "プレビューが作れませんでした" ]
                    , button
                        [ HA.class "btn btn-ghost btn-mini"
                        , stopClick BakeLogToggled
                        ]
                        [ text "ログ" ]
                    ]
                ]
        )


previewUrl : String -> Int -> String -> String
previewUrl base bust file =
    base ++ "/atelier/preview?file=" ++ Url.percentEncode file ++ "&t=" ++ String.fromInt bust


viewCard : String -> Model -> Bool -> String -> Candidate -> Html Msg
viewCard base model baking slotName candidate =
    let
        selected =
            model.selected == Just { slot = slotName, file = candidate.file }
    in
    div
        [ HA.classList
            [ ( "atelier-card w-56 cursor-pointer rounded-lg border bg-panel p-3", True )
            , ( "border-accent ring-1 ring-accent/50", selected )
            , ( "border-edge hover:border-ink-faint", not selected )
            , ( "atelier-card-prev opacity-60", candidate.isPrev )
            ]
        , HE.onClick (CandidateClicked slotName candidate.file)
        ]
        [ viewPreview base
            model.bust
            { file = candidate.file, note = candidate.note, compareWith = Just slotName }
            (previewState { ready = candidate.previewReady, baking = baking })
        , div [ HA.class "mt-2 flex items-center gap-1.5" ]
            [ span [ HA.class "min-w-0 flex-1 truncate font-mono text-[11px] text-ink", HA.title candidate.file ]
                [ text (baseName candidate.file) ]
            , if candidate.isPrev then
                span [ HA.class "badge shrink-0 bg-white/10" ] [ text (prevTag candidate.file) ]

              else
                text ""
            ]
        , case candidate.note of
            Just note ->
                div [ HA.class "mt-1.5 text-[11px] leading-relaxed text-ink-soft" ] [ text note ]

            Nothing ->
                text ""
        , div [ HA.class "mt-1.5 flex items-center gap-2" ]
            [ span [ HA.class "text-[10px] text-ink-faint" ] [ text (mtimeLabel candidate.mtime) ]
            , span [ HA.class "flex-1" ] []
            , if candidate.isPrev then
                button
                    [ HA.class "btn btn-mini"
                    , stopClick (RollbackClicked slotName candidate.file)
                    ]
                    [ text "↩ この版に戻す" ]

              else
                text ""
            ]
        ]


{-| カード内ボタンはカードの選択クリックと二重に効かせない。 -}
stopClick : msg -> Html.Attribute msg
stopClick msg =
    HE.stopPropagationOn "click" (D.succeed ( msg, True ))


viewActions : Model -> List (Html Msg)
viewActions model =
    [ div [ HA.class "atelier-actions mt-1 flex flex-wrap items-center gap-3" ]
        [ button
            [ HA.class "btn btn-primary"
            , HA.disabled (model.selected == Nothing || model.pending /= Nothing)
            , HE.onClick SwapClicked
            ]
            [ text
                (if model.pending /= Nothing then
                    "装着しています…"

                 else
                    "🔄 ゲームに装着(swap)"
                )
            ]
        , case model.promoteError of
            Just reason ->
                span [ HA.class "text-[11px] text-danger" ] [ text ("装着できませんでした — " ++ reason) ]

            Nothing ->
                text ""
        ]
    , if model.gate then
        div [ HA.class "atelier-gate mt-3 max-w-lg rounded-lg border border-edge bg-panel p-4" ]
            (div [ HA.class "text-xs text-ink" ]
                [ text "ゲームが起きていません。装着は今すぐできますが、変化をその場で見るには起動しましょう" ]
                :: div [ HA.class "mt-3 flex items-center gap-3" ]
                    [ button
                        [ HA.class "btn btn-primary"
                        , HA.disabled (launchBusy model.launch)
                        , HE.onClick StartGameClicked
                        ]
                        [ text
                            (case model.launch of
                                LaunchStarting _ ->
                                    "⏳ 起動しています…"

                                _ ->
                                    "▶ ゲームを起動する"
                            )
                        ]
                    , button [ HA.class "btn", HE.onClick PromoteAnywayClicked ]
                        [ text "そのまま装着" ]
                    ]
                :: viewLaunch model
            )

      else
        text ""
    ]


launchBusy : Launch -> Bool
launchBusy launch =
    case launch of
        LaunchStarting _ ->
            True

        LaunchRunning ->
            True

        _ ->
            False


{-| 起動の進み(進捗ミニパネル — bake と同じ形)。Flix のコンパイルは 1 分近く
かかるので、待たせる理由を一言で固定し、ログは末尾だけ(⤢ で全文)。
完了は静かな 1 行、失敗の時だけは自動で全文を開く。
-}
viewLaunch : Model -> List (Html Msg)
viewLaunch model =
    case model.launch of
        LaunchIdle ->
            []

        LaunchStarting info ->
            [ div [ HA.class "atelier-launch mt-3" ]
                [ Progress.view
                    { message = "起動しています… コンパイルには1分ほどかかります。窓が開いたらそのまま遊べます"
                    , lines = info.lines
                    , failed = False
                    , expanded = model.launchLogExpanded
                    , onToggle = LaunchLogToggled
                    }
                ]
            ]

        LaunchRunning ->
            [ div [ HA.class "atelier-launch-ok mt-3 rounded border border-ok/40 bg-ok/10 px-3 py-2 text-xs font-semibold text-ok" ]
                [ text "✓ ゲームが起きました。装着すると走っている画面にすぐ映ります" ]
            ]

        LaunchFailed info ->
            [ div [ HA.class "atelier-launch mt-3" ]
                [ Progress.view
                    { message = "起動に失敗しました。ログを確認してください(ターミナルの make debug でも試せます)"
                    , lines = info.lines
                    , failed = True
                    , expanded = model.launchLogExpanded
                    , onToggle = LaunchLogToggled
                    }
                ]
            ]


viewLoose : List Loose -> List (Html msg)
viewLoose loose =
    if List.isEmpty loose then
        []

    else
        [ div [ HA.class "atelier-loose mt-4" ]
            (div [ HA.class "mb-1 text-[11px] text-ink-faint" ] [ text "スロットが見つからない候補" ]
                :: List.map
                    (\l ->
                        div [ HA.class "flex items-baseline gap-2 text-[11px] text-ink-faint" ]
                            [ span [ HA.class "font-mono" ] [ text l.file ]
                            , span [] [ text l.reason ]
                            ]
                    )
                    loose
            )
        ]


{-| 装着の瞬間のオーバーレイ。段送り(Placed → Settled)は CSS の遷移で
ふわりと出す(prefers-reduced-motion では即時)。
-}
viewOverlay : Overlay -> Html Msg
viewOverlay overlay =
    div [ HA.class "atelier-overlay fixed inset-0 z-50 flex items-center justify-center bg-black/70" ]
        [ div [ HA.class "atelier-overlay-card mx-4 w-full max-w-md rounded-lg border border-edge bg-panel p-8 text-center" ]
            [ div [ HA.class "text-lg font-semibold text-ink" ]
                [ text
                    (case overlay.mode of
                        Swap ->
                            "スロットに装着しました"

                        Rollback ->
                            "まえの見た目に戻しました"
                    )
                ]
            , div
                [ HA.classList
                    [ ( "atelier-settle mt-4 flex flex-col gap-2", True )
                    , ( "atelier-settle-on", overlay.phase == Settled )
                    ]
                ]
                [ if overlay.gameWasRunning then
                    div [ HA.class "text-sm font-semibold text-ok" ]
                        [ text "✓ 走っているゲームに反映されました(watchFile)" ]

                  else
                    div [ HA.class "text-xs text-ink-soft" ]
                        [ text "ゲームを起動すると、この見た目で立ち上がります" ]
                , case overlay.retired of
                    Just retired ->
                        div [ HA.class "text-[11px] text-ink-faint" ]
                            [ text ("旧版は " ++ retired ++ " に退避しました。いつでも戻せます") ]

                    Nothing ->
                        text ""
                ]
            , button [ HA.class "btn mt-6", HE.onClick OverlayClosed ]
                [ text "アトリエに戻る" ]
            ]
        ]

{-| プレビューの拡大(lightbox)。暗幕の上に PNG を大きく
(〜90vw / 85vh・ドットのまま)。閉じるのは 閉じる ボタン・暗幕クリック・Esc
(Esc の購読は Main)。候補には「いまの見た目と見比べる」トグル —
同じ場所で絵だけが入れ替わるのが一番比べやすい。
-}
viewLightbox : String -> Int -> Lightbox -> Html Msg
viewLightbox base bust lightbox =
    let
        shownFile =
            if lightbox.showingCurrent then
                Maybe.withDefault lightbox.file lightbox.compareWith

            else
                lightbox.file
    in
    div
        [ HA.class "atelier-lightbox fixed inset-0 z-50 flex flex-col items-center justify-center gap-3 bg-black/80"
        , HE.onClick LightboxClosed
        ]
        [ Html.img
            [ HA.src (previewUrl base bust shownFile)
            , HA.alt ""
            , HA.class "max-h-[85vh] max-w-[90vw] object-contain"
            , HA.style "image-rendering" "pixelated"
            , stopClick NoOp
            ]
            []
        , div
            [ HA.class "flex max-w-[90vw] flex-col items-center gap-2 text-center"
            , stopClick NoOp
            ]
            [ div [ HA.class "font-mono text-[11px] text-ink" ]
                [ text
                    (if lightbox.showingCurrent then
                        "いまの見た目"

                     else
                        baseName lightbox.file
                    )
                ]
            , case ( lightbox.note, lightbox.showingCurrent ) of
                ( Just note, False ) ->
                    div [ HA.class "text-[11px] text-ink-soft" ] [ text note ]

                _ ->
                    text ""
            , div [ HA.class "mt-1 flex items-center gap-3" ]
                [ case lightbox.compareWith of
                    Just _ ->
                        button [ HA.class "btn btn-mini", stopClick CompareToggled ]
                            [ text
                                (if lightbox.showingCurrent then
                                    "候補に戻す"

                                 else
                                    "👀 いまの見た目と見比べる"
                                )
                            ]

                    Nothing ->
                        text ""
                , button [ HA.class "btn btn-mini", stopClick LightboxClosed ]
                    [ text "閉じる" ]
                ]
            ]
        ]



-- 小物


baseName : String -> String
baseName path =
    String.split "/" path
        |> List.reverse
        |> List.head
        |> Maybe.withDefault path


{-| 退避版の札。ファイル名の "prev-N" から N を拾う(見つからなければ「退避」)。 -}
prevTag : String -> String
prevTag file =
    case String.indexes "prev-" file of
        i :: _ ->
            let
                digits =
                    String.dropLeft (i + 5) file
                        |> String.toList
                        |> takeWhileDigit
                        |> String.fromList
            in
            if digits == "" then
                "退避"

            else
                "退避 " ++ digits

        [] ->
            "退避"


takeWhileDigit : List Char -> List Char
takeWhileDigit chars =
    case chars of
        c :: rest ->
            if Char.isDigit c then
                c :: takeWhileDigit rest

            else
                []

        [] ->
            []


{-| mtime(epoch 秒、ms でも吸収)を UTC の「MM/DD HH:MM」で。0 は出さない。 -}
mtimeLabel : Int -> String
mtimeLabel mtime =
    if mtime <= 0 then
        ""

    else
        let
            seconds =
                if mtime > 100000000000 then
                    -- ms で来た(10^11 超は秒ではあり得ない未来)
                    mtime // 1000

                else
                    mtime

            days =
                seconds // 86400

            secondsOfDay =
                seconds - days * 86400

            -- 1970-01-01 からの日数 → 月日(グレゴリオ暦)
            ( _, month, day ) =
                civilFromDays days

            pad n =
                String.padLeft 2 '0' (String.fromInt n)
        in
        pad month ++ "/" ++ pad day ++ " " ++ pad (secondsOfDay // 3600) ++ ":" ++ pad (modBy 60 (secondsOfDay // 60))


{-| 日数 → (年, 月, 日)。Howard Hinnant の civil_from_days。 -}
civilFromDays : Int -> ( Int, Int, Int )
civilFromDays z0 =
    let
        z =
            z0 + 719468

        era =
            (if z >= 0 then
                z

             else
                z - 146096
            )
                // 146097

        doe =
            z - era * 146097

        yoe =
            (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365

        y =
            yoe + era * 400

        doy =
            doe - (365 * yoe + yoe // 4 - yoe // 100)

        mp =
            (5 * doy + 2) // 153

        d =
            doy - (153 * mp + 2) // 5 + 1

        m =
            if mp < 10 then
                mp + 3

            else
                mp - 9
    in
    ( if m <= 2 then
        y + 1

      else
        y
    , m
    , d
    )
