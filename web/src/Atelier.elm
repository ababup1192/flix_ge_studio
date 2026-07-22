module Atelier exposing
    ( Candidates
    , CreatePath(..)
    , CreateSlot
    , Launch(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Out(..)
    , Phase(..)
    , PreviewState(..)
    , candidatesDecoder
    , candidatesFailed
    , cardAction
    , autoCopyName
    , copyDone
    , chosenPath
    , copyFailed
    , copyRetry
    , createOpen
    , createPaths
    , createSlotsDecoder
    , directionPlaceholder
    , gameLogDecoder
    , gameStartFailed
    , gameStarted
    , gotCandidates
    , gotGameLog
    , gamePromptErrorShown
    , gotGamePrompt
    , gamePromptFailed
    , gotGameStatus
    , gotPrompt
    , gotRunnerLines
    , gotSlots
    , hasCandidates
    , init
    , isBaking
    , isLaunchPolling
    , lightboxOpen
    , lightboxShownFile
    , needsCopyReset
    , needsTick
    , openCreateForGame
    , previewState
    , promoteFailed
    , promoted
    , promptFailed
    , scaffoldDone
    , scaffoldErrorShown
    , scaffoldFailed
    , scaffoldResultDecoder
    , scaffoldUnavailable
    , selectedSlotHint
    , shownGamePrompt
    , shownWirePrompt
    , shownPrompt
    , showPicks
    , slotsFailed
    , statusDecoder
    , update
    , view
    , viewCreate
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

    -- 退避(prev)版か — 決めた瞬間のボタンが「装着」でなく「戻す」になる
    , isPrev : Bool

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


{-| 「つくる」の素材スロット(GET /atelier/slots — material 役だけ来る契約)。 -}
type alias CreateSlot =
    { file : String
    , entityId : Maybe String
    , kind : String
    , title : String

    -- このスロットに何を書けばいいかの一言(サーバの案内。欠けは空)
    , hint : String
    }


{-| AI プロンプトの進み。失敗はサーバの日本語の理由を箱の場所に出す。 -}
type PromptState
    = PromptIdle
    | PromptLoading
    | PromptReady String
    | PromptFailed String


{-| 「新しい種類の素材/設定を足す」(POST /scaffold/doc)の状態。
成功はできたファイル一覧と配線プロンプト(wirePrompt)を同じ場所に出す。
-}
type alias Scaffold =
    { kind : String
    , title : String

    -- "material"(えらんで装着する物)| "tuning"(数値やデータ)
    , role : String
    , pending : Bool
    , error : Maybe String
    , result : Maybe ScaffoldResult
    , copied : Bool
    }


type alias ScaffoldResult =
    { files : List String
    , wirePrompt : String
    }


initScaffold : Scaffold
initScaffold =
    { kind = ""
    , title = ""
    , role = "material"
    , pending = False
    , error = Nothing
    , result = Nothing
    , copied = False
    }


{-| 「つくる」(創作の第一幕)。open = Nothing は既定に従う —
候補が 1 つも無い時は開いて出迎え(旅路の「つくる」の降り立ち先)、
候補があれば畳んで「えらぶ」を主役のままにする。
-}
type alias Create =
    { open : Maybe Bool
    , slots : List CreateSlot
    , slot : Maybe String
    , count : Int
    , direction : String
    , prompt : PromptState
    , copied : Bool

    -- 「写しを作って直す」(POST /atelier/copy)の飛行中の印と、直前に試した名前
    -- (409 の時に次の空き番へ進めるための控え)
    , copyPending : Bool
    , lastCopy : Maybe { slot : String, name : String }
    , scaffold : Scaffold

    -- 🕹 あそびを作らせる(GET /prompt/game)
    , gameDirection : String
    , gamePrompt : PromptState
    , gameCopied : Bool

    -- 旅路の「あそびを考える」(design)から降り立った印。
    -- 「なにをつくる?」の並びで あそび が先頭・推しになる
    , gameHighlight : Bool

    -- 「なにをつくる?」で選んだ道。Nothing = まだ選んでいない(選択リストを出す)。
    -- とじる→ひらくでも保持する(入力も同様に消えない)
    , path : Maybe CreatePath
    }


{-| 「なにをつくる?」の 3 つの道。選ぶと その 1 つのフォームだけが出る。
(手直し・複製は「つくる」ではなく、えらぶ側の候補カードから入る)
-}
type CreatePath
    = PathGame
    | PathAi
    | PathScaffold


initCreate : Create
initCreate =
    { open = Nothing
    , slots = []
    , slot = Nothing
    , count = 3
    , direction = ""
    , prompt = PromptIdle
    , copied = False
    , copyPending = False
    , lastCopy = Nothing
    , scaffold = initScaffold
    , gameDirection = ""
    , gamePrompt = PromptIdle
    , gameCopied = False
    , gameHighlight = False
    , path = Nothing
    }


type alias Model =
    { data : Data
    , create : Create
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
    , create = initCreate
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
    | PreviewClicked { file : String, note : Maybe String, compareWith : Maybe String, isPrev : Bool }
    | LightboxClosed
    | CompareToggled
    | LightboxSwapClicked
    | LightboxRollbackClicked
    | CreateToggled
    | CreateSlotChosen String
    | CreateCountChosen Int
    | CreateDirectionEdited String
    | MakePromptClicked
    | CopyPromptClicked
    | PathChosen CreatePath
    | PathCleared
    | GameDirectionEdited String
    | MakeGamePromptClicked
    | CopyGamePromptClicked
    | CopyResetTick
    | EditCandidateClicked String
    | CopyCurrentClicked String
    | ScaffoldKindEdited String
    | ScaffoldTitleEdited String
    | ScaffoldRoleChosen String
    | ScaffoldClicked
    | ScaffoldCopyClicked
    | NoOp


{-| サーバへ送りたい事(封筒の発行は Main)。OutClosed はオーバーレイを
閉じた合図 — Main が候補と旅路を取り直し、焼きへの誘いを出す。
-}
type Out
    = OutNone
    | OutPromote { candidate : String, slot : String }
    | OutStartGame
    | OutClosed
    | OutFetchPrompt { slot : String, count : Int, direction : String }
    | OutFetchGamePrompt String
    | OutCopyPrompt String
    | OutCopyFile { slot : String, name : String }
    | OutEditFile String
    | OutScaffold { kind : String, title : String, role : String }


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
                        , isPrev = info.isPrev
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

        LightboxSwapClicked ->
            -- 拡大で「これだ」と決めた瞬間に装着へ(閉じる→選ぶ→装着の 3 手を 1 手に)。
            -- ライトボックスを畳み、その候補を選択にした上で既存の SwapClicked に
            -- 委譲する — 未起動の案内(gate)も二度押し防止もそのまま効く
            case Maybe.andThen (\lb -> Maybe.map (Tuple.pair lb) lb.compareWith) model.lightbox of
                Just ( lightbox, slot ) ->
                    update SwapClicked
                        { model
                            | lightbox = Nothing
                            , selected = Just { slot = slot, file = lightbox.file }
                            , promoteError = Nothing
                        }

                Nothing ->
                    -- 「いまの見た目」には装着する物がない(ボタンも出ない防御)
                    ( model, OutNone )

        LightboxRollbackClicked ->
            -- 退避版の拡大からの「この版に戻す」— 既存のロールバック経路に委譲
            case Maybe.andThen (\lb -> Maybe.map (Tuple.pair lb) lb.compareWith) model.lightbox of
                Just ( lightbox, slot ) ->
                    update (RollbackClicked slot lightbox.file)
                        { model | lightbox = Nothing }

                Nothing ->
                    ( model, OutNone )

        CreateToggled ->
            ( mapCreate (\c -> { c | open = Just (not (createOpen model)) }) model
            , OutNone
            )

        CreateSlotChosen slot ->
            ( mapCreate (\c -> { c | slot = Just slot }) model
            , OutNone
            )

        CreateCountChosen n ->
            ( mapCreate (\c -> { c | count = clamp 1 5 n }) model, OutNone )

        CreateDirectionEdited text_ ->
            ( mapCreate (\c -> { c | direction = text_ }) model, OutNone )

        MakePromptClicked ->
            case ( model.create.slot, model.create.prompt ) of
                ( _, PromptLoading ) ->
                    -- 飛行中の二度押しは送らない
                    ( model, OutNone )

                ( Just slot, _ ) ->
                    ( mapCreate (\c -> { c | prompt = PromptLoading, copied = False }) model
                    , OutFetchPrompt
                        { slot = slot
                        , count = model.create.count
                        , direction = String.trim model.create.direction
                        }
                    )

                _ ->
                    ( model, OutNone )

        CopyPromptClicked ->
            case model.create.prompt of
                PromptReady prompt ->
                    ( mapCreate (\c -> { c | copied = True }) model
                    , OutCopyPrompt prompt
                    )

                _ ->
                    ( model, OutNone )

        PathChosen path ->
            ( mapCreate (\c -> { c | path = Just path }) model, OutNone )

        PathCleared ->
            -- 「← ほかのつくり方」。フォームの入力・結果は消さない(戻っても残る)
            ( mapCreate (\c -> { c | path = Nothing }) model, OutNone )

        GameDirectionEdited text_ ->
            ( mapCreate (\c -> { c | gameDirection = text_ }) model, OutNone )

        MakeGamePromptClicked ->
            if model.create.gamePrompt == PromptLoading then
                -- 飛行中の二度押しは送らない
                ( model, OutNone )

            else
                case String.trim model.create.gameDirection of
                    "" ->
                        -- 空はサーバへ行かない(その場で理由を出す)
                        ( mapCreate (\c -> { c | gamePrompt = PromptFailed "どんなゲームか一言書いてください(例: 落ちてくる星をバケツで受け止める)" }) model
                        , OutNone
                        )

                    direction ->
                        ( mapCreate (\c -> { c | gamePrompt = PromptLoading, gameCopied = False }) model
                        , OutFetchGamePrompt direction
                        )

        CopyGamePromptClicked ->
            case model.create.gamePrompt of
                PromptReady prompt ->
                    ( mapCreate (\c -> { c | gameCopied = True }) model
                    , OutCopyPrompt prompt
                    )

                _ ->
                    ( model, OutNone )

        CopyResetTick ->
            ( mapCreate (\c -> { c | copied = False, gameCopied = False }) model
                |> mapScaffold (\s -> { s | copied = False })
            , OutNone
            )

        EditCandidateClicked file ->
            -- 候補カードの「✏️ 手直し」— そうこ(エディタ)でその atelier/ の
            -- ファイルを開く(開くのは Main の仕事なので値で返す)
            ( { model | storehouse = True }, OutEditFile file )

        CopyCurrentClicked slotFile ->
            -- 「いまの見た目」の「✏️ 写しを作って直す」— 名前は自動採番
            -- (スロット base + 空き番の一文字)。成功で写しがそうこに開く
            if model.create.copyPending then
                ( model, OutNone )

            else
                let
                    name =
                        autoCopyName slotFile (slotCandidateFiles model slotFile)
                in
                ( mapCreate (\c -> { c | copyPending = True, lastCopy = Just { slot = slotFile, name = name } }) model
                , OutCopyFile { slot = slotFile, name = name }
                )

        ScaffoldKindEdited text_ ->
            ( mapScaffold (\s -> { s | kind = text_, error = Nothing }) model, OutNone )

        ScaffoldTitleEdited text_ ->
            ( mapScaffold (\s -> { s | title = text_ }) model, OutNone )

        ScaffoldRoleChosen role ->
            ( mapScaffold (\s -> { s | role = role }) model, OutNone )

        ScaffoldClicked ->
            if model.create.scaffold.pending then
                -- 飛行中の二度押しは送らない
                ( model, OutNone )

            else
                let
                    kind =
                        String.trim model.create.scaffold.kind
                in
                if kind == "" then
                    ( mapScaffold (\s -> { s | error = Just "種類の名前を入れてください(半角の小文字。例: enemy)" }) model
                    , OutNone
                    )

                else if not (isValidKindName kind) then
                    ( mapScaffold (\s -> { s | error = Just "半角の小文字で始め、a-z 0-9 _ だけが使えます(例: enemy_wave)" }) model
                    , OutNone
                    )

                else
                    ( mapScaffold (\s -> { s | pending = True, error = Nothing, result = Nothing }) model
                    , OutScaffold
                        { kind = kind
                        , title =
                            case String.trim model.create.scaffold.title of
                                "" ->
                                    -- 表示名が空なら kind で代用(空の題は寂しい)
                                    kind

                                title ->
                                    title
                        , role = model.create.scaffold.role
                        }
                    )

        ScaffoldCopyClicked ->
            case model.create.scaffold.result of
                Just result ->
                    ( mapScaffold (\s -> { s | copied = True }) model
                    , OutCopyPrompt result.wirePrompt
                    )

                Nothing ->
                    ( model, OutNone )

        NoOp ->
            ( model, OutNone )


mapCreate : (Create -> Create) -> Model -> Model
mapCreate f model =
    { model | create = f model.create }


mapScaffold : (Scaffold -> Scaffold) -> Model -> Model
mapScaffold f =
    mapCreate (\c -> { c | scaffold = f c.scaffold })


{-| 種類の名前の規則(^[a-z][a-z0-9_]*$)。 -}
isValidKindName : String -> Bool
isValidKindName name =
    case String.uncons name of
        Just ( c, rest ) ->
            Char.isLower c && String.all (\x -> Char.isLower x || Char.isDigit x || x == '_') rest

        Nothing ->
            False


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


{-| GET /atelier/slots。選択中のスロットがまだ居るなら保ち、
居なければ先頭を既定にする(ドロップダウンを空のまま迷わせない)。
-}
gotSlots : List CreateSlot -> Model -> Model
gotSlots slots model =
    mapCreate
        (\c ->
            { c
                | slots = slots
                , slot =
                    case Maybe.andThen (\s -> ifTrue (List.any (\x -> x.file == s) slots) s) c.slot of
                        Just kept ->
                            Just kept

                        Nothing ->
                            List.head slots |> Maybe.map .file
            }
        )
        model


{-| /atelier/slots の失敗(旧サーバの 404 等)。スロット無し = AI カードが
「準備中」の一言になるだけ(fail-open — そうこも候補えらびも生きる)。
-}
slotsFailed : Model -> Model
slotsFailed model =
    mapCreate (\c -> { c | slots = [], slot = Nothing }) model


{-| GET /prompt/atelier 成功。 -}
gotPrompt : String -> Model -> Model
gotPrompt prompt model =
    mapCreate (\c -> { c | prompt = PromptReady prompt, copied = False }) model


{-| GET /prompt/atelier の失敗。理由だけを箱の場所に出す。 -}
promptFailed : String -> Model -> Model
promptFailed message model =
    mapCreate (\c -> { c | prompt = PromptFailed (cleanReason message) }) model


{-| GET /prompt/game 成功。 -}
gotGamePrompt : String -> Model -> Model
gotGamePrompt prompt model =
    mapCreate (\c -> { c | gamePrompt = PromptReady prompt, gameCopied = False }) model


{-| GET /prompt/game の失敗。理由だけを箱の場所に出す(404 は Main が
「準備中」の文言に均してから呼ぶ)。
-}
gamePromptFailed : String -> Model -> Model
gamePromptFailed message model =
    mapCreate (\c -> { c | gamePrompt = PromptFailed (cleanReason message) }) model


{-| 画面に映っているゲームプロンプト(テストの覗き窓)。 -}
shownGamePrompt : Model -> Maybe String
shownGamePrompt model =
    case model.create.gamePrompt of
        PromptReady prompt ->
            Just prompt

        _ ->
            Nothing


{-| 「あそびを作らせる」の失敗文言(検証・サーバとも同じ場所。テストの覗き窓)。 -}
gamePromptErrorShown : Model -> Maybe String
gamePromptErrorShown model =
    case model.create.gamePrompt of
        PromptFailed reason ->
            Just reason

        _ ->
            Nothing


{-| 旅路の「あそびを考える」(design)から降り立った。「つくる」を開き、
「なにをつくる?」の並びで あそび を先頭・推しにする(候補があっても畳まない)。
-}
openCreateForGame : Model -> Model
openCreateForGame model =
    mapCreate (\c -> { c | open = Just True, gameHighlight = True, path = Nothing }) model


{-| 「なにをつくる?」の並び(優先度順)。誕生期(design で降り立った)は
あそびが先頭、素材サイクル中は素材の道が先。
-}
createPaths : Model -> List CreatePath
createPaths model =
    if model.create.gameHighlight then
        [ PathGame, PathAi, PathScaffold ]

    else
        [ PathAi, PathGame, PathScaffold ]


{-| いま選んでいる道(Nothing = 選択リストを出している)。テストの覗き窓。 -}
chosenPath : Model -> Maybe CreatePath
chosenPath model =
    model.create.path


{-| POST /atelier/copy 成功。そうこ(エディタ)へ切り替える —
写しを開くのは Main(トーストと openFile)。
-}
copyDone : Model -> Model
copyDone model =
    { model | storehouse = True }
        |> mapCreate (\c -> { c | copyPending = False, lastCopy = Nothing })


{-| POST /atelier/copy の失敗(打ち切り)。ボタンを戻すだけ —
理由の告げ方(トースト)は Main の仕事。
-}
copyFailed : Model -> Model
copyFailed model =
    mapCreate (\c -> { c | copyPending = False, lastCopy = Nothing }) model


{-| 409(名前衝突)。次の空き番の名前でもう一度送る(z まで来たら諦める)。
戻りの Maybe が Just なら Main が同じ封筒(atelierCopy)を再発行する。
-}
copyRetry : Model -> ( Model, Maybe { slot : String, name : String } )
copyRetry model =
    case model.create.lastCopy of
        Just info ->
            case nextCopyName info.name of
                Just next ->
                    let
                        retry =
                            { slot = info.slot, name = next }
                    in
                    ( mapCreate (\c -> { c | lastCopy = Just retry }) model, Just retry )

                Nothing ->
                    ( copyFailed model, Nothing )

        Nothing ->
            ( copyFailed model, Nothing )


{-| 「写しを作って直す」の自動採番。スロットの base に、その列の候補で
まだ使われていない一文字(a, b, c, …)を足す(例: 既存 a・b → 「base.c」)。
-}
autoCopyName : String -> List String -> String
autoCopyName slotFile candidateFiles =
    let
        base =
            firstToken (baseName slotFile)

        used =
            List.filterMap (copySuffixOf base) candidateFiles

        free =
            String.toList "abcdefghijklmnopqrstuvwxyz"
                |> List.map String.fromChar
                |> List.filter (\letter -> not (List.member letter used))
                |> List.head
    in
    base ++ "." ++ Maybe.withDefault "z" free


firstToken : String -> String
firstToken name =
    String.split "." name |> List.head |> Maybe.withDefault name


{-| 候補ファイル名から base の直後の一文字(採番)を拾う。
base が違う・一文字でない物は数えない。
-}
copySuffixOf : String -> String -> Maybe String
copySuffixOf base file =
    case String.split "." (baseName file) of
        b :: suffix :: _ ->
            if b == base && String.length suffix == 1 then
                Just suffix

            else
                Nothing

        _ ->
            Nothing


{-| 「base.x」の x を次の文字へ(z なら Nothing = 打ち切り)。 -}
nextCopyName : String -> Maybe String
nextCopyName name =
    case List.reverse (String.split "." name) of
        last :: rest ->
            case String.uncons last of
                Just ( c, "" ) ->
                    if c < 'z' then
                        Just (String.join "." (List.reverse (String.fromChar (Char.fromCode (Char.toCode c + 1)) :: rest)))

                    else
                        Nothing

                _ ->
                    Nothing

        [] ->
            Nothing


{-| スロットの候補ファイル名(自動採番の材料)。 -}
slotCandidateFiles : Model -> String -> List String
slotCandidateFiles model slotFile =
    case model.data of
        Ready candidates ->
            candidates.slots
                |> List.filter (\s -> s.slot == slotFile)
                |> List.concatMap (\s -> List.map .file s.candidates)

        _ ->
            []


{-| POST /scaffold/doc 成功。できたファイルと配線プロンプトを同じ場所に出す。 -}
scaffoldDone : ScaffoldResult -> Model -> Model
scaffoldDone result model =
    mapScaffold (\s -> { s | pending = False, result = Just result, error = Nothing, copied = False }) model


{-| POST /scaffold/doc の失敗(400/409)。日本語の理由をその場に出す。 -}
scaffoldFailed : String -> Model -> Model
scaffoldFailed message model =
    mapScaffold (\s -> { s | pending = False, error = Just (cleanReason message) }) model


{-| エンドポイント未実装のサーバ(404 等)。「準備中」に倒すだけ(fail-open)。 -}
scaffoldUnavailable : Model -> Model
scaffoldUnavailable model =
    mapScaffold (\s -> { s | pending = False, error = Just "この機能はまだ準備中です(サーバが古い可能性があります)" }) model


{-| 画面に映っている配線プロンプト(テストの覗き窓)。 -}
shownWirePrompt : Model -> Maybe String
shownWirePrompt model =
    Maybe.map .wirePrompt model.create.scaffold.result


{-| 「新しい種類」の失敗文言(検証・サーバ 400/409 とも同じ場所)。 -}
scaffoldErrorShown : Model -> Maybe String
scaffoldErrorShown model =
    model.create.scaffold.error


{-| POST /scaffold/doc の応答。欠けは既定値に倒す(fail-open)。 -}
scaffoldResultDecoder : D.Decoder ScaffoldResult
scaffoldResultDecoder =
    D.map2 ScaffoldResult
        (D.oneOf [ D.field "files" (D.list D.string), D.succeed [] ])
        (D.oneOf [ D.field "wirePrompt" D.string, D.succeed "" ])


{-| 「✓ コピーしました」の戻し待ちか。Main の subscriptions がこれを見て
2 秒後の CopyResetTick を流す(オーバーレイの段送りと同じ流儀)。
-}
needsCopyReset : Model -> Bool
needsCopyReset model =
    model.create.copied || model.create.gameCopied || model.create.scaffold.copied


{-| 「つくる」パネルが開いているか。手で触っていなければ既定に従う —
候補ゼロなら開いて出迎え、候補があれば畳んで「えらぶ」を主役に。
-}
createOpen : Model -> Bool
createOpen model =
    Maybe.withDefault (not (hasCandidates model)) model.create.open


{-| 画面に映っているプロンプト(テストの覗き窓)。 -}
shownPrompt : Model -> Maybe String
shownPrompt model =
    case model.create.prompt of
        PromptReady prompt ->
            Just prompt

        _ ->
            Nothing


{-| いま選んでいるスロット(一覧から引く)。 -}
selectedCreateSlot : Create -> Maybe CreateSlot
selectedCreateSlot create =
    create.slot
        |> Maybe.andThen (\file -> List.head (List.filter (\s -> s.file == file) create.slots))


{-| 選択中スロットの案内(サーバの hint)。空なら出さない(Nothing)。 -}
selectedSlotHint : Model -> Maybe String
selectedSlotHint model =
    selectedCreateSlot model.create
        |> Maybe.map .hint
        |> Maybe.andThen (\h -> ifTrue (h /= "") h)


{-| 方向性の書き出し例。スロットの kind で切り替える(不明は汎用の一言)。 -}
directionPlaceholder : String -> String
directionPlaceholder kind =
    case kind of
        "theme" ->
            "例: 晩夏の乾いた草の色。赤とんぼの夕暮れの空気"

        "sprite" ->
            "例: 冬支度の村人。網で虫を追いかける子供"

        "sound" ->
            "例: 短く鋭い斧の音。夕暮れのひぐらし"

        "shader" ->
            "例: 夕凪の静かな水面。おだやかな揺れ"

        _ ->
            "例: イナゴやトンボが飛び、網で虫を追いかける子供がいる晩夏の情景"


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


{-| GET /atelier/slots。file だけは必須(無ければそのスロットは捨てる)、
題も種別も欠けは空に倒す(fail-open)。
-}
createSlotsDecoder : D.Decoder (List CreateSlot)
createSlotsDecoder =
    D.oneOf [ D.field "slots" (D.list createSlotDecoder), D.succeed [] ]


createSlotDecoder : D.Decoder CreateSlot
createSlotDecoder =
    D.map5 CreateSlot
        (D.field "file" D.string)
        (D.oneOf [ D.field "entityId" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "kind" D.string, D.succeed "" ])
        (D.oneOf [ D.field "title" D.string, D.succeed "" ])
        (D.oneOf [ D.field "hint" D.string, D.succeed "" ])


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


{-| カードの中に出す決めボタン(導線の規則)。選んだカードにだけ出る —
候補なら装着(Swap)、退避(prev)版なら戻す(Rollback)。未選択は何も出ない
(クリック → その場にボタン出現、が導線)。
-}
cardAction : Model -> { slot : String, file : String, isPrev : Bool } -> Maybe Mode
cardAction model info =
    if model.selected == Just { slot = info.slot, file = info.file } then
        Just
            (if info.isPrev then
                Rollback

             else
                Swap
            )

    else
        Nothing



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
                        [ [ viewCreate model ]
                        , [ div [ HA.class "mb-5" ]
                                [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "🎨 候補えらび" ]
                                , div [ HA.class "mt-1 text-[11px] text-ink-soft" ]
                                    [ text "新しい見た目の候補です。カードを選んで、ゲームに装着しましょう" ]
                                ]
                          ]
                        , viewBakePanel model candidates
                        , List.map (viewSlot base model candidates.baking) candidates.slots
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
        (div [ HA.class "mb-1.5 flex items-center gap-2" ]
            [ span [ HA.class "font-mono text-[11px] text-ink-soft" ] [ text slot.slot ]
            , case slot.entityId of
                Just entity ->
                    span [ HA.class "badge" ] [ text entity ]

                Nothing ->
                    text ""
            ]
            :: div [ HA.class "flex flex-wrap gap-2" ]
                (viewCurrentCard base model baking slot
                    :: List.map (viewCard base model baking slot.slot) slot.candidates
                )
            :: -- ゲーム未起動の案内は、選んだカードのあるスロットの直下に出す
               (if model.gate && Maybe.map .slot model.selected == Just slot.slot then
                    viewGate model

                else
                    []
               )
        )


{-| 「いまの見た目」の参照カード — 装着中の姿を並べて見比べる基準。
押せない(候補ではない)ので、枠も落ち着かせる。
-}
viewCurrentCard : String -> Model -> Bool -> Slot -> Html Msg
viewCurrentCard base model baking slot =
    div [ HA.class "atelier-card atelier-card-current w-56 rounded-lg border border-edge/60 bg-panel p-3 opacity-80" ]
        [ viewPreview base
            model.bust
            { file = slot.slot, note = Nothing, compareWith = Nothing, isPrev = False }
            (previewState { ready = slot.currentPreviewReady, baking = baking })
        , div [ HA.class "mt-2 flex items-center gap-1.5" ]
            [ span [ HA.class "badge shrink-0" ] [ text "いまの見た目" ]
            , span [ HA.class "min-w-0 flex-1 truncate font-mono text-[10px] text-ink-faint", HA.title slot.slot ]
                [ text (baseName slot.slot) ]
            ]

        -- 写し(atelier/)が候補の列に増えて、そこを直す — 元には触らない
        , div [ HA.class "mt-2.5" ]
            [ button
                [ HA.class "btn btn-mini w-full"
                , HA.disabled model.create.copyPending
                , HE.onClick (CopyCurrentClicked slot.slot)
                ]
                [ text
                    (if model.create.copyPending then
                        "写しを作っています…"

                     else
                        "✏️ 写しを作って直す"
                    )
                ]
            ]
        ]


{-| カード上段のプレビュー領域(高さ固定 — 焼き待ちでもカードが跳ねない)。
絵が映っている時はクリックで拡大(lightbox)— カードの選択とは別の動詞なので
伝播は止める。note はカードの説明文、compareWith は「いまの見た目」の
プレビューキー(候補カードだけ Just — 拡大の中で A/B に切り替える)。
-}
viewPreview : String -> Int -> { file : String, note : Maybe String, compareWith : Maybe String, isPrev : Bool } -> PreviewState -> Html Msg
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
            { file = candidate.file, note = candidate.note, compareWith = Just slotName, isPrev = candidate.isPrev }
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
            [ span [ HA.class "min-w-0 flex-1 truncate text-[10px] text-ink-faint" ] [ text (mtimeLabel candidate.mtime) ]

            -- そうこ(エディタ)でこの候補ファイルを開く。カードの選択とは
            -- 別の動詞なので伝播は止める
            , button
                [ HA.class "btn btn-ghost btn-mini shrink-0"
                , stopClick (EditCandidateClicked candidate.file)
                ]
                [ text "✏️ 手直し" ]
            ]
        , case cardAction model { slot = slotName, file = candidate.file, isPrev = candidate.isPrev } of
            Just action ->
                -- 選んだカードの中に決めボタン(「次何をするの?」をその場で答える)。
                -- 押下は既存の SwapClicked / RollbackClicked に委譲 —
                -- 未起動の案内も二度押し防止もそのまま効く
                div [ HA.class "mt-2.5" ]
                    [ case action of
                        Swap ->
                            button
                                [ HA.class "btn btn-primary w-full"
                                , HA.disabled (model.pending /= Nothing)
                                , stopClick SwapClicked
                                ]
                                [ text
                                    (if model.pending /= Nothing then
                                        "装着しています…"

                                     else
                                        "🔄 ゲームに装着"
                                    )
                                ]

                        Rollback ->
                            button
                                [ HA.class "btn w-full"
                                , HA.disabled (model.pending /= Nothing)
                                , stopClick (RollbackClicked slotName candidate.file)
                                ]
                                [ text
                                    (if model.pending /= Nothing then
                                        "戻しています…"

                                     else
                                        "↩ この版に戻す"
                                    )
                                ]
                    , case model.promoteError of
                        Just reason ->
                            div [ HA.class "mt-1.5 text-[11px] text-danger" ]
                                [ text ("装着できませんでした — " ++ reason) ]

                        Nothing ->
                            text ""
                    ]

            Nothing ->
                text ""
        ]


{-| カード内ボタンはカードの選択クリックと二重に効かせない。 -}
stopClick : msg -> Html.Attribute msg
stopClick msg =
    HE.stopPropagationOn "click" (D.succeed ( msg, True ))


{-| ゲーム未起動の案内カード(選んだカードのあるスロットの直下)。
起動と「そのまま装着」の両方を差し出す。
-}
viewGate : Model -> List (Html Msg)
viewGate model =
    [ div [ HA.class "atelier-gate mt-3 max-w-lg rounded-lg border border-edge bg-panel p-4" ]
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
                        -- 見比べて「これだ」の瞬間にその場で決められる —
                        -- 候補は装着、退避(prev)版は戻す(既存経路に委譲)
                        if lightbox.isPrev then
                            button [ HA.class "btn btn-mini", stopClick LightboxRollbackClicked ]
                                [ text "↩ この版に戻す" ]

                        else
                            button [ HA.class "btn btn-primary btn-mini", stopClick LightboxSwapClicked ]
                                [ text "🔄 この案をゲームに装着" ]

                    Nothing ->
                        text ""
                , case lightbox.compareWith of
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



-- つくる(創作の第一幕)


{-| 「つくる」の畳めるセクション。畳んでいる時は 1 行の帯だけ
(候補がある時の既定 — えらぶが主役のまま)。開くと「なにをつくる?」の
選択リスト(優先度順の 1 問)を出し、選んだ 1 つのフォームだけを見せる。
そうこモードでも Main がこれを最上段に描く(創作の入口はどちらのモードにもある)。
-}
viewCreate : Model -> Html Msg
viewCreate model =
    div [ HA.class "atelier-create mb-5 rounded-lg border border-edge bg-panel" ]
        (button
            [ HA.class "atelier-create-bar flex w-full cursor-pointer items-center gap-2 rounded-lg px-4 py-2.5 text-left text-sm font-semibold text-ink transition-colors hover:bg-white/5"
            , HE.onClick CreateToggled
            ]
            [ span [] [ text "✨ 新しい素材をつくる" ]
            , span [ HA.class "flex-1" ] []

            -- 開閉できる事が言葉で分かる目印(バー全体が押せる — これは chip)
            , span [ HA.class "rounded border border-edge px-2 py-0.5 text-[12px] font-normal text-ink-soft" ]
                [ text
                    (if createOpen model then
                        "▾ とじる"

                     else
                        "▸ ひらく"
                    )
                ]
            ]
            :: (if createOpen model then
                    -- 中身は画面の半分までで内側スクロール — プロンプト箱が育っても
                    -- 下の編集領域を潰さず、開閉バーは常に見え、常に押せる
                    [ div [ HA.class "atelier-create-body max-h-[50vh] overflow-y-auto" ]
                        (case model.create.path of
                            Nothing ->
                                -- まだ選んでいない: 「なにをつくる?」の 1 問だけ
                                [ div [ HA.class "px-4 pb-2 text-xs font-semibold text-ink" ] [ text "なにをつくる?" ]
                                , div [ HA.class "flex flex-col gap-1.5 px-4 pb-4" ]
                                    (List.map (viewPathRow model) (createPaths model))
                                ]

                            Just path ->
                                -- 選んだ 1 つのフォームだけを出す(他は出さない)
                                [ button
                                    [ HA.class "atelier-path-back cursor-pointer px-4 pb-2 text-left text-[11px] text-ink-faint transition-colors hover:text-ink-soft"
                                    , HE.onClick PathCleared
                                    ]
                                    [ text "← ほかのつくり方" ]
                                , div [ HA.class "px-4 pb-4" ]
                                    [ case path of
                                        PathGame ->
                                            viewGameCard model.create

                                        PathAi ->
                                            viewAiCard model.create

                                        PathScaffold ->
                                            viewScaffoldCard model.create.scaffold
                                    ]
                                ]
                        )
                    ]

                else
                    []
               )
        )


{-| 「なにをつくる?」の 1 行(アイコン+名前+一言)。誕生期の先頭(あそび)には
推しバッジ「まずはこれ」を添える。
-}
viewPathRow : Model -> CreatePath -> Html Msg
viewPathRow model path =
    let
        info =
            pathInfo path

        recommended =
            model.create.gameHighlight && path == PathGame
    in
    button
        [ HA.classList
            [ ( "atelier-path-row flex w-full cursor-pointer items-center gap-2.5 rounded-lg border px-3 py-2 text-left transition-colors hover:bg-white/5", True )
            , ( "border-accent/60 ring-1 ring-accent/40", recommended )
            , ( "border-edge", not recommended )
            ]
        , HE.onClick (PathChosen path)
        ]
        [ span [ HA.class "text-base leading-none" ] [ text info.icon ]
        , span [ HA.class "text-xs font-semibold text-ink" ] [ text info.title ]
        , if recommended then
            span [ HA.class "badge shrink-0 bg-accent/20 text-accent" ] [ text "まずはこれ" ]

          else
            text ""
        , span [ HA.class "min-w-0 flex-1 truncate text-[11px] text-ink-faint" ] [ text info.blurb ]
        ]


pathInfo : CreatePath -> { icon : String, title : String, blurb : String }
pathInfo path =
    case path of
        PathGame ->
            { icon = "🕹"
            , title = "あそびを作らせる"
            , blurb = "遊びのルールをAIに作らせる — 先に遊びを決めると必要な素材がはっきりします"
            }

        PathAi ->
            { icon = "🤖"
            , title = "素材をAIに作らせる"
            , blurb = "見た目や音の候補をAIに作らせて、えらんで装着する"
            }

        PathScaffold ->
            { icon = "📦"
            , title = "新しい種類を足す"
            , blurb = "新しい種類の素材/設定の骨組み(Doc)を作る"
            }


{-| 「AIに作らせる」— プロンプトを組んでコピーする道。作るのは AI(外)で、
候補が atelier/ に置かれた瞬間からこの画面が拾う(watchFile と同じ約束)。
-}
viewAiCard : Create -> Html Msg
viewAiCard create =
    div [ HA.class "atelier-create-ai min-w-[280px] flex-1 rounded-lg border border-edge bg-black/20 p-4" ]
        (div [ HA.class "mb-2 text-xs font-semibold text-ink" ] [ text "🤖 AIに作らせる" ]
            :: (if List.isEmpty create.slots then
                    [ div [ HA.class "text-[11px] text-ink-faint" ]
                        [ text "スロット一覧を取得できませんでした(サーバが古い可能性があります)" ]
                    ]

                else
                    List.concat
                        [ [ viewSlotSelect create
                          , viewSlotHint create
                          , div [ HA.class "mt-2 flex items-center gap-2" ]
                                (span [ HA.class "text-[11px] text-ink-soft" ] [ text "案数" ]
                                    :: List.map (viewCountButton create.count) [ 1, 2, 3, 4, 5 ]
                                )
                          , Html.textarea
                                [ HA.class "field mt-2 h-auto min-h-[4.5rem] w-full resize-y py-1.5 text-xs leading-relaxed"
                                , HA.rows 3
                                , HA.placeholder
                                    ("方向性 "
                                        ++ directionPlaceholder
                                            (selectedCreateSlot create
                                                |> Maybe.map .kind
                                                |> Maybe.withDefault ""
                                            )
                                    )
                                , HA.value create.direction
                                , HE.onInput CreateDirectionEdited
                                ]
                                []
                          , div [ HA.class "mt-3" ]
                                [ button
                                    [ HA.class "btn btn-primary"
                                    , HA.disabled (create.prompt == PromptLoading)
                                    , HE.onClick MakePromptClicked
                                    ]
                                    [ text
                                        (if create.prompt == PromptLoading then
                                            "作っています…"

                                         else
                                            "プロンプトを作る"
                                        )
                                    ]
                                ]
                          ]
                        , viewPromptBox create
                        ]
               )
        )


{-| 選択中スロットの案内(hint)。何を書けばいいかをドロップダウンの直下で
そっと教える。空なら何も出さない(古いサーバでも欠けない)。
-}
viewSlotHint : Create -> Html Msg
viewSlotHint create =
    case selectedCreateSlot create |> Maybe.map .hint of
        Just hint ->
            if hint == "" then
                text ""

            else
                div [ HA.class "mt-1 text-[11px] leading-relaxed text-ink-faint" ] [ text hint ]

        Nothing ->
            text ""


viewCountButton : Int -> Int -> Html Msg
viewCountButton current n =
    button
        [ HA.classList
            [ ( "btn btn-mini", True )
            , ( "btn-primary", current == n )
            ]
        , HE.onClick (CreateCountChosen n)
        ]
        [ text (String.fromInt n) ]


viewSlotSelect : Create -> Html Msg
viewSlotSelect create =
    Html.select
        [ HA.class "field w-full text-xs"
        , HE.onInput CreateSlotChosen
        ]
        (List.map
            (\slot ->
                Html.option
                    [ HA.value slot.file
                    , HA.selected (create.slot == Just slot.file)
                    ]
                    [ text (slotLabel slot) ]
            )
            create.slots
        )


slotLabel : CreateSlot -> String
slotLabel slot =
    if slot.title == "" then
        slot.file

    else
        slot.title ++ "(" ++ baseName slot.file ++ ")"


viewPromptBox : Create -> List (Html Msg)
viewPromptBox create =
    case create.prompt of
        PromptIdle ->
            []

        PromptLoading ->
            []

        PromptFailed reason ->
            [ div [ HA.class "mt-2 text-[11px] text-danger" ]
                [ text ("プロンプトを作れませんでした — " ++ reason) ]
            ]

        PromptReady prompt ->
            [ Html.textarea
                [ HA.class "atelier-prompt mt-3 h-48 max-h-48 w-full resize-none overflow-y-auto rounded border border-edge bg-black/40 p-2 font-mono text-[11px] leading-relaxed text-ink"
                , HA.readonly True
                , HA.value prompt
                ]
                []
            , div [ HA.class "mt-2" ]
                [ button [ HA.class "btn btn-primary w-full", HE.onClick CopyPromptClicked ]
                    [ text
                        (if create.copied then
                            "✓ コピーしました"

                         else
                            "📋 コピー"
                        )
                    ]
                ]
            , div [ HA.class "mt-2 text-[11px] leading-relaxed text-ink-faint" ]
                [ text "Claude Code などのAIに貼り付けてください。候補が atelier/ にできると、ここに自動で現れます(プレビューも自動で焼けます)" ]
            ]


{-| 「あそびを作らせる」— ゲームのルールそのものを AI に作らせる道。
プロンプトを組んでコピーするだけ(作るのは AI で、続きはホームの旅路が拾う)。
-}
viewGameCard : Create -> Html Msg
viewGameCard create =
    div
        [ HA.classList
            [ ( "atelier-create-game min-w-[280px] flex-1 rounded-lg border bg-black/20 p-4", True )
            , ( "border-accent/60 ring-1 ring-accent/40", create.gameHighlight )
            , ( "border-edge", not create.gameHighlight )
            ]
        ]
        (List.concat
            [ [ div [ HA.class "mb-2 text-xs font-semibold text-ink" ] [ text "🕹 あそびを作らせる" ]
              , div [ HA.class "mb-2 text-[11px] leading-relaxed text-ink-faint" ]
                    [ text "先に遊びを決めると、必要な素材がはっきりします。素材づくりはその後がおすすめ" ]
              , Html.textarea
                    [ HA.class "field h-auto min-h-[4.5rem] w-full resize-y py-1.5 text-xs leading-relaxed"
                    , HA.rows 3
                    , HA.placeholder "どんなゲーム? 例: 落ちてくる星をバケツで受け止める。3回落とすと終わり"
                    , HA.value create.gameDirection
                    , HE.onInput GameDirectionEdited
                    ]
                    []
              , div [ HA.class "mt-3" ]
                    [ button
                        [ HA.class "btn btn-primary"
                        , HA.disabled (create.gamePrompt == PromptLoading)
                        , HE.onClick MakeGamePromptClicked
                        ]
                        [ text
                            (if create.gamePrompt == PromptLoading then
                                "作っています…"

                             else
                                "プロンプトを作る"
                            )
                        ]
                    ]
              ]
            , viewGamePromptBox create
            ]
        )


viewGamePromptBox : Create -> List (Html Msg)
viewGamePromptBox create =
    case create.gamePrompt of
        PromptIdle ->
            []

        PromptLoading ->
            []

        PromptFailed reason ->
            [ div [ HA.class "mt-2 text-[11px] text-danger" ]
                [ text ("プロンプトを作れませんでした — " ++ reason) ]
            ]

        PromptReady prompt ->
            [ Html.textarea
                [ HA.class "atelier-game-prompt mt-3 h-48 max-h-48 w-full resize-none overflow-y-auto rounded border border-edge bg-black/40 p-2 font-mono text-[11px] leading-relaxed text-ink"
                , HA.readonly True
                , HA.value prompt
                ]
                []
            , div [ HA.class "mt-2" ]
                [ button [ HA.class "btn btn-primary w-full", HE.onClick CopyGamePromptClicked ]
                    [ text
                        (if create.gameCopied then
                            "✓ コピーしました"

                         else
                            "📋 コピー"
                        )
                    ]
                ]
            , div [ HA.class "mt-2 text-[11px] leading-relaxed text-ink-faint" ]
                [ text "AIに貼ると、ルールとテストとbakeまで作ります。できたらホームの『焼いて確かめよう』に続きます" ]
            ]


{-| 「新しい種類の素材/設定を足す」— Doc の骨組み(scaffold)を作る道。
成功はできたファイル一覧と、ゲームへ配線するためのプロンプト(AI に貼る)を出す。
-}
viewScaffoldCard : Scaffold -> Html Msg
viewScaffoldCard scaffold =
    div [ HA.class "atelier-create-scaffold min-w-[280px] flex-1 rounded-lg border border-edge bg-black/20 p-4" ]
        (List.concat
            [ [ div [ HA.class "mb-2 text-xs font-semibold text-ink" ] [ text "📦 新しい種類の素材/設定を足す" ]
              , Html.input
                    [ HA.class "field w-full font-mono text-xs"
                    , HA.placeholder "種類の名前(例: enemy_wave)"
                    , HA.value scaffold.kind
                    , HE.onInput ScaffoldKindEdited
                    ]
                    []
              , Html.input
                    [ HA.class "field mt-2 w-full text-xs"
                    , HA.placeholder "表示名(例: 敵の波)"
                    , HA.value scaffold.title
                    , HE.onInput ScaffoldTitleEdited
                    ]
                    []
              , div [ HA.class "mt-2 flex flex-col gap-1" ]
                    [ viewRoleRadio scaffold "material" "素材(えらんで装着する物)"
                    , viewRoleRadio scaffold "tuning" "調整(数値やデータ)"
                    ]
              , div [ HA.class "mt-3" ]
                    [ button
                        [ HA.class "btn"
                        , HA.disabled scaffold.pending
                        , HE.onClick ScaffoldClicked
                        ]
                        [ text
                            (if scaffold.pending then
                                "作っています…"

                             else
                                "📦 骨組みを作る"
                            )
                        ]
                    ]
              , case scaffold.error of
                    Just reason ->
                        div [ HA.class "mt-2 text-[11px] text-danger" ]
                            [ text ("作れませんでした — " ++ reason) ]

                    Nothing ->
                        text ""
              ]
            , viewScaffoldResult scaffold
            ]
        )


viewRoleRadio : Scaffold -> String -> String -> Html Msg
viewRoleRadio scaffold role label =
    Html.label [ HA.class "flex cursor-pointer items-center gap-1.5 text-[11px] text-ink-soft" ]
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "scaffold-role"
            , HA.checked (scaffold.role == role)
            , HE.onClick (ScaffoldRoleChosen role)
            ]
            []
        , text label
        ]


viewScaffoldResult : Scaffold -> List (Html Msg)
viewScaffoldResult scaffold =
    case scaffold.result of
        Nothing ->
            []

        Just result ->
            List.concat
                [ [ div [ HA.class "mt-3 text-[11px] font-semibold text-ok" ] [ text "✓ 骨組みができました" ] ]
                , if List.isEmpty result.files then
                    []

                  else
                    [ div [ HA.class "mt-1" ]
                        (List.map
                            (\file -> div [ HA.class "font-mono text-[11px] text-ink-soft" ] [ text file ])
                            result.files
                        )
                    ]
                , if result.wirePrompt == "" then
                    []

                  else
                    -- 配線プロンプトは AI カードと同じコピーの作法(箱 + 📋コピー)
                    [ Html.textarea
                        [ HA.class "atelier-wire-prompt mt-3 h-48 max-h-48 w-full resize-none overflow-y-auto rounded border border-edge bg-black/40 p-2 font-mono text-[11px] leading-relaxed text-ink"
                        , HA.readonly True
                        , HA.value result.wirePrompt
                        ]
                        []
                    , div [ HA.class "mt-2" ]
                        [ button [ HA.class "btn btn-primary w-full", HE.onClick ScaffoldCopyClicked ]
                            [ text
                                (if scaffold.copied then
                                    "✓ コピーしました"

                                 else
                                    "📋 コピー"
                                )
                            ]
                        ]
                    , div [ HA.class "mt-2 text-[11px] leading-relaxed text-ink-faint" ]
                        [ text "このプロンプトをAIに貼ると、ゲームがこのDocを読むようになります" ]
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
