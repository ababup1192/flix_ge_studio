module Atelier exposing
    ( Archive
    , Candidates
    , CreatePath(..)
    , CreateSlot
    , Launch(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Out(..)
    , Phase(..)
    , PreviewState(..)
    , archiveAvailable
    , archiveCount
    , archiveDecoder
    , archiveFailed
    , archiveSettled
    , setStarterFresh
    , candidatesDecoder
    , candidatesFailed
    , cardAction
    , autoCopyName
    , cleanReason
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
    , gotArchive
    , gotBakeLog
    , gotCandidates
    , gotGameLog
    , gamePromptErrorShown
    , gotGamePrompt
    , gamePromptFailed
    , gotGameStatus
    , gotPrompt
    , gotSlots
    , hasCandidates
    , init
    , isBaking
    , isLaunchPolling
    , launchStarting
    , lightboxOpen
    , lightboxShownFile
    , mockChip
    , needsCopyReset
    , needsTick
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
    , showArchiver
    , showLanding
    , showPicks
    , slotsFailed
    , statusDecoder
    , update
    , view
    , viewArchiver
    , viewCreate
    , viewLanding
    , viewSectionTop
    )

{-| アトリエの「候補選び」— 生成された見た目候補を見比べて、ゲームで使う(採用する)。

アトリエタブは入口(viewLanding)から始まり、行き先は
素材(候補選び+つくるバー)/ 調整(Doc エディタ)/ アーカイブの 3 つ。
どれを描くかは Main が showLanding / showArchiver / showPicks で判定し、
各セクションの最上段の「← アトリエ」(viewSectionTop)で入口へ戻る。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。
送りたい事は update の戻り値 Out で Main へ返す(Journey と同じ流儀)。
エンドポイント未実装のサーバでは候補の一覧が出ないだけ(fail-open —
つくるバーも調整(Doc エディタ)もそのまま生きる)。候補ゼロのスロットはサーバが
返さない契約だが、来ても捨てる(防御 — ファイル名の羅列で画面を埋めない)。

装着の瞬間はこの画面の見せ場。成功はオーバーレイで大きく祝い、
前のバージョンの退避先を必ず添える — 戻せると分かっていれば装着は怖くない。

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
候補ゾーンを出さないだけで、調整(エディタ)は普通に使える。
-}
type Data
    = Loading
    | Unavailable
    | Ready Candidates


{-| アトリエタブのセクション。Landing(入口)から
Picks(素材 = 候補選び+つくる)/ Storehouse(調整 = Doc エディタ)/
Archiver(アーカイブ)へ入る。
-}
type Section
    = SectionLanding
    | SectionPicks
    | SectionStorehouse
    | SectionArchiver


{-| 切り替えで積まれたバージョン 1 枚(atelier/archive/<base>.vN.<kind>.json)。
slot はこの札の戻し先(assets/)。
-}
type alias HistoryEntry =
    { file : String
    , slot : String
    , ver : Int
    , note : Maybe String
    , mtime : Int
    }


{-| 手でアーカイブした候補 1 枚(候補カードの 🗃️ で送った物。候補に戻せる)。 -}
type alias ArchivedCandidate =
    { file : String
    , entityId : Maybe String
    , note : Maybe String
    , mtime : Int
    }


{-| GET /atelier/archive の本文。何も捨てない置き場の中身。 -}
type alias Archive =
    { history : List HistoryEntry
    , candidates : List ArchivedCandidate
    }


{-| candidates の Data と同じ 3 態。Unavailable(旧サーバの 404 等)は
アーカイブの入口ごと出さない(fail-open)。
-}
type ArchiveData
    = ArchiveLoading
    | ArchiveUnavailable
    | ArchiveReady Archive


type alias Selection =
    { slot : String
    , file : String
    }


{-| 装着(Swap)か巻き戻し(Rollback)か。オーバーレイの文言が変わるだけで、
サーバへの頼み事はどちらも同じ promote(戻すのも「前のバージョンを装着する」)。
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


{-| 「つくる」(創作の第一幕)。open = Nothing は既定(畳む)に従う —
開くのは手だけ。
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

    -- 🕹️ あそびを作らせる(GET /prompt/game)。
    -- インタビュー(2 問 + エッセンス): 選択肢(Pick)と「言葉で」(Text)は
    -- 別に持ち、言葉が入っていればそちらが答え(消せば選択肢に戻る)。
    -- 答えは direction に「動かすもの: …/終わり方: …/エッセンス: …」で合成する
    , gameDirection : String
    , gameMoverPick : String
    , gameMoverText : String
    , gameEndPick : String
    , gameEndText : String
    , gameEssence : String
    , gamePrompt : PromptState
    , gameCopied : Bool

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


{-| インタビュー 1 問目「動かすのは、何?」の選択肢。先頭がテンプレートのまま。 -}
moverOptions : List String
moverOptions =
    [ "パドルのまま", "猫(しっぽで打ち返す)", "宇宙船" ]


moverDefault : String
moverDefault =
    "パドルのまま"


{-| インタビュー 2 問目「一回の遊びは、どう終わる?」の選択肢。先頭がテンプレートのまま。 -}
endOptions : List String
endOptions =
    [ "ぜんぶ壊したら勝ち", "時間切れまでスコア稼ぎ", "エンドレスで、どんどん速く" ]


endDefault : String
endDefault =
    "ぜんぶ壊したら勝ち"


{-| エッセンスの例(押すとそのまま入る)。 -}
essenceExamples : List String
essenceExamples =
    [ "夜だけの世界", "すべて和菓子でできている", "音がすべて猫の声" ]


{-| 「言葉で」が入っていればそちらが答え、空なら選択肢。 -}
answerOf : String -> String -> String
answerOf text_ pick =
    case String.trim text_ of
        "" ->
            pick

        answer ->
            answer


{-| いま効いているエッセンス(素材づくりへの提案にも映す)。空 = 無し。 -}
essenceOf : Create -> String
essenceOf create =
    String.trim create.gameEssence


{-| インタビューの答えと自由記述を 1 本の direction に合成する。
形式: 「動かすもの: …/終わり方: …/エッセンス: …」(エッセンスは空なら省く)。
自由記述があれば先頭に連結する。
-}
interviewDirection : Bool -> Create -> String
interviewDirection starterFresh create =
    let
        essence =
            essenceOf create

        composed =
            String.join "/"
                ((if starterFresh then
                    [ "動かすもの: " ++ answerOf create.gameMoverText create.gameMoverPick
                    , "終わり方: " ++ answerOf create.gameEndText create.gameEndPick
                    ]

                  else
                    -- 育ったゲームでは「テンプレートのまま」の既定が意味を持たないので、
                    -- 2 問は依頼文に載せない(エッセンスと自由記述だけが答え)
                    []
                 )
                    ++ (if essence == "" then
                            []

                        else
                            [ "エッセンス: " ++ essence ]
                       )
                )
    in
    case ( String.trim create.gameDirection, composed ) of
        ( "", c ) ->
            c

        ( free, "" ) ->
            free

        ( free, c ) ->
            free ++ "/" ++ c


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
    , gameMoverPick = moverDefault
    , gameMoverText = ""
    , gameEndPick = endDefault
    , gameEndText = ""
    , gameEssence = ""
    , gamePrompt = PromptIdle
    , gameCopied = False
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

    -- いま開いているセクション(入口 / 素材 / 調整 / アーカイブ)
    , section : Section

    -- アーカイブの中身(GET /atelier/archive)
    , archive : ArchiveData

    -- 🗃️(アーカイブへ送る)/ ↩ 候補に戻す の飛行中の印(押した瞬間 ⏳ + 無効化)
    , archivePending : Maybe String
    , restorePending : Maybe String

    -- 履歴の「vN に戻す」を押した札(⏳ と失敗理由をその行に出す控え)
    , versionPending : Maybe String

    -- ゲーム起動(make debug)の進み
    , launch : Launch

    -- 起動ログの全文展開(進捗ミニパネル)。既定は畳み、失敗時だけ自動で開く
    , launchLogExpanded : Bool

    -- プレビューの描き出し(/runner/log)のログ末尾。進捗ミニパネルの材料
    , bakeLines : List String

    -- 焼きの進捗ミニパネルの全文展開。焼いていない時は
    -- 「作れませんでした」の『ログ』リンクでパネルごと開く印を兼ねる
    , bakeLogExpanded : Bool

    -- プレビュー画像のキャッシュ避け。候補の中身が変わった取得でだけ進む
    , bust : Int

    -- プレビューの拡大表示(開いている間だけ Just)
    , lightbox : Maybe Lightbox

    -- 生まれたてのテンプレートプロジェクトか(journey の hasStarterDoc。Main が入れる)。
    -- 真の間だけ、インタビューに「テンプレートのまま」前提のチップ(パドル等)を出す —
    -- 育ったゲームでブロック崩しの言葉が並ぶのは無意味なので隠す
    , starterFresh : Bool
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
    , section = SectionLanding
    , archive = ArchiveLoading
    , archivePending = Nothing
    , restorePending = Nothing
    , versionPending = Nothing
    , launch = LaunchIdle
    , launchLogExpanded = False
    , bakeLines = []
    , bakeLogExpanded = False
    , bust = 0
    , lightbox = Nothing
    , starterFresh = False
    }


{-| journey の hasStarterDoc を受け取る(Main が state 取得のたびに呼ぶ)。 -}
setStarterFresh : Bool -> Model -> Model
setStarterFresh fresh model =
    { model | starterFresh = fresh }


type Msg
    = CandidateClicked String String
    | SwapClicked
    | StartGameClicked
    | PromoteAnywayClicked
    | RollbackClicked String String
    | OverlayTick
    | OverlayClosed
    | OpenLanding
    | OpenStorehouse
    | OpenPicks
    | OpenArchiver
    | ComboTrialClicked
    | AuditionClicked
    | ArchiveClicked String
    | RestoreClicked String
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
    | GameMoverPicked String
    | GameMoverEdited String
    | GameEndPicked String
    | GameEndEdited String
    | GameEssenceEdited String
    | GamePromptEdited String
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
閉じた合図 — Main が候補と提案を取り直し、焼きへの誘いを出す。
OutToast は一言の知らせ(◇これから の答えもこれで返す — 黙るボタンを作らない)。
-}
type Out
    = OutNone
    | OutPromote { candidate : String, slot : String }
    | OutStartGame
    | OutClosed
    | OutToast String
    | OutFetchPrompt { slot : String, count : Int, direction : String }
    | OutFetchGamePrompt String
    | OutCopyPrompt String
    | OutCopyFile { slot : String, name : String }
    | OutEditFile String
    | OutScaffold { kind : String, title : String, role : String }
    | OutArchive String
    | OutRestore String


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
            -- 戻すのは怖くない操作なので、起動の案内も選択も挟まず直行する。
            -- どの札を押したかは控える(アーカイブの行の ⏳ と失敗理由の置き場)
            if model.pending == Nothing then
                promote Rollback { slot = slot, file = file } { model | versionPending = Just file }

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

        OpenLanding ->
            ( switchSection SectionLanding model, OutNone )

        OpenStorehouse ->
            ( switchSection SectionStorehouse model, OutNone )

        OpenPicks ->
            ( switchSection SectionPicks model, OutNone )

        OpenArchiver ->
            ( switchSection SectionArchiver model, OutNone )

        ComboTrialClicked ->
            ( model, OutToast "これから: 複数の候補を、何も書き換えずに組み合わせて走らせて比べられるようになります" )

        AuditionClicked ->
            ( model, OutToast "これから: 候補の音をこの場で聴き比べられるようになります" )

        ArchiveClicked file ->
            -- 候補カードの 🗃️ — アーカイブへ送る(消さない。候補に戻せる)。
            -- 押した瞬間から ⏳(飛行中の二度押しは送らない)
            if model.archivePending /= Nothing then
                ( model, OutNone )

            else
                ( { model | archivePending = Just file }, OutArchive file )

        RestoreClicked file ->
            -- アーカイブの「↩ 候補に戻す」— 候補の列へ戻す
            if model.restorePending /= Nothing then
                ( model, OutNone )

            else
                ( { model | restorePending = Just file }, OutRestore file )

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
            -- 退避バージョンの拡大からの「vN に戻す」— 既存のロールバック経路に委譲
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

        GameMoverPicked option ->
            -- 選択肢を選んだら「言葉で」は畳む(答えはいつも 1 つ)
            ( mapCreate (\c -> { c | gameMoverPick = option, gameMoverText = "" }) model, OutNone )

        GameMoverEdited text_ ->
            ( mapCreate (\c -> { c | gameMoverText = text_ }) model, OutNone )

        GameEndPicked option ->
            ( mapCreate (\c -> { c | gameEndPick = option, gameEndText = "" }) model, OutNone )

        GameEndEdited text_ ->
            ( mapCreate (\c -> { c | gameEndText = text_ }) model, OutNone )

        GameEssenceEdited text_ ->
            ( mapCreate (\c -> { c | gameEssence = text_ }) model, OutNone )

        GamePromptEdited text_ ->
            -- 届いた依頼文は下書き — 渡す前に書き換えて構わない
            case model.create.gamePrompt of
                PromptReady _ ->
                    ( mapCreate (\c -> { c | gamePrompt = PromptReady text_ }) model, OutNone )

                _ ->
                    ( model, OutNone )

        MakeGamePromptClicked ->
            if model.create.gamePrompt == PromptLoading then
                -- 飛行中の二度押しは送らない
                ( model, OutNone )

            else
                -- 2 問の答え(テンプレートのままが既定)があるので direction は空にならない
                ( mapCreate (\c -> { c | gamePrompt = PromptLoading, gameCopied = False }) model
                , OutFetchGamePrompt (interviewDirection model.starterFresh model.create)
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
            -- 候補カードの「✏️ 手直し」— 調整(エディタ)でその atelier/ の
            -- ファイルを開く(開くのは Main の仕事なので値で返す)
            ( switchSection SectionStorehouse model, OutEditFile file )

        CopyCurrentClicked slotFile ->
            -- 「いまの見た目」の「✏️ 写しを作って直す」— 名前は自動採番
            -- (スロット base + 空き番の一文字)。成功で写しが調整に開く
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


{-| セクションの行き来。前の失敗文言は持ち越さない(古い理由を残さない)。 -}
switchSection : Section -> Model -> Model
switchSection section model =
    { model | section = section, promoteError = Nothing, versionPending = Nothing }


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
候補ゾーンを出さないだけ(調整は生きる)。
-}
candidatesFailed : Model -> Model
candidatesFailed model =
    { model | data = Unavailable }


{-| GET /atelier/archive。 -}
gotArchive : Archive -> Model -> Model
gotArchive archive model =
    { model | archive = ArchiveReady archive }


{-| /atelier/archive の失敗。旧サーバ(404)も読めない応答も同じ —
アーカイブの入口ごと出さないだけ(fail-open)。
-}
archiveFailed : Model -> Model
archiveFailed model =
    { model | archive = ArchiveUnavailable }


{-| POST /atelier/archive・/atelier/restore の応答(成功も失敗も)。飛行中の
印を畳むだけ — 一覧の取り直しと理由の告げ方(トースト)は Main の仕事。
-}
archiveSettled : Model -> Model
archiveSettled model =
    { model | archivePending = Nothing, restorePending = Nothing }


{-| アーカイブが使えるか(GET /atelier/archive が読めた)。
入口のアーカイブ行きと候補カードの 🗃️ を出す条件。
-}
archiveAvailable : Model -> Bool
archiveAvailable model =
    case model.archive of
        ArchiveReady _ ->
            True

        _ ->
            False


{-| アーカイブの中身の件数(入口のアーカイブ行きに添える)。 -}
archiveCount : Model -> Int
archiveCount model =
    case model.archive of
        ArchiveReady archive ->
            List.length archive.history + List.length archive.candidates

        _ ->
            0


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
/atelier/candidates(2 秒)と /runner/log(1 秒)を回す。
-}
isBaking : Model -> Bool
isBaking model =
    case model.data of
        Ready candidates ->
            candidates.baking

        _ ->
            False


{-| プレビューの描き出しのログ(/runner/log — 封筒の受領は Main、こちらは
進捗パネルの材料に写すだけ)。走りが止まって exitCode が 0 以外なら失敗 —
その時だけログを自動で全文展開する(失敗の時だけは隠さない)。
-}
gotBakeLog : GameLog -> Model -> Model
gotBakeLog log model =
    { model
        | bakeLines = log.lines
        , bakeLogExpanded =
            model.bakeLogExpanded || (not log.running && Maybe.withDefault 0 log.exitCode /= 0)
    }


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


{-| 起動待ちの間だけ Just(実況パネルの材料)。ホームの提案カードの下も同じ形で使う。 -}
launchStarting : Model -> Maybe { lines : List String, expanded : Bool }
launchStarting model =
    case model.launch of
        LaunchStarting info ->
            Just { lines = info.lines, expanded = model.launchLogExpanded }

        _ ->
            Nothing


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
        , versionPending = Nothing
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
「準備中」の一言になるだけ(fail-open — 調整も候補選びも生きる)。
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


{-| 「なにをつくる?」の並び。素材の道が先(あそび・新種はその後)。 -}
createPaths : List CreatePath
createPaths =
    [ PathAi, PathGame, PathScaffold ]


{-| いま選んでいる道(Nothing = 選択リストを出している)。テストの覗き窓。 -}
chosenPath : Model -> Maybe CreatePath
chosenPath model =
    model.create.path


{-| POST /atelier/copy 成功。調整(エディタ)へ切り替える —
写しを開くのは Main(トーストと openFile)。
-}
copyDone : Model -> Model
copyDone model =
    switchSection SectionStorehouse model
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


{-| 「つくる」パネルが開いているか。手で触っていなければ畳む。 -}
createOpen : Model -> Bool
createOpen model =
    Maybe.withDefault False model.create.open


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


{-| GET /atelier/archive。file だけは必須(無ければその行は読めず全体が
既定に倒れるのではなく、その行だけ捨てたいが — 契約上 file は常に来る)。
他の欠け・null は既定値(fail-open)。
-}
archiveDecoder : D.Decoder Archive
archiveDecoder =
    D.map2 Archive
        (D.oneOf [ D.field "history" (D.list historyEntryDecoder), D.succeed [] ])
        (D.oneOf [ D.field "candidates" (D.list archivedCandidateDecoder), D.succeed [] ])


historyEntryDecoder : D.Decoder HistoryEntry
historyEntryDecoder =
    D.map5 HistoryEntry
        (D.field "file" D.string)
        (D.oneOf [ D.field "slot" D.string, D.succeed "" ])
        (D.oneOf [ D.field "ver" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "note" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "mtime" D.int, D.succeed 0 ])


archivedCandidateDecoder : D.Decoder ArchivedCandidate
archivedCandidateDecoder =
    D.map4 ArchivedCandidate
        (D.field "file" D.string)
        (D.oneOf [ D.field "entityId" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "note" (D.map Just D.string), D.succeed Nothing ])
        (D.oneOf [ D.field "mtime" D.int, D.succeed 0 ])


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


{-| 候補(1 件以上)が届いているか。候補ゾーンを出すかの材料。 -}
hasCandidates : Model -> Bool
hasCandidates model =
    case model.data of
        Ready candidates ->
            not (List.isEmpty candidates.slots)

        _ ->
            False


{-| アトリエタブで入口を描くべきか。タブを手で開いた直後の既定 —
提案(journey)からの直行はセクションを開いてから来る。
-}
showLanding : Model -> Bool
showLanding model =
    model.section == SectionLanding


{-| アトリエタブで素材(候補選び+つくる)を主役に描くべきか。 -}
showPicks : Model -> Bool
showPicks model =
    model.section == SectionPicks


{-| アトリエタブでアーカイブを描くべきか。 -}
showArchiver : Model -> Bool
showArchiver model =
    model.section == SectionArchiver


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


{-| 素材セクションの画面(Main が showPicks の時だけこれを主役に描く —
Doc エディタもそのタブ群もここには出ない)。1 画面に収まる密度を保つ:
スロットは実際には 1〜数個で、カードの直下に装着ボタンが必ず見える。
候補が届く前(Loading 等)でも、つくるバーは出す(空の画面で待たせない)。
-}
view : String -> Model -> Html Msg
view base model =
    div [ HA.class "atelier-picks min-h-0 flex-1 overflow-y-auto px-6 py-6" ]
        [ div [ HA.class "mx-auto w-full max-w-3xl" ]
            (List.concat
                [ [ div [ HA.class "mb-4" ] [ viewSectionTop "🎨 素材を作る" ] ]
                , [ viewCreate model ]
                , case model.data of
                    Ready candidates ->
                        List.concat
                            [ if List.isEmpty candidates.slots then
                                []

                              else
                                [ div [ HA.class "mb-5" ]
                                    [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "🎨 候補選び" ]
                                    , div [ HA.class "mt-1 text-[11px] text-ink-soft" ]
                                        [ text "新しい見た目の候補です。カードを選んで、ゲームで使いましょう" ]
                                    ]
                                ]
                            , viewBakePanel model candidates
                            , List.map (viewSlot base model candidates.baking) candidates.slots
                            , viewComboTrial candidates.slots
                            , viewLoose candidates.loose
                            ]

                    _ ->
                        []
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


{-| セクションの最上段。「← アトリエ」で入口へ戻る(現在地名を添える)。 -}
viewSectionTop : String -> Html Msg
viewSectionTop place =
    div [ HA.class "atelier-section-top flex items-center gap-2" ]
        [ button [ HA.class "btn btn-mini", HE.onClick OpenLanding ] [ text "← アトリエ" ]
        , span [ HA.class "text-[11px] text-ink-faint" ] [ text place ]
        ]


{-| アトリエの入口。行き先の大きなカード 2 枚(初期表示で両方見える大きさ)と、
アーカイブへの控えめな入口。生まれたてはパラメータ側を推す。
-}
viewLanding : Model -> Html Msg
viewLanding model =
    div [ HA.class "atelier-landing flex min-h-0 flex-1 flex-col items-center justify-center overflow-y-auto px-6 py-6" ]
        [ div [ HA.class "flex w-full max-w-2xl flex-col gap-4 sm:flex-row" ]
            [ landingCard
                { icon = "⚙️"
                , title = "パラメータを変える"
                , body = "ゲームのパラメータを変えられます。保存した瞬間、走っているゲームに反映されます。"
                , chip =
                    if model.starterFresh then
                        Just "おすすめ"

                    else
                        Nothing
                , msg = OpenStorehouse
                }
            , landingCard
                { icon = "🎨"
                , title = "素材を作る"
                , body = "見た目や音の候補を AI に作らせて、選んで使う。あそびを大きく変えるのもここから。"
                , chip =
                    case candidateCount model of
                        0 ->
                            Nothing

                        n ->
                            Just ("候補 " ++ String.fromInt n ++ " 件")
                , msg = OpenPicks
                }
            ]
        , if archiveAvailable model then
            button
                [ HA.class "atelier-landing-archive mt-6 cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
                , HE.onClick OpenArchiver
                ]
                [ text (archiverLabel model) ]

          else
            text ""
        ]


landingCard : { icon : String, title : String, body : String, chip : Maybe String, msg : Msg } -> Html Msg
landingCard info =
    button
        [ HA.class "atelier-landing-card flex-1 cursor-pointer rounded-lg border border-edge bg-panel p-5 text-left transition-colors hover:border-ink-faint hover:bg-white/5"
        , HE.onClick info.msg
        ]
        [ div [ HA.class "flex items-center gap-2.5" ]
            [ span [ HA.class "text-2xl leading-none" ] [ text info.icon ]
            , span [ HA.class "text-sm font-semibold text-ink" ] [ text info.title ]
            , case info.chip of
                Just label ->
                    span [ HA.class "badge shrink-0 bg-accent/20 text-accent" ] [ text label ]

                Nothing ->
                    text ""
            ]
        , div [ HA.class "mt-2 text-xs leading-relaxed text-ink-soft" ] [ text info.body ]
        ]


{-| 入口カードのチップに出す候補の総数(prev の退避札は数えない)。 -}
candidateCount : Model -> Int
candidateCount model =
    case model.data of
        Ready candidates ->
            candidates.slots
                |> List.concatMap .candidates
                |> List.filter (\c -> not c.isPrev)
                |> List.length

        _ ->
            0


archiverLabel : Model -> String
archiverLabel model =
    case archiveCount model of
        0 ->
            "🗃️ アーカイブ"

        n ->
            "🗃️ アーカイブ(" ++ String.fromInt n ++ ")"


{-| アーカイブのセクション。使っていない候補と昔のバージョンの置き場 —
何も捨てない。ボタンは対象の行の中(次のアクションを探させない)。
「vN に戻す」は既存の promote 経路なので、成功のオーバーレイもここに出る。
-}
viewArchiver : Model -> Html Msg
viewArchiver model =
    let
        archive =
            case model.archive of
                ArchiveReady a ->
                    a

                _ ->
                    { history = [], candidates = [] }
    in
    div [ HA.class "atelier-archiver min-h-0 flex-1 overflow-y-auto px-6 py-6" ]
        [ div [ HA.class "mx-auto w-full max-w-3xl" ]
            (List.concat
                [ [ div [ HA.class "mb-4" ] [ viewSectionTop "🗃️ アーカイブ" ]
                  , div [ HA.class "mb-5" ]
                        [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "🗃️ アーカイブ" ]
                        , div [ HA.class "mt-1 text-[11px] text-ink-soft" ]
                            [ text "使っていない候補と、昔のバージョンの置き場。アトリエはいつもきれいに — でも、何も捨てません。" ]
                        ]
                  ]
                , viewArchivedCandidates model archive.candidates
                , viewHistory model archive.history
                ]
            )
        , case model.overlay of
            Just overlay ->
                viewOverlay overlay

            Nothing ->
                text ""
        ]


viewArchivedCandidates : Model -> List ArchivedCandidate -> List (Html Msg)
viewArchivedCandidates model entries =
    [ div [ HA.class "atelier-archived mb-6" ]
        (div [ HA.class "mb-1.5 text-xs font-semibold text-ink" ]
            [ text "アーカイブした候補(atelier/archive/)" ]
            :: (if List.isEmpty entries then
                    [ div [ HA.class "text-[11px] text-ink-faint" ]
                        [ text "まだ空です。候補カードの 🗃️ で送れます。" ]
                    ]

                else
                    List.map (viewArchivedRow model) entries
               )
        )
    ]


viewArchivedRow : Model -> ArchivedCandidate -> Html Msg
viewArchivedRow model entry =
    div [ HA.class "atelier-archived-row flex items-center gap-2 border-b border-edge/40 py-1.5" ]
        [ span [ HA.class "min-w-0 flex-1 truncate font-mono text-[11px] text-ink", HA.title entry.file ]
            [ text (baseName entry.file) ]
        , case entry.entityId of
            Just entity ->
                span [ HA.class "badge shrink-0" ] [ text entity ]

            Nothing ->
                text ""
        , case entry.note of
            Just note ->
                span [ HA.class "min-w-0 flex-1 truncate text-[11px] text-ink-soft", HA.title note ]
                    [ text note ]

            Nothing ->
                span [ HA.class "flex-1" ] []
        , span [ HA.class "shrink-0 text-[10px] text-ink-faint" ] [ text (mtimeLabel entry.mtime) ]
        , button
            [ HA.class "btn btn-mini shrink-0"
            , HA.disabled (model.restorePending /= Nothing)
            , HE.onClick (RestoreClicked entry.file)
            ]
            [ text
                (if model.restorePending == Just entry.file then
                    "⏳ 候補に戻しています…"

                 else
                    "↩ 候補に戻す"
                )
            ]
        ]


viewHistory : Model -> List HistoryEntry -> List (Html Msg)
viewHistory model entries =
    [ div [ HA.class "atelier-history" ]
        (div [ HA.class "mb-1.5 text-xs font-semibold text-ink" ]
            [ text "バージョン履歴" ]
            :: (if List.isEmpty entries then
                    [ div [ HA.class "text-[11px] text-ink-faint" ]
                        [ text "切り替えると、前のバージョンがここに積まれます。" ]
                    ]

                else
                    historyBySlot entries
                        |> List.concatMap (\( slot, versions ) -> List.map (viewHistoryRow model slot) versions)
               )
        )
    ]


{-| 履歴をスロット別に束ね、各スロットは新しい版から。スロットの並びは
一番新しい札を持つものが先(直近に触った場所が上)。
-}
historyBySlot : List HistoryEntry -> List ( String, List HistoryEntry )
historyBySlot entries =
    entries
        |> List.map .slot
        |> uniques
        |> List.map
            (\slot ->
                ( slot
                , entries
                    |> List.filter (\e -> e.slot == slot)
                    |> List.sortBy (\e -> negate e.ver)
                )
            )
        |> List.sortBy
            (\( _, versions ) ->
                List.head versions
                    |> Maybe.map (\e -> negate e.mtime)
                    |> Maybe.withDefault 0
            )


{-| 順序を保ったまま重複を落とす。 -}
uniques : List String -> List String
uniques values =
    List.foldl
        (\v acc ->
            if List.member v acc then
                acc

            else
                acc ++ [ v ]
        )
        []
        values


viewHistoryRow : Model -> String -> HistoryEntry -> Html Msg
viewHistoryRow model slot entry =
    div [ HA.class "atelier-history-row border-b border-edge/40 py-1.5" ]
        (div [ HA.class "flex items-center gap-2" ]
            [ span [ HA.class "badge shrink-0 bg-white/10" ] [ text ("v" ++ String.fromInt entry.ver) ]
            , span [ HA.class "min-w-0 flex-1 truncate font-mono text-[11px] text-ink", HA.title entry.file ]
                [ text (baseName slot) ]
            , case entry.note of
                Just note ->
                    span [ HA.class "min-w-0 flex-1 truncate text-[11px] text-ink-soft", HA.title note ]
                        [ text note ]

                Nothing ->
                    span [ HA.class "flex-1" ] []
            , span [ HA.class "shrink-0 text-[10px] text-ink-faint" ] [ text (mtimeLabel entry.mtime) ]
            , button
                [ HA.class "btn btn-mini shrink-0"
                , HA.disabled (model.pending /= Nothing)
                , HE.onClick (RollbackClicked slot entry.file)
                ]
                [ text
                    (if model.pending /= Nothing && model.versionPending == Just entry.file then
                        "⏳ 戻しています…"

                     else
                        "↩ v" ++ String.fromInt entry.ver ++ " に戻す"
                    )
                ]
            ]
            :: (case ( model.versionPending == Just entry.file, model.promoteError ) of
                    ( True, Just reason ) ->
                        [ div [ HA.class "mt-1 text-[11px] text-danger" ]
                            [ text ("戻せませんでした — " ++ reason) ]
                        ]

                    _ ->
                        []
               )
        )


{-| 焼きの進捗ミニパネル(ゾーンに 1 枚 — カード毎には出さない)。
焼いている間は常に出す。焼きが止まってもプレビューが欠けている時は
「作れませんでした」の『ログ』リンクから同じパネルを開ける(失敗の赤)。
-}
viewBakePanel : Model -> Candidates -> List (Html Msg)
viewBakePanel model candidates =
    if candidates.baking then
        [ div [ HA.class "atelier-bake mb-4" ]
            [ Progress.view
                { message = "プレビューを描き出しています… できたカードから順に映ります"
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
                    , span [ HA.class "text-[10px] text-ink-faint" ] [ text "プレビューを描き出しています…" ]
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

            -- 音の候補の聴き比べ(これから)。押すと予定を一言で返す
            , if slotKind model slotName == Just "sound" then
                button
                    [ HA.class "btn btn-ghost btn-mini shrink-0"
                    , stopClick AuditionClicked
                    ]
                    [ text "▶ 試聴", mockChip ]

              else
                text ""

            -- 調整(エディタ)でこの候補ファイルを開く。カードの選択とは
            -- 別の動詞なので伝播は止める
            , button
                [ HA.class "btn btn-ghost btn-mini shrink-0"
                , stopClick (EditCandidateClicked candidate.file)
                ]
                [ text "✏️ 手直し" ]

            -- アーカイブへ送る(消さない)。押した瞬間 ⏳ + 無効化
            , if archiveAvailable model then
                button
                    [ HA.class "btn btn-ghost btn-mini shrink-0"
                    , HA.title "アーカイブへ送る(いつでも候補に戻せます)"
                    , HA.disabled (model.archivePending /= Nothing)
                    , stopClick (ArchiveClicked candidate.file)
                    ]
                    [ text
                        (if model.archivePending == Just candidate.file then
                            "⏳"

                         else
                            "🗃️ アーカイブ"
                        )
                    ]

              else
                text ""
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
                                        "切り替えています…"

                                     else
                                        "🔄 これを使う"
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
                                        "↩ このバージョンに戻す"
                                    )
                                ]
                    , case model.promoteError of
                        Just reason ->
                            div [ HA.class "mt-1.5 text-[11px] text-danger" ]
                                [ text ("切り替えられませんでした — " ++ reason) ]

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


{-| まだ形だけの機能の目印。これを付けたボタンは黙らない —
押されたら必ず一言(何になる予定か)を返す。
-}
mockChip : Html msg
mockChip =
    span [ HA.class "mock-chip ml-1.5 rounded border border-dashed border-ink-faint/70 px-1 text-[10px] font-normal text-ink-faint" ]
        [ text "◇これから" ]


{-| 候補一覧の下の「選んだ組み合わせで試す」(これから)。押すと予定を一言で返す。 -}
viewComboTrial : List Slot -> List (Html Msg)
viewComboTrial slots =
    if List.isEmpty slots then
        []

    else
        [ div [ HA.class "atelier-combo mb-4" ]
            [ button [ HA.class "btn btn-mini", HE.onClick ComboTrialClicked ]
                [ text "選んだ組み合わせで試す", mockChip ]
            ]
        ]


{-| スロットの素材種(/atelier/slots の kind)。一覧に無ければ不明。 -}
slotKind : Model -> String -> Maybe String
slotKind model file =
    model.create.slots
        |> List.filter (\s -> s.file == file)
        |> List.head
        |> Maybe.map .kind


{-| ゲーム未起動の案内カード(選んだカードのあるスロットの直下)。
起動と「そのまま装着」の両方を差し出す。
-}
viewGate : Model -> List (Html Msg)
viewGate model =
    [ div [ HA.class "atelier-gate mt-3 max-w-lg rounded-lg border border-edge bg-panel p-4" ]
        (div [ HA.class "text-xs text-ink" ]
            [ text "ゲームが起きていません。切り替えは今すぐできますが、変化をその場で見るには起動しましょう" ]
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
                    [ text "そのまま切り替える" ]
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
                [ text "✓ ゲームが起きました。切り替えると走っている画面にすぐ映ります" ]
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
            (div [ HA.class "mb-1 text-[11px] text-ink-faint" ] [ text "素材スロットが見つからない候補" ]
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
                            "採用しました"

                        Rollback ->
                            "前の見た目に戻しました"
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
                            [ text ("前のバージョンは " ++ retired ++ " に退避しました。いつでも戻せます") ]

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
                                [ text "↩ このバージョンに戻す" ]

                        else
                            button [ HA.class "btn btn-primary btn-mini", stopClick LightboxSwapClicked ]
                                [ text "🔄 この案を使う" ]

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
調整でも Main がこれを最上段に描く(創作の入口はどちらのセクションにもある)。
-}
viewCreate : Model -> Html Msg
viewCreate model =
    div [ HA.class "atelier-create mb-5 rounded-lg border border-edge bg-panel" ]
        (button
            [ HA.class "atelier-create-bar flex w-full cursor-pointer items-center gap-2 rounded-lg px-4 py-2.5 text-left text-sm font-semibold text-ink transition-colors hover:bg-white/5"
            , HE.onClick CreateToggled
            ]
            [ span [] [ text "✨ 新しい素材を作る" ]
            , span [ HA.class "flex-1" ] []

            -- 開閉できる事が言葉で分かる目印(バー全体が押せる — これは chip)
            , span [ HA.class "rounded border border-edge px-2 py-0.5 text-[12px] font-normal text-ink-soft" ]
                [ text
                    (if createOpen model then
                        "▾ 閉じる"

                     else
                        "▸ 開く"
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
                                    (List.map viewPathRow createPaths)
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
                                            viewGameCard model.starterFresh model.create

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


{-| 「なにをつくる?」の 1 行(アイコン+名前+一言)。 -}
viewPathRow : CreatePath -> Html Msg
viewPathRow path =
    let
        info =
            pathInfo path
    in
    button
        [ HA.class "atelier-path-row flex w-full cursor-pointer items-center gap-2.5 rounded-lg border border-edge px-3 py-2 text-left transition-colors hover:bg-white/5"
        , HE.onClick (PathChosen path)
        ]
        [ span [ HA.class "text-base leading-none" ] [ text info.icon ]
        , span [ HA.class "text-xs font-semibold text-ink" ] [ text info.title ]
        , span [ HA.class "min-w-0 flex-1 truncate text-[11px] text-ink-faint" ] [ text info.blurb ]
        ]


pathInfo : CreatePath -> { icon : String, title : String, blurb : String }
pathInfo path =
    case path of
        PathGame ->
            { icon = "🕹️"
            , title = "あそびを作らせる"
            , blurb = "遊びのルールをAIに作らせる — 先に遊びを決めると必要な素材がはっきりします"
            }

        PathAi ->
            { icon = "🤖"
            , title = "素材をAIに作らせる"
            , blurb = "見た目や音の候補をAIに作らせて、選んで使う"
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
                        [ text "素材スロット一覧を取得できませんでした(サーバが古い可能性があります)" ]
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
                                    -- あそびのインタビューでエッセンスが決まって
                                    -- いれば、素材の方向性にも同じ軸を提案する
                                    (case essenceOf create of
                                        "" ->
                                            "方向性 "
                                                ++ directionPlaceholder
                                                    (selectedCreateSlot create
                                                        |> Maybe.map .kind
                                                        |> Maybe.withDefault ""
                                                    )

                                        essence ->
                                            "方向性: エッセンス『" ++ essence ++ "』"
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
                [ text "Claude Code などのAIに貼り付けてください。候補が atelier/ にできると、ここに自動で現れます(プレビューも自動で描き出されます)" ]
            ]


{-| 「あそびを作らせる」— ゲームのルールそのものを AI に作らせる道。
プロンプトを組んでコピーするだけ(作るのは AI で、続きはホームの提案が拾う)。
-}
viewGameCard : Bool -> Create -> Html Msg
viewGameCard starterFresh create =
    div
        [ HA.class "atelier-create-game min-w-[280px] flex-1 rounded-lg border border-edge bg-black/20 p-4" ]
        (List.concat
            [ [ div [ HA.class "mb-2 text-xs font-semibold text-ink" ] [ text "🕹️ あそびを作らせる" ]
              , div [ HA.class "mb-2 text-[11px] leading-relaxed text-ink-faint" ]
                    [ text "先に遊びを決めると、必要な素材がはっきりします。素材づくりはその後がおすすめ" ]
              ]

            -- インタビューの 2 問は「テンプレートのまま」が答えになる生まれたて限定。
            -- 育ったゲームにパドルやブロックの言葉は無意味なので出さない
            , if starterFresh then
                [ viewInterviewQuestion
                    { label = "動かすのは、何?"
                    , options = moverOptions
                    , pick = create.gameMoverPick
                    , text_ = create.gameMoverText
                    , placeholder = "言葉で(例: 傘をさしたおじいさん)"
                    , onPick = GameMoverPicked
                    , onEdit = GameMoverEdited
                    }
                , viewInterviewQuestion
                    { label = "一回の遊びは、どう終わる?"
                    , options = endOptions
                    , pick = create.gameEndPick
                    , text_ = create.gameEndText
                    , placeholder = "言葉で(例: 3回落としたら終わり)"
                    , onPick = GameEndPicked
                    , onEdit = GameEndEdited
                    }
                ]

              else
                []
            , [ viewEssenceRow create
              , Html.textarea
                    [ HA.class "field mt-2 h-auto min-h-[3.5rem] w-full resize-y py-1.5 text-xs leading-relaxed"
                    , HA.rows 2
                    , HA.placeholder
                        (if starterFresh then
                            "自由に書き足す(例: 面は5つ。だんだん狭くなる)"

                         else
                            "どう変えたい?(例: 冬の夜だけ現れる行商人を足す)"
                        )
                    , HA.value create.gameDirection
                    , HE.onInput GameDirectionEdited
                    ]
                    []
              , div [ HA.class "mt-3" ]
                    [ button
                        [ HA.class "btn btn-primary"
                        , HA.disabled
                            (create.gamePrompt
                                == PromptLoading
                                || interviewDirection starterFresh create
                                == ""
                            )
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


{-| インタビュー 1 問(選択肢チップ + 「言葉で」)。言葉が入っていれば
そちらが答えなので、チップの光は消す。
-}
viewInterviewQuestion :
    { label : String
    , options : List String
    , pick : String
    , text_ : String
    , placeholder : String
    , onPick : String -> Msg
    , onEdit : String -> Msg
    }
    -> Html Msg
viewInterviewQuestion q =
    let
        custom =
            String.trim q.text_ /= ""
    in
    div [ HA.class "atelier-interview-q mt-2" ]
        [ div [ HA.class "text-[11px] text-ink-soft" ] [ text q.label ]
        , div [ HA.class "mt-1 flex flex-wrap items-center gap-1.5" ]
            (List.map
                (\option ->
                    button
                        [ HA.classList
                            [ ( "btn btn-mini", True )
                            , ( "btn-primary", not custom && q.pick == option )
                            ]
                        , HE.onClick (q.onPick option)
                        ]
                        [ text option ]
                )
                q.options
                ++ [ Html.input
                        [ HA.class "field w-44 text-xs"
                        , HA.placeholder q.placeholder
                        , HA.value q.text_
                        , HE.onInput q.onEdit
                        ]
                        []
                   ]
            )
        ]


{-| 「あなたのエッセンスを、ひとこと」。例チップは押すとそのまま入る。
入れたエッセンスは素材づくり(🤖)の方向性の提案にも映る。
-}
viewEssenceRow : Create -> Html Msg
viewEssenceRow create =
    div [ HA.class "atelier-interview-essence mt-2" ]
        [ div [ HA.class "text-[11px] text-ink-soft" ] [ text "あなたのエッセンスを、ひとこと" ]
        , Html.input
            [ HA.class "field mt-1 w-full text-xs"
            , HA.placeholder "例: 夜だけの世界"
            , HA.value create.gameEssence
            , HE.onInput GameEssenceEdited
            ]
            []
        , div [ HA.class "mt-1 flex flex-wrap gap-1.5" ]
            (List.map
                (\example ->
                    button
                        [ HA.class "btn btn-mini"
                        , HE.onClick (GameEssenceEdited example)
                        ]
                        [ text example ]
                )
                essenceExamples
            )
        ]


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
            [ -- 届いた依頼文は下書き — 渡す前に書き換えて構わない(編集可)
              Html.textarea
                [ HA.class "atelier-game-prompt mt-3 h-48 max-h-48 w-full resize-none overflow-y-auto rounded border border-edge bg-black/40 p-2 font-mono text-[11px] leading-relaxed text-ink"
                , HA.value prompt
                , HE.onInput GamePromptEdited
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
                [ text "AIに貼ると、ルールとテストとbakeまで作ります。できたら、ホームに知らせが届きます" ]
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
                    [ viewRoleRadio scaffold "material" "素材(選んで使う物)"
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


{-| 退避バージョンの札。ファイル名の "prev-N" から N を拾う(見つからなければ「退避」)。 -}
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
