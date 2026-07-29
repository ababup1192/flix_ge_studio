port module Main exposing (Model, Msg(..), PaneSide(..), clampPaneWidth, init, main, update, view)

{-| リソース(スキーマ付き JSON)エディタ。

画面は 3 モード: ビジュアル(既定・テーブル+フォームで完結)/分割(テキスト+ビジュアル)/
コード(テキスト全面)。生テキストは逃げ道として常に生きていて、スキーマが無い・壊れている・
JSON が読めない間はビジュアルを選んでいてもコードに落ちる。

保存は lost update ガード付き: 保存前に GET /file で最新を取り、開いた時のテキストと
違っていたら止めて「再読込 / 構わず上書き」の 2 択を出す。

update は Cmd でなく自前データ Effect を返す(Cmd 化は main の Effect.perform だけ)—
「どの操作でどの封筒が飛ぶか」を elm-program-test(tests/FlowTest.elm)で検査するため。

-}

import Api
import Browser
import Browser.Events
import Atelier
import Dashboards
import Dict exposing (Dict)
import Doc
import Draft
import Edit exposing (Op(..), Seg(..), encodeSeg, pathKey)
import EditHistory
import Effect exposing (Effect)
import EntryOps
import EntryTable
import FileVerbs
import Html exposing (Html, button, datalist, div, h1, h2, img, input, label, option, pre, select, span, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import FormHelp
import Html.Lazy as HL
import Svg
import Svg.Attributes as SA
import Journey
import Json.Decode as D
import Json.Encode as E
import Lint
import NewGame
import MapEditor
import PixelEditor
import SfxEditor
import Plugins
import Progress
import Refs
import ContextMenu
import CrossEdit
import DocKind
import SceneView
import SearchView
import Schema
import SchemaForm
import Selection exposing (EntrySel(..))
import Skeleton
import Set exposing (Set)
import Sources
import Time
import Table
import Url
import Widgets.Weights as Weights
import Wizard



-- ポート(JS 境界)


port apiRequest : E.Value -> Cmd msg


port apiResponse : (D.Value -> msg) -> Sub msg



-- モデル


type Screen
    = Booting
    | NoServer
    | Picker
    | Editing


{-| 上部ナビの現在地。アトリエ = 既存の Doc エディタ一式(3 ペイン)。 -}
type Tab
    = HomeTab
    | AtelierTab


{-| プロジェクトピッカーの状態。候補一覧そのものはサーバ応答(Api.Projects)が正で、
ここが持つのは入力欄・切替中の dir・失敗文言だけ。
-}
type alias PickerState =
    { projects : Maybe Api.Projects
    , input : String

    -- POST /project 中の dir(二重送信の抑止と「開いています…」表示)
    , busy : Maybe String
    , error : Maybe String

    -- いま走っているゲームの作業ディレクトリ一覧(Tauri の list_running_games)。
    -- 素のブラウザでは常に空 = バッジが出ないだけ。候補 dir と突き合わせて
    -- 「● 起動中」を出す元にする。
    , runningCwds : List String
    }


emptyPicker : PickerState
emptyPicker =
    { projects = Nothing, input = "", busy = Nothing, error = Nothing, runningCwds = [] }


{-| 開いたファイルの隣(foo.json → foo.schema.json)を探した結果。
スキーマが無い/壊れていても生テキスト編集は常に生きている(閉じ込めない)ので、
ここは右ペインの表示を選ぶだけの状態。
-}
type SchemaState
    = SchemaNone
    | SchemaLoading
    | SchemaMissing
    | SchemaBroken String
      -- draft-07 など JSON-Schema 形式 = 意図的にフォーム化対象外の種類(ドット絵等)。
      -- 壊れではないので赤にしない。previewBroken は /atelier/preview が 404 だった印
    | SchemaForeign { previewBroken : Bool }
    | SchemaReady Schema.Schema


{-| 編集 1 件の封筒(語彙は Edit モジュール — 履歴も同じ形を運ぶ)。 -}
type alias EditPayload =
    Edit.Payload


{-| 作業モード。ビジュアル=テーブル+フォーム主役(テキスト非表示)・
分割=テキスト+ビジュアル・コード=テキスト全面。
-}
type ViewMode
    = VisualMode
    | SplitMode
    | CodeMode


{-| catalog へのエントリ追加の小ダイアログ(id を決めてから雛形を挿す)。
error は直前の確定が拒まれた理由(改名と同じ draft 方式)。
-}
type alias AddDialogState =
    { sectionKey : String
    , text : String
    , error : Maybe String
    }


{-| 使用中エントリの削除確認(Just = ダイアログ表示中)。sites は押した瞬間の
使用箇所 — 確認中に文書は動かないので固めてよい。
-}
type alias DeleteConfirmState =
    { sectionKey : String
    , entry : EntrySel
    , sites : List Refs.UsageSite
    }


{-| フォーカス中フィールドの打ちかけ。view はこの path の欄にだけ text を出し、
他の欄には文書の値を出す — docEdit のエコー・プレビュー往復・将来の /changes
追従のどれが挟まっても、フォーカス中の欄に整形済みの値が流し込まれない。
フォーカスは常に 1 箇所なので Dict でなく Maybe 1 個で足りる。
-}
type alias ActiveDraft =
    { path : List Seg
    , text : String

    -- 確定の仕方(数値の clamp / 生 JSON のパース検査)は focus した瞬間の
    -- スキーマ知識で固める — 確定時に view から引き直さない
    , kind : DraftKind

    -- focus 時点の表示文字。無変更 blur で同値の docEdit を流さない比較相手
    , original : String
    }


type DraftKind
    = NumberDraft Draft.NumberSpec
    | TextDraft
      -- ASCII マップ(type "grid")。確定 = 改行で割った文字列列を書き戻す
    | GridDraft
    | RawJsonDraft
      -- 文字列の列(type {"list":"text"})の 1 行。確定 = その行を差し替えた
      -- 列を丸ごと書く。items は focus した瞬間の列(確定時に view から引き直さない)
    | ListTextDraft { fieldPath : List Seg, index : Int, items : List String }
      -- weights の数値欄。確定 = 自分の値+他の行の比例配分をバッチ 1 本で書く。
      -- entries は focus した瞬間の配分(確定時に view から引き直さない流儀と同じ)
    | WeightsDraft
        { config : Weights.Config
        , fieldPath : List Seg
        , key : String
        , entries : List ( String, Float )
        }


{-| weights の数値欄の確定規則(0〜total・整数 total は整数丸め)。 -}
weightsSpec : Weights.Config -> Draft.NumberSpec
weightsSpec config =
    { isInt = config.decimals == 0
    , min = Just 0
    , max = Just config.total
    , step = Nothing
    }


{-| dirty のまま移動しようとした先。ダイアログの「破棄して開く」が
この値を読んで移動をやり直す — 移動の種類ごとに再送の口が違うため。
-}
type NavTarget
    = NavFile String
    | NavProject String
    | NavWizard
    | NavJump Jump


{-| ダッシュボードからのジャンプ先(ファイルを開いて、このエントリを選ぶ)。 -}
type alias Jump =
    { path : String
    , sectionKey : String
    , entry : EntrySel
    }


{-| ダッシュボードが読む 1 ファイルぶんの入れ物。本文とスキーマの往復を
別々に待つ(スキーマ欠けは普通のことなので、本文だけでも表示へ進む)。
-}
type alias DashSlot =
    { resource : String
    , path : String
    , dataReq : Maybe Int
    , doc : Maybe D.Value
    , dataError : Maybe String
    , schemaReq : Maybe Int
    , schema : Maybe Schema.Schema
    }


{-| 開いているダッシュボード。宣言(decl)ごと持つのは、後着の /resources で
宣言一覧が入れ替わっても表示中のボードが揺れないようにするため。
-}
type alias DashState =
    { decl : Api.Dashboard
    , slots : List DashSlot
    , selected : Maybe String
    }


{-| 横断辞書の 1 ファイルぶん。DashSlot と似るが、改名の書き戻し(applyDocEdits)は
生テキストに当てるので text も持つ。
-}
type alias CrossSlot =
    { resource : String
    , path : String
    , dataReq : Maybe Int
    , text : Maybe String
    , doc : Maybe D.Value
    , schemaReq : Maybe Int
    , schema : Maybe Schema.Schema
    }


{-| 肖像(ui.json)のエンジン焼き 1 枚。crop は design 座標での切り出し範囲
(root は design 全面に置かれるので、子ノード群の外接矩形が絵の範囲)。
-}
type alias PortraitImage =
    { png : String
    , imgW : Float
    , imgH : Float
    , crop : { x : Float, y : Float, w : Float, h : Float }
    , scale : Float
    }


{-| 肖像キャッシュの 1 項目。Loading を項目として持つのが二重リクエストの抑止。 -}
type PortraitState
    = PortraitLoading
    | PortraitReady PortraitImage
    | PortraitFailed String


{-| 改名が他ファイルにも及ぶ時の確認(Just = ダイアログ表示中)。
他ファイルは開いていない(dirty ガードの外)ので、保存まで進めてよいかを先に聞く。
-}
type alias CrossRenamePlan =
    { req : RenameRequest
    , files : List { path : String, count : Int }
    }


{-| 他ファイル改名の進行。1 ファイルずつ 取得 → 編集 → 保存 と直列に進める —
並列にすると失敗時に「どこまで書けたか」が言えなくなる。
-}
type alias CrossRenameRun =
    { req : RenameRequest
    , pending : List String
    , step : Maybe CrossStep
    , doneFiles : Int
    , doneRefs : Int
    }


{-| 進行中の 1 段(封筒 id・対象パス)。Putting は保存する本文の反映数も抱える。 -}
type CrossStep
    = CrossGetting Int String
    | CrossEditing Int String Int
    | CrossPutting Int String Int


{-| catalog id 改名の 1 回ぶん(どのセクションのどの id を何に)。 -}
type alias RenameRequest =
    { sectionKey : String
    , oldId : String
    , newId : String
    }


{-| 改名のインライン入力(draft 方式: Enter 確定 / Esc 破棄)。
error は直前の確定が拒まれた理由(赤枠の根拠)。
-}
type alias RenameState =
    { sectionKey : String
    , oldId : String
    , text : String
    , error : Maybe String
    }


{-| view のイベントが持ち回る「draft の素」。text だけが後から育つ。 -}
type alias DraftSeed =
    { path : List Seg
    , kind : DraftKind
    , original : String
    }


{-| 盤面の spawn 点をドラッグ中の状態。位置は design 座標で持ち、mousedown からの
クライアント座標の移動量(startX/Y 基準)で更新する。ratio・designH を掴んだ瞬間に
固めるのは、ドラッグ中に届く応答でプレビューが差し替わっても基準がずれないため。
-}
type alias DragState =
    { index : Int

    -- 掴んだ点の design 座標(rect 中心)
    , center : Plugins.Point

    -- mousedown のクライアント座標(移動量の基準)
    , startX : Float
    , startY : Float

    -- design px / 表示 px
    , ratio : Float
    , designH : Float

    -- 現在の design 座標(clamp 済み)
    , pos : Plugins.Point

    -- 動かしていない単なるクリック(選択)で同値の編集 2 本を流さないための印
    , moved : Bool
    }


{-| 幅を変えられるペイン。中央は残り幅なのでつまみを持たない。 -}
type PaneSide
    = LeftPane
    | RightPane


{-| ペイン境界をドラッグ中の状態。startX/startW は mousedown の瞬間に固める —
移動量(dx)を掴んだ位置基準で足すことで、速いドラッグでもつまみを取り逃さない。
-}
type alias PaneDrag =
    { side : PaneSide
    , startX : Float
    , startW : Int
    }


{-| 盤面プレビュー(右ペイン最上部)。取り直しの往復中も前の絵を出したままにする
(ちらつき防止)ので、「表示中の物」と「往復中か(previewReq)」は別持ち。
-}
type PreviewState
    = PreviewNone
    | PreviewShowing Api.Preview
    | PreviewFailed String


-- 「+ 新しいファイル」ウィザード


type WizardStep
    = WizBasics
    | WizFields
    | WizConfirm


{-| 3 点セット書き込みの進行段階(持っている Int は封筒の id)。
順番はスキーマ → データ → project.json — 途中で失敗しても
「宣言だけあって実ファイルが無い」を作らないため。
-}
type WizardWrite
    = WizNotStarted
    | WizPutSchema Int
    | WizPutData Int
    | WizGetProject Int
    | WizEditProject Int
    | WizPutProject Int


type alias WizardState =
    { step : WizardStep
    , draft : Wizard.Draft
    , write : WizardWrite

    -- 途中失敗の文言(部分成功を隠さず、どの段で止まったかを見せる)
    , error : Maybe String
    }


emptyWizard : WizardState
emptyWizard =
    { step = WizBasics, draft = Wizard.emptyDraft, write = WizNotStarted, error = Nothing }


type alias Model =
    { screen : Screen

    -- 上部ナビ。ホームの中身は Journey が持つ(Main は配線だけ)
    , tab : Tab
    , journey : Journey.Model

    -- 見た目の自動検査(/journey/changes)。baking = エンジンが全場面を
    -- 描き出している最中(ホームに実況を出す)。available = False は
    -- この口を持たないサーバ(404)の印 — 実況もモーダルも出さない(fail-open)
    , changesBaking : Bool
    , changesAvailable : Bool

    -- 「前と今」の見比べモーダル(Nothing = 閉じている)
    , changesModal : Maybe ChangesModal

    -- 「全場面を見る」モーダル(Nothing = 閉じている)
    , scenes : Maybe Scenes

    -- アトリエの「候補選び」(swap)。調整(Doc エディタ)は従来どおり Main が持つ
    , atelier : Atelier.Model

    -- ミニプレイヤー(「編集の今」を映す右下の枠。アトリエタブの間だけ出す)。
    -- open は折りたたみ(セッション中だけ記憶)。pin はピン留め中の場面
    -- (Nothing = 自動 — 知らせの最新を追う)。miniScenes は場面チップの材料
    -- (/gallery/list)、miniChanges は知らせの列の写し(自動追従と v= の種)。
    -- miniRefresh は絵のキャッシュ破りの目盛り(描き直しが終わるたび進む)、
    -- miniSwapNotice は「✓ 差し替わりました」の点灯(2 秒で消える)。
    -- miniZoom は絵の拡大表示(開いた時の場面名。Nothing = 閉じている)
    , miniPlayerOpen : Bool
    , miniPin : Maybe String
    , miniScenes : List String
    , miniChanges : List Journey.Change
    , miniRefresh : Int
    , miniSwapNotice : Bool
    , miniZoom : Maybe String

    -- 画像・音の URL の付け根(vite dev では別オリジンのサーバ)。JS が起動時に
    -- 一方向の封筒(kind serverBase)で知らせる。届くまでは同一オリジン扱い("")
    , serverBase : String
    , reqCounter : Int
    , notice : Maybe String

    -- トースト通知(数秒で消える notice)の連番。消灯タイマーが発火した時に
    -- 「まだ自分が出した通知のままか」を確かめる合鍵
    , noticeSeq : Int
    , picker : PickerState

    -- 「＋ 新しいゲームをはじめる」(ピッカー画面のまっさら開始)
    , newGame : NewGame.Model
    , title : String
    , root : String
    , files : List String

    -- project.json の "editor" 宣言に一致したグループ(/resources)。一覧の先頭に
    -- 見出し付きで出し、開いたファイルのプラグイン・スキーマパスもここから引く。
    -- /files の応答と別持ちなのは、後着の /files 応答に上書きされないため
    , groups : List Api.ResourceGroup

    -- ダッシュボード宣言(/resources)と、開いているボード(Nothing = 閉)
    , dashboards : List Api.Dashboard
    , dashboard : Maybe DashState

    -- /resources が添える宣言と実ファイルのずれ(壊れ JSON 等)。左レールに出す
    , resourceWarnings : List String

    -- ダッシュボードからのジャンプで「開けたら選ぶ」エントリ。開く往復(loadReq)の
    -- 応答が届いた足で消費する — 選択はファイルが実際に開けた時だけ意味を持つ
    , pendingJump : Maybe Jump
    , current : Maybe String
    , docText : String

    -- docText を解いた写し(と、ドット絵ならその読み取り結果)。current/docText を
    -- 差し替える唯一の口 withDoc がここも一緒に作り直すので陳腐化はしない
    , docValue : Maybe D.Value
    , spriteDoc : Maybe PixelEditor.Doc

    -- 開いた時(または保存成功時)のテキスト。dirty 判定はこれと比べる。
    , openedText : String
    , dirty : Bool

    -- 開いた時(または保存成功時)のディスク mtime。保存の ifMtime に添えて、
    -- 外部変更への上書きをサーバ側で 409 に弾いてもらう(GET で見比べる往復を持たない)
    , mtime : Maybe Int

    -- in-flight リクエストの id。応答の id と突き合わせて古い応答を捨てる
    , loadReq : Maybe Int
    , putReq : Maybe Int

    -- 保存ボタンを押した瞬間の本文(往復中に編集が進んでも、送るのはこれ)
    , savingText : Maybe String

    -- 保存が 409(外部変更)で弾かれた印(Just = 2 択ダイアログ表示中)。
    -- currentMtime は今ディスクに居る版 — 「構わず上書き」はこの版までしか潰さない
    , conflict : Maybe { currentMtime : Maybe Int }

    -- 開いているファイルがディスクで変わった印(Just = 今ディスクに居る版の mtime)。
    -- 保存時の 409 は最後の砦で、こちらは「編集を始める前に気付く」ための見張り。
    -- 打ちかけが無ければ黙って読み直し、あるときだけ帯を出して選ばせる
    , staleMtime : Maybe Int

    -- スキーマ駆動フォーム(右ペイン)
    , schemaState : SchemaState
    , schemaReq : Maybe Int
    , sectionKey : Maybe String
    , entrySel : Maybe EntrySel

    -- テーブルビューの並べ替え・絞り込み。表示だけに効く(選択・書き戻しは
    -- rowId が論理の指し先を持ち回るので、この 2 つが何であってもずれない)
    , tableSort : Maybe Table.SortState
    , tableFilter : String

    -- 開いている説明書き("?" の押された欄・セクション)。中身は開いている間だけ
    -- 画面に置く — 説明の長い文書でも、閉じている分は描く仕事が要らない
    , helpOpen : Set String

    -- 問題パネルの開閉。問題一覧そのものはモデルに持たず view で毎回計算する —
    -- docText が変わるたびの再計算が構造で保証され、陳腐化のしようがない
    , problemsOpen : Bool

    -- フォーカス中フィールドの打ちかけ(Nothing = 打ちかけ無し)
    , activeDraft : Maybe ActiveDraft

    -- 右の JSON ペインを開いているか(つまみ系 Doc の 2 ペインでだけ効く)。
    -- 端末に覚える — 「常に JSON が邪魔」な人に毎回畳ませない
    , jsonPaneOpen : Bool

    -- いま鳴らしている音(Nothing = 鳴っていない)。止める札を出すためだけ
    , playingSound : Maybe String

    -- project.json が宣言している音の名前(/resources が一緒に返す)。
    -- ▶ を出してよいのはこの列にある名前だけ
    , sounds : List String

    -- 横断検索・置換のパネルと、置換の進行(開いていないファイルへの直列書き戻し)
    , search : SearchView.Model
    , crossEdit : Maybe CrossEdit.Run

    -- 検索から飛んだ先で、届いた後に画面を送る欄(送ったら消す)
    , scrollTarget : Maybe (List Seg)

    -- 一覧の行の右クリックメニュー(開いている場所と対象)と、その場の名前変更
    , fileMenu : Maybe { anchor : ContextMenu.Anchor, path : String }
    , fileRename : Maybe { path : String, text : String }

    -- ファイルそのものへの動詞(新規 / 複製 / 改名 / 削除)の聞き取り中の物。
    -- skelReq は「新規」で骨格を組むためのスキーマ待ち(id と作る先)
    , fileVerb : Maybe FileVerbs.Dialog
    , skelReq : Maybe { id : Int, path : String }

    -- 動詞が通った後の行き先。open=True は作った / 名前を変えたファイルを開く、
    -- False は消したファイル(開いていたなら閉じる)
    , verbTarget : Maybe { path : String, open : Bool }

    -- 元に戻す / やり直すの履歴(編集はすべて queueEdit を通るので、そこで積む)。
    -- editSeq は「1 回のやり取り」の通し番号 — 打ちかけ・ドラッグの間に出る
    -- 編集の洪水を 1 手に畳む鍵にする
    , history : EditHistory.History
    , editSeq : Int

    -- 文書編集(docEdit ポート)の in-flight id と、反映待ちの列。
    -- 往復中に来た編集を古い本文へ重ねると先の編集が消えるので、直列に流す。
    -- Maybe(最新 1 件)でなく List なのは、ドラッグ確定が atX・y の 2 本組で
    -- 片方を落とせないため(同じ path 同士だけ最新で畳む)
    , editReq : Maybe Int
    , pendingEdits : List EditPayload

    -- 盤面プレビューの in-flight id と「往復中に文書が進んだ」印。
    -- リクエスト本体は送る瞬間の docText から導くので、覚えるのは印だけで足りる
    , preview : PreviewState
    , previewReq : Maybe Int
    , previewStale : Bool

    -- 盤面の spawn 点をドラッグ中(Nothing = していない)。ドラッグ中は
    -- docEdit を出さない — 焼き直しは mouseup 後の反映 1 回に任せる
    , drag : Maybe DragState

    -- ドット絵の手直し(*.sprite.json のビジュアル編集)。文書の正本は持たず、
    -- 道具・選択・一筆の途中だけ(書き戻しは既存の編集直列に乗せる)
    , pixel : PixelEditor.Model
    , sfx : SfxEditor.Model

    -- 焼き係を温め始めたか(1 つの文書につき 1 回だけ頼む)
    , sfxWarmed : Bool

    -- 準備できるまで様子を見に行っている最中か(見に行く先が preview でなく warm になる)
    , sfxWarming : Bool

    -- 焼き上がり待ち。走っているゲームが保存を見て焼き直すのを少し待ってから、
    -- 絵を取り直して鳴らす(焼き上がりを知らせてくれる口が無いため)
    , sfxWaitSeq : Int
    , sfxWaitLeft : Int

    -- 焼き上がりを待たせている再生(待ち終えた拍に鳴らす)
    , sfxPendingPlay : Maybe { name : String, loop : Bool }

    -- ドット絵 legend の実色表(サーバの POST /sprite/colors が返す「値 → #rrggbb」と
    -- 解けなかった値)。開いている sprite Doc の分だけ持ち、届くまで・解けないキーは仮色に倒す
    , spriteColors : Api.SpriteColors
    , spriteColorsReq : Maybe Int

    -- マップ(*.map.json のビジュアル編集)。持ち方はドット絵と同じ
    , mapEd : MapEditor.Model

    -- 「+ 新しいファイル」ウィザード(Just = 3 ペインの代わりに表示中)
    , wizard : Maybe WizardState

    -- 使用箇所一覧を開いている id("セクション/id"。Nothing = 閉じている)
    , usagesOpenFor : Maybe String

    -- catalog id 改名のインライン入力(Nothing = していない)
    , rename : Maybe RenameState

    -- 送信済みの改名(応答が来たら選択をこの newId へ追従させる)。
    -- refCount は完了文言用の参照書き換え数(送った瞬間に固める)
    , renameInflight : Maybe { req : RenameRequest, refCount : Int }

    -- 編集往復中に確定された改名。編集列は送る瞬間の最新本文から導出し直すので、
    -- ここには意図(どの id を何に)だけを覚える
    , pendingRename : Maybe RenameRequest

    -- dirty のまま移動(ファイル切替・プロジェクト切替・ウィザード)しようと
    -- した先(Just = 破棄確認ダイアログ表示中)。黙って編集を捨てないための関所
    , pendingNav : Maybe NavTarget

    -- 作業モードの選択。実際に出すモードは effectiveMode(スキーマ無し等で
    -- ビジュアルが組めない間はコードへ落ちる)が決める
    , viewMode : ViewMode

    -- catalog エントリ追加の id 入力(Nothing = 閉じている)
    , addDialog : Maybe AddDialogState

    -- 使用中エントリの削除確認(Nothing = 閉じている)
    , deleteConfirm : Maybe DeleteConfirmState

    -- texture 欄の候補(project.json の textures manifest 名)。texture 欄を持つ
    -- スキーマが届いた時だけ取りに行く(texturesReq = 往復中の印)
    , textures : List String
    , texturesReq : Maybe Int

    -- weights の行追加入力(Nothing = 閉じている)
    , weightsAdd : Maybe WeightsAddState

    -- 横断辞書: どのファイル向けに(crossFor)何を読み込んだか。ref の参照先が
    -- 別ファイルに住む時のフォーム候補・lint・逆参照・改名の素
    , crossFor : Maybe String
    , crossSlots : List CrossSlot

    -- 肖像(ui.json → エンジン焼き PNG)のキャッシュ。キーはプロジェクト相対パス
    , portraits : Dict.Dict String PortraitState
    , portraitReqs : Dict.Dict Int String

    -- 改名が他ファイルに及ぶ時の確認と進行
    , crossRename : Maybe CrossRenamePlan
    , crossRun : Maybe CrossRenameRun

    -- 左レール・右ペインの幅(px)。ドラッグで変え、localStorage(JS 側)に
    -- 覚えて次回起動時に復元する
    , leftPaneW : Int
    , rightPaneW : Int
    , paneDrag : Maybe PaneDrag

    -- ライブ反映(編集を debounce 自動保存し、走るゲームの watchFile に拾わせる)。
    -- autosaveSeq は編集のたびに進む予約番号(最新の予約だけが保存する = debounce)。
    -- lastSaveWasAuto は保存成功トーストを黙らせる印(自動保存の連打をうるさくしない)
    , liveSave : Bool
    , autosaveSeq : Int
    , lastSaveWasAuto : Bool

    -- 「いま画面に出ている Doc」(ゲームが書く debug/active-docs.json)。
    -- グループ id → 表示中パス列。ファイルが無い/読めない間は空(何も出さない)
    , activeDocs : Dict.Dict String (List String)
    , activeReq : Maybe Int
    }


{-| weights の行追加のインライン入力(Enter 確定 / Esc 破棄の draft 方式)。
error は直前の確定が拒まれた理由(赤枠の根拠)。
-}
type alias WeightsAddState =
    { path : List Seg
    , text : String
    , error : Maybe String
    }


{-| 見比べモーダル。開いた瞬間は Loading(押した足でモーダルを出す)、
知らせが届いたら 1 場面ずつ歩く。remaining の先頭がいま見せている場面。
見るだけで承認は無い — 基準はサーバが既に追随させている。
-}
type ChangesModal
    = ChangesLoading
    | ChangesReady { remaining : List Journey.Change, total : Int }


{-| 「全場面を見る」モーダル。gallery/ の一覧(読むだけ)。 -}
type Scenes
    = ScenesLoading
    | ScenesReady (List String)


init : () -> ( Model, Effect )
init _ =
    request "health" (E.object [])
        { screen = Booting
        , tab = HomeTab
        , journey = Journey.init
        , changesBaking = False
        , changesAvailable = True
        , changesModal = Nothing
        , scenes = Nothing
        , atelier = Atelier.init
        , miniPlayerOpen = False
        , miniPin = Nothing
        , miniScenes = []
        , miniChanges = []
        , miniRefresh = 0
        , miniSwapNotice = False
        , miniZoom = Nothing
        , serverBase = ""
        , reqCounter = 0
        , notice = Nothing
        , noticeSeq = 0
        , picker = emptyPicker
        , newGame = NewGame.init
        , title = ""
        , root = ""
        , files = []
        , groups = []
        , dashboards = []
        , dashboard = Nothing
        , resourceWarnings = []
        , pendingJump = Nothing
        , current = Nothing
        , docText = ""
        , docValue = Nothing
        , spriteDoc = Nothing
        , openedText = ""
        , dirty = False
        , mtime = Nothing
        , loadReq = Nothing
        , putReq = Nothing
        , savingText = Nothing
        , conflict = Nothing
        , staleMtime = Nothing
        , schemaState = SchemaNone
        , schemaReq = Nothing
        , sectionKey = Nothing
        , entrySel = Nothing
        , tableSort = Nothing
        , tableFilter = ""
        , helpOpen = Set.empty
        , problemsOpen = False
        , activeDraft = Nothing
        , jsonPaneOpen = True
        , playingSound = Nothing
        , sounds = []
        , search = SearchView.init
        , crossEdit = Nothing
        , scrollTarget = Nothing
        , fileMenu = Nothing
        , fileRename = Nothing
        , fileVerb = Nothing
        , skelReq = Nothing
        , verbTarget = Nothing
        , history = EditHistory.empty
        , editSeq = 0
        , editReq = Nothing
        , pendingEdits = []
        , preview = PreviewNone
        , previewReq = Nothing
        , previewStale = False
        , drag = Nothing
        , pixel = PixelEditor.init
        , sfx = SfxEditor.init
        , sfxWarmed = False
        , sfxWarming = False
        , sfxWaitSeq = 0
        , sfxWaitLeft = 0
        , sfxPendingPlay = Nothing
        , spriteColors = Api.noSpriteColors
        , spriteColorsReq = Nothing
        , mapEd = MapEditor.init
        , wizard = Nothing
        , usagesOpenFor = Nothing
        , rename = Nothing
        , renameInflight = Nothing
        , pendingRename = Nothing
        , pendingNav = Nothing
        , viewMode = VisualMode
        , addDialog = Nothing
        , deleteConfirm = Nothing
        , textures = []
        , texturesReq = Nothing
        , weightsAdd = Nothing
        , crossFor = Nothing
        , crossSlots = []
        , portraits = Dict.empty
        , portraitReqs = Dict.empty
        , crossRename = Nothing
        , crossRun = Nothing
        , leftPaneW = 240
        , rightPaneW = 320
        , paneDrag = Nothing
        , liveSave = False
        , autosaveSeq = 0
        , lastSaveWasAuto = False
        , activeDocs = Dict.empty
        , activeReq = Nothing
        }



-- メッセージ


type Msg
    = GotApiResponse D.Value
    | RetryClicked
    | PickerInput String
    | ProjectClicked String
    | OpenPathClicked
    | FileClicked String
    | DashboardClicked String
    | DashboardClosed
    | DashEntrySelected String
    | DashJumped Jump
    | DocChanged String
    | SaveClicked
    | ReloadChosen
    | OverwriteChosen
    | NavDiscarded
    | NavStayed
    | ModeChosen ViewMode
    | SectionClicked String
    | EntryClicked EntrySel
    | AddClicked
    | AddIdTyped String
    | AddConfirmed
    | AddCancelled
    | DuplicateClicked
    | DeleteClicked
    | DeleteConfirmed
    | DeleteCancelled
    | SortClicked String
    | FilterChanged String
    | HelpToggled String
    | SearchToggled
    | SearchClosed
    | SearchTyped String
    | SearchDebounced Int
    | SearchReplacementTyped String
    | JsonPaneToggled
    | SoundPlayClicked String
    | SoundStopClicked
    | SearchMoved Int
    | SearchActivated
    | SearchFileClicked String
    | SearchHitClicked Api.SearchHit
    | ReplaceRunClicked
    | FileMenuOpened String ContextMenu.Anchor
    | FileMenuClosed
    | FileRenameStarted String
    | FileRenameTyped String
    | FileRenameCommitted
    | FileRenameCancelled
    | FileNewClicked Api.ResourceGroup
    | FileDuplicateClicked String
    | FileDeleteClicked String
    | VerbTyped String
    | VerbConfirmed
    | VerbCancelled
    | UndoPressed
    | RedoPressed
    | RowOpClicked String EntryOps.RowEdit
    | RowDeleteClicked String Int
    | ProblemBarToggled
    | ProblemClicked Lint.Problem
    | FieldEdited EditPayload
    | EditsQueued (List EditPayload)
    | PixelMsg PixelEditor.Msg
    | SfxMsg SfxEditor.Msg
    | SfxWaitTick Int
    | MapMsg MapEditor.Msg
    | WeightsAddOpened (List Seg)
    | WeightsAddTyped String
    | WeightsAddCommitted
    | WeightsAddCancelled
    | UsagesToggled String
    | UsageJumped { sectionKey : String, entry : Maybe EntrySel }
    | RenameStarted { sectionKey : String, oldId : String }
    | RenameTyped String
    | RenameCommitted
    | RenameCancelled
    | CrossRenameConfirmed
    | CrossRenameCancelled
    | DraftStarted DraftSeed
    | DraftTyped DraftSeed String
    | DraftStepped DraftSeed { dir : Int, shift : Bool }
    | DraftCommitted { release : Bool }
    | DraftCancelled
    | PreviewClicked { x : Float, y : Float }
    | SpawnPressed { index : Int, center : Plugins.Point, clientX : Float, clientY : Float, clientW : Float }
    | DragMoved { x : Float, y : Float }
    | DragEnded
    | WizardOpened
    | WizardClosed
    | WizardStepChosen WizardStep
    | WizardIdChanged String
    | WizardTitleChanged String
    | WizardPathChanged String
    | WizardShapeChosen Wizard.Shape
    | WizardFieldChanged Int Wizard.FieldDraft
    | WizardFieldAdded
    | WizardFieldRemoved Int
    | WizardFieldMoved Int Int
    | WizardCreateClicked
    | NoticeExpired { seq : Int, message : String }
    | PanePressed PaneSide Float
    | PaneMoved Float
    | PaneReleased
    | LiveToggled
    | AutosaveFired Int
    | ActivePollTick
    | RunningGamesPollTick
    | TabClicked Tab
    | JourneyMsg Journey.Msg
    | ChangesPollTick
    | ReloadClicked
    | StaleDismissed
    | FileWatchTick
    | ChangesNextClicked
    | ChangesModalClosed
    | ScenesOpened
    | ScenesClosed
    | AtelierMsg Atelier.Msg
    | AtelierOverlayTick
    | AtelierBakePollTick
    | LaunchPollTick
    | RunnerPollTick
    | MiniPlayerToggled
    | MiniSceneClicked (Maybe String)
    | MiniStartClicked
    | MiniSwapNoticeExpired
    | MiniZoomOpened String
    | MiniZoomClosed
    | ProjectPickerOpened
    | BackToEditingClicked
    | NewGameMsg NewGame.Msg
    | ProjectNewPollTick
      -- フォーム対象外(JSON-Schema)の右ペインに出すアトリエプレビューの読み込み失敗
    | ForeignPreviewFailed



-- 更新


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        GotApiResponse value ->
            case D.decodeValue Api.envelopeDecoder value of
                Ok env ->
                    if env.ok then
                        handleOk env model

                    else
                        handleErr env model

                Err _ ->
                    ( { model | notice = Just "サーバ応答が読めませんでした" }, Effect.none )

        RetryClicked ->
            request "health" (E.object []) { model | screen = Booting }

        TabClicked tab ->
            gotoTab tab model

        JourneyMsg jmsg ->
            let
                ( journey, nav ) =
                    Journey.update jmsg model.journey

                m1 =
                    { model | journey = journey }
            in
            case nav of
                Just Journey.ToAtelier ->
                    -- アトリエ入口(3枚のカード)へ。提案からでもセクション直行は
                    -- しない — ホーム発の着地は入口に統一する
                    gotoTab AtelierTab m1

                Just Journey.ToLaunch ->
                    -- ホームに居たまま起動する(アトリエの起動と同じ経路に委譲 —
                    -- 二度押しの守りもそのまま効く)。進みは提案カードの下の実況が見せる
                    let
                        ( atelier, out ) =
                            Atelier.update Atelier.StartGameClicked m1.atelier

                        m2 =
                            { m1 | atelier = atelier }
                    in
                    case out of
                        Atelier.OutStartGame ->
                            request "gameStart" (E.object []) m2

                        _ ->
                            ( m2, Effect.none )

                Just Journey.ToArrange ->
                    -- アトリエ入口へ。3枚のカードから自分でやることを選んでもらう
                    gotoTab AtelierTab m1

                Just Journey.ToChanges ->
                    -- 押した足でモーダル(読み込み中)を出し、知らせを取りに行く
                    if m1.changesModal == Nothing then
                        ( { m1 | changesModal = Just ChangesLoading }, requestInfo "journeyChanges" )

                    else
                        ( m1, Effect.none )

                Just Journey.ToHome ->
                    gotoTab HomeTab m1

                Nothing ->
                    ( m1, Effect.none )

        ChangesPollTick ->
            -- ホームに居る間の定期便。呼ぶだけで検査が 1 目盛り進み、
            -- 描き出しの実況(baking)も乗ってくる
            ( model, requestInfo "journeyChanges" )

        ChangesNextClicked ->
            case model.changesModal of
                Just (ChangesReady info) ->
                    ( { model
                        | changesModal =
                            Just (ChangesReady { info | remaining = List.drop 1 info.remaining })
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ChangesModalClosed ->
            -- 見終わった時だけ既読を送る(読み込み中に閉じたなら知らせは立ったまま)
            case model.changesModal of
                Just (ChangesReady _) ->
                    ( { model | changesModal = Nothing }, requestInfo "journeyChangesSeen" )

                _ ->
                    ( { model | changesModal = Nothing }, Effect.none )

        ScenesOpened ->
            ( { model | scenes = Just ScenesLoading }, requestInfo "galleryList" )

        ScenesClosed ->
            ( { model | scenes = Nothing }, Effect.none )

        AtelierMsg amsg ->
            let
                ( atelier, out ) =
                    Atelier.update amsg model.atelier

                m1 =
                    { model | atelier = atelier }
            in
            case out of
                Atelier.OutNone ->
                    ( m1, Effect.none )

                Atelier.OutPromote info ->
                    request "promoteCandidate"
                        (E.object [ ( "candidate", E.string info.candidate ), ( "slot", E.string info.slot ) ])
                        m1

                Atelier.OutStartGame ->
                    request "gameStart" (E.object []) m1

                Atelier.OutToast message ->
                    showToast message m1

                Atelier.OutFetchPrompt info ->
                    request "promptAtelier"
                        (E.object
                            [ ( "slot", E.string info.slot )
                            , ( "count", E.int info.count )
                            , ( "direction", E.string info.direction )
                            ]
                        )
                        m1

                Atelier.OutFetchExtendPrompt kind ->
                    -- 「ゲームを広げる」の依頼文の下書き(genesisPrompt と同じ流儀)
                    request "promptExtend"
                        (E.object [ ( "kind", E.string kind ) ])
                        m1

                Atelier.OutCopyPrompt prompt ->
                    -- クリップボードへ(JS 側で解決するローカルな封筒)
                    request "copyClipboard" (E.object [ ( "text", E.string prompt ) ]) m1

                Atelier.OutCopyFile info ->
                    request "atelierCopy"
                        (E.object [ ( "slot", E.string info.slot ), ( "name", E.string info.name ) ])
                        m1

                Atelier.OutEditFile file ->
                    -- 候補カードの「✏️ 手直し」— 調整(エディタ)でそのまま開く
                    openFile file m1

                Atelier.OutArchive candidate ->
                    -- 候補カードの 🗃️ — アーカイブへ送る(消さない)
                    request "atelierArchiveAdd"
                        (E.object [ ( "candidate", E.string candidate ) ])
                        m1

                Atelier.OutRestore file ->
                    -- アーカイブの「↩ 候補に戻す」— 候補の列へ戻す
                    request "atelierRestore"
                        (E.object [ ( "file", E.string file ) ])
                        m1

                Atelier.OutClosed ->
                    -- 採用の祝いを閉じた。世界が変わったので候補と提案を取り直す。
                    -- 見た目の検査は裏で自動に進み、変わればホームに知らせが立つ
                    let
                        ( m2, toastFx ) =
                            showToast "見た目が変わると、ホームに知らせが届きます" m1
                    in
                    ( m2
                    , Effect.batch
                        [ toastFx
                        , requestInfo "atelierCandidates"

                        -- 切り替えの前のバージョン(retired)が archive に積まれた —
                        -- アーカイブのバッジ件数も追随させる
                        , requestInfo "atelierArchive"
                        , requestInfo "gameStatus"
                        , requestInfo "journeyState"
                        ]
                    )

        AtelierOverlayTick ->
            -- オーバーレイの段送り待ちだけ購読が生きている(needsTick が判定)
            let
                ( atelier, _ ) =
                    Atelier.update Atelier.OverlayTick model.atelier
            in
            ( { model | atelier = atelier }, Effect.none )

        AtelierBakePollTick ->
            -- サーバがプレビューを焼いている間だけ購読が生きている(Atelier.isBaking
            -- が判定)。baking=false が届いた取得がそのまま「最終の取り直し」になる
            ( model, requestInfo "atelierCandidates" )

        LaunchPollTick ->
            -- ゲーム起動待ちの間だけ購読が生きている(isLaunchPolling が判定)。
            -- ログ(進捗)と状態(起動完了)を同じ拍で追う
            ( model, Effect.batch [ requestInfo "gameLog", requestInfo "gameStatus" ] )

        RunnerPollTick ->
            -- プレビューの描き出し中だけ購読が生きている(subscriptions が
            -- Atelier.isBaking で判定)。進捗パネルのログ末尾の材料
            ( model, requestInfo "runnerLog" )

        MiniPlayerToggled ->
            ( { model | miniPlayerOpen = not model.miniPlayerOpen }, Effect.none )

        MiniSceneClicked pin ->
            -- Just = チップでピン留め、Nothing = 「自動」で追従に戻る
            ( { model | miniPin = pin }, Effect.none )

        MiniStartClicked ->
            -- ミニプレイヤー下段の起動。アトリエの起動と同じ経路に委譲する —
            -- 二度押しの守り(LaunchStarting / LaunchRunning は黙る)もそのまま効く
            let
                ( atelier, out ) =
                    Atelier.update Atelier.StartGameClicked model.atelier

                m1 =
                    { model | atelier = atelier }
            in
            case out of
                Atelier.OutStartGame ->
                    request "gameStart" (E.object []) m1

                _ ->
                    ( m1, Effect.none )

        MiniSwapNoticeExpired ->
            ( { model | miniSwapNotice = False }, Effect.none )

        MiniZoomOpened name ->
            -- 開いた時の場面名を覚える — 拡大中に自動追従が進んでも
            -- 別場面へ勝手に切り替えない(差し替えは URL の v が進むので映る)
            ( { model | miniZoom = Just name }, Effect.none )

        MiniZoomClosed ->
            ( { model | miniZoom = Nothing }, Effect.none )

        ProjectPickerOpened ->
            -- 上のバーの右端から選択画面へ。編集状態(model)は捨てない —
            -- 「← いまのゲームに戻る」の戻り道と、実際に別プロジェクトを
            -- 選んだ時の dirty 関所(selectProject)がそのまま生きるため。
            -- 封筒は起動時の未選択フォールバックと同じ 2 通
            let
                ( m1, c1 ) =
                    request "projects" (E.object []) { model | screen = Picker }

                ( m2, c2 ) =
                    request "runningGames" (E.object []) m1
            in
            ( m2, Effect.batch [ c1, c2 ] )

        BackToEditingClicked ->
            -- 何も再読み込みしない — 開いていた編集へそのまま戻るだけ
            ( { model | screen = Editing }, Effect.none )

        NewGameMsg nmsg ->
            let
                ( newGame, out ) =
                    NewGame.update nmsg model.newGame

                m1 =
                    { model | newGame = newGame }
            in
            case out of
                NewGame.OutNone ->
                    ( m1, Effect.none )

                NewGame.OutCreate info ->
                    request "projectNew"
                        (E.object
                            [ ( "name", E.string info.name )
                            , ( "title", E.string info.title )
                            , ( "w", E.int info.w )
                            , ( "h", E.int info.h )

                            -- 選んだジャンルの公式テンプレート(空 = 既定の複製元)
                            , ( "starter", E.string info.starter )
                            ]
                        )
                        m1

                NewGame.OutFetchFamilies ->
                    -- ジャンル一覧(人気順)。404(旧サーバ)はプリセット入力に倒れる
                    ( m1, requestInfo "genesisFamilies" )

                NewGame.OutFetchGenesisPrompt info ->
                    request "promptGenesis"
                        (E.object
                            [ ( "family", E.string info.family )
                            , ( "direction", E.string info.direction )
                            ]
                        )
                        m1

                NewGame.OutCopyPrompt prompt ->
                    -- クリップボードへ(JS 側で解決するローカルな封筒)
                    request "copyClipboard" (E.object [ ( "text", E.string prompt ) ]) m1

        ProjectNewPollTick ->
            -- ひな形づくりを待つ間だけ購読が生きている(NewGame.isPolling が判定)
            ( model, requestInfo "projectNewLog" )

        ForeignPreviewFailed ->
            -- プレビュー未焼成(404)。文言に切り替えるだけ(致命ではない)
            case model.schemaState of
                SchemaForeign _ ->
                    ( { model | schemaState = SchemaForeign { previewBroken = True } }, Effect.none )

                _ ->
                    ( model, Effect.none )

        PickerInput text_ ->
            ( { model | picker = updatePicker (\p -> { p | input = text_ }) model }, Effect.none )

        ProjectClicked dir ->
            selectProject dir model

        OpenPathClicked ->
            let
                dir =
                    String.trim model.picker.input
            in
            if dir == "" then
                ( model, Effect.none )

            else
                selectProject dir model

        FileClicked path ->
            if model.dirty then
                ( { model | pendingNav = Just (NavFile path) }, Effect.none )

            else
                openFile path model

        DashboardClicked id ->
            -- 開くだけなら編集は消えない(current・docText は据え置き)ので関所は不要
            case model.dashboards |> List.filter (\d -> d.id == id) |> List.head of
                Just decl ->
                    openDashboard decl model

                Nothing ->
                    ( model, Effect.none )

        DashboardClosed ->
            ( { model | dashboard = Nothing }, Effect.none )

        DashEntrySelected id ->
            ( { model | dashboard = model.dashboard |> Maybe.map (\d -> { d | selected = Just id }) }
            , Effect.none
            )

        DashJumped jump ->
            -- ジャンプはファイル切替なので、既存の関所(dirty 確認)に乗せる
            if model.dirty then
                ( { model | pendingNav = Just (NavJump jump) }, Effect.none )

            else
                openFileAt jump model

        NavDiscarded ->
            case model.pendingNav of
                Just target ->
                    let
                        -- 破棄=開いた時の本文へ戻す(ReloadChosen と同じ意味論)。
                        -- dirty を消さないと移動のやり直しが再びこの関所に阻まれる
                        m1 =
                            withDoc model.current model.openedText
                                { model
                                    | pendingNav = Nothing
                                    , dirty = False
                                    , activeDraft = Nothing
                                    , history = EditHistory.cutOnExternalChange model.history
                                }
                    in
                    case target of
                        NavFile path ->
                            openFile path m1

                        NavProject dir ->
                            selectProject dir m1

                        NavWizard ->
                            ( { m1 | wizard = Just emptyWizard }, Effect.none )

                        NavJump jump ->
                            openFileAt jump m1

                Nothing ->
                    ( model, Effect.none )

        NavStayed ->
            ( { model | pendingNav = Nothing }, Effect.none )

        DocChanged text_ ->
            -- テキストを手で書き換えたら、控えてある旧値はもう今の文書のものではない
            scheduleAutosave
                (requestPreview
                    (withDoc model.current
                        text_
                        { model
                            | dirty = text_ /= model.openedText
                            , history = EditHistory.cutOnExternalChange model.history
                        }
                    )
                )

        SaveClicked ->
            case ( model.current, model.savingText ) of
                -- 保存中の再クリックは無視(往復が終わってから)
                ( Just _, Nothing ) ->
                    -- 鮮度はサーバの ifMtime 判定に任せる(外部変更なら 409 が返る)
                    sendPut model.mtime
                        { model | savingText = Just model.docText, notice = Nothing, lastSaveWasAuto = False }

                _ ->
                    ( model, Effect.none )

        ReloadChosen ->
            -- 409 応答はサーバ側の本文を含まないので、開き直しの getFile で取り直す
            reloadCurrent model

        ReloadClicked ->
            -- 手で読み直す。ディスクの版を素直に取り直すだけ(打ちかけは捨てる)
            reloadCurrent model

        StaleDismissed ->
            -- 「このまま続ける」。帯は畳むが、保存は ifMtime で 409 に弾かれる
            ( { model | staleMtime = Nothing }, Effect.none )

        FileWatchTick ->
            -- 開いている間だけの見張り。全ファイルの mtime を 1 回で貰う
            ( model, requestInfo "changes" )

        OverwriteChosen ->
            -- 409 が伝えた「今ディスクに居る版」を ifMtime に使う — ダイアログ表示中に
            -- さらに外で変わっていれば、もう一度 409 で止まる(黙って潰さない)
            sendPut (model.conflict |> Maybe.andThen .currentMtime) { model | conflict = Nothing }

        ModeChosen mode ->
            -- 打ちかけは欄ごと画面から消え得るので、確定せず破棄する(文書は無傷)。
            -- ドット絵の作業コピーも手放す — テキスト側で文書が動き得るため。
            -- ビジュアルへ戻る時は legend 実色表も取り直す(テキスト側で legend や
            -- テーマが変わっていても、古い色で描き続けない)
            let
                m1 =
                    { model | viewMode = mode, activeDraft = Nothing, pixel = PixelEditor.release model.pixel, mapEd = MapEditor.release model.mapEd }
            in
            if mode == VisualMode then
                requestSpriteColors m1

            else
                ( m1, Effect.none )

        SectionClicked key ->
            -- 並べ替え・絞り込みは列の意味ごとセクションに紐づくので持ち越さない
            warmThenShape
                { model
                    | sectionKey = Just key
                    , entrySel = Nothing
                    , tableSort = Nothing
                    , tableFilter = ""
                    , usagesOpenFor = Nothing
                    , rename = Nothing
                }

        SearchToggled ->
            ( { model
                | search =
                    if SearchView.isOpen model.search then
                        SearchView.close model.search

                    else
                        SearchView.open model.search
              }
            , Effect.none
            )

        SearchClosed ->
            ( { model | search = SearchView.close model.search }, Effect.none )

        SearchTyped text_ ->
            let
                search =
                    SearchView.typedQuery text_ model.search
            in
            ( { model | search = search }
            , if text_ == "" then
                Effect.none

              else
                Effect.SearchDebounce { seq = search.seq, afterMs = 150 }
            )

        SearchDebounced seq ->
            -- 打っている間に追い越された予約は捨てる(最後の 1 回だけが探す)
            if seq == model.search.seq && model.search.query /= "" then
                request "search" (E.object [ ( "q", E.string model.search.query ) ]) model

            else
                ( model, Effect.none )

        SearchReplacementTyped text_ ->
            ( { model | search = SearchView.typedReplacement text_ model.search }, Effect.none )

        JsonPaneToggled ->
            savePrefs { model | jsonPaneOpen = not model.jsonPaneOpen }

        SoundPlayClicked name ->
            -- 素の WAV をそのまま鳴らす(master のつまみは掛からない)
            request "playSound"
                (E.object [ ( "name", E.string (name ++ ".wav") ), ( "loop", E.bool False ) ])
                { model | playingSound = Just name }

        SoundStopClicked ->
            request "stopSound" (E.object []) { model | playingSound = Nothing }

        SearchMoved dir ->
            let
                search =
                    SearchView.moveSelection dir model.search
            in
            -- 選んだ行が一覧の外なら、そこまで送る(端まで来ていれば何も動かない)
            request "scrollTo"
                (E.object
                    [ ( "id", E.string (SearchView.selectedDomId search) )
                    , ( "block", E.string "nearest" )
                    , ( "flash", E.bool False )
                    ]
                )
                { model | search = search }

        SearchActivated ->
            case SearchView.activeRow model.search of
                Just (SearchView.FileRow path) ->
                    update (SearchFileClicked path) model

                Just (SearchView.HitRow hit) ->
                    jumpToHit hit model

                Nothing ->
                    ( model, Effect.none )

        SearchFileClicked path ->
            -- ファイル名の当たり: そのファイルを開くだけ(飛ぶ欄は無い)
            update (FileClicked path)
                (toEditorScreen { model | search = SearchView.close model.search })

        SearchHitClicked hit ->
            jumpToHit hit model

        ReplaceRunClicked ->
            startReplace model

        FileNewClicked group ->
            ( { model
                | fileVerb =
                    Just
                        (FileVerbs.forNew
                            { groupId = group.id
                            , groupLabel = Maybe.withDefault group.id group.title
                            , pattern = group.pattern
                            , schemaPath = groupSchemaPath group
                            }
                        )
              }
            , Effect.none
            )

        FileDuplicateClicked path ->
            ( { model | fileMenu = Nothing, fileVerb = Just (FileVerbs.forDuplicate (patternFor model path) path) }
            , Effect.none
            )

        FileDeleteClicked path ->
            ( { model | fileMenu = Nothing, fileVerb = Just (FileVerbs.forDelete path) }, Effect.none )

        FileMenuOpened path anchor ->
            ( { model | fileMenu = Just { anchor = anchor, path = path } }, Effect.none )

        FileMenuClosed ->
            ( { model | fileMenu = Nothing }, Effect.none )

        FileRenameStarted path ->
            -- その場編集(IDE の F2)。初期値は宣言の飾りを外した名前だけ。
            -- 欄が描かれてからカーソルを置きたいので、focus は頼み事として出す
            request "focusId"
                (E.object [ ( "id", E.string fileRenameBoxId ) ])
                { model
                    | fileMenu = Nothing
                    , fileRename = Just { path = path, text = Skeleton.bareNameOf (patternFor model path) path }
                }

        FileRenameTyped text_ ->
            ( { model | fileRename = model.fileRename |> Maybe.map (\r -> { r | text = text_ }) }, Effect.none )

        FileRenameCommitted ->
            commitFileRename model

        FileRenameCancelled ->
            ( { model | fileRename = Nothing }, Effect.none )

        VerbTyped text_ ->
            -- 打ち直しで前の断りを引きずらない(理由は次の確定で判定し直す)
            ( { model | fileVerb = model.fileVerb |> Maybe.map (\d -> { d | text = text_, error = Nothing }) }
            , Effect.none
            )

        VerbConfirmed ->
            confirmVerb model

        VerbCancelled ->
            ( { model | fileVerb = Nothing }, Effect.none )

        UndoPressed ->
            stepHistory EditHistory.undo model

        RedoPressed ->
            stepHistory EditHistory.redo model

        HelpToggled key ->
            ( { model | helpOpen = toggleMember key model.helpOpen }, Effect.none )

        RowOpClicked sectionKey edit ->
            rowOp sectionKey edit model

        RowDeleteClicked sectionKey index ->
            queueOp (EntryOps.deleteOp sectionKey (Refs.AtIndex index))
                { model | entrySel = Nothing }

        AddClicked ->
            addEntry model

        AddIdTyped text_ ->
            ( { model
                | addDialog =
                    model.addDialog
                        -- 打ち直しで赤枠を引きずらない(理由は次の確定で判定し直す)
                        |> Maybe.map (\d -> { d | text = text_, error = Nothing })
              }
            , Effect.none
            )

        AddConfirmed ->
            confirmAdd model

        AddCancelled ->
            ( { model | addDialog = Nothing }, Effect.none )

        DuplicateClicked ->
            duplicateEntry model

        DeleteClicked ->
            deleteEntry model

        DeleteConfirmed ->
            case model.deleteConfirm of
                Just confirm ->
                    queueOp (EntryOps.deleteOp confirm.sectionKey (Selection.toRefsEntry confirm.entry))
                        { model
                            | deleteConfirm = Nothing
                            , entrySel = Nothing
                            , usagesOpenFor = Nothing
                        }

                Nothing ->
                    ( model, Effect.none )

        DeleteCancelled ->
            ( { model | deleteConfirm = Nothing }, Effect.none )

        EntryClicked sel ->
            -- 改名の打ちかけは選び直しで破棄(別エントリの id に化けさせない)
            ( { model | entrySel = Just sel, rename = Nothing, usagesOpenFor = Nothing }, Effect.none )

        SortClicked column ->
            ( { model | tableSort = Just (toggleSort column model.tableSort) }, Effect.none )

        FilterChanged text_ ->
            ( { model | tableFilter = text_ }, Effect.none )

        ProblemBarToggled ->
            ( { model | problemsOpen = not model.problemsOpen }, Effect.none )

        ProblemClicked problem ->
            -- 絞り込みは消す(ジャンプ先の行がフィルタで隠れていると迷子になる)
            ( { model
                | sectionKey = Just problem.sectionKey
                , entrySel = problem.entry |> Maybe.map targetToSel
                , tableFilter = ""
              }
            , Effect.none
            )

        FieldEdited payload ->
            queueEdit payload model

        EditsQueued payloads ->
            queueEdits payloads model

        SfxMsg smsg ->
            case sfxConfigOf model of
                Just config ->
                    let
                        ( sfx, out ) =
                            SfxEditor.update config smsg model.sfx

                        m1 =
                            { model | sfx = sfx }
                    in
                    case out of
                        SfxEditor.Silent ->
                            ( m1, Effect.none )

                        SfxEditor.Edited edit ->
                            -- 文書へは書くが、掴んでいる間は焼かない。
                            -- 途中で焼くと、実測から導く縁が指の下で飛び、音も動かすたびに
                            -- 鳴ってしまう。焼くのは指を離した拍（Play）に 1 回だけ。
                            let
                                ( m2, editFx ) =
                                    queueEdit (sfxPayload config edit) m1
                            in
                            if SfxEditor.isDragging m2.sfx then
                                ( m2, editFx )

                            else
                                let
                                    seq =
                                        m2.sfxWaitSeq + 1
                                in
                                ( { m2 | sfxWaitSeq = seq, sfxWaitLeft = 1, sfxPendingPlay = Nothing }
                                , Effect.batch
                                    [ editFx, Effect.Delayed { seq = seq, afterMs = 200 } ]
                                )

                        SfxEditor.Play info ->
                            requestPreview_ info config m1

                        SfxEditor.Stop ->
                            request "stopSound" (E.object []) { m1 | sfxPendingPlay = Nothing }

                Nothing ->
                    ( model, Effect.none )

        SfxWaitTick seq ->
            -- 焼き上がりを知らせてくれる口が無いので、少し置いてから絵を取り直し、
            -- 待たせていた再生をここで鳴らす。
            if seq /= model.sfxWaitSeq || model.sfxWaitLeft <= 0 then
                ( model, Effect.none )

            else
                if model.sfxWarming then
                    request "sfxWarm" (E.object [])
                        { model | sfxWaitLeft = model.sfxWaitLeft - 1 }

                else
                    case sfxConfigOf model of
                        Just config ->
                            requestPreview_
                                { name = config.sound
                                , loop = model.sfx.looping == Just config.sound
                                }
                                config
                                { model | sfxWaitLeft = model.sfxWaitLeft - 1 }

                        Nothing ->
                            ( model, Effect.none )

        PixelMsg pmsg ->
            case spriteDocCurrent model of
                Just pdoc ->
                    let
                        ( pixel, out ) =
                            PixelEditor.update pdoc pmsg model.pixel

                        m1 =
                            { model | pixel = pixel }
                    in
                    case out of
                        PixelEditor.Silent ->
                            ( m1, Effect.none )

                        PixelEditor.Edited edit ->
                            queueEdit (pixelPayload edit) m1

                        PixelEditor.Noticed message ->
                            showToast message m1

                Nothing ->
                    ( model, Effect.none )

        MapMsg mmsg ->
            case mapDocCurrent model of
                Just mdoc ->
                    let
                        ( mapEd, out ) =
                            MapEditor.update mdoc mmsg model.mapEd

                        m1 =
                            { model | mapEd = mapEd }
                    in
                    case out of
                        MapEditor.Silent ->
                            ( m1, Effect.none )

                        MapEditor.Edited edit ->
                            let
                                ( m2, editFx ) =
                                    queueEdits (mapPayloads m1 edit) (selectAddedEntry model edit m1)
                            in
                            -- 編集は通しつつ、気をつけたい形(部屋の行が 2 本目)は
                            -- 一言だけ添える(止めはしない — 規則はゲーム側の話)
                            case mapNotice edit of
                                Just message ->
                                    showToast message m2
                                        |> Tuple.mapSecond (\fx -> Effect.batch [ editFx, fx ])

                                Nothing ->
                                    ( m2, editFx )

                        MapEditor.Noticed message ->
                            showToast message m1

                Nothing ->
                    ( model, Effect.none )

        WeightsAddOpened path ->
            ( { model | weightsAdd = Just { path = path, text = "", error = Nothing } }, Effect.none )

        WeightsAddTyped text_ ->
            ( { model
                | weightsAdd =
                    model.weightsAdd
                        -- 打ち直しで赤枠を引きずらない(理由は次の確定で判定し直す)
                        |> Maybe.map (\w -> { w | text = text_, error = Nothing })
              }
            , Effect.none
            )

        WeightsAddCommitted ->
            commitWeightsAdd model

        WeightsAddCancelled ->
            ( { model | weightsAdd = Nothing }, Effect.none )

        UsagesToggled key ->
            ( { model
                | usagesOpenFor =
                    if model.usagesOpenFor == Just key then
                        Nothing

                    else
                        Just key
              }
            , Effect.none
            )

        UsageJumped target ->
            -- 問題ジャンプと同じ流儀: 絞り込みは消す(ジャンプ先が隠れていると迷子になる)
            ( { model
                | sectionKey = Just target.sectionKey
                , entrySel = target.entry
                , tableFilter = ""
                , usagesOpenFor = Nothing
              }
            , Effect.none
            )

        RenameStarted target ->
            ( { model
                | rename =
                    Just
                        { sectionKey = target.sectionKey
                        , oldId = target.oldId
                        , text = target.oldId
                        , error = Nothing
                        }
              }
            , Effect.none
            )

        RenameTyped text_ ->
            ( { model
                | rename =
                    model.rename
                        -- 打ち直しで赤枠を引きずらない(理由は次の確定で判定し直す)
                        |> Maybe.map (\r -> { r | text = text_, error = Nothing })
              }
            , Effect.none
            )

        RenameCommitted ->
            commitRename model

        RenameCancelled ->
            ( { model | rename = Nothing }, Effect.none )

        CrossRenameConfirmed ->
            case model.crossRename of
                Just plan ->
                    let
                        ( m1, ownFx ) =
                            -- 開いている文書側は普段の改名と同じ道(編集往復中なら意図だけ覚える)
                            if model.editReq /= Nothing then
                                ( { model | pendingRename = Just plan.req }, Effect.none )

                            else
                                sendRename plan.req model

                        ( m2, crossFx ) =
                            startCrossRun plan { m1 | crossRename = Nothing }
                    in
                    ( m2, Effect.batch [ ownFx, crossFx ] )

                Nothing ->
                    ( model, Effect.none )

        CrossRenameCancelled ->
            -- 何も書き換えない(開いている文書側も含めて丸ごとやめる —
            -- 片側だけ改名すると参照が割れる)
            ( { model | crossRename = Nothing }, Effect.none )

        DraftStarted seed ->
            -- 触り始めた欄を、右の JSON でも指し示す(2 ペインで開いている時だけ)
            highlightJson seed.path <|
                case model.activeDraft of
                -- 同じ欄への focus 再入(確定失敗の赤を直しに戻った等)は打ちかけを消さない
                Just d ->
                    if d.path == seed.path then
                        ( model, Effect.none )

                    else
                        ( startInteraction { model | activeDraft = Just (draftFrom seed seed.original) }, Effect.none )

                Nothing ->
                    ( startInteraction { model | activeDraft = Just (draftFrom seed seed.original) }, Effect.none )

        DraftTyped seed text_ ->
            let
                m1 =
                    case model.activeDraft of
                        Just d ->
                            if d.path == seed.path then
                                { model | activeDraft = Just { d | text = text_ } }

                            else
                                { model | activeDraft = Just (draftFrom seed text_) }

                        -- focus を経ない入力(Esc 直後に打ち直した等)でも打ちかけとして拾う
                        Nothing ->
                            { model | activeDraft = Just (draftFrom seed text_) }
            in
            liveTypedCommit m1

        DraftStepped seed arg ->
            stepDraft seed arg model

        DraftCommitted release ->
            commitDraft release model

        DraftCancelled ->
            ( { model | activeDraft = Nothing }, Effect.none )

        PreviewClicked point ->
            case ( previewHit model point, currentPlugin model ) of
                ( Just i, Just plugin ) ->
                    -- 行選択は既存クリックと同じ経路に流す(選択の意味を 1 箇所に保つ)。
                    -- 絞り込みは消す(選ばれた行がフィルタで隠れていると迷子になる)
                    update (EntryClicked (ByIndex i))
                        { model | sectionKey = Just plugin.sectionKey, tableFilter = "" }

                _ ->
                    ( model, Effect.none )

        SpawnPressed press ->
            case ( model.preview, currentPlugin model ) of
                ( PreviewShowing p, Just plugin ) ->
                    let
                        -- 掴んだ瞬間に行選択も済ませる(押した点と選択行がずれない)
                        ( selected, cmd ) =
                            update (EntryClicked (ByIndex press.index))
                                { model | sectionKey = Just plugin.sectionKey, tableFilter = "" }

                        -- 掴んだ所からの一連のドラッグは 1 手(戻すとき 1 回で元へ)
                        m1 =
                            startInteraction selected
                    in
                    ( { m1
                        | drag =
                            Just
                                { index = press.index
                                , center = press.center
                                , startX = press.clientX
                                , startY = press.clientY
                                , ratio = p.design.w / max 1 press.clientW
                                , designH = p.design.h
                                , pos = press.center
                                , moved = False
                                }
                      }
                    , cmd
                    )

                _ ->
                    ( model, Effect.none )

        DragMoved client ->
            case ( model.drag, currentPlugin model ) of
                ( Just d, Just plugin ) ->
                    ( { model
                        | drag =
                            Just
                                { d
                                    | pos =
                                        plugin.dragPoint
                                            { center = d.center, ratio = d.ratio, designH = d.designH }
                                            { dx = client.x - d.startX, dy = client.y - d.startY }
                                    , moved = True
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        DragEnded ->
            case ( model.drag, currentPlugin model ) of
                ( Just d, Just plugin ) ->
                    let
                        m1 =
                            { model | drag = Nothing }
                    in
                    if d.moved then
                        -- atX・y の 2 本を編集キューへ(1 本目は即送信・2 本目は応答後)
                        plugin.dragEdits d.designH d.pos
                            |> List.map
                                (\e ->
                                    { op = SetOp
                                    , path = [ KeySeg plugin.sectionKey, IdxSeg d.index, KeySeg e.field ]
                                    , value = E.int e.value
                                    , isInt = True
                                    }
                                )
                            |> List.foldl
                                (\payload ( m, cmds ) ->
                                    queueEdit payload m |> Tuple.mapSecond (\c -> c :: cmds)
                                )
                                ( m1, [] )
                            |> Tuple.mapSecond Effect.batch

                    else
                        ( m1, Effect.none )

                _ ->
                    ( model, Effect.none )

        WizardOpened ->
            if model.dirty then
                -- ウィザード完走は別ファイルを開いて戻るので、ここも編集が消える口
                ( { model | pendingNav = Just NavWizard }, Effect.none )

            else
                ( { model | wizard = Just emptyWizard }, Effect.none )

        WizardClosed ->
            -- 書き込み中は閉じない(応答の受け先が消えて進行が迷子になる)
            if wizardBusy model then
                ( model, Effect.none )

            else
                ( { model | wizard = Nothing }, Effect.none )

        WizardStepChosen step ->
            if wizardBusy model then
                ( model, Effect.none )

            else
                ( updateWizard (\w -> { w | step = step }) model, Effect.none )

        WizardIdChanged text_ ->
            ( updateDraft (\d -> { d | id = text_ }) model, Effect.none )

        WizardTitleChanged text_ ->
            ( updateDraft (\d -> { d | title = text_ }) model, Effect.none )

        WizardPathChanged text_ ->
            -- 空に戻したら既定(assets/<id>.json)追従へ帰す
            ( updateDraft
                (\d ->
                    { d
                        | path =
                            if String.trim text_ == "" then
                                Nothing

                            else
                                Just text_
                    }
                )
                model
            , Effect.none
            )

        WizardShapeChosen shape ->
            ( updateDraft (\d -> { d | shape = shape }) model, Effect.none )

        WizardFieldChanged i field ->
            ( updateDraft
                (\d ->
                    { d
                        | fields =
                            d.fields
                                |> List.indexedMap
                                    (\k f ->
                                        if k == i then
                                            field

                                        else
                                            f
                                    )
                    }
                )
                model
            , Effect.none
            )

        WizardFieldAdded ->
            ( updateDraft (\d -> { d | fields = d.fields ++ [ Wizard.emptyField ] }) model, Effect.none )

        WizardFieldRemoved i ->
            ( updateDraft
                (\d ->
                    { d
                        | fields =
                            List.take i d.fields ++ List.drop (i + 1) d.fields
                    }
                )
                model
            , Effect.none
            )

        WizardFieldMoved i delta ->
            ( updateDraft (\d -> { d | fields = Wizard.moveField i delta d.fields }) model, Effect.none )

        WizardCreateClicked ->
            case model.wizard of
                Just w ->
                    if w.write /= WizNotStarted || not (List.isEmpty (wizardErrors model w.draft)) then
                        ( model, Effect.none )

                    else
                        sendWizardSchema w model

                Nothing ->
                    ( model, Effect.none )

        NoticeExpired info ->
            -- 消すのは「自分が出した通知がまだ最新のまま」の時だけ。連続保存は
            -- 最後の通知の番号(seq)だけが合い、後から出た別文言(エラー等)は
            -- message が合わないので消さない
            if info.seq == model.noticeSeq && model.notice == Just info.message then
                ( { model | notice = Nothing }, Effect.none )

            else
                ( model, Effect.none )

        PanePressed side x ->
            ( { model
                | paneDrag =
                    Just
                        { side = side
                        , startX = x
                        , startW =
                            case side of
                                LeftPane ->
                                    model.leftPaneW

                                RightPane ->
                                    model.rightPaneW
                        }
              }
            , Effect.none
            )

        PaneMoved x ->
            case model.paneDrag of
                Just pd ->
                    let
                        dx =
                            round (x - pd.startX)

                        -- 左レールは右へ引くと広がる・右ペインは左へ引くと広がる
                        w =
                            case pd.side of
                                LeftPane ->
                                    clampPaneWidth LeftPane (pd.startW + dx)

                                RightPane ->
                                    clampPaneWidth RightPane (pd.startW - dx)
                    in
                    ( case pd.side of
                        LeftPane ->
                            { model | leftPaneW = w }

                        RightPane ->
                            { model | rightPaneW = w }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        PaneReleased ->
            case model.paneDrag of
                Just _ ->
                    savePrefs { model | paneDrag = Nothing }

                Nothing ->
                    ( model, Effect.none )

        LiveToggled ->
            let
                m1 =
                    { model | liveSave = not model.liveSave }
            in
            if m1.liveSave && m1.dirty then
                -- ON にした瞬間に未保存があれば、それも予約に乗せる
                let
                    ( m2, prefsFx ) =
                        savePrefs m1

                    ( m3, autoFx ) =
                        scheduleAutosave ( m2, Effect.none )
                in
                ( m3, Effect.batch [ prefsFx, autoFx ] )

            else
                savePrefs m1

        AutosaveFired seq ->
            -- 最新の予約(seq 一致)だけが保存する。往復中・競合ダイアログ中は
            -- 触らない(putFile 成功側が dirty 残りを見て予約し直す)
            if
                (seq == model.autosaveSeq)
                    && model.liveSave
                    && model.dirty
                    && (model.savingText == Nothing)
                    && (model.conflict == Nothing)
            then
                sendPut model.mtime
                    { model | savingText = Just model.docText, lastSaveWasAuto = True }

            else
                ( model, Effect.none )

        ActivePollTick ->
            -- 起動状態も同じ拍で問い直す(止まった・起きたにバッジを追従させる)。
            -- activeDocs は走っている間だけ読む — 止まったゲームの
            -- debug/active-docs.json は消されず残るので、読み直しても真実にならない。
            -- 前の往復が残っていれば見送る(次の周期でまた来る)
            if model.screen == Editing then
                if projectGameRunning model && model.activeReq == Nothing then
                    let
                        ( m1, cmd ) =
                            request "activeDocs" (E.object []) model
                    in
                    ( { m1 | activeReq = Just m1.reqCounter }
                    , Effect.batch [ cmd, requestInfo "gameStatus" ]
                    )

                else
                    ( model, requestInfo "gameStatus" )

            else
                ( model, Effect.none )

        RunningGamesPollTick ->
            -- プロジェクト選択画面にいる間だけ、走っているゲームを問い直して
            -- バッジを最新に保つ(起動/終了に追従)
            if model.screen == Picker then
                request "runningGames" (E.object []) model

            else
                ( model, Effect.none )


updatePicker : (PickerState -> PickerState) -> Model -> PickerState
updatePicker f model =
    f model.picker


selectProject : String -> Model -> ( Model, Effect )
selectProject dir model =
    if model.dirty then
        -- 切替は開いていた編集を丸ごと消すので、ファイル切替と同じ関所を通す
        ( { model | pendingNav = Just (NavProject dir) }, Effect.none )

    else if model.picker.busy /= Nothing then
        ( model, Effect.none )

    else
        request "selectProject"
            (E.object [ ( "dir", E.string dir ) ])
            { model | picker = updatePicker (\p -> { p | busy = Just dir, error = Nothing }) model }


openFile : String -> Model -> ( Model, Effect )
openFile path model =
    let
        ( m1, loadFx ) =
            request "getFile" (E.object [ ( "path", E.string path ) ]) model

        -- 前のファイルのスキーマ要求・編集往復は無効化(遅れて届く古い応答を受けない)
        m2 =
            { m1
                | loadReq = Just m1.reqCounter
                , dashboard = Nothing
                , pendingJump = Nothing
                , sectionKey = Nothing
                , entrySel = Nothing
                , tableSort = Nothing
                , tableFilter = ""
                , schemaReq = Nothing
                , activeDraft = Nothing
                , editReq = Nothing
                , pendingEdits = []
                , preview = PreviewNone
                , previewReq = Nothing
                , previewStale = False
                , drag = Nothing
                , usagesOpenFor = Nothing
                , rename = Nothing
                , renameInflight = Nothing
                , pendingRename = Nothing
                , addDialog = Nothing
                , deleteConfirm = Nothing
                , crossFor = Nothing
                , crossSlots = []
                , crossRename = Nothing
            }
    in
    if String.endsWith ".schema.json" path then
        -- スキーマ自身にスキーマは探さない(foo.schema.schema.json は掘らない)
        ( { m2 | schemaState = SchemaMissing }, loadFx )

    else
        case schemaPlanFor m2.groups path of
            Just schemaPath ->
                let
                    ( m3, schemaFx ) =
                        request "getFile"
                            (E.object [ ( "path", E.string schemaPath ) ])
                            m2
                in
                ( { m3 | schemaReq = Just m3.reqCounter, schemaState = SchemaLoading }
                , Effect.batch [ loadFx, schemaFx ]
                )

            Nothing ->
                -- スキーマ無しが確定しているファイル。取りにも行かない(404 を作らない)
                ( { m2 | schemaState = SchemaMissing }, loadFx )


-- ダッシュボード(複数リソース横断の閲覧ボード)


{-| ジャンプ付きで開く。選択(pendingJump)は openFile のリセットの後に張る —
openFile は前のファイルの取り残しを消す口で、この意図まで消させないため。
-}
openFileAt : Jump -> Model -> ( Model, Effect )
openFileAt jump model =
    let
        ( m1, fx ) =
            openFile jump.path model
    in
    ( { m1 | pendingJump = Just jump }, fx )


{-| uses の全リソースの全ファイル(本文+スキーマ)を取りに行く。
再クリックも取り直しとして同じ道を通す(古い封筒 id はスロットごと捨てられる)。
-}
openDashboard : Api.Dashboard -> Model -> ( Model, Effect )
openDashboard decl model =
    let
        files =
            decl.uses
                |> List.concatMap
                    (\use ->
                        model.groups
                            |> List.filter (\g -> g.id == use)
                            |> List.concatMap .files
                            |> List.map (\f -> ( use, f.path ))
                    )

        step ( use, path ) ( m, slots, fxs ) =
            let
                ( m1, dataFx ) =
                    request "getFile" (E.object [ ( "path", E.string path ) ]) m

                dataId =
                    m1.reqCounter

                ( m2, schemaReq, schemaFx ) =
                    requestSchemaIfPlanned path m1
            in
            ( m2
            , slots
                ++ [ { resource = use
                     , path = path
                     , dataReq = Just dataId
                     , doc = Nothing
                     , dataError = Nothing
                     , schemaReq = schemaReq
                     , schema = Nothing
                     }
                   ]
            , fxs ++ [ dataFx, schemaFx ]
            )

        ( mLast, allSlots, allFx ) =
            List.foldl step ( model, [], [] ) files
    in
    -- 肖像キャッシュはボードを開き直すたびに空へ(隣の ui エディタで肖像が
    -- 描き替えられていても、開き直しで必ず新しい焼きになる)
    ( { mLast
        | dashboard = Just { decl = decl, slots = allSlots, selected = Nothing }
        , portraits = Dict.empty
      }
    , Effect.batch allFx
    )


{-| ダッシュボード読み込みの応答なら該当スロットへ収める(Nothing = 別物)。 -}
dashApply : Int -> Api.FileContent -> Maybe DashState -> Maybe DashState
dashApply id fc =
    dashUpdate id
        (\slot ->
            if slot.dataReq == Just id then
                case D.decodeString D.value fc.content of
                    Ok doc ->
                        { slot | dataReq = Nothing, doc = Just doc, dataError = Nothing }

                    Err _ ->
                        { slot | dataReq = Nothing, doc = Nothing, dataError = Just "JSON として読めません" }

            else
                -- スキーマが壊れているのは生の要約に倒すだけ(欠けと同じ扱い)
                { slot | schemaReq = Nothing, schema = Schema.decodeString fc.content |> Result.toMaybe }
        )


{-| ダッシュボード読み込みの失敗。スキーマ欠けは普通のこと(生の要約に倒す)、
本文の失敗は理由をスロットに残して見せる。
-}
dashFailApply : Int -> String -> Maybe DashState -> Maybe DashState
dashFailApply id message =
    dashUpdate id
        (\slot ->
            if slot.dataReq == Just id then
                { slot | dataReq = Nothing, dataError = Just message }

            else
                { slot | schemaReq = Nothing }
        )


dashUpdate : Int -> (DashSlot -> DashSlot) -> Maybe DashState -> Maybe DashState
dashUpdate id fill maybeDash =
    maybeDash
        |> Maybe.andThen
            (\dash ->
                if List.any (\s -> s.dataReq == Just id || s.schemaReq == Just id) dash.slots then
                    Just
                        { dash
                            | slots =
                                dash.slots
                                    |> List.map
                                        (\s ->
                                            if s.dataReq == Just id || s.schemaReq == Just id then
                                                fill s

                                            else
                                                s
                                        )
                        }

                else
                    Nothing
            )



-- 横断辞書(開いている文書の外に住む参照先)の配管


{-| 宣言済みリソースのファイルを開いたら、他の宣言ファイル(本文+スキーマ)を
全部取りに行く。前向き(この文書の ref の参照先)だけでなく、逆向き
(他文書からこの文書への参照 = 使用数・改名)にも他文書のスキーマが要るため、
「ref する相手だけ」に絞らない。宣言はプロジェクトの数個規模が前提。
-}
requestCrossDocsIfNeeded : Model -> ( Model, Effect )
requestCrossDocsIfNeeded model =
    case model.current of
        Just path ->
            if List.member path (declaredPaths model.groups) && model.crossFor /= Just path then
                let
                    files =
                        model.groups
                            |> List.concatMap (\g -> g.files |> List.map (\f -> ( g.id, f.path )))
                            |> List.filter (\( _, p ) -> p /= path)

                    step ( use, p ) ( m, slots, fxs ) =
                        let
                            ( m1, dataFx ) =
                                request "getFile" (E.object [ ( "path", E.string p ) ]) m

                            dataId =
                                m1.reqCounter

                            ( m2, schemaReq, schemaFx ) =
                                requestSchemaIfPlanned p m1
                        in
                        ( m2
                        , slots
                            ++ [ { resource = use
                                 , path = p
                                 , dataReq = Just dataId
                                 , text = Nothing
                                 , doc = Nothing
                                 , schemaReq = schemaReq
                                 , schema = Nothing
                                 }
                               ]
                        , fxs ++ [ dataFx, schemaFx ]
                        )

                    ( mLast, allSlots, allFx ) =
                        List.foldl step ( model, [], [] ) files
                in
                ( { mLast | crossFor = Just path, crossSlots = allSlots }, Effect.batch allFx )

            else
                ( model, Effect.none )

        Nothing ->
            ( model, Effect.none )


{-| 横断辞書読み込みの応答なら該当スロットへ収める(Nothing = 別物)。 -}
crossApply : Int -> Api.FileContent -> List CrossSlot -> Maybe (List CrossSlot)
crossApply id fc slots =
    if List.any (\s -> s.dataReq == Just id || s.schemaReq == Just id) slots then
        Just
            (slots
                |> List.map
                    (\s ->
                        if s.dataReq == Just id then
                            { s
                                | dataReq = Nothing
                                , text = Just fc.content
                                , doc = D.decodeString D.value fc.content |> Result.toMaybe
                            }

                        else if s.schemaReq == Just id then
                            { s | schemaReq = Nothing, schema = Schema.decodeString fc.content |> Result.toMaybe }

                        else
                            s
                    )
            )

    else
        Nothing


{-| 横断辞書読み込みの失敗。読めないファイルは辞書に居ないのと同じ
(候補・使用数が出ないだけで、編集は止めない)。
-}
crossFailApply : Int -> List CrossSlot -> Maybe (List CrossSlot)
crossFailApply id slots =
    if List.any (\s -> s.dataReq == Just id || s.schemaReq == Just id) slots then
        Just
            (slots
                |> List.map
                    (\s ->
                        if s.dataReq == Just id then
                            { s | dataReq = Nothing }

                        else if s.schemaReq == Just id then
                            { s | schemaReq = Nothing }

                        else
                            s
                    )
            )

    else
        Nothing


{-| 読み込み済みの横断辞書を純ロジック(Sources / Lint / SchemaForm)の形に。 -}
crossSources : Model -> List Sources.SourceDoc
crossSources model =
    crossSourcesOf model.crossSlots


crossSourcesOf : List CrossSlot -> List Sources.SourceDoc
crossSourcesOf slots =
    slots
        |> List.filterMap
            (\s ->
                s.doc
                    |> Maybe.map
                        (\doc -> { resource = s.resource, path = s.path, doc = doc, schema = s.schema })
            )


{-| この改名で書き換えが要る他ファイル(パスと箇所数)。 -}
crossRenameFiles : Model -> RenameRequest -> List { path : String, count : Int }
crossRenameFiles model req =
    Sources.externalUsages (crossSources model)
        |> Dict.get (Refs.usageKey req.sectionKey req.oldId)
        |> Maybe.withDefault []
        |> List.foldl
            (\usage acc ->
                if List.any (\f -> f.path == usage.path) acc then
                    acc
                        |> List.map
                            (\f ->
                                if f.path == usage.path then
                                    { f | count = f.count + 1 }

                                else
                                    f
                            )

                else
                    acc ++ [ { path = usage.path, count = 1 } ]
            )
            []


{-| 他ファイル改名の開始: 対象パスの列を積んで最初の 1 本を取りに行く。 -}
startCrossRun : CrossRenamePlan -> Model -> ( Model, Effect )
startCrossRun plan model =
    advanceCross
        { model
            | crossRun =
                Just
                    { req = plan.req
                    , pending = plan.files |> List.map .path
                    , step = Nothing
                    , doneFiles = 0
                    , doneRefs = 0
                    }
        }


{-| 次のファイルへ。残りが無ければ締めの文言を出して終わる。 -}
advanceCross : Model -> ( Model, Effect )
advanceCross model =
    case model.crossRun of
        Nothing ->
            ( model, Effect.none )

        Just run ->
            case run.pending of
                [] ->
                    ( { model
                        | crossRun = Nothing
                        , notice =
                            Just
                                ("改名しました: "
                                    ++ run.req.oldId
                                    ++ " → "
                                    ++ run.req.newId
                                    ++ "(他 "
                                    ++ String.fromInt run.doneFiles
                                    ++ " ファイル "
                                    ++ String.fromInt run.doneRefs
                                    ++ " 箇所も書き換えて保存)"
                                )
                      }
                    , Effect.none
                    )

                path :: rest ->
                    let
                        ( m1, fx ) =
                            request "getFile" (E.object [ ( "path", E.string path ) ]) model
                    in
                    ( { m1
                        | crossRun =
                            Just { run | pending = rest, step = Just (CrossGetting m1.reqCounter path) }
                      }
                    , fx
                    )


{-| 他ファイル改名の応答受け(Nothing = この進行の封筒ではない)。
取得は最新の本文で行い(開いてから時間が経っていても他所の編集を潰さない)、
編集は jsonc の最小書き換え、保存はそのまま PUT — 開いていないファイルなので
dirty ガードの外で完結する。
-}
crossRunOk : Api.Envelope -> Model -> Maybe ( Model, Effect )
crossRunOk env model =
    model.crossRun
        |> Maybe.andThen
            (\run ->
                case run.step of
                    Just (CrossGetting reqId path) ->
                        if env.id == reqId && env.kind == "getFile" then
                            Just (crossApplyFresh path run env model)

                        else
                            Nothing

                    Just (CrossEditing reqId path count) ->
                        if env.id == reqId && env.kind == "applyDocEdits" then
                            Just (crossPut path count run env model)

                        else
                            Nothing

                    Just (CrossPutting reqId _ count) ->
                        if env.id == reqId && env.kind == "putFile" then
                            Just
                                (advanceCross
                                    { model
                                        | crossRun =
                                            Just
                                                { run
                                                    | step = Nothing
                                                    , doneFiles = run.doneFiles + 1
                                                    , doneRefs = run.doneRefs + count
                                                }
                                    }
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


{-| 取れた最新の本文へ書き換え列を導いて 1 バッチで当てる(0 件なら次へ)。 -}
crossApplyFresh : String -> CrossRenameRun -> Api.Envelope -> Model -> ( Model, Effect )
crossApplyFresh path run env model =
    let
        schemaOf =
            model.crossSlots
                |> List.filter (\s -> s.path == path)
                |> List.head
                |> Maybe.andThen .schema

        content =
            D.decodeValue Api.fileContentDecoder env.body
                |> Result.toMaybe
                |> Maybe.map .content
    in
    case ( schemaOf, content |> Maybe.andThen (\text_ -> D.decodeString D.value text_ |> Result.toMaybe), content ) of
        ( Just schema, Just doc, Just text_ ) ->
            case Refs.refRewriteEdits schema doc { sectionKey = run.req.sectionKey, oldId = run.req.oldId, newId = run.req.newId } of
                [] ->
                    advanceCross { model | crossRun = Just { run | step = Nothing } }

                edits ->
                    let
                        ( m1, fx ) =
                            request "applyDocEdits"
                                (E.object
                                    [ ( "text", E.string text_ )
                                    , ( "edits", E.list encodeDocEdit edits )
                                    ]
                                )
                                model
                    in
                    ( { m1 | crossRun = Just { run | step = Just (CrossEditing m1.reqCounter path (List.length edits)) } }
                    , fx
                    )

        _ ->
            -- スキーマか本文が読めない = 書き換えを導けない。黙って飛ばさず理由を残して中断
            crossAbort path "本文かスキーマが読めません" model


{-| 書き換え済みの本文を保存し、横断辞書のスロットも新しい本文へ進める。 -}
crossPut : String -> Int -> CrossRenameRun -> Api.Envelope -> Model -> ( Model, Effect )
crossPut path count run env model =
    case D.decodeValue (D.field "text" D.string) env.body of
        Ok newText ->
            let
                ( m1, fx ) =
                    request "putFile"
                        (E.object [ ( "path", E.string path ), ( "content", E.string newText ) ])
                        { model
                            | crossSlots =
                                model.crossSlots
                                    |> List.map
                                        (\s ->
                                            if s.path == path then
                                                { s
                                                    | text = Just newText
                                                    , doc = D.decodeString D.value newText |> Result.toMaybe
                                                }

                                            else
                                                s
                                        )
                        }
            in
            ( { m1 | crossRun = Just { run | step = Just (CrossPutting m1.reqCounter path count) } }, fx )

        Err _ ->
            crossAbort path "編集応答が読めませんでした" model


{-| 中断: 進行を捨てて理由を出す。ここまでに保存できたファイルはそのまま
(改名済みの id を指しているので、開いている文書の改名と食い違わない)。
-}
crossAbort : String -> String -> Model -> ( Model, Effect )
crossAbort path reason model =
    ( { model
        | crossRun = Nothing
        , notice = Just ("改名の他ファイル反映に失敗(" ++ path ++ "): " ++ reason)
      }
    , Effect.none
    )


{-| 進行中の封筒の失敗(Nothing = この進行の封筒ではない)。 -}
crossRunErr : Api.Envelope -> String -> Model -> Maybe ( Model, Effect )
crossRunErr env message model =
    model.crossRun
        |> Maybe.andThen
            (\run ->
                let
                    abortIf reqId path =
                        if env.id == reqId then
                            Just (crossAbort path message model)

                        else
                            Nothing
                in
                case run.step of
                    Just (CrossGetting reqId path) ->
                        abortIf reqId path

                    Just (CrossEditing reqId path _) ->
                        abortIf reqId path

                    Just (CrossPutting reqId path _) ->
                        abortIf reqId path

                    Nothing ->
                        Nothing
            )



-- 肖像(ui.json のエンジン焼き)の配管


{-| いま画面が要る肖像(開いている文書+ダッシュボード)をキャッシュに無い分だけ
取りに行く。要求の重複は Loading の項目が抑える。
-}
requestPortraits : Model -> ( Model, Effect )
requestPortraits model =
    let
        fromOpen =
            case ( model.schemaState, parsedDoc model ) of
                ( SchemaReady schema, Just doc ) ->
                    portraitPathsIn schema doc

                _ ->
                    []

        fromDash =
            model.dashboard
                |> Maybe.map
                    (\dash ->
                        dash.slots
                            |> List.filterMap (\s -> Maybe.map2 Tuple.pair s.doc s.schema)
                            |> List.concatMap (\( doc, schema ) -> portraitPathsIn schema doc)
                    )
                |> Maybe.withDefault []

        needed =
            (fromOpen ++ fromDash)
                |> List.filter (\p -> p /= "" && not (Dict.member p model.portraits))
                |> List.foldl
                    (\p acc ->
                        if List.member p acc then
                            acc

                        else
                            acc ++ [ p ]
                    )
                    []

        step path ( m, fxs ) =
            let
                ( m1, fx ) =
                    request "previewUi"
                        (E.object [ ( "path", E.string path ), ( "scale", E.int 3 ) ])
                        m
            in
            ( { m1
                | portraits = Dict.insert path PortraitLoading m1.portraits
                , portraitReqs = Dict.insert m1.reqCounter path m1.portraitReqs
              }
            , fxs ++ [ fx ]
            )
    in
    List.foldl step ( model, [] ) needed
        |> Tuple.mapSecond Effect.batch


{-| 文書の中の uiDoc フィールド値(= 肖像 ui.json のパス)を全部拾う。 -}
portraitPathsIn : Schema.Schema -> D.Value -> List String
portraitPathsIn schema doc =
    schema.sections
        |> List.concatMap
            (\( key, section ) ->
                let
                    uiDocFields =
                        section.fields
                            |> List.filter
                                (\( _, f ) -> f.type_ == Schema.TText && Schema.widgetIs "uiDoc" f.widget)
                            |> List.map Tuple.first

                    pathsOf entry =
                        uiDocFields
                            |> List.filterMap
                                (\name -> D.decodeValue (D.field name D.string) entry |> Result.toMaybe)
                in
                if List.isEmpty uiDocFields then
                    []

                else
                    case section.kind of
                        Schema.Catalog ->
                            Doc.catalog key doc |> Dict.values |> List.concatMap pathsOf

                        Schema.RecordKind ->
                            pathsOf (Doc.record key doc)

                        Schema.ListKind ->
                            Doc.list key doc |> List.concatMap pathsOf

                        Schema.ValueKind _ ->
                            []

                        Schema.Unsupported _ ->
                            []
            )


{-| /preview/ui の応答 1 枚をキャッシュ項目へ。切り出し範囲は root 以外の全 rect の
外接矩形 — root は design 全面に置かれるので絵の範囲を語らない。
-}
portraitImage : Api.Preview -> PortraitImage
portraitImage p =
    let
        childRects =
            p.rects
                |> Dict.toList
                |> List.filter (\( k, _ ) -> String.contains "/" k)
                |> List.map Tuple.second

        crop =
            case childRects of
                [] ->
                    { x = 0, y = 0, w = p.design.w, h = p.design.h }

                first :: rest ->
                    List.foldl
                        (\r acc ->
                            let
                                x1 =
                                    min acc.x r.x

                                y1 =
                                    min acc.y r.y

                                x2 =
                                    max (acc.x + acc.w) (r.x + r.w)

                                y2 =
                                    max (acc.y + acc.h) (r.y + r.h)
                            in
                            { x = x1, y = y1, w = x2 - x1, h = y2 - y1 }
                        )
                        { x = first.x, y = first.y, w = first.w, h = first.h }
                        rest
    in
    { png = p.png
    , imgW = toFloat p.width
    , imgH = toFloat p.height
    , crop = crop
    , scale = p.scale
    }



-- ウィザードの書き込み配管(スキーマ → データ → project.json の直列)


updateWizard : (WizardState -> WizardState) -> Model -> Model
updateWizard f model =
    { model | wizard = Maybe.map f model.wizard }


updateDraft : (Wizard.Draft -> Wizard.Draft) -> Model -> Model
updateDraft f =
    updateWizard (\w -> { w | draft = f w.draft })


wizardBusy : Model -> Bool
wizardBusy model =
    case model.wizard of
        Just w ->
            w.write /= WizNotStarted

        Nothing ->
            False


{-| 重なり検査は「今画面が知っているファイル一覧」との突き合わせ。 -}
wizardErrors : Model -> Wizard.Draft -> List String
wizardErrors model draft =
    Wizard.validate (declaredPaths model.groups ++ model.files) draft


sendWizardSchema : WizardState -> Model -> ( Model, Effect )
sendWizardSchema w model =
    let
        ( m1, cmd ) =
            requestPut (Wizard.schemaPathOf w.draft) (Wizard.schemaText w.draft) model
    in
    ( { m1 | wizard = Just { w | write = WizPutSchema m1.reqCounter, error = Nothing } }, cmd )


sendWizardData : WizardState -> Model -> ( Model, Effect )
sendWizardData w model =
    let
        ( m1, cmd ) =
            requestPut (Wizard.dataPathOf w.draft) (Wizard.dataText w.draft) model
    in
    ( { m1 | wizard = Just { w | write = WizPutData m1.reqCounter } }, cmd )


sendWizardProjectGet : WizardState -> Model -> ( Model, Effect )
sendWizardProjectGet w model =
    let
        ( m1, cmd ) =
            request "getFile" (E.object [ ( "path", E.string "project.json" ) ]) model
    in
    ( { m1 | wizard = Just { w | write = WizGetProject m1.reqCounter } }, cmd )


{-| 取ってきた project.json へ "editor"."resources" 追記を jsonc 最小編集
(docEdit ポート)で当てる。全文の再直列化はしない — キー順・注釈を保つため。
-}
sendWizardProjectEdit : String -> WizardState -> Model -> ( Model, Effect )
sendWizardProjectEdit projectText w model =
    case D.decodeString D.value projectText of
        Err _ ->
            -- 壊れた JSON に jsonc 編集を当てると化けた本文を書き戻しかねないので止める
            ( wizardFail "project.json が JSON として読めないため中止しました" w model, Effect.none )

        Ok _ ->
            let
                edit =
                    Wizard.declEdit w.draft

                ( m1, cmd ) =
                    request "applyDocAppend"
                        (E.object
                            [ ( "text", E.string projectText )
                            , ( "path", E.list encodeWizardSeg edit.path )
                            , ( "value", edit.value )
                            ]
                        )
                        model
            in
            ( { m1 | wizard = Just { w | write = WizEditProject m1.reqCounter } }, cmd )


encodeWizardSeg : Wizard.PathSeg -> E.Value
encodeWizardSeg seg =
    case seg of
        Wizard.Key key ->
            E.string key

        Wizard.Idx i ->
            E.int i


sendWizardProjectPut : String -> WizardState -> Model -> ( Model, Effect )
sendWizardProjectPut newText w model =
    let
        ( m1, cmd ) =
            requestPut "project.json" newText model
    in
    ( { m1 | wizard = Just { w | write = WizPutProject m1.reqCounter } }, cmd )


{-| 3 点そろったら一覧を取り直し、作ったリソースを開いた状態で戻る。 -}
finishWizard : WizardState -> Model -> ( Model, Effect )
finishWizard w model =
    let
        ( m1, c1 ) =
            request "resources" (E.object []) { model | wizard = Nothing }

        ( m2, c2 ) =
            request "files" (E.object []) m1

        ( m3, c3 ) =
            update (FileClicked (Wizard.dataPathOf w.draft)) m2
    in
    ( m3, Effect.batch [ c1, c2, c3 ] )


{-| 途中失敗はその場で止める。書けた分は消さない(部分成功を隠さない)。 -}
wizardFail : String -> WizardState -> Model -> Model
wizardFail message w model =
    { model
        | wizard =
            Just
                { w
                    | write = WizNotStarted
                    , error = Just (message ++ " — 書き込み済みのファイルはそのまま残しています")
                }
    }


requestPut : String -> String -> Model -> ( Model, Effect )
requestPut path content model =
    request "putFile"
        (E.object [ ( "path", E.string path ), ( "content", E.string content ) ])
        model


{-| ウィザードの往復への応答なら次の 1 手を返す(Nothing = ウィザードの物ではない)。
封筒 kind ごとの分岐に散らさず、進行段階(write)側で突き合わせる。
-}
wizardAdvance : Api.Envelope -> Model -> Maybe ( Model, Effect )
wizardAdvance env model =
    model.wizard
        |> Maybe.andThen
            (\w ->
                case w.write of
                    WizNotStarted ->
                        Nothing

                    WizPutSchema id ->
                        onMatch id env (\_ -> sendWizardData w model)

                    WizPutData id ->
                        onMatch id env (\_ -> sendWizardProjectGet w model)

                    WizGetProject id ->
                        onMatch id env
                            (\body ->
                                case D.decodeValue Api.fileContentDecoder body of
                                    Ok fc ->
                                        sendWizardProjectEdit fc.content w model

                                    Err _ ->
                                        ( wizardFail "project.json の応答が読めませんでした" w model, Effect.none )
                            )

                    WizEditProject id ->
                        onMatch id env
                            (\body ->
                                case D.decodeValue (D.field "text" D.string) body of
                                    Ok newText ->
                                        sendWizardProjectPut newText w model

                                    Err _ ->
                                        ( wizardFail "project.json への追記結果が読めませんでした" w model, Effect.none )
                            )

                    WizPutProject id ->
                        onMatch id env (\_ -> finishWizard w model)
            )


onMatch : Int -> Api.Envelope -> (D.Value -> ( Model, Effect )) -> Maybe ( Model, Effect )
onMatch id env continue =
    if env.id == id then
        Just (continue env.body)

    else
        Nothing


{-| ウィザードの往復の失敗ならその場で止めて文言に段階名を添える。 -}
wizardFailed : Api.Envelope -> String -> Model -> Maybe ( Model, Effect )
wizardFailed env message model =
    model.wizard
        |> Maybe.andThen
            (\w ->
                wizardPhase w.write
                    |> Maybe.andThen
                        (\( id, label ) ->
                            if env.id == id then
                                Just ( wizardFail (label ++ "に失敗: " ++ message) w model, Effect.none )

                            else
                                Nothing
                        )
            )


wizardPhase : WizardWrite -> Maybe ( Int, String )
wizardPhase write =
    case write of
        WizNotStarted ->
            Nothing

        WizPutSchema id ->
            Just ( id, "スキーマの書き込み" )

        WizPutData id ->
            Just ( id, "データ雛形の書き込み" )

        WizGetProject id ->
            Just ( id, "project.json の読み込み" )

        WizEditProject id ->
            Just ( id, "project.json への追記" )

        WizPutProject id ->
            Just ( id, "project.json の書き込み" )


{-| 見た目の好み(ペイン幅・ライブ反映)を端末(localStorage)へ。 -}
savePrefs : Model -> ( Model, Effect )
savePrefs model =
    request "saveUiPrefs"
        (E.object
            [ ( "leftW", E.int model.leftPaneW )
            , ( "rightW", E.int model.rightPaneW )
            , ( "live", E.bool model.liveSave )
            , ( "json", E.bool model.jsonPaneOpen )
            ]
        )
        model


{-| ライブ反映中は編集のたびに自動保存を予約し直す(debounce ~800ms)。
発火時の条件検査(dirty 等)は AutosaveFired 側 — ここは予約番号を進めるだけ。
-}
scheduleAutosave : ( Model, Effect ) -> ( Model, Effect )
scheduleAutosave ( model, fx ) =
    if model.liveSave && model.current /= Nothing then
        let
            seq =
                model.autosaveSeq + 1
        in
        ( { model | autosaveSeq = seq }
        , Effect.batch [ fx, Effect.Autosave { seq = seq, afterMs = 250 } ]
        )

    else
        ( model, fx )


{-| ペイン幅の可動域(px)。潰れて掴めなくなる事故と、中央の本文が
消える事故の両方を防ぐ。
-}
clampPaneWidth : PaneSide -> Int -> Int
clampPaneWidth side w =
    case side of
        LeftPane ->
            clamp 160 480 w

        RightPane ->
            clamp 240 640 w


{-| 数秒で消える通知(トースト)。表示と同時に消灯を予約する。 -}
showToast : String -> Model -> ( Model, Effect )
showToast message model =
    let
        seq =
            model.noticeSeq + 1
    in
    ( { model | notice = Just message, noticeSeq = seq }
    , Effect.ExpireNotice { seq = seq, message = message, afterMs = 3500 }
    )


{-| 保存完了の文言。宣言リソース(project.json の editor 宣言)のファイルは
走るゲーム側が watchFile で見ているので、保存 = 即反映をひと言添える。
-}
savedNotice : Model -> String
savedNotice model =
    case currentGroup model of
        Just _ ->
            "保存しました — 走るゲームに即反映されます"

        Nothing ->
            "保存しました"


{-| savingText を ifMtime 付きの PUT /file へ送る(保存・上書き強行の共通口)。 -}
sendPut : Maybe Int -> Model -> ( Model, Effect )
sendPut ifMtime model =
    case ( model.current, model.savingText ) of
        ( Just path, Just content ) ->
            let
                ( m1, cmd ) =
                    request "putFile"
                        (E.object
                            ([ ( "path", E.string path ), ( "content", E.string content ) ]
                                -- 旧サーバ相手(mtime を配らない)は無条件書き込みに倒す
                                ++ (ifMtime
                                        |> Maybe.map (\m -> [ ( "ifMtime", E.int m ) ])
                                        |> Maybe.withDefault []
                                   )
                            )
                        )
                        model
            in
            ( { m1 | putReq = Just m1.reqCounter }, cmd )

        _ ->
            ( { model | savingText = Nothing }, Effect.none )


{-| 同じ列の再クリックは昇降の反転、別の列は昇順から。 -}
toggleSort : String -> Maybe Table.SortState -> Table.SortState
toggleSort column current =
    case current of
        Just st ->
            if st.column == column then
                { column = column
                , dir =
                    if st.dir == Table.Asc then
                        Table.Desc

                    else
                        Table.Asc
                }

            else
                { column = column, dir = Table.Asc }

        Nothing ->
            { column = column, dir = Table.Asc }


targetToSel : Lint.Target -> EntrySel
targetToSel target =
    case target of
        Lint.AtKey name ->
            ByKey name

        Lint.AtIndex i ->
            ByIndex i


{-| スキーマの在り処が分かるファイルだけ取りに行く(スロット読み込みの共通部品)。
Nothing = スキーマ無しが確定していて要求も出していない。
-}
requestSchemaIfPlanned : String -> Model -> ( Model, Maybe Int, Effect )
requestSchemaIfPlanned path model =
    case schemaPlanFor model.groups path of
        Just schemaPath ->
            let
                ( m1, fx ) =
                    request "getFile" (E.object [ ( "path", E.string schemaPath ) ]) model
            in
            ( m1, Just m1.reqCounter, fx )

        Nothing ->
            ( model, Nothing, Effect.none )


siblingSchemaPath : String -> String
siblingSchemaPath path =
    if String.endsWith ".json" path then
        String.dropRight 5 path ++ ".schema.json"

    else
        path ++ ".schema.json"


{-| スキーマの探し先。/resources の宣言(サーバが実在確認済み)を優先し、
宣言に無いファイルは従来どおり隣接規約で試す。
-}
schemaPlanFor : List Api.ResourceGroup -> String -> Maybe String
schemaPlanFor groups path =
    case
        groups
            |> List.concatMap .files
            |> List.filter (\f -> f.path == path)
            |> List.head
    of
        Just file ->
            -- 宣言済みファイルは /resources の schema が唯一の源(サーバが実在確認
            -- 済み)。無ければ「無い」で確定 — kind 共有スキーマ(dungeon.schema.json
            -- 等)の世界でファイル別の隣接パスを推測すると、存在しないパスへの
            -- 404 を量産するだけになる
            file.schema

        Nothing ->
            -- 宣言に居ないファイル(「その他」)だけ隣接規約を試す
            Just (siblingSchemaPath path)


declaredPaths : List Api.ResourceGroup -> List String
declaredPaths groups =
    List.concatMap (\g -> List.map .path g.files) groups


{-| 開いているファイルのプラグイン。モデルに覚えず毎回引くのは、/resources の
後着・プロジェクト切替で覚えた値が陳腐化する余地を作らないため。
-}
currentPlugin : Model -> Maybe Plugins.Plugin
currentPlugin model =
    model.current
        |> Maybe.andThen (Plugins.forPath model.groups)


{-| current / docText を差し替える唯一の口。解いた写し(docValue・spriteDoc)を
ここで一緒に作り直すので、毎回引くのと同じく陳腐化はしない。

WhyNot: 以前は view から都度 D.decodeString していた。陳腐化はしないが、
判定(effectiveMode)と本体で 1 回の描画に何度も呼ばれるため、ドット絵の
一筆はセルを跨ぐたびに 89KB の JSON を 6 回解き直していた — 重さの正体。
入口を 1 つに絞れば、覚えても陳腐化は同じく構造で防げる。
-}
withDoc : Maybe String -> String -> Model -> Model
withDoc path text model =
    let
        value =
            D.decodeString D.value text |> Result.toMaybe
    in
    { model
        | current = path
        , docText = text
        , docValue = value
        , spriteDoc =
            case ( path, value ) of
                ( Just p, Just doc ) ->
                    if String.endsWith ".sprite.json" p then
                        PixelEditor.fromDoc doc

                    else
                        Nothing

                _ ->
                    Nothing
    }


{-| 開いているファイルがドット絵(パスが .sprite.json で終わる)なら、その
読み取り結果。legend が読めない・絵が残らない文書は Nothing — 従来のフォーム/
コード表示へ静かに倒れる(壊さない)。
-}
spriteDocCurrent : Model -> Maybe PixelEditor.Doc
spriteDocCurrent model =
    model.spriteDoc


{-| 開いているドット絵の legend 実色表をサーバに頼む(POST /sprite/colors)。
サーバはゲームと同じ色解決(テーマ・パレット)で「値 → #rrggbb」を返し、
パレットの見た目がゲーム画面と一致する。sprite Doc でなければ何もしない。
往復中の重複要求は出さない(応答は id で判定するので追い越しも安全)。
-}
requestSpriteColors : Model -> ( Model, Effect )
requestSpriteColors model =
    case ( model.current, spriteDocCurrent model, model.spriteColorsReq ) of
        ( Just path, Just _, Nothing ) ->
            case parsedDoc model of
                Just doc ->
                    let
                        ( m1, fx ) =
                            request "spriteColors"
                                (E.object [ ( "path", E.string path ), ( "doc", doc ) ])
                                model
                    in
                    ( { m1 | spriteColorsReq = Just m1.reqCounter }, fx )

                Nothing ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


{-| いま開いているタブが効果音のつまみなら、その材料。 -}
sfxConfigOf : Model -> Maybe SfxEditor.Config
sfxConfigOf model =
    currentSection model
        |> Maybe.andThen
            (\( key, section ) ->
                if Schema.widgetIs "sfx" section.widget then
                    Just
                        { sound = key
                        , label = Maybe.withDefault key section.label
                        , values = sfxValues key model
                        }

                else
                    Nothing
            )


{-| そのセクションの数値だけを名前で引ける形に。数でない欄は落とす。 -}
sfxValues : String -> Model -> Dict String Float
sfxValues key model =
    case D.decodeString D.value model.docText of
        Ok doc ->
            Doc.record key doc
                |> D.decodeValue (D.dict (D.maybe D.float))
                |> Result.withDefault Dict.empty
                |> Dict.foldl
                    (\name maybeValue acc ->
                        case maybeValue of
                            Just v ->
                                Dict.insert name v acc

                            Nothing ->
                                acc
                    )
                    Dict.empty

        Err _ ->
            Dict.empty


sfxPayload : SfxEditor.Config -> { field : String, value : Float } -> EditPayload
sfxPayload config edit =
    { op = SetOp
    , path = [ KeySeg config.sound, KeySeg edit.field ]
    , value = E.float edit.value
    , isInt = False
    }


{-| つまみの値をそのまま渡して 1 音だけ焼いてもらい、鳴らす。
保存もゲームの起動も要らない — 焼くのは常駐の焼き係(ゲーム自身のコード)。
焼けたら実ファイルも新しくなっているので、絵も取り直す。
-}
requestPreview_ : { name : String, loop : Bool } -> SfxEditor.Config -> Model -> ( Model, Effect )
requestPreview_ info config model =
    let
        ( m1, previewFx ) =
            request "previewSfx"
                (E.object
                    [ ( "name", E.string info.name )
                    , ( "loop", E.bool info.loop )
                    , ( "values", E.dict identity E.float config.values )
                    ]
                )
                model

        ( m2, shapeFx ) =
            requestSfxShape config.sound
                { m1 | sfx = SfxEditor.forget config.sound m1.sfx }
    in
    ( m2, Effect.batch [ previewFx, shapeFx ] )


{-| タブを選んだ拍: 焼き係を温めつつ、その音の絵を取りに行く。 -}
warmThenShape : Model -> ( Model, Effect )
warmThenShape model =
    let
        ( m1, warmFx ) =
            warmSfxIfNeeded model

        ( m2, shapeFx ) =
            requestSfxShapeIfNeeded m1
    in
    ( m2, Effect.batch [ warmFx, shapeFx ] )


{-| 焼き上がった音の実測を取りに行く。 -}
requestSfxShape : String -> Model -> ( Model, Effect )
requestSfxShape sound model =
    request "sfxShape" (E.object [ ( "name", E.string (sound ++ ".wav") ) ]) model


{-| まだ取りに行っていない音なら、絵を取りに行く。
文書を開いた拍とタブを選んだ拍に通す — 描画からは頼めないため。
-}
requestSfxShapeIfNeeded : Model -> ( Model, Effect )
requestSfxShapeIfNeeded model =
    case sfxConfigOf model |> Maybe.andThen (\config -> SfxEditor.wanted config.sound model.sfx) of
        Just sound ->
            requestSfxShape sound
                { model | sfx = SfxEditor.shapeLoaded sound Nothing Dict.empty model.sfx }

        Nothing ->
            ( model, Effect.none )


{-| 焼き係を温め始める。立ち上げに 1〜2 分かかるので、音の文書を開いた拍に頼んでおく
（つまみに手が伸びる頃には焼けている）。1 つの文書につき 1 回だけ。
絵を取りに行くかどうかとは切り離す — 束ねると、スキーマが後から届く順序で
一度も温まらないことがある。
-}
warmSfxIfNeeded : Model -> ( Model, Effect )
warmSfxIfNeeded model =
    if model.sfxWarmed || sfxConfigOf model == Nothing then
        ( model, Effect.none )

    else
        request "sfxWarm" (E.object []) { model | sfxWarmed = True }


{-| ドット絵の一筆(または戻す/やり直す)1 件を docEdit の編集に翻訳する。
書き戻しは sprites.<名前>.frames.<コマ> の rows 丸ごと 1 本 — 既存の編集直列
(dirty・自動保存・衝突検知・保存後の焼き)にそのまま乗る。
-}
pixelPayload : PixelEditor.Edit -> EditPayload
pixelPayload edit =
    { op = SetOp
    , path = [ KeySeg "sprites", KeySeg edit.sprite, KeySeg "frames", KeySeg edit.frame ]
    , value = E.list E.string edit.rows
    , isInt = False
    }


{-| 開いている文書がトップに rows(文字列の配列)を持つなら、その読み取り結果。
判定にファイル名を使わないのは、文字格子がマップ以外にも使われるから(コース・
波・盤面…)。名前で縛ると、同じ形の文書なのに塗って直せる物と直せない物が出る。

ドット絵(\*.sprite.json)は文字格子でも rows をトップに持たない
(sprites.<名前>.frames の下)ので、ここには掛からずピクセルエディタのままになる。

読めない文書は Nothing — 従来のフォーム/コード表示へ静かに倒れる
(spriteDocCurrent と同じ流儀。毎回引くのも同じ理由)。
-}
mapDocCurrent : Model -> Maybe MapEditor.Doc
mapDocCurrent model =
    parsedDoc model
        |> Maybe.andThen (MapEditor.fromDoc (mapAddableKeys model) (terrainDocCurrent model))


{-| 編集に添える一言(無ければ Nothing)。止める判断はしない。 -}
mapNotice : MapEditor.Edit -> Maybe String
mapNotice edit =
    case edit of
        MapEditor.RoomRowAdded added ->
            if added.hadRoom then
                Just ("「" ++ added.key ++ "」には部屋の行がもうあります(kaidan は 1 部屋 1 行まで)")

            else
                Nothing

        _ ->
            Nothing


{-| 雛形から生まれた行(triggers/props 等)は、中身(台詞など)をフォームで書く物
なので、そのセクションの新しい行を選んでおく — 分割モードへ切り替えた先で
「さっき置いた行」を探さずに済む。添字は追加前の長さ(= 末尾に入る位置)。
-}
selectAddedEntry : Model -> MapEditor.Edit -> Model -> Model
selectAddedEntry before edit model =
    let
        atEnd key =
            case parsedDoc before of
                Just doc ->
                    let
                        index =
                            List.length (Doc.list key doc)
                    in
                    { model
                        | sectionKey = Just key
                        , entrySel = Just (ByIndex index)

                        -- ビジュアルのインスペクタも同じ行を映す
                        , mapEd = MapEditor.select ( key, Just index ) model.mapEd
                    }

                Nothing ->
                    model
    in
    case edit of
        MapEditor.PointAdded added ->
            if added.fromSchema then
                atEnd added.key

            else
                model

        MapEditor.RoomRowAdded added ->
            atEnd added.key

        _ ->
            model


{-| 空きマスのクリックで 1 行足せる配列。スキーマに list セクションの
宣言があり、x と y を宣言している物だけ — 雛形(default 済み)の x,y を
クリック先へ差し替えれば、欠けの無い行がその場で作れる。
room = x,y が必須でない配列(triggers 型)。マスを見ない行(on:enter)も作れる。
-}
mapAddableKeys : Model -> List MapEditor.Addable
mapAddableKeys model =
    case model.schemaState of
        SchemaReady schema ->
            schema.sections
                |> List.filter (\( _, section ) -> isListSection section && declaresXy section)
                |> List.map (\( key, section ) -> { key = key, room = not (requiresXy section) })

        _ ->
            []


isListSection : Schema.Section -> Bool
isListSection section =
    case section.kind of
        Schema.ListKind ->
            True

        _ ->
            False


declaresXy : Schema.Section -> Bool
declaresXy section =
    let
        has name =
            List.any (\( n, _ ) -> n == name) section.fields
    in
    has "x" && has "y"


{-| x,y が必須のセクション(props/exits 型)。必須なら「マスを見ない行」は
そもそも書けない宣言なので、部屋の行の追加は出さない。
-}
requiresXy : Schema.Section -> Bool
requiresXy section =
    section.fields
        |> List.any (\( name, field ) -> (name == "x" || name == "y") && field.required)


{-| キーに対応する list セクション(雛形を作るのに使う)。 -}
mapSection : Model -> String -> Maybe Schema.Section
mapSection model key =
    case model.schemaState of
        SchemaReady schema ->
            schema.sections
                |> List.filter (\( k, section ) -> k == key && isListSection section)
                |> List.head
                |> Maybe.map Tuple.second

        _ ->
            Nothing


{-| クリックしたマスに置く新しい行。スキーマの雛形(default で埋めた全フィールド)の
x,y だけクリック先で上書きする。並びは宣言順のまま — 手で書いた行と同じ形で入る。
-}
mapNewEntry : Schema.Section -> Int -> Int -> E.Value
mapNewEntry section x y =
    mapEntryFrom section
        []
        (\name ->
            if name == "x" then
                Just (E.int x)

            else if name == "y" then
                Just (E.int y)

            else
                Nothing
        )


{-| マスを見ない行(部屋ぜんたいの仕掛け)の雛形。x,y は**書かない** —
書くとマスの行として読まれる。発火の欄(enum に "enter" がある物)は "enter"。
-}
mapRoomEntry : Schema.Section -> E.Value
mapRoomEntry section =
    let
        enterChoice name =
            section.fields
                |> List.filter (\( n, _ ) -> n == name)
                |> List.head
                |> Maybe.andThen
                    (\( _, field ) ->
                        case field.type_ of
                            Schema.TEnum choices ->
                                if List.member "enter" choices then
                                    Just (E.string "enter")

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    mapEntryFrom section [ "x", "y" ] enterChoice


{-| 雛形 1 行。既定は EntryOps.newEntry(schema の default 済み)。
dropped のフィールドは書かず、override が値を返したフィールドは差し替える。
並びは宣言順 — 手で書いた行と同じ形で入る。
-}
mapEntryFrom : Schema.Section -> List String -> (String -> Maybe E.Value) -> E.Value
mapEntryFrom section dropped override =
    let
        template =
            EntryOps.newEntry section
    in
    E.object
        (section.fields
            |> List.filter (\( name, _ ) -> not (List.member name dropped))
            |> List.filterMap
                (\( name, _ ) ->
                    case override name of
                        Just value ->
                            Just ( name, value )

                        Nothing ->
                            D.decodeValue (D.field name D.value) template
                                |> Result.toMaybe
                                |> Maybe.map (Tuple.pair name)
                )
        )


{-| 地形パレットの素になる terrain Doc。editor.resources に宣言された
*.terrain.json が横断辞書(crossSlots)で読めていればそれ(fail-open:
無ければ Nothing で、パレットは rows の文字から導かれる)。
-}
terrainDocCurrent : Model -> Maybe D.Value
terrainDocCurrent model =
    model.crossSlots
        |> List.filter (\slot -> String.endsWith ".terrain.json" slot.path)
        |> List.filterMap .doc
        |> List.head


{-| マップの 1 操作を docEdit の編集列に翻訳する。rows は配列丸ごと 1 本、
配置の移動は x・y の 2 本(それ以外のフィールドは触らない)、追加は配列末尾へ、
削除は該当添字 — どれも既存の編集直列(dirty・自動保存・409・保存後の焼き)に乗る。
-}
mapPayloads : Model -> MapEditor.Edit -> List EditPayload
mapPayloads model edit =
    case edit of
        MapEditor.RowsEdited rows ->
            [ { op = SetOp, path = [ KeySeg "rows" ], value = E.list E.string rows, isInt = False } ]

        MapEditor.PointMoved move ->
            let
                base =
                    KeySeg move.key
                        :: (move.index |> Maybe.map (\i -> [ IdxSeg i ]) |> Maybe.withDefault [])
            in
            [ { op = SetOp, path = base ++ [ KeySeg "x" ], value = E.int move.x, isInt = True }
            , { op = SetOp, path = base ++ [ KeySeg "y" ], value = E.int move.y, isInt = True }
            ]

        MapEditor.PointAdded added ->
            let
                -- 雛形が要る配列(triggers/props 等)はスキーマから作る。
                -- x,y だけの配列と、スキーマを引けなかったときは {x,y} 1 個
                value =
                    case
                        if added.fromSchema then
                            mapSection model added.key

                        else
                            Nothing
                    of
                        Just section ->
                            mapNewEntry section added.x added.y

                        Nothing ->
                            E.object [ ( "x", E.int added.x ), ( "y", E.int added.y ) ]
            in
            [ { op = AppendOp
              , path = [ KeySeg added.key ]
              , value = value
              , isInt = False
              }
            ]

        MapEditor.RoomRowAdded added ->
            case mapSection model added.key of
                Just section ->
                    [ { op = AppendOp
                      , path = [ KeySeg added.key ]
                      , value = mapRoomEntry section
                      , isInt = False
                      }
                    ]

                Nothing ->
                    []

        MapEditor.PointRemoved removed ->
            [ { op = RemoveOp
              , path = [ KeySeg removed.key, IdxSeg removed.index ]
              , value = E.null
              , isInt = False
              }
            ]



-- エントリの追加・複製・削除(テーブル上部の CRUD)


parsedDoc : Model -> Maybe D.Value
parsedDoc model =
    model.docValue


{-| 表示中セクション(キーとスキーマ)。未クリックなら先頭セクション —
view のタブの既定と同じ決め方でないと、操作が見えていない表に当たる。
-}
currentSection : Model -> Maybe ( String, Schema.Section )
currentSection model =
    case model.schemaState of
        SchemaReady schema ->
            let
                key =
                    activeSectionKey schema model
            in
            case activeTab schema model of
                Just tab ->
                    case tab.sections of
                        [ only ] ->
                            Just only

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| 追加。catalog は id を決めるダイアログへ、list は末尾へ即挿入。
list は送る前から挿入先(今の長さ)が確定しているので、その行を先に選んでおく —
応答で本文が届いた瞬間にフォームが出る。
-}
addEntry : Model -> ( Model, Effect )
addEntry model =
    case ( currentSection model, parsedDoc model ) of
        ( Just ( key, section ), Just doc ) ->
            case section.kind of
                Schema.Catalog ->
                    ( { model | addDialog = Just { sectionKey = key, text = "", error = Nothing } }
                    , Effect.none
                    )

                Schema.ListKind ->
                    queueOp (EntryOps.addListOp key section)
                        { model | entrySel = Just (ByIndex (List.length (Doc.list key doc))) }

                Schema.RecordKind ->
                    ( model, Effect.none )

                Schema.ValueKind _ ->
                    ( model, Effect.none )

                Schema.Unsupported _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


{-| catalog 追加の確定。拒む理由(空・重複)は赤枠+理由で入力を残す — 黙って捨てない。 -}
confirmAdd : Model -> ( Model, Effect )
confirmAdd model =
    case ( model.addDialog, currentSection model, parsedDoc model ) of
        ( Just dialog, Just ( key, section ), Just doc ) ->
            let
                id =
                    String.trim dialog.text
            in
            case EntryOps.addProblem (Doc.catalogKeys key doc) id of
                Just reason ->
                    ( { model | addDialog = Just { dialog | error = Just reason } }, Effect.none )

                Nothing ->
                    queueOp (EntryOps.addCatalogOp key section id)
                        { model | addDialog = Nothing, entrySel = Just (ByKey id) }

        _ ->
            ( model, Effect.none )


toggleMember : comparable -> Set comparable -> Set comparable
toggleMember key set =
    if Set.member key set then
        Set.remove key set

    else
        Set.insert key set


{-| 一覧の行そのものへの操作(上へ・下へ・直後へ複製)。動かした行を選び直して
から、既存の編集直列(queueOp)へ流す — 書き戻しの経路は CRUD と同じ 1 本。
-}
rowOp : String -> EntryOps.RowEdit -> Model -> ( Model, Effect )
rowOp sectionKey edit model =
    case parsedDoc model |> Maybe.andThen (\doc -> EntryOps.listRowOp sectionKey doc edit) of
        Just plan ->
            queueOp plan.op { model | entrySel = Just (Selection.fromRefsEntry plan.select) }

        Nothing ->
            ( model, Effect.none )


duplicateEntry : Model -> ( Model, Effect )
duplicateEntry model =
    case ( currentSection model, parsedDoc model, model.entrySel ) of
        ( Just ( key, _ ), Just doc, Just sel ) ->
            case EntryOps.duplicateOp key doc (Selection.toRefsEntry sel) of
                Just plan ->
                    queueOp plan.op { model | entrySel = Just (Selection.fromRefsEntry plan.select) }

                Nothing ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


{-| 削除。catalog id は逆参照を数え、使用中なら確認ダイアログへ(0 件は即)。
list の行は参照されない(Refs の対象は catalog id だけ)ので常に即。
-}
deleteEntry : Model -> ( Model, Effect )
deleteEntry model =
    case ( currentSection model, parsedDoc model, model.entrySel ) of
        ( Just ( key, _ ), Just doc, Just sel ) ->
            case ( sel, model.schemaState ) of
                ( ByKey name, SchemaReady schema ) ->
                    let
                        sites =
                            Refs.usages schema doc
                                |> Dict.get (Refs.usageKey key name)
                                |> Maybe.withDefault []
                    in
                    if List.isEmpty sites then
                        queueOp (EntryOps.deleteOp key (Refs.AtKey name))
                            { model | entrySel = Nothing, usagesOpenFor = Nothing }

                    else
                        ( { model | deleteConfirm = Just { sectionKey = key, entry = sel, sites = sites } }
                        , Effect.none
                        )

                ( ByIndex i, _ ) ->
                    queueOp (EntryOps.deleteOp key (Refs.AtIndex i)) { model | entrySel = Nothing }

                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


{-| EntryOps の編集 1 件を docEdit の直列(queueEdit)へ流す。 -}
queueOp : EntryOps.Op -> Model -> ( Model, Effect )
queueOp op model =
    queueEdit (opPayload op) model


opPayload : EntryOps.Op -> EditPayload
opPayload op =
    case op of
        EntryOps.SetAt path value ->
            { op = SetOp, path = List.map Edit.fromRefsSeg path, value = value, isInt = False }

        EntryOps.AppendAt path value ->
            { op = AppendOp, path = List.map Edit.fromRefsSeg path, value = value, isInt = False }

        EntryOps.RemoveAt path ->
            { op = RemoveOp, path = List.map Edit.fromRefsSeg path, value = E.null, isInt = False }



-- 打ちかけ(draft)の確定・増減


draftFrom : DraftSeed -> String -> ActiveDraft
draftFrom seed text_ =
    { path = seed.path, kind = seed.kind, original = seed.original, text = text_ }


{-| ↑↓ の増減。draft を先に増減後の文字へ差し替えてから確定を流す —
docEdit のエコーが返っても欄の表示は draft 側が勝つので打ちかけが消えない。
増減はスライダー同様「操作=意図が明確」なので blur を待たず即確定する。
-}
stepDraft : DraftSeed -> { dir : Int, shift : Bool } -> Model -> ( Model, Effect )
stepDraft seed arg model =
    case seed.kind of
        NumberDraft spec ->
            let
                baseText =
                    case model.activeDraft of
                        Just d ->
                            if d.path == seed.path then
                                d.text

                            else
                                seed.original

                        Nothing ->
                            seed.original
            in
            case Draft.step spec arg baseText of
                Just v ->
                    let
                        newText =
                            Draft.format spec v
                    in
                    queueEdit { op = SetOp, path = seed.path, value = encodeNumber spec v, isInt = spec.isInt }
                        { model | activeDraft = Just (draftFrom { seed | original = newText } newText) }

                Nothing ->
                    ( model, Effect.none )

        -- 数値欄以外に ↑↓ は無い
        _ ->
            ( model, Effect.none )


{-| blur(release=True)と Enter(release=False)の確定。
パース不能なら docEdit を出さない=文書を壊さない。draft も残す —
消すと「保存されなかった」ことが画面から見えなくなる(赤枠の根拠が draft)。
Enter は focus が残るので draft を確定値で張り直す(文書の値を流し込まない)。
-}
commitDraft : { release : Bool } -> Model -> ( Model, Effect )
commitDraft { release } model =
    case model.activeDraft of
        Nothing ->
            ( model, Effect.none )

        Just d ->
            if d.text == d.original then
                -- 無変更の blur/Enter で同値の docEdit(と再 bake)を流さない
                ( { model
                    | activeDraft =
                        if release then
                            Nothing

                        else
                            Just d
                  }
                , Effect.none
                )

            else
                case draftPayload d of
                    Just ( payload, doneText ) ->
                        queueEdit payload
                            { model
                                | activeDraft =
                                    if release then
                                        Nothing

                                    else
                                        Just { d | text = doneText, original = doneText }
                            }

                    Nothing ->
                        ( model, Effect.none )


{-| ライブ反映 ON のときだけ、打っている途中でも妥当な値を doc へ反映する
(blur を待たず走るゲームに届かせる)。draft の text/original は入力生値のまま
据え置く — doneText へ正規化すると入力中のカーソルがずれ、エコーで自分の入力が
消える。壊れている途中値(範囲外の数・不完全な JSON)は draftPayload が Nothing を
返すので何も流さない(watchFile 側が既定に倒れてチラつくのを防ぐ)。debounce 自動
保存は docEdit 応答後(afterDocEditResponse)の scheduleAutosave が担う。
-}
liveTypedCommit : Model -> ( Model, Effect )
liveTypedCommit model =
    case ( model.liveSave, model.activeDraft ) of
        ( True, Just d ) ->
            if d.text == d.original then
                ( model, Effect.none )

            else
                case draftPayload d of
                    Just ( payload, _ ) ->
                        -- activeDraft は触らない(生入力を保ったまま doc だけ進める)
                        queueEdit payload model

                    Nothing ->
                        ( model, Effect.none )

        _ ->
            ( model, Effect.none )


{-| 確定できるなら (編集 1 件, 確定後に欄へ見せる文字)。 -}
draftPayload : ActiveDraft -> Maybe ( EditPayload, String )
draftPayload d =
    case d.kind of
        NumberDraft spec ->
            Draft.parse spec d.text
                |> Maybe.map
                    (\v ->
                        ( { op = SetOp, path = d.path, value = encodeNumber spec v, isInt = spec.isInt }
                        , Draft.format spec v
                        )
                    )

        TextDraft ->
            Just ( { op = SetOp, path = d.path, value = E.string d.text, isInt = False }, d.text )

        GridDraft ->
            -- 行末の空白も含め見たままを保存する(マップは空白が意味を持つ)
            Just
                ( { op = SetOp
                  , path = d.path
                  , value = E.list E.string (String.split "\n" d.text)
                  , isInt = False
                  }
                , d.text
                )

        RawJsonDraft ->
            D.decodeString D.value d.text
                |> Result.toMaybe
                |> Maybe.map (\v -> ( { op = SetOp, path = d.path, value = v, isInt = False }, d.text ))

        ListTextDraft l ->
            Just
                ( listTextPayload l.fieldPath (SchemaForm.applyListEdit (SchemaForm.SetLine l.index d.text) l.items)
                , d.text
                )

        WeightsDraft w ->
            Draft.parse (weightsSpec w.config) d.text
                |> Maybe.map
                    (\v ->
                        let
                            newEntries =
                                Weights.redistribute w.config w.key v w.entries

                            own =
                                newEntries
                                    |> List.filter (\( k, _ ) -> k == w.key)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.withDefault v
                        in
                        ( weightsBatchPayload w.config w.fieldPath (Weights.changedEntries w.entries newEntries)
                        , Weights.format w.config own
                        )
                    )


{-| 文字列の列の書き戻し。行ごとの編集に散らさず、フィールドへ配列を丸ごと
1 本の set で書く(周りの整形は jsonc の最小編集が保つ)。
-}
listTextPayload : List Seg -> List String -> EditPayload
listTextPayload fieldPath items =
    { op = SetOp, path = fieldPath, value = E.list E.string items, isInt = False }


{-| weights の連動書き戻し 1 回ぶん(変わった行の set 列を applyDocEdits 1 本に)。 -}
weightsBatchPayload : Weights.Config -> List Seg -> List ( String, Float ) -> EditPayload
weightsBatchPayload config fieldPath edits =
    { op = BatchSetOp
    , path = fieldPath
    , value =
        E.list
            (\( key, v ) ->
                E.object
                    [ ( "op", E.string "set" )
                    , ( "path", E.list encodeSeg (fieldPath ++ [ KeySeg key ]) )
                    , ( "value", encodeWeight config v )
                    , ( "intField", E.bool (config.decimals == 0) )
                    ]
            )
            edits
    , isInt = False
    }


encodeWeight : Weights.Config -> Float -> E.Value
encodeWeight config v =
    if config.decimals == 0 then
        E.int (round v)

    else
        E.float v


encodeNumber : Draft.NumberSpec -> Float -> E.Value
encodeNumber spec v =
    if spec.isInt then
        E.int (round v)

    else
        E.float v


{-| 編集 1 件を docEdit ポートへ。往復中は列に積んで応答後に流す。
同じ path はスライダのドラッグ等で洪水になるので最新 1 件に畳む(中間値は
捨てて良い)が、別の path は全部残す — ドラッグ確定の atX・y 2 本組の
後発が先発を消さないため。
-}
queueEdit : EditPayload -> Model -> ( Model, Effect )
queueEdit payload model =
    queueEditWith Derive payload model


{-| 履歴への積み方。Derive=今の状況から 1 手のまとまりを決める・
Forced=呼び側が「この 2 本は 1 手」と言う(1 回の操作が複数の編集を生む場合)・
NoHistory=積まない(戻す / やり直すが流す逆操作。積むと自分の記録を食べて回り出す)。
-}
type Grouping
    = Derive
    | Forced String
    | NoHistory


queueEditWith : Grouping -> EditPayload -> Model -> ( Model, Effect )
queueEditWith grouping payload model =
    let
        m0 =
            case grouping of
                NoHistory ->
                    model

                Derive ->
                    recordEdit (editGroup model payload) payload model

                Forced group ->
                    recordEdit (Just group) payload model
    in
    if m0.editReq /= Nothing then
        ( { m0 | pendingEdits = enqueueEdit payload m0.pendingEdits }, Effect.none )

    else
        sendEdit payload m0


{-| その場の名前変更の確定。名前が変わっていなければ黙って畳む(やめたのと同じ)。
断る理由があれば、その場に留まってトーストで伝える。
-}
commitFileRename : Model -> ( Model, Effect )
commitFileRename model =
    case model.fileRename of
        Nothing ->
            ( model, Effect.none )

        Just renaming ->
            let
                pattern =
                    patternFor model renaming.path

                target =
                    FileVerbs.renameTarget pattern renaming.text
            in
            case FileVerbs.renameProblem (knownPaths model) { pattern = pattern, path = renaming.path, text = renaming.text } of
                Just reason ->
                    showToast reason model

                Nothing ->
                    if target == renaming.path then
                        ( { model | fileRename = Nothing }, Effect.none )

                    else
                        request "fileRename"
                            (E.object [ ( "path", E.string renaming.path ), ( "toPath", E.string target ) ])
                            { model
                                | fileRename = Nothing
                                , verbTarget = Just { path = target, open = model.current == Just renaming.path }
                            }


{-| 検索結果から飛ぶ。開くのは既存のジャンプ経路(dirty の関所も通る)で、
そのうえで「どの欄か」を控えておき、届いた後に画面をそこまで送る。
-}
jumpToHit : Api.SearchHit -> Model -> ( Model, Effect )
jumpToHit hit model =
    let
        jump =
            { path = hit.file
            , sectionKey = hitSectionKey hit
            , entry = hitEntry hit
            }

        m1 =
            toEditorScreen { model | search = SearchView.close model.search, scrollTarget = Just hit.path }
    in
    if Just hit.file == model.current && model.tab == AtelierTab then
        -- 開いているファイルの中。選び直して欄まで送るだけ
        scrollToField hit.path
            { m1
                | sectionKey = Just jump.sectionKey
                , entrySel = Just jump.entry
                , scrollTarget = Nothing
            }

    else
        update (DashJumped jump) m1


{-| 検索から飛ぶ先は必ず調整(エディタ)の画面。ホームや入口に居ても、
結果を押したらそこへ移ってから開く — 押した物が出ない方が驚きになる。
-}
toEditorScreen : Model -> Model
toEditorScreen model =
    { model | tab = AtelierTab, atelier = Atelier.toStorehouse model.atelier }


{-| 当たりのパスの 1 段目がセクション、2 段目がエントリ。
それより浅い当たり(文書直下の値)はセクションだけ選ぶ。
-}
hitSectionKey : Api.SearchHit -> String
hitSectionKey hit =
    case hit.path of
        (KeySeg key) :: _ ->
            key

        _ ->
            ""


hitEntry : Api.SearchHit -> EntrySel
hitEntry hit =
    case hit.path of
        _ :: (KeySeg name) :: _ ->
            ByKey name

        _ :: (IdxSeg i) :: _ ->
            ByIndex i

        _ ->
            ByKey ""


{-| 欄まで画面を送る(描き終わるのを待つのはブラウザ側の仕事)。 -}
scrollToField : List Seg -> Model -> ( Model, Effect )
scrollToField path model =
    request "scrollTo" (E.object [ ( "id", E.string (fieldDomId path) ) ]) model


{-| フォーム行の DOM id。検索から飛んだ欄を名指しするためだけの物。 -}
fieldDomId : List Seg -> String
fieldDomId path =
    "row-" ++ pathDomId path


{-| 置換の実行。ファイルごとに 1 本の直列で書き戻し、履歴には
「1 手 = 1 回の置換」として全ファイルぶんをまとめて積む(⌘Z 1 回で全部戻る)。
-}
startReplace : Model -> ( Model, Effect )
startReplace model =
    let
        files =
            SearchView.plan model.search
    in
    if List.isEmpty files || model.search.query == "" then
        ( model, Effect.none )

    else
        let
            -- 旧値は検索結果が持っている(当たった文字列そのもの)ので、
            -- 戻すための読み直しは要らない
            steps =
                model.search.results.hits
                    |> List.filter (\hit -> not (List.isEmpty hit.path))
                    |> List.map
                        (\hit ->
                            { file = hit.file
                            , payload =
                                { op = SetOp
                                , path = hit.path
                                , value = E.string (SearchView.replacedValue model.search.query model.search.replacement hit.value)
                                , isInt = False
                                }
                            , before = EditHistory.Value (Just (E.string hit.value))
                            }
                        )

            history =
                case model.current of
                    Just file ->
                        EditHistory.pushCross
                            { file = file, label = "置換", steps = steps }
                            model.history

                    Nothing ->
                        model.history
        in
        advanceCrossEdit
            { model
                | crossEdit = Just (CrossEdit.start "置換しました" files)
                , history = history
                , search = SearchView.close model.search
            }


{-| 直列を 1 歩進める(次のファイルの本文を取りに行く)。終わっていれば報せて畳む。
触ったファイルの中に開いている物があれば、最後に取り直して画面と揃える。
-}
advanceCrossEdit : Model -> ( Model, Effect )
advanceCrossEdit model =
    case model.crossEdit of
        Nothing ->
            ( model, Effect.none )

        Just run ->
            case CrossEdit.takeNext run of
                Just ( fileEdits, rest ) ->
                    let
                        ( m1, fx ) =
                            request "getFile" (E.object [ ( "path", E.string fileEdits.file ) ]) model
                    in
                    ( { m1 | crossEdit = Just (CrossEdit.getting m1.reqCounter fileEdits rest) }, fx )

                Nothing ->
                    let
                        ( m1, fx ) =
                            showToast (CrossEdit.doneText run) { model | crossEdit = Nothing }
                    in
                    case model.current of
                        Just path ->
                            let
                                ( m2, reloadFx ) =
                                    openFile path m1
                            in
                            ( m2, Effect.batch [ fx, reloadFx ] )

                        Nothing ->
                            ( m1, fx )


{-| 直列の応答受け(Nothing = この進行の封筒ではない)。
取得 → 最小編集 → 保存の 3 拍を、id の突き合わせで 1 本ずつ進める。
-}
crossEditOk : Api.Envelope -> Model -> Maybe ( Model, Effect )
crossEditOk env model =
    model.crossEdit
        |> Maybe.andThen
            (\run ->
                case run.step of
                    Just (CrossEdit.Getting reqId file edits) ->
                        if env.id == reqId && env.kind == "getFile" then
                            Just (crossEditApply file edits run env model)

                        else
                            Nothing

                    Just (CrossEdit.Editing reqId file count) ->
                        if env.id == reqId && env.kind == "applyDocEdits" then
                            Just (crossEditSave file count run env model)

                        else
                            Nothing

                    Just (CrossEdit.Putting reqId _ count) ->
                        if env.id == reqId && env.kind == "putFile" then
                            Just (advanceCrossEdit { model | crossEdit = Just (CrossEdit.tookFile count run) })

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


crossEditApply : String -> List EditPayload -> CrossEdit.Run -> Api.Envelope -> Model -> ( Model, Effect )
crossEditApply file edits run env model =
    case D.decodeValue Api.fileContentDecoder env.body of
        Ok fc ->
            let
                ( m1, fx ) =
                    request "applyDocEdits"
                        (E.object
                            [ ( "text", E.string fc.content )
                            , ( "edits", E.list encodeSetEdit edits )
                            ]
                        )
                        model
            in
            ( { m1 | crossEdit = Just (CrossEdit.editing m1.reqCounter file (List.length edits) run) }, fx )

        Err _ ->
            crossEditAbort file "本文が読めませんでした" model


crossEditSave : String -> Int -> CrossEdit.Run -> Api.Envelope -> Model -> ( Model, Effect )
crossEditSave file count run env model =
    case D.decodeValue (D.field "text" D.string) env.body of
        Ok newText ->
            let
                ( m1, fx ) =
                    request "putFile"
                        (E.object [ ( "path", E.string file ), ( "content", E.string newText ) ])
                        model
            in
            ( { m1 | crossEdit = Just (CrossEdit.putting m1.reqCounter file count run) }, fx )

        Err _ ->
            crossEditAbort file "編集応答が読めませんでした" model


{-| 中断: 進行を捨てて理由を出す。ここまでに保存できたファイルはそのまま
(戻すのは履歴の 1 手として残っている)。
-}
crossEditAbort : String -> String -> Model -> ( Model, Effect )
crossEditAbort file reason model =
    showToast ("書き戻しに失敗(" ++ file ++ "): " ++ reason) { model | crossEdit = Nothing }


{-| 値の書き込み 1 件を applyDocEdits の形へ(改名バッチと同じ封筒)。 -}
encodeSetEdit : EditPayload -> E.Value
encodeSetEdit payload =
    E.object
        [ ( "op", E.string "set" )
        , ( "path", E.list encodeSeg payload.path )
        , ( "value", payload.value )
        , ( "intField", E.bool payload.isInt )
        ]


{-| 宣言のパターン。動詞のダイアログが「置き場と拡張子」を埋めるのに使う。
どのグループにも属さないファイル(その他)は、パスそのものを 1 本の型とみなす。
-}
patternFor : Model -> String -> String
patternFor model path =
    model.groups
        |> List.filter (\g -> g.files |> List.any (\f -> f.path == path))
        |> List.head
        |> Maybe.map .pattern
        |> Maybe.withDefault path


{-| グループの骨格に使うスキーマ。宣言済みファイルが 1 つでもあれば、その
スキーマを兄弟として使い回す(同じ宣言に属す物は同じ形)。
-}
groupSchemaPath : Api.ResourceGroup -> Maybe String
groupSchemaPath group =
    group.files |> List.filterMap .schema |> List.head


{-| 動詞の確定。新規だけは骨格を組むためにスキーマを 1 往復取りに行き、
残りはそのままサーバの動詞へ流す。
-}
confirmVerb : Model -> ( Model, Effect )
confirmVerb model =
    case model.fileVerb of
        Nothing ->
            ( model, Effect.none )

        Just dialog ->
            case FileVerbs.problem (knownPaths model) dialog of
                Just reason ->
                    ( { model | fileVerb = Just { dialog | error = Just reason } }, Effect.none )

                Nothing ->
                    let
                        target =
                            FileVerbs.targetPath dialog
                    in
                    case dialog.kind of
                        FileVerbs.NewFile _ ->
                            case FileVerbs.schemaPathOf dialog.kind of
                                Just schemaPath ->
                                    let
                                        ( m1, fx ) =
                                            request "getFile" (E.object [ ( "path", E.string schemaPath ) ]) model
                                    in
                                    ( { m1 | skelReq = Just { id = m1.reqCounter, path = target } }, fx )

                                Nothing ->
                                    -- スキーマが無い宣言。空の入れ物だけ作る(旗の立てようがない)
                                    createFile target (Skeleton.docText Nothing) model

                        FileVerbs.Duplicate d ->
                            request "fileDuplicate"
                                (E.object [ ( "path", E.string d.path ), ( "toPath", E.string target ) ])
                                { model | fileVerb = Nothing, verbTarget = Just { path = target, open = True } }

                        FileVerbs.Delete d ->
                            request "fileDelete"
                                (E.object [ ( "path", E.string d.path ) ])
                                { model | fileVerb = Nothing, verbTarget = Just { path = d.path, open = False } }


createFile : String -> String -> Model -> ( Model, Effect )
createFile path content model =
    request "fileNew"
        (E.object [ ( "path", E.string path ), ( "content", E.string content ) ])
        { model | fileVerb = Nothing, skelReq = Nothing, verbTarget = Just { path = path, open = True } }


{-| いま分かっているファイルの全部(宣言済み+その他)。動詞の重複判定に使う。 -}
knownPaths : Model -> List String
knownPaths model =
    model.files ++ declaredPaths model.groups


{-| 動詞が通った後。一覧を取り直し、作った / 名前を変えたファイルを開く。 -}
afterVerb : Model -> ( Model, Effect )
afterVerb model =
    let
        ( m1, resourcesFx ) =
            request "resources" (E.object []) { model | verbTarget = Nothing }

        ( m2, filesFx ) =
            request "files" (E.object []) m1
    in
    case model.verbTarget of
        Just target ->
            if target.open then
                let
                    ( m3, openFx ) =
                        openFile target.path m2
                in
                ( m3, Effect.batch [ resourcesFx, filesFx, openFx ] )

            else if model.current == Just target.path then
                -- 開いていたファイルが消えた。空にして閉じる(履歴も前提を失う)
                ( withDoc Nothing
                    ""
                    { m2
                        | schemaState = SchemaNone
                        , dirty = False
                        , openedText = ""
                        , history = EditHistory.cutOnExternalChange m2.history
                    }
                , Effect.batch [ resourcesFx, filesFx ]
                )

            else
                ( m2, Effect.batch [ resourcesFx, filesFx ] )

        Nothing ->
            ( m2, Effect.batch [ resourcesFx, filesFx ] )


{-| 1 手ぶんを履歴へ。

境界の規則: 盤面(マップ)とドット絵が前面の間は積まない。あの 2 つは
「一筆」「1 ドラッグ」を単位にした自前の undo を持っていて、そちらの方が
手触りが正しい。両方に積むと ⌘Z が 2 つの履歴を交互に消費して、押した回数と
戻る量が合わなくなる。だから前面に居る方だけが履歴を持つ(⌘Z の購読も同じ
条件で切り替える)。
-}
recordEdit : Maybe String -> EditPayload -> Model -> Model
recordEdit group payload model =
    case ( model.current, parsedDoc model ) of
        ( Just file, Just doc ) ->
            if ownUndoFront model then
                model

            else
                { model
                    | history =
                        EditHistory.push
                            { file = file
                            , label = Edit.pathKey payload.path
                            , group = group
                            , payload = payload
                            , before = EditHistory.beforeFor payload doc
                            }
                            model.history
                }

        _ ->
            model


{-| 前面の編集器が自前の undo を持っているか(マップ / ドット絵)。 -}
ownUndoFront : Model -> Bool
ownUndoFront model =
    effectiveMode model
        == VisualMode
        && (mapDocCurrent model /= Nothing || spriteDocCurrent model /= Nothing)


{-| 1 手のまとまり。欄に打ちかけがある間・盤面をドラッグしている間に出る編集は
洪水になるので、その 1 回のやり取り(editSeq)を 1 手に畳む。押しただけの操作
(行の並べ替え・追加・削除)は 1 つずつ別の手。
-}
editGroup : Model -> EditPayload -> Maybe String
editGroup model payload =
    if model.activeDraft == Nothing && model.drag == Nothing then
        Nothing

    else
        Just (Edit.pathKey payload.path ++ "#" ++ String.fromInt model.editSeq)


{-| 新しいやり取りの始まり(欄への focus・盤面を掴んだ瞬間)。 -}
startInteraction : Model -> Model
startInteraction model =
    { model | editSeq = model.editSeq + 1 }


{-| 戻す / やり直す。逆操作は履歴に積まない道で既存の書き戻しへ流す。
打ちかけは畳む — 戻した値の上に古い打ちかけを残さない。

1 手が開いている文書の中で閉じているなら、いつもの編集直列(queueEdit)へ。
ファイルをまたぐ手(横断置換)は、開いていないファイルへの直列(CrossEdit)で
まとめて戻す — どちらも既存の書き戻し経路で、新しい道は作らない。
-}
stepHistory : (EditHistory.History -> Maybe ( List EditHistory.Step, EditHistory.History )) -> Model -> ( Model, Effect )
stepHistory step model =
    case step model.history of
        Just ( steps, history ) ->
            let
                m1 =
                    { model | history = history, activeDraft = Nothing }
            in
            if List.all (\st -> Just st.file == m1.current) steps then
                steps
                    |> List.foldl
                        (\st ( m, fxs ) ->
                            queueEditWith NoHistory st.payload m |> Tuple.mapSecond (\fx -> fx :: fxs)
                        )
                        ( m1, [] )
                    |> Tuple.mapSecond Effect.batch

            else
                advanceCrossEdit { m1 | crossEdit = Just (CrossEdit.start "戻しました" (groupByFile steps)) }

        Nothing ->
            ( model, Effect.none )


{-| ファイルごとに編集をまとめる(順序はそのまま — 同じファイルの手がばらけない)。 -}
groupByFile : List EditHistory.Step -> List CrossEdit.FileEdits
groupByFile steps =
    steps
        |> List.foldl
            (\st acc ->
                if List.any (\group -> group.file == st.file) acc then
                    acc |> List.map (\group ->
                        if group.file == st.file then
                            { group | edits = group.edits ++ [ st.payload ] }

                        else
                            group
                    )

                else
                    acc ++ [ { file = st.file, edits = [ st.payload ] } ]
            )
            []


{-| 複数の編集を順序を保って列に積む(weights の削除=Remove+BatchSet の 2 本組等)。
1 回の操作で出た物なので、履歴では 1 手に畳む — 押したのは 1 回なのに ⌘Z を
2 回押させない。
-}
queueEdits : List EditPayload -> Model -> ( Model, Effect )
queueEdits payloads model =
    let
        m0 =
            startInteraction model

        group =
            "batch#" ++ String.fromInt m0.editSeq
    in
    payloads
        |> List.foldl
            (\payload ( m, fxs ) ->
                queueEditWith (Forced group) payload m |> Tuple.mapSecond (\fx -> fx :: fxs)
            )
            ( m0, [] )
        |> Tuple.mapSecond Effect.batch


{-| 見比べるのは path だけ — E.Value を == に掛けると実行時に落ちるため。
畳んでよいのは値の上書き(Set / BatchSet)の同種同士だけ — 挿入・削除は
1 件ずつに意味がある。
-}
enqueueEdit : EditPayload -> List EditPayload -> List EditPayload
enqueueEdit payload queue =
    if payload.op == SetOp || payload.op == BatchSetOp then
        List.filter (\queued -> not (queued.op == payload.op && queued.path == payload.path)) queue
            ++ [ payload ]

    else
        queue ++ [ payload ]


sendEdit : EditPayload -> Model -> ( Model, Effect )
sendEdit payload model =
    let
        ( m1, cmd ) =
            case payload.op of
                SetOp ->
                    request "applyDocEdit"
                        (E.object
                            [ ( "text", E.string model.docText )
                            , ( "path", E.list encodeSeg payload.path )
                            , ( "value", payload.value )
                            , ( "intField", E.bool payload.isInt )
                            ]
                        )
                        model

                AppendOp ->
                    request "applyDocAppend"
                        (E.object
                            [ ( "text", E.string model.docText )
                            , ( "path", E.list encodeSeg payload.path )
                            , ( "value", payload.value )
                            ]
                        )
                        model

                RemoveOp ->
                    request "applyDocRemove"
                        (E.object
                            [ ( "text", E.string model.docText )
                            , ( "path", E.list encodeSeg payload.path )
                            ]
                        )
                        model

                BatchSetOp ->
                    request "applyDocEdits"
                        (E.object
                            [ ( "text", E.string model.docText )
                            , ( "edits", payload.value )
                            ]
                        )
                        model
    in
    ( { m1 | editReq = Just m1.reqCounter }, cmd )



-- weights の行追加


{-| 確定(Enter)。拒む理由(空 / 重複)があれば赤枠+理由で入力を残す。
新しい行は重み 0 で入る — 合計 total を崩さず、配分は後のスライダー操作に任せる。
-}
commitWeightsAdd : Model -> ( Model, Effect )
commitWeightsAdd model =
    case ( model.weightsAdd, D.decodeString D.value model.docText ) of
        ( Just w, Ok doc ) ->
            let
                existing =
                    valueAt w.path doc
                        |> Maybe.andThen (\raw -> D.decodeValue (D.keyValuePairs D.value) raw |> Result.toMaybe)
                        |> Maybe.withDefault []
                        |> List.map Tuple.first

                id =
                    String.trim w.text
            in
            case EntryOps.addProblem existing id of
                Just reason ->
                    ( { model | weightsAdd = Just { w | error = Just reason } }, Effect.none )

                Nothing ->
                    queueEdit
                        { op = SetOp, path = w.path ++ [ KeySeg id ], value = E.int 0, isInt = True }
                        { model | weightsAdd = Nothing }

        _ ->
            ( model, Effect.none )


{-| 文書パス(Seg 列)の指す先の値。 -}
valueAt : List Seg -> D.Value -> Maybe D.Value
valueAt segs doc =
    case segs of
        [] ->
            Just doc

        (KeySeg key) :: rest ->
            D.decodeValue (D.field key D.value) doc
                |> Result.toMaybe
                |> Maybe.andThen (valueAt rest)

        (IdxSeg i) :: rest ->
            D.decodeValue (D.index i D.value) doc
                |> Result.toMaybe
                |> Maybe.andThen (valueAt rest)



-- catalog id の改名(キー改名+全参照の書き換えを 1 バッチで)


{-| 確定(Enter)。拒む理由があれば赤枠+理由で入力を残す — 黙って捨てない。 -}
commitRename : Model -> ( Model, Effect )
commitRename model =
    case ( model.rename, D.decodeString D.value model.docText ) of
        ( Just r, Ok doc ) ->
            let
                req =
                    { sectionKey = r.sectionKey, oldId = r.oldId, newId = String.trim r.text }
            in
            case Refs.renameProblem doc req of
                Just reason ->
                    ( { model | rename = Just { r | error = Just reason } }, Effect.none )

                Nothing ->
                    case crossRenameFiles model req of
                        [] ->
                            if model.editReq /= Nothing then
                                -- 編集往復中は意図だけ覚えて応答後に送る(古い本文へ当てない)
                                ( { model | pendingRename = Just req, rename = Nothing }, Effect.none )

                            else
                                sendRename req model

                        files ->
                            -- 他ファイルの保存まで及ぶので、書く前に確認を挟む
                            ( { model
                                | crossRename = Just { req = req, files = files }
                                , rename = Nothing
                              }
                            , Effect.none
                            )

        _ ->
            ( model, Effect.none )


{-| 編集列(キー改名+参照書き換え)を今の本文から導出して 1 バッチで送る。
1 回の docText 更新に畳むのは、途中状態(参照が割れた文書)を画面に見せないため。
-}
sendRename : RenameRequest -> Model -> ( Model, Effect )
sendRename req model =
    case ( model.schemaState, D.decodeString D.value model.docText ) of
        ( SchemaReady schema, Ok doc ) ->
            let
                edits =
                    Refs.renameEdits schema doc req

                ( m1, cmd ) =
                    request "applyDocEdits"
                        (E.object
                            [ ( "text", E.string model.docText )
                            , ( "edits", E.list encodeDocEdit edits )
                            ]
                        )
                        model
            in
            ( { m1
                | editReq = Just m1.reqCounter
                , renameInflight = Just { req = req, refCount = List.length edits - 1 }
                , rename = Nothing
              }
            , cmd
            )

        _ ->
            -- 文書が JSON として読めない瞬間は送らない(壊れた本文へ最小編集を当てない)
            ( model, Effect.none )


encodeDocEdit : Refs.DocEdit -> E.Value
encodeDocEdit edit =
    case edit of
        Refs.RenameKey path newKey ->
            E.object
                [ ( "op", E.string "renameKey" )
                , ( "path", E.list encodeRefSeg path )
                , ( "newKey", E.string newKey )
                ]

        Refs.SetString path value ->
            E.object
                [ ( "op", E.string "set" )
                , ( "path", E.list encodeRefSeg path )
                , ( "value", E.string value )
                , ( "intField", E.bool False )
                ]


encodeRefSeg : Refs.PathSeg -> E.Value
encodeRefSeg seg =
    case seg of
        Refs.Key key ->
            E.string key

        Refs.Idx i ->
            E.int i


{-| プレビュー対象の文書なら取り直しを頼む(往復中なら印だけ立てて直列化 —
docEdit と同じ流儀)。対象でない/JSON が壊れている間は何もしない(前の絵を残す)。
-}
requestPreview : Model -> ( Model, Effect )
requestPreview model =
    let
        send kind payload =
            if model.previewReq /= Nothing then
                ( { model | previewStale = True }, Effect.none )

            else
                let
                    ( m1, cmd ) =
                        request kind payload model
                in
                ( { m1 | previewReq = Just m1.reqCounter }, cmd )
    in
    case previewItems model of
        Just items ->
            send "previewItems" items

        Nothing ->
            case enginePreviewRequest model of
                Just ( kind, payload ) ->
                    send kind payload

                Nothing ->
                    ( model, Effect.none )


{-| プレビューを描ける状態なら /preview/items リクエストを導く。描けるかは
「/resources の plugin id が引けるファイルか」で決まる(スキーマの有無とは独立)。
loadReq が残っている間は前のファイルの docText を読んでしまうので描かない。
-}
previewItems : Model -> Maybe E.Value
previewItems model =
    case ( currentPlugin model, model.loadReq ) of
        ( Just plugin, Nothing ) ->
            D.decodeString D.value model.docText
                |> Result.toMaybe
                |> Maybe.andThen plugin.preview

        _ ->
            Nothing


{-| 開いているファイルの kind(グループ id)がエンジン焼きの口を持つなら、その
封筒 kind。盤面プレビュー(plugin 宣言)とは独立 — こちらはサーバがファイル種
ごとに持つ描画口(/preview/ui 等)で、宣言は kind 名だけで足りる。
-}
enginePreviewKind : Model -> Maybe String
enginePreviewKind model =
    case currentGroup model |> Maybe.map .id of
        Just "ui" ->
            Just "previewUi"

        Just "hitbox" ->
            Just "previewHitbox"

        Just "fx" ->
            Just "previewFx"

        _ ->
            Nothing


{-| エンジン焼きプレビューのリクエストを導く。本文 doc をインラインで送るのは
未保存の編集も絵に出すため。パースが通らない打ちかけの間は送らない(前の絵を
出したまま) — タイピング途中の壊れた JSON で枠を赤くしない。
-}
enginePreviewRequest : Model -> Maybe ( String, E.Value )
enginePreviewRequest model =
    case ( enginePreviewKind model, model.loadReq ) of
        ( Just kind, Nothing ) ->
            parsedDoc model
                |> Maybe.map (\doc -> ( kind, E.object [ ( "doc", doc ), ( "scale", E.int 2 ) ] ))

        _ ->
            Nothing


previewHit : Model -> { x : Float, y : Float } -> Maybe Int
previewHit model point =
    case ( model.preview, currentPlugin model ) of
        ( PreviewShowing p, Just plugin ) ->
            plugin.hitTest p.rects point

        _ ->
            Nothing


{-| GET /prompt/extend の本文({title, prompt})。 -}
extendPromptDecoder : D.Decoder { title : String, prompt : String }
extendPromptDecoder =
    D.map2 (\title prompt -> { title = title, prompt = prompt })
        (D.field "title" D.string)
        (D.field "prompt" D.string)


handleOk : Api.Envelope -> Model -> ( Model, Effect )
handleOk env model =
    case wizardAdvance env model of
        Just result ->
            result

        Nothing ->
            case crossRunOk env model of
                Just result ->
                    result

                Nothing ->
                    case crossEditOk env model of
                        Just result ->
                            result

                        Nothing ->
                            handleOkByKind env model


handleOkByKind : Api.Envelope -> Model -> ( Model, Effect )
handleOkByKind env model =
    case env.kind of
        "health" ->
            case D.decodeValue Api.healthResultDecoder env.body of
                Ok (Api.HealthOk health) ->
                    let
                        ( m1, c1 ) =
                            request "files"
                                (E.object [])
                                { model | screen = Editing, title = health.title, root = health.dir }

                        ( m2, c2 ) =
                            request "resources" (E.object []) m1
                    in
                    -- 既定の画面はホームなので、その中身も最初に取っておく
                    ( m2, Effect.batch [ c1, c2, requestInfo "journeyState" ] )

                Ok (Api.HealthErr _) ->
                    -- プロジェクト未選択(または project.json が読めない)。候補を出して選ばせる。
                    -- 併せて「いま走っているゲーム」も問い合わせて起動中バッジの元にする。
                    let
                        ( m1, c1 ) =
                            request "projects" (E.object []) { model | screen = Picker }

                        ( m2, c2 ) =
                            request "runningGames" (E.object []) m1
                    in
                    ( m2, Effect.batch [ c1, c2 ] )

                Err _ ->
                    ( { model | notice = Just "health 応答が読めませんでした" }, Effect.none )

        "projects" ->
            case D.decodeValue Api.projectsDecoder env.body of
                Ok projects ->
                    ( { model | picker = updatePicker (\p -> { p | projects = Just projects }) model }
                    , Effect.none
                    )

                Err _ ->
                    ( { model | notice = Just "projects 応答が読めませんでした" }, Effect.none )

        "runningGames" ->
            case D.decodeValue Api.runningGamesDecoder env.body of
                Ok cwds ->
                    ( { model | picker = updatePicker (\p -> { p | runningCwds = cwds }) model }
                    , Effect.none
                    )

                Err _ ->
                    -- 起動中情報が読めないのは致命ではない(バッジが出ないだけ)。黙って無視する
                    ( model, Effect.none )

        "serverBase" ->
            -- JS が起動時に流し込む一方向の封筒(接続先サーバの付け根 URL)
            ( { model
                | serverBase =
                    D.decodeValue (D.field "base" D.string) env.body
                        |> Result.withDefault ""
              }
            , Effect.none
            )

        "journeyState" ->
            case D.decodeValue Journey.stateDecoder env.body of
                Ok state ->
                    -- 生まれたてのテンプレートかどうかはアトリエ(インタビューのチップの
                    -- 出し分け)にも渡す — 育ったゲームにテンプレートの言葉を並べない
                    ( { model
                        | journey = Journey.loaded state
                        , atelier = Atelier.setStarterFresh (Journey.starterFresh state) model.atelier
                      }
                    , Effect.none
                    )

                Err _ ->
                    -- 契約とずれた応答(旧サーバ等)も「準備中」の 1 枚に倒す(落とさない)
                    ( { model | journey = Journey.failed "契約とずれた応答" }, Effect.none )

        "changes" ->
            -- 開いているファイルの mtime だけ見る。変わっていて打ちかけが無ければ
            -- 黙って読み直す(何もしなくても最新になる)。打ちかけがあるときだけ帯を出す
            case ( model.current, D.decodeValue diskMtimesDecoder env.body ) of
                ( Just path, Ok mtimes ) ->
                    case ( Dict.get path mtimes, model.mtime ) of
                        ( Just disk, Just known ) ->
                            if disk == known then
                                ( { model | staleMtime = Nothing }, Effect.none )

                            else if model.dirty || model.savingText /= Nothing then
                                ( { model | staleMtime = Just disk }, Effect.none )

                            else
                                reloadCurrent model

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        "journeyChanges" ->
            case D.decodeValue Journey.changesDecoder env.body of
                Ok changes ->
                    let
                        modal =
                            case model.changesModal of
                                Just ChangesLoading ->
                                    if List.isEmpty changes.changes then
                                        -- 変わった場面が無いなら空のモーダルは出さない
                                        Nothing

                                    else
                                        Just
                                            (ChangesReady
                                                { remaining = changes.changes
                                                , total = List.length changes.changes
                                                }
                                            )

                                other ->
                                    other

                        -- 描き直しが終わった瞬間(true → false)。golden/ の絵は
                        -- この時に入れ替わる — ミニプレイヤーのキャッシュ破りの
                        -- 目盛りを進め、知らせの列が実際に動いた時だけ
                        -- 「✓ 差し替わりました」を点す(2 秒で消える)
                        bakeDone =
                            model.changesBaking && not changes.baking

                        m1 =
                            { model
                                | changesBaking = changes.baking
                                , changesModal = modal
                                , miniChanges = changes.changes
                                , miniRefresh =
                                    if bakeDone then
                                        model.miniRefresh + 1

                                    else
                                        model.miniRefresh
                                , miniSwapNotice =
                                    (bakeDone && changes.changes /= model.miniChanges)
                                        || model.miniSwapNotice
                            }

                        ( m2, toastFx ) =
                            if model.changesModal == Just ChangesLoading && modal == Nothing then
                                showToast "変わった場面はありません" m1

                            else
                                ( m1, Effect.none )
                    in
                    -- 描き出しが終わった瞬間に提案を取り直す(知らせが立つ)
                    if model.changesBaking && not changes.baking then
                        ( m2, Effect.batch [ toastFx, requestInfo "journeyState" ] )

                    else
                        ( m2, toastFx )

                Err _ ->
                    -- 契約とずれた応答。読み込み中のモーダルだけ静かに畳む
                    ( { model | changesModal = closeIfLoading model.changesModal }, Effect.none )

        "journeyChangesSeen" ->
            -- 既読が付いた。知らせのカードを最新に(提案が次へ進む)
            ( model, requestInfo "journeyState" )

        "galleryList" ->
            case D.decodeValue scenesDecoder env.body of
                Ok names ->
                    -- ミニプレイヤーの場面チップも同じ出どころ(1 応答で両方養う)
                    ( { model
                        | miniScenes = names
                        , scenes =
                            if model.scenes == Just ScenesLoading then
                                Just (ScenesReady names)

                            else
                                model.scenes
                      }
                    , Effect.none
                    )

                Err _ ->
                    -- 契約とずれた応答。モーダルは静かに畳み、チップは空へ(fail-open)
                    ( { model | scenes = Nothing, miniScenes = [] }, Effect.none )

        "atelierCandidates" ->
            case D.decodeValue Atelier.candidatesDecoder env.body of
                Ok candidates ->
                    ( { model | atelier = Atelier.gotCandidates candidates model.atelier }, Effect.none )

                Err _ ->
                    -- 契約とずれた応答(旧サーバ等)は候補ゾーンを出さないだけ
                    ( { model | atelier = Atelier.candidatesFailed model.atelier }, Effect.none )

        "atelierArchive" ->
            case D.decodeValue Atelier.archiveDecoder env.body of
                Ok archive ->
                    ( { model | atelier = Atelier.gotArchive archive model.atelier }, Effect.none )

                Err _ ->
                    ( { model | atelier = Atelier.archiveFailed model.atelier }, Effect.none )

        "atelierArchiveAdd" ->
            -- 候補がアーカイブへ移った。カードが消え、バッジ件数が進む —
            -- 候補とアーカイブの両方を取り直す
            let
                ( m1, toastFx ) =
                    showToast "🗃️ アーカイブしました(いつでも候補に戻せます)"
                        { model | atelier = Atelier.archiveSettled model.atelier }
            in
            ( m1
            , Effect.batch [ toastFx, requestInfo "atelierCandidates", requestInfo "atelierArchive" ]
            )

        "atelierRestore" ->
            -- 札が候補の列へ戻った。両方の一覧を取り直す
            let
                ( m1, toastFx ) =
                    showToast "↩ 候補に戻しました"
                        { model | atelier = Atelier.archiveSettled model.atelier }
            in
            ( m1
            , Effect.batch [ toastFx, requestInfo "atelierCandidates", requestInfo "atelierArchive" ]
            )

        "atelierSlots" ->
            case D.decodeValue Atelier.createSlotsDecoder env.body of
                Ok slots ->
                    ( { model | atelier = Atelier.gotSlots slots model.atelier }, Effect.none )

                Err _ ->
                    ( { model | atelier = Atelier.slotsFailed model.atelier }, Effect.none )

        "promptAtelier" ->
            case D.decodeValue (D.field "prompt" D.string) env.body of
                Ok prompt ->
                    ( { model | atelier = Atelier.gotPrompt prompt model.atelier }, Effect.none )

                Err _ ->
                    ( { model | atelier = Atelier.promptFailed "応答が読めませんでした" model.atelier }, Effect.none )

        "atelierCopy" ->
            -- 複製できた。調整(エディタ)へ切り替え、できた写しをそのまま開く
            let
                m1 =
                    { model | atelier = Atelier.copyDone model.atelier }

                ( m2, toastFx ) =
                    showToast "atelier/ に複製しました。調整で編集できます" m1
            in
            case D.decodeValue (D.field "file" D.string) env.body of
                Ok file ->
                    let
                        ( m3, openFx ) =
                            openFile file m2
                    in
                    ( m3, Effect.batch [ toastFx, openFx ] )

                Err _ ->
                    ( m2, toastFx )

        "gameStatus" ->
            let
                running =
                    D.decodeValue Atelier.statusDecoder env.body |> Result.withDefault False
            in
            ( { model | atelier = Atelier.gotGameStatus running model.atelier }
            , -- 起動待ちが実った瞬間だけ提案を取り直す(カードが次の一歩へ進む)
              if running && Atelier.isLaunchPolling model.atelier then
                requestInfo "journeyState"

              else
                Effect.none
            )

        "gameStart" ->
            -- 202(受理)。409(すでに起動中)も realApi が ok に均しており、
            -- どちらも「起動処理は走っている」— ポーリングが本当の姿を教える
            ( { model | atelier = Atelier.gameStarted model.atelier }, Effect.none )

        "gameLog" ->
            case D.decodeValue Atelier.gameLogDecoder env.body of
                Ok log ->
                    ( { model | atelier = Atelier.gotGameLog log model.atelier }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "runnerLog" ->
            -- /runner/log は /game/log と同じ形。プレビューの描き出しの
            -- 進捗パネルへ流し込む(失敗の自動展開も Atelier の規則)
            case D.decodeValue Atelier.gameLogDecoder env.body of
                Ok log ->
                    ( { model | atelier = Atelier.gotBakeLog log model.atelier }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "promoteCandidate" ->
            let
                retired =
                    D.decodeValue (D.field "retired" D.string) env.body
                        |> Result.toMaybe
            in
            -- オーバーレイの祝いへ(閉じた時に候補・提案を取り直す)
            ( { model | atelier = Atelier.promoted retired model.atelier }, Effect.none )

        "genesisFamilies" ->
            -- ジャンルえらびの札。読めない応答は従来のプリセット入力に倒す(fail-open)
            case D.decodeValue NewGame.familiesDecoder env.body of
                Ok families ->
                    ( { model | newGame = NewGame.gotFamilies families model.newGame }, Effect.none )

                Err _ ->
                    ( { model | newGame = NewGame.familiesUnavailable model.newGame }, Effect.none )

        "promptGenesis" ->
            case D.decodeValue (D.field "prompt" D.string) env.body of
                Ok prompt ->
                    ( { model | newGame = NewGame.gotGenesisPrompt prompt model.newGame }, Effect.none )

                Err _ ->
                    ( { model | newGame = NewGame.genesisPromptFailed "応答が読めませんでした" model.newGame }, Effect.none )

        "promptExtend" ->
            case D.decodeValue extendPromptDecoder env.body of
                Ok draft ->
                    ( { model | atelier = Atelier.gotExtendPrompt draft model.atelier }, Effect.none )

                Err _ ->
                    ( { model | atelier = Atelier.extendPromptFailed "応答が読めませんでした" model.atelier }, Effect.none )

        "projectNew" ->
            -- 202(受理)。応答の dir(産まれるゲームの絶対パス)を覚え、
            -- すぐ最初のログを取りに行く(以後は 2 秒のポーリング)。
            -- dir の無い旧サーバは Nothing(誕生時は /projects 再取得だけに倒す)
            ( { model
                | newGame =
                    NewGame.accepted
                        (D.decodeValue (D.field "dir" D.string) env.body |> Result.toMaybe)
                        model.newGame
              }
            , requestInfo "projectNewLog"
            )

        "projectNewLog" ->
            case D.decodeValue NewGame.logDecoder env.body of
                Ok log ->
                    let
                        ( newGame, result ) =
                            NewGame.gotLog log model.newGame

                        m1 =
                            { model | newGame = newGame }
                    in
                    case result of
                        NewGame.LogSuccess info ->
                            -- 誕生。候補を取り直しつつ、202 で覚えた dir を
                            -- 既存の選択フローでそのまま開く(成功でホームへ)。
                            -- dir 不明(旧サーバ)は取り直しだけ(fail-open)
                            let
                                ( m2, toastFx ) =
                                    showToast "うまれました。ホームの『次のやること』からどうぞ" m1

                                ( m3, selectFx ) =
                                    case info.dir of
                                        Just dir ->
                                            selectProject dir m2

                                        Nothing ->
                                            ( m2, Effect.none )
                            in
                            ( m3, Effect.batch [ toastFx, requestInfo "projects", selectFx ] )

                        _ ->
                            -- 走行中(継続)か失敗(パネルがログの尻尾を見せる)
                            ( m1, Effect.none )

                Err _ ->
                    -- 契約とずれた応答で回し続けても仕方ない(準備中に倒す)
                    ( { model | newGame = NewGame.unavailable model.newGame }, Effect.none )

        "selectProject" ->
            case D.decodeValue Api.projectSwitchDecoder env.body of
                Ok (Api.SwitchOk result) ->
                    let
                        ( m1, c1 ) =
                            request "files"
                                (E.object [])
                                { model
                                    | screen = Editing
                                    , tab = HomeTab
                                    , journey = Journey.init
                                    , changesBaking = False
                                    , changesAvailable = True
                                    , changesModal = Nothing
                                    , scenes = Nothing

                                    -- ミニプレイヤーは前のプロジェクトの残骸を持ち越さない —
                                    -- 場面一覧・ピン・知らせ・目盛り・拡大が残ると、次の取得までの間
                                    -- 前のゲームの場面名で描いてしまう(混線)
                                    , miniPin = Nothing
                                    , miniScenes = []
                                    , miniChanges = []
                                    , miniRefresh = 0
                                    , miniSwapNotice = False
                                    , miniZoom = Nothing
                                    , atelier = Atelier.init
                                    , title = result.title
                                    , root = result.dir
                                    , picker = emptyPicker
                                    , newGame = NewGame.init
                                    , groups = []
                                    , dashboards = []
                                    , dashboard = Nothing
                                    , pendingJump = Nothing
                                    , current = Nothing
                                    , docText = ""
                                    , docValue = Nothing
                                    , spriteDoc = Nothing
                                    , openedText = ""
                                    , dirty = False
                                    , schemaState = SchemaNone
                                    , schemaReq = Nothing
                                    , sectionKey = Nothing
                                    , entrySel = Nothing
                                    , tableSort = Nothing
                                    , tableFilter = ""
                                    , problemsOpen = False
                                    , activeDraft = Nothing
                                    , editReq = Nothing
                                    , pendingEdits = []
                                    , preview = PreviewNone
                                    , previewReq = Nothing
                                    , previewStale = False
                                    , drag = Nothing
                                    , pixel = PixelEditor.init
                                    , sfx = SfxEditor.init
                                    , spriteColors = Api.noSpriteColors
                                    , spriteColorsReq = Nothing
                                    , mapEd = MapEditor.init
                                    , usagesOpenFor = Nothing
                                    , rename = Nothing
                                    , renameInflight = Nothing
                                    , pendingRename = Nothing
                                    , addDialog = Nothing
                                    , deleteConfirm = Nothing
                                    , textures = []
                                    , sounds = []
                                    , texturesReq = Nothing
                                    , weightsAdd = Nothing
                                    , crossFor = Nothing
                                    , crossSlots = []
                                    , portraits = Dict.empty
                                    , portraitReqs = Dict.empty
                                    , crossRename = Nothing
                                    , crossRun = Nothing

                                    -- 前のプロジェクト宛ての往復・保存の残骸も無効化 —
                                    -- 特に loadReq が残ると、遅れて届く前プロジェクトの
                                    -- getFile 応答を本文として受けてしまう
                                    , mtime = Nothing
                                    , loadReq = Nothing
                                    , putReq = Nothing
                                    , savingText = Nothing
                                    , conflict = Nothing
                                    , wizard = Nothing
                                    , resourceWarnings = []
                                    , activeDocs = Dict.empty
                                    , activeReq = Nothing
                                }

                        ( m2, c2 ) =
                            request "resources" (E.object []) m1
                    in
                    ( m2, Effect.batch [ c1, c2, requestInfo "journeyState" ] )

                Ok (Api.SwitchErr message) ->
                    ( { model | picker = updatePicker (\p -> { p | busy = Nothing, error = Just message }) model }
                    , Effect.none
                    )

                Err _ ->
                    ( { model | notice = Just "project 切替応答が読めませんでした" }, Effect.none )

        "files" ->
            case D.decodeValue Api.filesDecoder env.body of
                Ok files ->
                    ( { model | files = files.paths, root = files.root }, Effect.none )

                Err _ ->
                    ( { model | notice = Just "files 応答が読めませんでした" }, Effect.none )

        "resources" ->
            case D.decodeValue Api.resourcesDecoder env.body of
                Ok res ->
                    -- 開いているファイルのプラグインがこの応答で初めて引けることが
                    -- あるので(応答順は保証されない)、ここでもプレビューと横断辞書を試す
                    let
                        ( m1, previewFx ) =
                            requestPreview
                                { model
                                    | groups = res.groups
                                    , dashboards = res.dashboards
                                    , resourceWarnings = res.warnings
                                    , sounds = res.sounds
                                }

                        ( m2, crossFx ) =
                            requestCrossDocsIfNeeded m1
                    in
                    ( m2, Effect.batch [ previewFx, crossFx ] )

                Err _ ->
                    ( { model | notice = Just "resources 応答が読めませんでした" }, Effect.none )

        "getFile" ->
            case D.decodeValue Api.fileContentDecoder env.body of
                Ok fc ->
                    if Just env.id == model.loadReq then
                        -- スキーマが先に届いていればここでプレビューが立ち上がる
                        -- (逆順ならスキーマ確定側が頼む)
                        let
                            ( m1, previewFx ) =
                                requestPreview
                                    (withDoc (Just fc.path) fc.content
                                    { model
                                        | loadReq = Nothing
                                        , openedText = fc.content
                                        , dirty = False
                                        , mtime = fc.mtime

                                        -- 読み込み(開く・読み直し・外で変わった)で
                                        -- 正本が入れ替わる → 古い逆操作は当たらない
                                        , history = EditHistory.cutOnExternalChange model.history
                                        , notice = Nothing
                                        , conflict = Nothing
                                        , savingText = Nothing

                                        -- ドット絵の道具・履歴・実色表は開いたファイルの物(持ち越さない)
                                        , pixel = PixelEditor.init
                                        , sfx = SfxEditor.init
                                        , spriteColors = Api.noSpriteColors
                                        , spriteColorsReq = Nothing
                                        , mapEd = MapEditor.init

                                        -- ジャンプで開いた時だけ選択が乗る。同じファイルの
                                        -- 読み直しでは開いている場所を保つ(読み直すたびに
                                        -- 先頭タブへ飛ぶと、作業の続きに戻れない)
                                        , sectionKey =
                                            case model.pendingJump of
                                                Just jump ->
                                                    Just jump.sectionKey

                                                Nothing ->
                                                    if model.current == Just fc.path then
                                                        model.sectionKey

                                                    else
                                                        Nothing
                                        , entrySel =
                                            case model.pendingJump of
                                                Just jump ->
                                                    Just jump.entry

                                                Nothing ->
                                                    if model.current == Just fc.path then
                                                        model.entrySel

                                                    else
                                                        Nothing
                                        , pendingJump = Nothing
                                    }
                                    )

                            ( m2, crossFx ) =
                                requestCrossDocsIfNeeded m1

                            ( m3, portraitFx ) =
                                requestPortraits m2

                            ( m4, spriteColorsFx ) =
                                requestSpriteColors m3

                            ( m5, sfxFx ) =
                                requestSfxShapeIfNeeded m4

                            ( m6, warmFx ) =
                                warmSfxIfNeeded { m5 | sfxWarmed = False }

                            -- 検索から飛んで来たなら、届いた本文の上で欄まで画面を送る
                            ( m7, scrollFx ) =
                                case m6.scrollTarget of
                                    Just path ->
                                        scrollToField path { m6 | scrollTarget = Nothing }

                                    Nothing ->
                                        ( m6, Effect.none )
                        in
                        ( m7
                        , Effect.batch
                            [ previewFx, crossFx, portraitFx, spriteColorsFx, sfxFx, warmFx, scrollFx ]
                        )

                    else if Just env.id == model.schemaReq then
                        case Schema.decodeString fc.content of
                            Ok schema ->
                                let
                                    ( m1, texFx ) =
                                        requestTexturesIfNeeded schema
                                            { model
                                                | schemaReq = Nothing
                                                , schemaState = SchemaReady schema

                                                -- ジャンプが先に選んだセクションは上書きしない
                                                , sectionKey =
                                                    case model.sectionKey of
                                                        Just key ->
                                                            Just key

                                                        Nothing ->
                                                            schema.sections |> List.head |> Maybe.map Tuple.first
                                            }

                                    ( m2, previewFx ) =
                                        requestPreview m1

                                    ( m3, crossFx ) =
                                        requestCrossDocsIfNeeded m2

                                    ( m4, portraitFx ) =
                                        requestPortraits m3
                                    ( m5, sfxFx ) =
                                        requestSfxShapeIfNeeded m4

                                    ( m6, warmFx ) =
                                        warmSfxIfNeeded m5
                                in
                                ( m6
                                , Effect.batch [ texFx, previewFx, crossFx, portraitFx, sfxFx, warmFx ]
                                )

                            Err reason ->
                                -- JSON-Schema 形式(draft-07 等)は壊れではなく
                                -- 「フォーム化対象外の種類」— 穏やかな案内に倒す
                                ( { model
                                    | schemaReq = Nothing
                                    , schemaState =
                                        if Schema.isJsonSchema fc.content then
                                            SchemaForeign { previewBroken = False }

                                        else
                                            SchemaBroken reason
                                  }
                                , Effect.none
                                )

                    else if (model.skelReq |> Maybe.map .id) == Just env.id then
                        -- 「新規」の骨格づくり。スキーマが読めない宣言では
                        -- 空の入れ物だけ作る(壊れたスキーマで作成を止めない)
                        case model.skelReq of
                            Just pending ->
                                createFile pending.path
                                    (Skeleton.docText (Schema.decodeString fc.content |> Result.toMaybe))
                                    model

                            Nothing ->
                                ( model, Effect.none )

                    else if Just env.id == model.texturesReq then
                        ( { model | texturesReq = Nothing, textures = texturesFrom fc.content }, Effect.none )

                    else
                        case crossApply env.id fc model.crossSlots of
                            Just slots ->
                                ( { model | crossSlots = slots }, Effect.none )

                            Nothing ->
                                case dashApply env.id fc model.dashboard of
                                    Just dash ->
                                        -- ボードの文書が届くたび、その中の肖像を取りに行く
                                        requestPortraits { model | dashboard = Just dash }

                                    Nothing ->
                                        -- 追い越された古い応答(別ファイルを続けて開いた等)は捨てる
                                        ( model, Effect.none )

                Err _ ->
                    ( { model | notice = Just "file 応答が読めませんでした" }, Effect.none )

        "search" ->
            case D.decodeValue Api.searchResultsDecoder env.body of
                Ok results ->
                    ( { model | search = SearchView.withResults results model.search }, Effect.none )

                Err _ ->
                    ( { model | search = SearchView.withResults { files = [], filesTotal = 0, hits = [], total = 0, truncated = False } model.search }
                    , Effect.none
                    )

        -- 画面送り・カーソル置き・JSON の指し示しは頼むだけ(応答に用は無い)
        "scrollTo" ->
            ( model, Effect.none )

        "highlightJson" ->
            ( model, Effect.none )

        "focusId" ->
            ( model, Effect.none )

        "fileNew" ->
            afterVerb model

        "fileDuplicate" ->
            afterVerb model

        "fileRename" ->
            afterVerb model

        "fileDelete" ->
            afterVerb model

        "sfxWarm" ->
            let
                ready =
                    D.decodeValue (D.field "ready" D.bool) env.body
                        |> Result.withDefault False

                m1 =
                    { model | sfx = SfxEditor.startingChanged (not ready) model.sfx }
            in
            if ready then
                ( { m1 | sfxWarming = False }, Effect.none )

            else
                -- まだ焼けない。少し置いてもう一度様子を見る（見に行く先は warm）。
                let
                    seq =
                        m1.sfxWaitSeq + 1
                in
                ( { m1 | sfxWarming = True, sfxWaitSeq = seq, sfxWaitLeft = 1 }
                , Effect.Delayed { seq = seq, afterMs = 2000 }
                )

        "previewSfx" ->
            let
                starting =
                    D.decodeValue (D.field "starting" D.bool) env.body
                        |> Result.withDefault False

                m1 =
                    { model | sfx = SfxEditor.startingChanged starting model.sfx }
            in
            if starting then
                -- 焼き係の立ち上げ待ち。誰も試し直さないと、こちらから触るまで
                -- 鳴らないままになるので、間を置いて自分でもう一度頼む。
                let
                    seq =
                        m1.sfxWaitSeq + 1
                in
                ( { m1 | sfxWaitSeq = seq, sfxWaitLeft = 1 }
                , Effect.Delayed { seq = seq, afterMs = 4000 }
                )

            else
                ( m1, Effect.none )

        "sfxShape" ->
            let
                shape =
                    D.decodeValue SfxEditor.shapeDecoder env.body |> Result.toMaybe

                name =
                    shape |> Maybe.map .name |> Maybe.withDefault ""

                sound =
                    if String.endsWith ".wav" name then
                        String.dropRight 4 name

                    else
                        name
            in
            ( { model | sfx = SfxEditor.shapeLoaded sound shape (sfxValues sound model) model.sfx }
            , Effect.none
            )

        "putFile" ->
            if Just env.id == model.putReq then
                case D.decodeValue Api.putFileResultDecoder env.body of
                    Ok (Api.PutOk result) ->
                        let
                            saved =
                                Maybe.withDefault model.docText model.savingText

                            m1 =
                                { model
                                    | putReq = Nothing
                                    , savingText = Nothing
                                    , openedText = saved
                                    , dirty = model.docText /= saved

                                    -- 次の保存の ifMtime。旧サーバ(mtime 無し)は控えを進めない
                                    , mtime =
                                        case result.mtime of
                                            Just m ->
                                                Just m

                                            Nothing ->
                                                model.mtime

                                    -- サーバが保存の瞬間に検査を蹴った知らせ。その場でスピナーを
                                    -- 出し、知らせのポーリングも 2 秒間隔へ(次の 8 秒を待たない)。
                                    -- 旧サーバ(baking 無し=False)は従来どおり。誤検知は
                                    -- /journey/changes の baking:false が通常どおり倒す
                                    , changesBaking = model.changesBaking || result.baking
                                }
                        in
                        if model.lastSaveWasAuto then
                            -- ライブ反映は静かに保存(トースト連打をうるさくしない)。
                            -- 往復中に編集が進んでいれば次の自動保存を予約し直す
                            scheduleAutosave ( m1, Effect.none )

                        else
                            showToast (savedNotice model) m1

                    Ok (Api.PutConflict c) ->
                        -- 409: 何も書かれていない。savingText は残す —
                        -- 「構わず上書き」が同じ本文をもう一度送るため
                        ( { model
                            | putReq = Nothing
                            , conflict = Just { currentMtime = Just c.currentMtime }
                          }
                        , Effect.none
                        )

                    Ok (Api.PutErr message) ->
                        ( { model | putReq = Nothing, savingText = Nothing, notice = Just ("保存に失敗: " ++ message) }
                        , Effect.none
                        )

                    Err _ ->
                        ( { model | putReq = Nothing, savingText = Nothing, notice = Just "保存応答が読めませんでした" }
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        "applyDocEdit" ->
            docEditOk env model

        "applyDocAppend" ->
            docEditOk env model

        "applyDocRemove" ->
            docEditOk env model

        "applyDocEdits" ->
            if Just env.id == model.editReq then
                case D.decodeValue (D.field "text" D.string) env.body of
                    Ok newText ->
                        afterDocEditResponse newText (followRename model)

                    Err _ ->
                        ( { model
                            | editReq = Nothing
                            , pendingEdits = []
                            , renameInflight = Nothing
                            , notice = Just "改名の応答が読めませんでした"
                          }
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        "uiPrefs" ->
            -- JS が起動時に localStorage から流し込む一方向の封筒(id 0)。
            -- 覚えた頃と可動域が変わっていても壊れないよう、復元時も丸める
            case
                D.decodeValue
                    (D.map4 (\l r live json -> ( ( l, r ), live, json ))
                        (D.field "leftW" D.int)
                        (D.field "rightW" D.int)
                        (D.oneOf [ D.field "live" D.bool, D.succeed False ])
                        (D.oneOf [ D.field "json" D.bool, D.succeed True ])
                    )
                    env.body
            of
                Ok ( ( l, r ), live, json ) ->
                    ( { model
                        | leftPaneW = clampPaneWidth LeftPane l
                        , rightPaneW = clampPaneWidth RightPane r
                        , liveSave = live
                        , jsonPaneOpen = json
                      }
                    , Effect.none
                    )

                Err _ ->
                    ( model, Effect.none )

        "activeDocs" ->
            if Just env.id == model.activeReq then
                ( { model
                    | activeReq = Nothing

                    -- 形が違う内容は空へ倒す(fail-open: 何も出さないだけ)
                    , activeDocs =
                        D.decodeValue Api.fileContentDecoder env.body
                            |> Result.toMaybe
                            |> Maybe.andThen (\fc -> D.decodeString activeDocsDecoder fc.content |> Result.toMaybe)
                            |> Maybe.withDefault Dict.empty
                  }
                , Effect.none
                )

            else
                ( model, Effect.none )

        "spriteColors" ->
            -- 追い越された古い応答は捨てる(開き直しの取り直しが正)。
            -- ok:false や形違いは空の表 = 従来の仮色に倒す(fail-open)
            if Just env.id == model.spriteColorsReq then
                ( { model
                    | spriteColorsReq = Nothing
                    , spriteColors =
                        D.decodeValue Api.spriteColorsDecoder env.body
                            |> Result.withDefault Api.noSpriteColors
                  }
                , Effect.none
                )

            else
                ( model, Effect.none )

        "previewItems" ->
            previewOk env model

        "previewHitbox" ->
            previewOk env model

        "previewFx" ->
            previewOk env model

        "previewUi" ->
            case Dict.get env.id model.portraitReqs of
                Just path ->
                    let
                        m1 =
                            { model | portraitReqs = Dict.remove env.id model.portraitReqs }
                    in
                    -- キャッシュが空にされた後の遅い応答は捨てる(開き直しの取り直しが正)
                    if Dict.get path m1.portraits == Just PortraitLoading then
                        case D.decodeValue Api.previewResultDecoder env.body of
                            Ok (Api.PreviewOk p) ->
                                ( { m1 | portraits = Dict.insert path (PortraitReady (portraitImage p)) m1.portraits }
                                , Effect.none
                                )

                            Ok (Api.PreviewErr reason) ->
                                ( { m1 | portraits = Dict.insert path (PortraitFailed reason) m1.portraits }
                                , Effect.none
                                )

                            Err _ ->
                                ( { m1 | portraits = Dict.insert path (PortraitFailed "応答が読めませんでした") m1.portraits }
                                , Effect.none
                                )

                    else
                        ( m1, Effect.none )

                Nothing ->
                    -- 肖像でなければ、開いているファイル(kind = ui)のエンジン焼き
                    previewOk env model

        _ ->
            ( model, Effect.none )


{-| 盤面/エンジン焼きプレビューの応答(previewItems / previewUi / previewHitbox /
previewFx 共通)。追い越された古い応答は捨てる(最新の要求だけが絵になる)。
-}
previewOk : Api.Envelope -> Model -> ( Model, Effect )
previewOk env model =
    if Just env.id == model.previewReq then
        let
            m1 =
                case D.decodeValue Api.previewResultDecoder env.body of
                    Ok (Api.PreviewOk p) ->
                        { model | previewReq = Nothing, preview = PreviewShowing p }

                    Ok (Api.PreviewErr reason) ->
                        { model | previewReq = Nothing, preview = PreviewFailed reason }

                    Err _ ->
                        { model | previewReq = Nothing, preview = PreviewFailed "応答が読めませんでした" }
        in
        resendPreviewIfStale m1

    else
        ( model, Effect.none )


{-| 単発の文書編集(Set/Append/Remove)の応答。ウィザードの applyDocAppend は
handleOk が id で先に拾うので、ここへ来るのは編集直列(editReq)の分だけ。
-}
docEditOk : Api.Envelope -> Model -> ( Model, Effect )
docEditOk env model =
    if Just env.id == model.editReq then
        case D.decodeValue (D.field "text" D.string) env.body of
            Ok newText ->
                afterDocEditResponse newText model

            Err _ ->
                ( { model | editReq = Nothing, pendingEdits = [], notice = Just "編集応答が読めませんでした" }
                , Effect.none
                )

    else
        ( model, Effect.none )


{-| 文書編集(単発・バッチ共通)の応答後処理: 本文差し替え → 積まれた編集を
順に流す(編集が空なら待たせた改名) → プレビュー取り直し。
-}
afterDocEditResponse : String -> Model -> ( Model, Effect )
afterDocEditResponse newText model =
    let
        m1 =
            withDoc model.current
                newText
                { model
                    | editReq = Nothing
                    , dirty = newText /= model.openedText
                }

        ( m2, editFx ) =
            case ( m1.pendingEdits, m1.pendingRename ) of
                ( next :: rest, _ ) ->
                    sendEdit next { m1 | pendingEdits = rest }

                ( [], Just req ) ->
                    sendRename req { m1 | pendingRename = Nothing }

                ( [], Nothing ) ->
                    ( m1, Effect.none )

        ( m3, previewFx ) =
            requestPreview m2

        -- 肖像パスの打ち替えで新しい ui.json が要るようになった時のため
        ( m4, portraitFx ) =
            requestPortraits m3
    in
    scheduleAutosave ( m4, Effect.batch [ editFx, previewFx, portraitFx ] )


{-| 改名が本文に入った瞬間の追従: 選択を新 id へ・完了の文言。
選択が改名した本人でない時(往復中に選び直した等)は触らない。
-}
followRename : Model -> Model
followRename model =
    case model.renameInflight of
        Just inflight ->
            { model
                | renameInflight = Nothing
                , entrySel =
                    if
                        (model.sectionKey == Just inflight.req.sectionKey)
                            && (model.entrySel == Just (ByKey inflight.req.oldId))
                    then
                        Just (ByKey inflight.req.newId)

                    else
                        model.entrySel
                , notice =
                    Just
                        ("改名しました: "
                            ++ inflight.req.oldId
                            ++ " → "
                            ++ inflight.req.newId
                            ++ "(参照 "
                            ++ String.fromInt inflight.refCount
                            ++ " 箇所を書き換え)"
                        )
            }

        Nothing ->
            model


{-| texture 欄を持つスキーマが届いた時だけ project.json を取りに行く
(候補のドロップダウンの素)。無条件に取ると起動の封筒列が全プロジェクトで
1 本増えるので、要る時だけにする。
-}
requestTexturesIfNeeded : Schema.Schema -> Model -> ( Model, Effect )
requestTexturesIfNeeded schema model =
    if schemaUsesTexture schema && model.texturesReq == Nothing then
        let
            ( m1, fx ) =
                request "getFile" (E.object [ ( "path", E.string "project.json" ) ]) model
        in
        ( { m1 | texturesReq = Just m1.reqCounter }, fx )

    else
        ( model, Effect.none )


schemaUsesTexture : Schema.Schema -> Bool
schemaUsesTexture schema =
    schema.sections
        |> List.any (\( _, section ) -> section.fields |> List.any (\( _, f ) -> typeUsesTexture f.type_))


typeUsesTexture : Schema.FieldType -> Bool
typeUsesTexture type_ =
    case type_ of
        Schema.TTexture ->
            True

        Schema.TList inner ->
            typeUsesTexture inner

        Schema.TRecord fields ->
            fields |> List.any (\( _, f ) -> typeUsesTexture f.type_)

        _ ->
            False


{-| project.json から textures manifest の名前一覧(name)を引く。
形が違う・無いは候補ゼロ(自由入力だけになる)へ倒す。
-}
texturesFrom : String -> List String
texturesFrom content =
    D.decodeString (D.field "textures" (D.list (D.field "name" D.string))) content
        |> Result.withDefault []


{-| 往復中に文書が進んでいたら、応答を受けた足で取り直す。 -}
resendPreviewIfStale : Model -> ( Model, Effect )
resendPreviewIfStale model =
    if model.previewStale then
        requestPreview { model | previewStale = False }

    else
        ( model, Effect.none )


{-| 封筒 ok:false(fetch 失敗・HTTP エラー)。body は {message}。 -}
handleErr : Api.Envelope -> Model -> ( Model, Effect )
handleErr env model =
    let
        message =
            D.decodeValue (D.field "message" D.string) env.body
                |> Result.withDefault "不明なエラー"
    in
    case wizardFailed env message model of
        Just result ->
            result

        Nothing ->
            case crossRunErr env message model of
                Just result ->
                    result

                Nothing ->
                    handleErrByKind env message model


handleErrByKind : Api.Envelope -> String -> Model -> ( Model, Effect )
handleErrByKind env message model =
    case env.kind of
        "health" ->
            ( { model | screen = NoServer }, Effect.none )

        "projects" ->
            ( { model | picker = updatePicker (\p -> { p | error = Just message }) model }, Effect.none )

        "selectProject" ->
            -- エラーを見せたうえで候補を取り直す — 消えたプロジェクトはサーバが
            -- 一覧から外すので、開けなかった項目はその場で候補から消える
            request "projects"
                (E.object [])
                { model | picker = updatePicker (\p -> { p | busy = Nothing, error = Just message }) model }

        "genesisFamilies" ->
            -- 旧サーバ(404 等)。従来のプリセット入力に倒す(fail-open)
            ( { model | newGame = NewGame.familiesUnavailable model.newGame }, Effect.none )

        "promptGenesis" ->
            -- 404(旧サーバ)は「準備中」、400 は日本語の理由をその場に出す
            if String.contains "404" message then
                ( { model | newGame = NewGame.genesisPromptFailed "準備中 — このサーバはまだ対応していません" model.newGame }, Effect.none )

            else
                ( { model | newGame = NewGame.genesisPromptFailed message model.newGame }, Effect.none )

        "promptExtend" ->
            -- 404(旧サーバ)は「準備中」、400 は日本語の理由をその場に出す
            if String.contains "404" message then
                ( { model | atelier = Atelier.extendPromptFailed "準備中 — このサーバはまだ対応していません" model.atelier }, Effect.none )

            else
                ( { model | atelier = Atelier.extendPromptFailed message model.atelier }, Effect.none )

        "projectNew" ->
            -- 404(旧サーバ)は「準備中」に倒す。400/409 は日本語の理由をその場に出す
            if String.contains "404" message then
                ( { model | newGame = NewGame.unavailable model.newGame }, Effect.none )

            else
                ( { model | newGame = NewGame.createFailed message model.newGame }, Effect.none )

        "projectNewLog" ->
            -- ログ口が無い・落ちた。回し続けても仕方ないので止める(準備中に倒す)
            ( { model | newGame = NewGame.unavailable model.newGame }, Effect.none )

        "getFile" ->
            if Just env.id == model.schemaReq then
                -- スキーマが無いのは普通のこと(生テキスト編集は常に生きている)
                ( { model | schemaReq = Nothing, schemaState = SchemaMissing }, Effect.none )

            else if Just env.id == model.texturesReq then
                -- project.json が読めなくても編集は続く(候補が出ないだけ)
                ( { model | texturesReq = Nothing }, Effect.none )

            else
                case crossFailApply env.id model.crossSlots of
                    Just slots ->
                        ( { model | crossSlots = slots }, Effect.none )

                    Nothing ->
                        case dashFailApply env.id message model.dashboard of
                            Just dash ->
                                ( { model | dashboard = Just dash }, Effect.none )

                            Nothing ->
                                -- 「開けません」は本文(loadReq)の失敗だけ。追い越された
                                -- 古い要求(開き直したファイルのスキーマ等)の失敗を
                                -- 赤エラーにすると、開けているのに開けない顔になる
                                if Just env.id == model.loadReq then
                                    ( { model | loadReq = Nothing, notice = Just ("開けません: " ++ message) }, Effect.none )

                                else
                                    ( model, Effect.none )

        "putFile" ->
            ( { model | putReq = Nothing, savingText = Nothing, notice = Just ("保存に失敗: " ++ message) }
            , Effect.none
            )

        "applyDocEdit" ->
            ( { model | editReq = Nothing, pendingEdits = [], notice = Just ("編集を反映できません: " ++ message) }
            , Effect.none
            )

        "applyDocAppend" ->
            ( { model | editReq = Nothing, pendingEdits = [], notice = Just ("追加を反映できません: " ++ message) }
            , Effect.none
            )

        "applyDocRemove" ->
            ( { model | editReq = Nothing, pendingEdits = [], notice = Just ("削除を反映できません: " ++ message) }
            , Effect.none
            )

        "applyDocEdits" ->
            -- バッチは全成功か全失敗(docEdit 側が途中で例外)なので、失敗 = 文書は無傷
            ( { model
                | editReq = Nothing
                , pendingEdits = []
                , renameInflight = Nothing
                , notice =
                    Just
                        ((if model.renameInflight /= Nothing then
                            "改名できません: "

                          else
                            "編集を反映できません: "
                         )
                            ++ message
                        )
              }
            , Effect.none
            )

        "activeDocs" ->
            -- ファイルが無い(ゲームが書いていない)のは普通のこと。
            -- 印を消すだけで何も言わない(fail-open)
            if Just env.id == model.activeReq then
                ( { model | activeReq = Nothing, activeDocs = Dict.empty }, Effect.none )

            else
                ( model, Effect.none )

        "spriteColors" ->
            -- 実色表が取れないのは致命ではない(仮色で編集は続く)。
            -- 旧サーバの 404 もここに来るので、赤エラーは出さない(fail-open)
            if Just env.id == model.spriteColorsReq then
                ( { model | spriteColorsReq = Nothing }, Effect.none )

            else
                ( model, Effect.none )

        "previewItems" ->
            previewErr env message model

        "previewHitbox" ->
            previewErr env message model

        "previewFx" ->
            previewErr env message model

        "previewUi" ->
            case Dict.get env.id model.portraitReqs of
                Just path ->
                    ( { model
                        | portraitReqs = Dict.remove env.id model.portraitReqs
                        , portraits = Dict.insert path (PortraitFailed message) model.portraits
                      }
                    , Effect.none
                    )

                Nothing ->
                    previewErr env message model

        "runningGames" ->
            -- 起動中の問い合わせが失敗しても致命ではない(バッジが出ないだけ)。
            -- サーバが一瞬繋がらないだけの定期ポーリングで赤エラーを出さない
            ( model, Effect.none )

        "journeyState" ->
            -- エンドポイント未実装のサーバでも赤エラーは出さない(「準備中」の 1 枚へ)
            ( { model | journey = Journey.failed message }, Effect.none )

        "journeyChanges" ->
            -- 404 = この口を持たないサーバ。実況もモーダルも出さないだけ(fail-open)
            if String.contains "404" message then
                ( { model
                    | changesAvailable = False
                    , changesBaking = False
                    , changesModal = Nothing
                  }
                , Effect.none
                )

            else if model.changesModal == Just ChangesLoading then
                -- 開こうとした矢先の失敗だけは理由を告げる(押しっぱなしにしない)
                showToast ("見比べを開けませんでした — " ++ message)
                    { model | changesModal = Nothing }

            else
                -- 定期便の一時的な失敗。次の拍がまた試す
                ( model, Effect.none )

        "journeyChangesSeen" ->
            -- 既読が付けられなくても見る分には困らない。提案だけ最新へ
            ( model, requestInfo "journeyState" )

        "galleryList" ->
            -- 一覧が取れないならモーダルは畳み、ミニプレイヤーは空の一言へ
            -- (404 の旧サーバも同じ。パネル自体は出したまま — 起動は生きている)
            ( { model | scenes = Nothing, miniScenes = [] }, Effect.none )

        "atelierCandidates" ->
            -- エンドポイント未実装のサーバ(404 等)。候補ゾーンを出さないだけで
            -- 調整(エディタ)は生きる(fail-open)
            ( { model | atelier = Atelier.candidatesFailed model.atelier }, Effect.none )

        "atelierSlots" ->
            -- エンドポイント未実装のサーバ(404 等)。AI カードが準備中になるだけ
            ( { model | atelier = Atelier.slotsFailed model.atelier }, Effect.none )

        "atelierArchive" ->
            -- エンドポイント未実装のサーバ(404 等)。アーカイブの入口が
            -- 出ないだけで、候補選びも調整も生きる(fail-open)
            ( { model | atelier = Atelier.archiveFailed model.atelier }, Effect.none )

        "atelierArchiveAdd" ->
            -- 409(同名あり)等。⏳ を戻してから理由をそのまま告げる
            showToast ("アーカイブへ送れませんでした — " ++ Atelier.cleanReason message)
                { model | atelier = Atelier.archiveSettled model.atelier }

        "atelierRestore" ->
            -- 409(同じ名前の候補が既にある)等。⏳ を戻してから理由を告げる
            showToast ("候補に戻せませんでした — " ++ Atelier.cleanReason message)
                { model | atelier = Atelier.archiveSettled model.atelier }

        "promptAtelier" ->
            -- 理由(日本語)はボタンの近くに出す。生のエラー行は見せない
            ( { model | atelier = Atelier.promptFailed message model.atelier }, Effect.none )

        "atelierCopy" ->
            -- 409(名前衝突)は次の空き番でもう一度。その他はトーストで理由を告げる
            if String.contains "409" message then
                case Atelier.copyRetry model.atelier of
                    ( atelier, Just retry ) ->
                        request "atelierCopy"
                            (E.object [ ( "slot", E.string retry.slot ), ( "name", E.string retry.name ) ])
                            { model | atelier = atelier }

                    ( atelier, Nothing ) ->
                        showToast ("複製できませんでした — " ++ message) { model | atelier = atelier }

            else
                showToast ("複製できませんでした — " ++ message) { model | atelier = Atelier.copyFailed model.atelier }

        "copyClipboard" ->
            showToast "コピーできませんでした(手で選択してコピーしてください)" model

        "gameStatus" ->
            -- 状態が取れないのは致命ではない(採用前に起動の案内が挟まるだけ)
            ( model, Effect.none )

        "gameStart" ->
            -- ボタンを戻してから理由を告げる(押せないまま残さない)
            showToast ("ゲームを起動できませんでした — " ++ message)
                { model | atelier = Atelier.gameStartFailed model.atelier }

        "gameLog" ->
            -- ログ口が無い・落ちた。回し続けても仕方ない(status 側が真実を教える)
            ( { model | atelier = Atelier.gameStartFailed model.atelier }, Effect.none )

        "runnerLog" ->
            -- ログ口が無いサーバでも進捗パネルは一言だけで生きる(fail-open)。
            -- ポーリングは baking が終われば自然に止まる
            ( model, Effect.none )

        "promoteCandidate" ->
            -- 400 の理由(日本語)はボタンの近くに出す。生のエラー行は見せない
            ( { model | atelier = Atelier.promoteFailed message model.atelier }, Effect.none )

        _ ->
            ( { model | notice = Just message }, Effect.none )


{-| debug/active-docs.json の中身。値は 1 本(文字列)でも列でも同じ形に読む。 -}
activeDocsDecoder : D.Decoder (Dict.Dict String (List String))
activeDocsDecoder =
    D.field "active"
        (D.dict (D.oneOf [ D.map List.singleton D.string, D.list D.string ]))


previewErr : Api.Envelope -> String -> Model -> ( Model, Effect )
previewErr env message model =
    if Just env.id == model.previewReq then
        -- 通信失敗でも編集は止めない(枠に理由を出すだけ)
        resendPreviewIfStale
            { model | previewReq = Nothing, preview = PreviewFailed message }

    else
        ( model, Effect.none )



-- 更新の部品


request : String -> E.Value -> Model -> ( Model, Effect )
request kind payload model =
    let
        id =
            model.reqCounter + 1
    in
    ( { model | reqCounter = id }
    , Effect.SendApi { id = id, kind = kind, payload = payload }
    )


{-| 開いているファイルをディスクから取り直す。打ちかけ・保存待ち・競合の印は畳む。
409 のダイアログからも手の「読み直す」からも同じ道を通す(道が 2 本あると片方が腐る)。
-}
reloadCurrent : Model -> ( Model, Effect )
reloadCurrent model =
    case model.current of
        Just path ->
            let
                ( m1, cmd ) =
                    request "getFile"
                        (E.object [ ( "path", E.string path ) ])
                        { model
                            | conflict = Nothing
                            , staleMtime = Nothing
                            , savingText = Nothing
                            , activeDraft = Nothing
                        }
            in
            ( { m1 | loadReq = Just m1.reqCounter }, cmd )

        Nothing ->
            ( { model | conflict = Nothing, staleMtime = Nothing, savingText = Nothing }, Effect.none )


{-| GET /changes の files(パス → mtime ミリ秒)。token は使わない — 見たいのは
「開いているファイルが変わったか」の 1 点だけなので、集計より個別の方が素直。
-}
diskMtimesDecoder : D.Decoder (Dict.Dict String Int)
diskMtimesDecoder =
    D.field "files" (D.dict (D.map round D.float))


{-| 読み取り専用(ホームの提案・知らせ等)の封筒。応答は kind で受けるので、
古い応答を id で捨てる採番(reqCounter)を進めない — 既存フロー(ファイルを
開く・保存等)の封筒 id 並びを乱さないため。id 0 はどの往復とも衝突しない。
-}
requestInfo : String -> Effect
requestInfo kind =
    Effect.SendApi { id = 0, kind = kind, payload = E.object [] }


{-| 読み込み中の見比べモーダルだけ畳む(場面を見ている最中は触らない)。 -}
closeIfLoading : Maybe ChangesModal -> Maybe ChangesModal
closeIfLoading modal =
    if modal == Just ChangesLoading then
        Nothing

    else
        modal


{-| GET /gallery/list の gallery 節から場面名だけ引く(「全場面を見る」用)。
節が欠けたサーバでも空で通す(fail-open)。
-}
scenesDecoder : D.Decoder (List String)
scenesDecoder =
    D.oneOf
        [ D.field "gallery" (D.list (D.field "name" D.string))
        , D.succeed []
        ]


{-| タブ移動。開くたびに中身を取り直す(ホーム = 提案と知らせ)—
外で世界が進んでいても、開いた瞬間の最新を見せるため。
-}
gotoTab : Tab -> Model -> ( Model, Effect )
gotoTab tab model =
    case tab of
        HomeTab ->
            ( { model | tab = HomeTab }
            , Effect.batch
                (requestInfo "journeyState"
                    :: (if model.changesAvailable then
                            -- 知らせと描き出しの実況も開いた足で取る
                            [ requestInfo "journeyChanges" ]

                        else
                            []
                       )
                )
            )

        AtelierTab ->
            -- 開くたび入口へ戻す(提案からの直行はこの後にセクションを開く)。
            -- 候補選び(swap)の材料。無いサーバでは fail-open でゾーンごと出ない
            ( { model
                | tab = AtelierTab
                , atelier = Atelier.update Atelier.OpenLanding model.atelier |> Tuple.first
              }
            , Effect.batch
                ([ requestInfo "atelierCandidates"
                 , requestInfo "gameStatus"

                 -- 「つくる」の素材スロット(無いサーバでは AI カードが準備中になるだけ)
                 , requestInfo "atelierSlots"

                 -- アーカイブの中身(無いサーバでは入口ごと出ないだけ)
                 , requestInfo "atelierArchive"

                 -- ミニプレイヤーの場面チップ(全場面モーダルと同じ出どころ)
                 , requestInfo "galleryList"
                 ]
                    ++ (if model.changesAvailable then
                            -- 自動追従の種(知らせの最新)。口が無いサーバでは呼ばない
                            [ requestInfo "journeyChanges" ]

                        else
                            []
                       )
                )
            )



-- 画面


view : Model -> Html Msg
view model =
    case model.screen of
        Booting ->
            div [ HA.class "center-screen flex h-screen flex-col items-center justify-center gap-3 text-ink-soft" ] [ text "サーバへ接続中…" ]

        NoServer ->
            div [ HA.class "center-screen flex h-screen flex-col items-center justify-center gap-3 text-ink-soft" ]
                [ text "Studio の中の処理に繋がりません。アプリを開き直してください。"
                , button [ HA.class "btn", HE.onClick RetryClicked ] [ text "再試行" ]
                ]

        Picker ->
            -- root が入っている = 編集の状態を保持したまま開いた(戻る道を出す)
            viewPicker (model.root /= "") model.newGame model.picker

        Editing ->
            -- ミニプレイヤー(fixed)はアトリエタブの間だけ右下に居る
            div []
                [ case model.tab of
                    HomeTab ->
                        viewShell model (viewHome model)

                    AtelierTab ->
                        -- 入口から素材 / 調整 / 広げる / アーカイブへ。調整は従来の Doc エディタ
                        if Atelier.showLanding model.atelier then
                            viewShell model (Html.map AtelierMsg (Atelier.viewLanding model.atelier))

                        else if Atelier.showArchiver model.atelier then
                            viewShell model (Html.map AtelierMsg (Atelier.viewArchiver model.atelier))

                        else if Atelier.showExtend model.atelier then
                            viewShell model (Html.map AtelierMsg (Atelier.viewExtend model.atelier))

                        else if Atelier.showPicks model.atelier then
                            viewShell model (Html.map AtelierMsg (Atelier.view { base = model.serverBase, project = Api.projectKey model.root } model.atelier))

                        else
                            viewEditing model
                , if model.tab == AtelierTab then
                    div []
                        [ SceneView.view miniHandlers (miniState model)
                        , SceneView.viewZoom miniHandlers (miniState model)
                        ]

                  else
                    text ""
                ]


{-| ホームの外枠。題名とナビだけの静かな上部にする。
-}
viewShell : Model -> Html Msg -> Html Msg
viewShell model content =
    div [ HA.class "app flex h-screen flex-col" ]
        [ div [ HA.class "topbar flex h-9 shrink-0 items-center gap-3 border-b border-edge bg-panel px-3" ]
            [ span [ HA.class "title shrink-0 text-xs font-semibold" ] [ text model.title ]
            , viewNavTabs model.tab
            , viewSearchButton
            , viewProjectSwitch
            ]
        , content

        -- 検索はどの画面からでも開ける(ホームで開いた結果もエディタへ飛ぶ)
        , viewSearchPanel model
        ]


{-| 検索の入口。ショートカットだけだと、知らない人には無い機能と同じ。 -}
viewSearchButton : Html Msg
viewSearchButton =
    button
        [ HA.class "search-open btn btn-ghost btn-mini shrink-0"
        , HA.title "検索 ⌘⇧F"
        , HE.onClick SearchToggled
        ]
        [ text "🔍" ]


viewSearchPanel : Model -> Html Msg
viewSearchPanel model =
    SearchView.view
        { onQuery = SearchTyped
        , onReplacement = SearchReplacementTyped
        , onFile = SearchFileClicked
        , onHit = SearchHitClicked
        , onMove = SearchMoved
        , onActivate = SearchActivated
        , onReplaceRun = ReplaceRunClicked
        , onClose = SearchClosed
        }
        model.search


{-| 上のバーの右端 — プロジェクト選択画面へ戻る道(静かな一言)。
これが無いと編集に入った後、別のゲームへ移る術が無い(行き止まり)。
-}
viewProjectSwitch : Html Msg
viewProjectSwitch =
    button
        [ HA.class "project-switch ml-auto shrink-0 cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
        , HE.onClick ProjectPickerOpened
        ]
        [ text "プロジェクトを選ぶ" ]


viewNavTabs : Tab -> Html Msg
viewNavTabs tab =
    let
        item target label =
            button
                [ HA.classList
                    [ ( "nav-tab", True )
                    , ( "nav-tab-active", tab == target )
                    ]
                , HE.onClick (TabClicked target)
                ]
                [ text label ]
    in
    div [ HA.class "nav-tabs flex shrink-0 items-center gap-0.5" ]
        [ item HomeTab "ホーム"
        , item AtelierTab "アトリエ"
        ]


{-| ミニプレイヤー — 「ゲームの今」でなく「編集の今」を映す右下の枠
(アトリエタブの間だけ)。場面の絵は基準(golden/)の PNG。既定の「自動」は
知らせの最新の場面を追い、場面チップでピン留めできる。知らせの既読(seen)は
ここでは付けない — 通知と見比べモーダルの責務を侵さない。
-}
miniHandlers : SceneView.Handlers Msg
miniHandlers =
    { onToggle = MiniPlayerToggled
    , onScene = MiniSceneClicked
    , onZoomOpen = MiniZoomOpened
    , onZoomClosed = MiniZoomClosed
    , onStart = MiniStartClicked
    }


{-| 小窓が映すのに要る状態だけを渡す。走っているか(running)と起動しかけか
(starting)は、起動の事情を知っている Main が判じてから渡す。
-}
miniState : Model -> SceneView.State
miniState model =
    { open = model.miniPlayerOpen
    , swapNotice = model.miniSwapNotice
    , scenes = model.miniScenes
    , pin = model.miniPin
    , changes = model.miniChanges
    , baking = model.changesBaking
    , zoom = model.miniZoom
    , refresh = model.miniRefresh
    , serverBase = model.serverBase
    , root = model.root
    , running = projectGameRunning model
    , starting =
        case model.atelier.launch of
            Atelier.LaunchStarting _ ->
                True

            _ ->
                False
    }


{-| ホーム。提案のカードに、描き出しの実況と「全場面を見る」の入口を添える。
見比べ・全場面のモーダルもここにぶら下がる。
-}
viewHome : Model -> Html Msg
viewHome model =
    div [ HA.class "home flex min-h-0 flex-1 flex-col overflow-y-auto pb-6" ]
        [ Html.map JourneyMsg (Journey.view model.journey)
        , viewLaunchLine model
        , viewDrawingLine model
        , div [ HA.class "mt-auto flex justify-center pt-10" ]
            [ button
                [ HA.class "all-scenes-link cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
                , HE.onClick ScenesOpened
                ]
                [ text
                    (if model.scenes == Just ScenesLoading then
                        "⏳ 全場面を見る"

                     else
                        "全場面を見る"
                    )
                ]
            ]
        , viewChangesModal model
        , viewScenesModal model
        ]


{-| ゲーム起動の実況。起動を待つ間だけ、提案のカードの下に
アトリエと同じ形(インジケーター+一言+ログ末尾)で出す。
-}
viewLaunchLine : Model -> Html Msg
viewLaunchLine model =
    case Atelier.launchStarting model.atelier of
        Just info ->
            div [ HA.class "launch-line mx-auto mt-3 w-full max-w-lg px-4" ]
                [ Html.map AtelierMsg
                    (Progress.view
                        { message = "起動しています…(初回は少しかかります)"
                        , lines = info.lines
                        , failed = False
                        , expanded = info.expanded
                        , onToggle = Atelier.LaunchLogToggled
                        }
                    )
                ]

        Nothing ->
            text ""


{-| 描き出しの実況。エンジンが全場面を出力し直している間だけ、
提案のカードの下に小さく出す。
-}
viewDrawingLine : Model -> Html Msg
viewDrawingLine model =
    if model.changesBaking && model.changesAvailable then
        div [ HA.class "drawing-line mx-auto mt-3 w-full max-w-lg px-4" ]
            [ div [ HA.class "flex items-center gap-2 text-[11px] text-ink-faint" ]
                [ span [ HA.class "progress-spinner shrink-0", HA.attribute "aria-hidden" "true" ] []
                , text "全場面の絵を描き直しています…"
                ]
            ]

    else
        text ""


{-| 見比べモーダル。変わった場面を 1 件ずつ「前」と「今」で並べる。
見るだけ(承認は無い — 基準はサーバが既に追随させている)。
-}
viewChangesModal : Model -> Html Msg
viewChangesModal model =
    case model.changesModal of
        Nothing ->
            text ""

        Just ChangesLoading ->
            changesDialog
                [ div [ HA.class "flex items-center gap-2 text-xs text-ink-soft" ]
                    [ span [ HA.class "progress-spinner shrink-0", HA.attribute "aria-hidden" "true" ] []
                    , text "知らせを読み込んでいます…"
                    ]
                ]
                [ button [ HA.class "btn", HE.onClick ChangesModalClosed ] [ text "閉じる" ] ]

        Just (ChangesReady info) ->
            case info.remaining of
                [] ->
                    text ""

                current :: rest ->
                    let
                        position =
                            info.total - List.length info.remaining + 1
                    in
                    changesDialog
                        [ div [ HA.class "mb-3 flex items-center gap-2" ]
                            [ span [ HA.class "badge bg-accent/20 text-accent" ]
                                [ text (String.fromInt position ++ " / " ++ String.fromInt info.total) ]
                            , span [ HA.class "min-w-0 flex-1 truncate font-mono text-[11px] text-ink-soft", HA.title current.name ]
                                [ text current.name ]
                            , span [ HA.class "shrink-0 text-[11px] text-ink-faint" ]
                                [ text ("v" ++ String.fromInt current.ver) ]
                            ]
                        , div [ HA.class "flex flex-wrap gap-4" ]
                            [ changePane "前" (SceneView.galleryImageUrl model.serverBase model.root "golden/archive" (baseName current.before))
                            , changePane "今"
                                (SceneView.galleryImageUrl model.serverBase model.root "golden" (baseName current.after)
                                    -- 中身が入れ替わるファイルなので v でキャッシュを避ける
                                    ++ ("&t=" ++ String.fromInt current.ver)
                                )
                            ]
                        ]
                        [ if List.isEmpty rest then
                            button [ HA.class "btn btn-primary", HE.onClick ChangesModalClosed ] [ text "閉じる" ]

                          else
                            button [ HA.class "btn btn-primary", HE.onClick ChangesNextClicked ] [ text "次へ" ]
                        ]


changesDialog : List (Html Msg) -> List (Html Msg) -> Html Msg
changesDialog body footer =
    Html.node "sl-dialog"
        [ HA.class "changes-dialog"
        , HE.on "sl-request-close" (D.succeed ChangesModalClosed)
        , HA.attribute "label" "見た目が変わりました"
        , HA.attribute "open" ""
        , HA.attribute "style" "--width: 56rem"
        ]
        (body
            ++ [ div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ] footer ]
        )


changePane : String -> String -> Html msg
changePane label url =
    div [ HA.class "min-w-0 flex-1" ]
        [ div [ HA.class "mb-1 text-[11px] font-semibold text-ink-faint" ] [ text label ]
        , img [ HA.class "scene-shot w-full rounded border border-edge bg-well", HA.src url, HA.alt label ] []
        ]


{-| 全場面モーダル。gallery/ の絵を格子で眺めるだけ(操作は無い)。 -}
viewScenesModal : Model -> Html Msg
viewScenesModal model =
    let
        dialog body =
            Html.node "sl-dialog"
                [ HA.class "scenes-dialog"
                , HE.on "sl-request-close" (D.succeed ScenesClosed)
                , HA.attribute "label" "全場面"
                , HA.attribute "open" ""
                , HA.attribute "style" "--width: 64rem"
                ]
                (body
                    ++ [ div [ HA.attribute "slot" "footer", HA.class "flex justify-end" ]
                            [ button [ HA.class "btn", HE.onClick ScenesClosed ] [ text "閉じる" ] ]
                       ]
                )
    in
    case model.scenes of
        Nothing ->
            text ""

        Just ScenesLoading ->
            dialog
                [ div [ HA.class "flex items-center gap-2 text-xs text-ink-soft" ]
                    [ span [ HA.class "progress-spinner shrink-0", HA.attribute "aria-hidden" "true" ] []
                    , text "読み込んでいます…"
                    ]
                ]

        Just (ScenesReady names) ->
            dialog
                [ if List.isEmpty names then
                    div [ HA.class "text-xs text-ink-soft" ] [ text "まだ場面がありません。" ]

                  else
                    div [ HA.class "grid max-h-[70vh] grid-cols-[repeat(auto-fill,minmax(160px,1fr))] gap-3 overflow-y-auto" ]
                        (names
                            |> List.map
                                (\name ->
                                    div [ HA.class "overflow-hidden rounded border border-edge bg-panel" ]
                                        [ img
                                            [ HA.class "scene-shot block w-full bg-well"
                                            , HA.src (SceneView.galleryImageUrl model.serverBase model.root "gallery" name)
                                            , HA.alt name
                                            , HA.attribute "loading" "lazy"
                                            ]
                                            []
                                        , div [ HA.class "truncate px-2 py-1.5 font-mono text-[10px] text-ink-soft", HA.title name ]
                                            [ text name ]
                                        ]
                                )
                        )
                ]


{-| 候補プロジェクトの dir が、走っているゲームの cwd 一覧のどれかと同じ場所を
指すか。末尾スラッシュの有無をならしたうえで、片方がもう片方の末尾一致
(/ 区切りの境目)であれば同一とみなす — cwd とパス表記が微妙に違っても
(片方が絶対、片方に末尾 / 等)拾えるようにするため。
-}
isRunning : List String -> String -> Bool
isRunning runningCwds dir =
    let
        norm s =
            if String.endsWith "/" s then
                String.dropRight 1 s

            else
                s

        target =
            norm dir

        matches cwd =
            let
                c =
                    norm cwd
            in
            (c == target)
                || String.endsWith ("/" ++ target) c
                || String.endsWith ("/" ++ c) target
    in
    target /= "" && List.any matches runningCwds


viewPicker : Bool -> NewGame.Model -> PickerState -> Html Msg
viewPicker canReturn newGame picker =
    let
        busyLabel dir =
            if picker.busy == Just dir then
                " — 開いています…"

            else
                ""

        card entry =
            button
                [ HA.class "picker-card mb-1.5 block w-full cursor-pointer rounded border border-edge bg-panel px-3 py-2 text-left hover:border-accent/60 hover:bg-raised"
                , HE.onClick (ProjectClicked entry.dir)
                ]
                [ div [ HA.class "flex items-center gap-1.5" ]
                    [ span [ HA.class "text-xs text-ink" ] [ text (entry.title ++ busyLabel entry.dir) ]
                    , if isRunning picker.runningCwds entry.dir then
                        span
                            [ HA.class "running-badge shrink-0 rounded-sm bg-emerald-500/25 px-1.5 py-px text-[10px] font-medium text-emerald-300 ring-1 ring-emerald-400/50"
                            , HA.title "このプロジェクトのゲームが今起動中です"
                            ]
                            [ text "● 起動中" ]

                      else
                        text ""
                    ]
                , div [ HA.class "dir mt-0.5 font-mono text-[11px] text-ink-faint" ] [ text entry.dir ]
                ]

        section label entries =
            if List.isEmpty entries then
                []

            else
                div [ HA.class "picker-section mt-4 mb-1.5 text-[11px] text-ink-faint" ] [ text label ] :: List.map card entries
    in
    div [ HA.class "picker mx-auto mt-12 max-w-xl px-4" ]
        (List.concat
            [ [ div [ HA.class "mb-3 flex items-baseline gap-3" ]
                    [ h1 [ HA.class "text-sm font-semibold text-ink" ] [ text "プロジェクトを選ぶ" ]
                    , if canReturn then
                        -- 編集中から開いた時だけ。押しても何も再読み込みしない
                        button
                            [ HA.class "back-to-editing cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
                            , HE.onClick BackToEditingClicked
                            ]
                            [ text "← いまのゲームに戻る" ]

                      else
                        text ""
                    ]
              , div [ HA.class "picker-open-row mb-5 flex gap-2" ]
                    [ input
                        [ HA.class "field flex-1"
                        , HA.type_ "text"
                        , HA.placeholder "プロジェクトのパスを直接入力"
                        , HA.value picker.input
                        , HE.onInput PickerInput
                        ]
                        []
                    , button [ HA.class "btn", HE.onClick OpenPathClicked ] [ text "開く" ]
                    ]

              -- まっさらから(ひな形を写して新しいゲームを生む)
              , Html.map NewGameMsg (NewGame.view newGame)
              ]
            , case picker.error of
                Just message ->
                    [ Html.node "sl-alert"
                        [ HA.class "picker-error notice my-2"
                        , HA.attribute "variant" "danger"
                        , HA.attribute "open" ""
                        ]
                        [ text message ]
                    ]

                Nothing ->
                    []
            , case picker.projects of
                Nothing ->
                    [ div [ HA.class "picker-section mt-4 mb-1.5 text-[11px] text-ink-faint" ] [ text "候補を読み込み中…" ] ]

                Just projects ->
                    section "最近開いた" projects.recent
                        ++ section "見つかった" projects.found
            ]
        )


viewEditing : Model -> Html Msg
viewEditing model =
    case model.wizard of
        Just w ->
            div
                [ HA.classList
                    [ ( "app flex h-screen flex-col", True )

                    -- 掴んでいる間は画面ぜんぶを col-resize に(1px の帯から
                    -- 指が外れた瞬間にカーソルが戻ると、掴み損ねたように見える)
                    , ( "cursor-col-resize select-none", model.paneDrag /= Nothing )
                    ]
                ]
                [ viewTopbar model
                , viewWizard model w
                ]

        Nothing ->
            div [ HA.class "app flex h-screen flex-col" ]
                [ viewTopbar model

                -- 調整セクションの最上段 — 「← アトリエ」で入口へ戻れる。
                -- 全ファイル表示の間は見出しも「🗂 すべてのファイル」— 押した
                -- リンクの言葉と着地の見出しを一致させる
                , div [ HA.class "flex h-8 shrink-0 items-center border-b border-edge bg-panel px-3" ]
                    [ Html.map AtelierMsg
                        (Atelier.viewSectionTop
                            (if Atelier.showAllFiles model.atelier then
                                "🗂 すべてのファイル"

                             else
                                "⚙️ パラメータを変える"
                            )
                        )
                    ]

                -- 開いたファイルの道具は編集枠の直上に(対象のそばに道具を置く)
                , viewEditToolbar model
                , div [ HA.class "panes flex min-h-0 flex-1" ]
                    (viewFilePane model
                        :: viewPaneHandle LeftPane
                        :: (case model.dashboard of
                                -- ボードは中央+右の代わり。左の一覧は出したまま —
                                -- ファイルクリックがそのままボードの出口になる
                                Just dash ->
                                    [ viewDashboard model dash ]

                                Nothing ->
                                    if knobDoc model then
                                        -- つまみ系の Doc は常時 2 ペイン(モード切替を持たない)。
                                        -- 左 = フォーム、右 = 上プレビュー + 下 JSON
                                        viewKnobPanes model

                                    else
                                    case effectiveMode model of
                                        VisualMode ->
                                            case ( mapDocCurrent model, spriteDocCurrent model ) of
                                                -- マップは 1 枚のマップエディタで完結
                                                -- (道具・グリッド・パレットを内側に持つ)
                                                ( Just mdoc, _ ) ->
                                                    [ MapEditor.view MapMsg (viewMapInspector model) mdoc model.mapEd ]

                                                -- ドット絵は 1 枚のピクセルエディタで完結
                                                ( _, Just pdoc ) ->
                                                    [ Html.map PixelMsg (PixelEditor.view model.spriteColors pdoc model.pixel) ]

                                                _ ->
                                                    [ viewVisualCenter model
                                                    , viewPaneHandle RightPane
                                                    , viewVisualSide model
                                                    ]

                                        SplitMode ->
                                            [ viewEditorPane model
                                            , viewPaneHandle RightPane
                                            , viewFormPane model
                                            ]

                                        CodeMode ->
                                            [ viewEditorPane model ]
                           )
                    )

                -- 開いたファイルの境界の 1 行(素材なら②への行き来、調整値なら完結の一言)
                , viewRoleBoundary model
                , viewProblemBar model
                , case model.conflict of
                    Just _ ->
                        viewConflictDialog

                    Nothing ->
                        text ""
                , case model.pendingNav of
                    Just _ ->
                        viewDiscardDialog

                    Nothing ->
                        text ""
                , case model.addDialog of
                    Just dialog ->
                        viewAddDialog dialog

                    Nothing ->
                        text ""
                , case model.deleteConfirm of
                    Just confirm ->
                        viewDeleteDialog confirm

                    Nothing ->
                        text ""
                , case model.crossRename of
                    Just plan ->
                        viewCrossRenameDialog plan

                    Nothing ->
                        text ""
                , viewFileMenu model
                , viewSearchPanel model
                , case model.fileVerb of
                    Just dialog ->
                        FileVerbs.view
                            { onTyped = VerbTyped, onConfirmed = VerbConfirmed, onCancelled = VerbCancelled }
                            (knownPaths model)
                            dialog

                    Nothing ->
                        text ""
                ]


{-| 触っている欄を右の JSON でも選択状態にする(2 ペインで JSON を出している時だけ)。
編集そのものではないので、失敗しても静かに何もしない。
-}
highlightJson : List Seg -> ( Model, Effect ) -> ( Model, Effect )
highlightJson path ( model, fx ) =
    if knobDoc model && model.jsonPaneOpen then
        let
            ( m1, hlFx ) =
                request "highlightJson"
                    (E.object
                        [ ( "text", E.string model.docText )
                        , ( "path", E.list encodeSeg path )
                        , ( "id", E.string jsonBoxId )
                        ]
                    )
                    model
        in
        ( m1, Effect.batch [ fx, hlFx ] )

    else
        ( model, fx )


{-| つまみ系の Doc(2 ペイン常設)か。判定は型だけ — 盤面(grid 欄)を持つ物と
ドット絵は自前の絵エディタが主役なので外れる。ファイル名は見ない。
-}
knobDoc : Model -> Bool
knobDoc model =
    case model.schemaState of
        SchemaReady schema ->
            DocKind.isKnob
                { schema = Just schema
                , isSprite = spriteDocCurrent model /= Nothing
                , supported = List.length (supportedSections schema)
                }

        _ ->
            False


{-| つまみ系 Doc の 2 ペイン。左はフォーム(従来の分割モードと同じ部品)、
右は上が見え方(絵 / 音)・下が生 JSON。

JSON を畳んでも右ペインは畳まない — 畳むのは「JSON の箱」だけで、見え方は
残ってその高さを受け取る。見せる物が何も無い Doc でだけ、空のペインを残さず
細い縦タブに畳む(開き直す入口は必ず右側にも置く)。

-}
viewKnobPanes : Model -> List (Html Msg)
viewKnobPanes model =
    let
        preview =
            viewKnobPreview model
    in
    viewKnobFormPane model
        :: (case ( preview, model.jsonPaneOpen ) of
                -- 見せる物も JSON も無い = 空のペインになる。縦タブだけ残す
                ( Nothing, False ) ->
                    [ viewJsonTab ]

                _ ->
                    [ viewPaneHandle RightPane
                    , div
                        [ HA.class "pane-side flex shrink-0 flex-col overflow-hidden border-l border-edge bg-panel"
                        , HA.style "width" (String.fromInt model.rightPaneW ++ "px")
                        ]
                        (viewKnobPreviewBox model preview ++ [ viewKnobJsonBox model ])
                    ]
           )


{-| 見え方の枠。JSON を畳んでいる間は、空いた高さをこちらが受け取る。 -}
viewKnobPreviewBox : Model -> Maybe (List (Html Msg)) -> List (Html Msg)
viewKnobPreviewBox model preview =
    case preview of
        Nothing ->
            []

        Just content ->
            [ div
                [ HA.classList
                    [ ( "knob-preview flex flex-col overflow-y-auto border-b border-edge p-3", True )

                    -- 音の編集器は縦の場所が要る(波形・帯・2D パッド)。
                    -- 高さを与えないと中の grid が潰れて絵が線になる
                    , ( "min-h-[19rem] flex-1", tallPreview model || not model.jsonPaneOpen )
                    , ( "shrink-0", not (tallPreview model || not model.jsonPaneOpen) )
                    ]
                ]
                content
            ]


{-| 生 JSON の箱。見出しは畳んでいる間も残す — 開き直す入口を、上の道具列まで
戻らずに右ペインの中で押せるように。
-}
viewKnobJsonBox : Model -> Html Msg
viewKnobJsonBox model =
    div
        [ HA.classList
            [ ( "knob-json flex flex-col", True )
            , ( "min-h-0 flex-1", model.jsonPaneOpen )
            , ( "shrink-0", not model.jsonPaneOpen )
            ]
        ]
        (button
            [ HA.class "json-head flex w-full shrink-0 cursor-pointer items-center gap-1.5 px-3 py-1 text-left text-[10px] tracking-[0.14em] text-ink-faint uppercase hover:text-ink"
            , HA.title
                (if model.jsonPaneOpen then
                    "JSON を畳む(⌘J / Ctrl+J)"

                 else
                    "JSON を出す(⌘J / Ctrl+J)"
                )
            , HE.onClick JsonPaneToggled
            ]
            [ span [ HA.class "flex-1" ] [ text "JSON" ]
            , span []
                [ text
                    (if model.jsonPaneOpen then
                        "✕"

                     else
                        "▸"
                    )
                ]
            ]
            :: (if model.jsonPaneOpen then
                    [ viewJsonBox model ]

                else
                    []
               )
        )


{-| 見せる物が何も無い Doc で JSON を畳んだ時の細い縦タブ(開き直す入口)。 -}
viewJsonTab : Html Msg
viewJsonTab =
    button
        [ HA.class "json-tab flex w-6 shrink-0 cursor-pointer items-center justify-center border-l border-edge bg-panel text-[10px] tracking-[0.14em] text-ink-faint uppercase hover:text-ink"
        , HA.style "writing-mode" "vertical-rl"
        , HA.title "JSON を出す(⌘J / Ctrl+J)"
        , HE.onClick JsonPaneToggled
        ]
        [ text "JSON" ]


{-| プレビュー枠に縦の場所が要るか(音の編集器を出す時)。 -}
tallPreview : Model -> Bool
tallPreview model =
    sfxConfigOf model /= Nothing


{-| 右上の「見え方」。音の文書なら ▶(選んでいる音)、絵のある文書なら
これまでのプレビュー札。どちらでもなければ静かな一言。
-}
viewKnobPreview : Model -> Maybe (List (Html Msg))
viewKnobPreview model =
    case sfxConfigOf model of
        -- 効果音のつまみ: 波形と ▶ を持つ既存の部品がそのまま preview になる
        Just config ->
            Just [ Html.map SfxMsg (SfxEditor.view config model.sfx) ]

        Nothing ->
            case DocKind.playableSound model.sounds (selectedSoundName model) of
                Just name ->
                    Just [ viewSoundPlayer model name ]

                Nothing ->
                    case viewPreviewCard model of
                        [] ->
                            -- 絵も音も無い文書。空の枠を出さない(呼び側が畳む)
                            Nothing

                        cards ->
                            Just cards


{-| 選んでいる場所の名前(音の宣言と突き合わせる材料)。
一覧のエントリ名か、単一セクションのキー。
-}
selectedSoundName : Model -> Maybe String
selectedSoundName model =
    case model.entrySel of
        Just (ByKey name) ->
            Just name

        _ ->
            currentSection model |> Maybe.map Tuple.first


{-| 焼いてある WAV を鳴らす札。焼き直しが要る間は、その旨を添える。 -}
viewSoundPlayer : Model -> String -> Html Msg
viewSoundPlayer model name =
    div [ HA.class "sound-player flex flex-wrap items-center gap-1.5" ]
        [ if model.playingSound == Just name then
            button
                [ HA.class "sound-stop btn btn-mini", HE.onClick SoundStopClicked ]
                [ text "■ 止める" ]

          else
            button
                [ HA.class "sound-play btn btn-mini"
                , HA.title "焼いてある WAV をそのまま鳴らす(全体の音量つまみは掛かりません)"
                , HE.onClick (SoundPlayClicked name)
                ]
                [ text ("▶ " ++ name) ]
        , if model.dirty then
            span
                [ HA.class "rebake-badge shrink-0 rounded-sm bg-amber-500/20 px-1.5 py-px text-[10px] text-amber-300 ring-1 ring-amber-400/40"
                , HA.title "つまみを変えました。ゲーム側で焼き直すまで、鳴るのは前の音です"
                ]
                [ text "焼き直しが要る" ]

          else
            text ""
        ]


{-| 生 JSON。分割モードのテキストと同じ部品(書き戻しも同じ経路)。 -}
viewJsonBox : Model -> Html Msg
viewJsonBox model =
    textarea
        [ HA.class "json-box min-h-0 flex-1 resize-none border-none bg-app p-3 font-mono text-[11px] leading-relaxed text-ink focus:outline-none"
        , HA.id jsonBoxId
        , HA.value model.docText
        , HA.spellcheck False
        , HE.onInput DocChanged
        ]
        []


jsonBoxId : String
jsonBoxId =
    "knob-json-box"


{-| kind value/field(セクション = 単一の値)を行フォーム機構に乗せる変換。
「文書全体を entry・セクション名をフィールド名」と見なすと、既存の行
(draft・min/max・スライダー)がそのまま使え、書き戻し先も文書直下の
[セクション名] になる。同じタブに束ねた複数の値も、まとめて 1 枚の表になる。
-}
valueSectionsAsRecord : List ( String, Schema.Field ) -> Schema.Section
valueSectionsAsRecord fields =
    { kind = Schema.RecordKind, label = Nothing, help = Nothing, group = Nothing, widget = Nothing, fields = fields }


{-| ペイン境界のつまみ(縦帯)。ドラッグで隣のペインの幅を変える。
preventDefault はドラッグ中のテキスト選択を出さないため。
-}
viewPaneHandle : PaneSide -> Html Msg
viewPaneHandle side =
    div
        [ HA.class "pane-handle w-1 shrink-0 cursor-col-resize transition-colors hover:bg-accent/50"
        , HE.custom "mousedown"
            (D.map
                (\x -> { message = PanePressed side x, stopPropagation = True, preventDefault = True })
                (D.field "clientX" D.float)
            )
        ]
        []


{-| 実際に画面へ出すモード。ビジュアルは「スキーマと JSON からテーブル+フォームが
組める」ことが前提なので、組めない間は分割(テキスト+右ペイン)へ落ちる —
テキストの逃げ道を開きつつ、右ペインの案内(スキーマ欠けの事情・kind の案内)も残す。
スキーマ探索中だけはビジュアルのまま待つ(開くたびにテキストが一瞬見えるのを防ぐ)。
-}
effectiveMode : Model -> ViewMode
effectiveMode model =
    case ( model.viewMode, model.current ) of
        ( VisualMode, Just _ ) ->
            -- ドット絵・マップはスキーマ(JSON-Schema 形式 = SchemaForeign)でなく
            -- 文書の形で判定する — 読めれば専用エディタがビジュアルの主役
            if mapDocCurrent model /= Nothing then
                VisualMode

            else
            case ( spriteDocCurrent model, model.schemaState, parsedDoc model ) of
                ( Just _, _, _ ) ->
                    VisualMode

                ( _, SchemaReady schema, Just _ ) ->
                    -- 出せるセクションが 1 つも無い(全部フォーム未対応)なら
                    -- テキストが主役の分割へ
                    if List.isEmpty (supportedSections schema) then
                        SplitMode

                    else
                        VisualMode

                ( _, SchemaLoading, _ ) ->
                    VisualMode

                _ ->
                    SplitMode

        ( mode, _ ) ->
            mode


{-| 画面全体のバー。ゲーム名・ナビと、画面共通の通知トーストだけ。
開いたファイルの道具は編集枠側(viewEditToolbar)にある。
-}
viewTopbar : Model -> Html Msg
viewTopbar model =
    div [ HA.class "topbar flex h-9 shrink-0 items-center gap-3 border-b border-edge bg-panel px-3" ]
        [ span [ HA.class "title shrink-0 text-xs font-semibold" ] [ text model.title ]
        , viewNavTabs model.tab
        , viewSearchButton
        , viewProjectSwitch
        , case model.notice of
            Just message ->
                -- 右下(問題バーの上)に出す — 右上は右ペインのフォームに被って邪魔
                div [ HA.class "pointer-events-none fixed right-3 bottom-10 z-50" ]
                    [ Html.node "sl-alert"
                        [ HA.class "notice"
                        , HA.attribute "variant"
                            (if String.startsWith "保存しました" message || String.startsWith "改名しました" message then
                                "success"

                             else
                                "danger"
                            )
                        , HA.attribute "open" ""
                        ]
                        [ text message ]
                    ]

            Nothing ->
                text ""
        ]


{-| 開いたファイルの道具(モード切替・ファイル名・ライブ反映・保存)。
編集枠の直上に置く。高さは固定せず、狭い幅では折り返して収める。
-}
viewEditToolbar : Model -> Html Msg
viewEditToolbar model =
    div [ HA.class "edit-toolbar flex min-h-9 shrink-0 flex-wrap items-center gap-x-3 gap-y-1 border-b border-edge bg-panel px-3 py-1" ]
        [ if knobDoc model then
            -- つまみ系はモードを持たない(常時 2 ペイン)。代わりに JSON の開閉だけ置く
            viewJsonToggle model

          else
            viewModeSeg model
        , case currentGroup model of
            Just group ->
                span [ HA.class "group-badge badge shrink-0 bg-accent/15 text-accent" ]
                    [ text (Maybe.withDefault group.id group.title) ]

            Nothing ->
                text ""
        , span [ HA.class "path truncate font-mono text-[11px] text-ink-faint" ] [ text (Maybe.withDefault "(ファイル未選択)" model.current) ]
        , case activeMismatch model of
            Just shownNames ->
                -- ライブ反映の迷子防止: 編集中のファイルが今ゲームの画面に出ていない。
                -- 左レールの「表示中」バッジと同系(琥珀の注意色)で揃える
                span
                    [ HA.class "active-warn shrink-0 rounded-sm bg-amber-500/20 px-1.5 py-px text-[11px] text-amber-300 ring-1 ring-amber-400/40" ]
                    [ text ("画面はこのファイルを表示していません(表示中: " ++ shownNames ++ ")") ]

            Nothing ->
                text ""
        , case model.staleMtime of
            -- 外で変わったのに手元に打ちかけがある。保存は 409 で弾かれるので
            -- 黙って潰れることは無いが、気付くのは早い方がよい
            Just _ ->
                span
                    [ HA.class "stale flex shrink-0 items-center gap-1.5 rounded-sm bg-amber-500/20 px-1.5 py-px text-[11px] text-amber-300 ring-1 ring-amber-400/40" ]
                    [ text "このファイルは外で変わりました"
                    , button
                        [ HA.class "cursor-pointer underline underline-offset-2"
                        , HE.onClick ReloadClicked
                        ]
                        [ text "読み直す" ]
                    , button
                        [ HA.class "cursor-pointer text-amber-300/70 hover:text-amber-300"
                        , HA.title "帯を畳む(保存しようとすると、それでも止まります)"
                        , HE.onClick StaleDismissed
                        ]
                        [ text "×" ]
                    ]

            Nothing ->
                text ""
        , if model.dirty then
            span [ HA.class "dirty flex shrink-0 items-center gap-1.5 text-[11px] text-ink-soft" ]
                [ span [ HA.class "inline-block h-1.5 w-1.5 rounded-full bg-accent" ] []
                , text "未保存"
                ]

          else
            text ""
        , viewUndoCount model
        , span [ HA.class "spacer flex-1" ] []
        , button
            [ HA.class "btn"
            , HA.title "ディスクの今の中身を取り直す(手元の打ちかけは捨てる)"
            , HA.disabled (model.current == Nothing)
            , HE.onClick ReloadClicked
            ]
            [ text "読み直す" ]
        , button
            [ HA.classList
                [ ( "live-toggle btn", True )
                , ( "bg-accent text-white hover:bg-accent", model.liveSave )
                ]
            , HA.title "ON の間、編集を自動保存して走るゲームに即反映する"
            , HE.onClick LiveToggled
            ]
            [ text
                (if model.liveSave then
                    "ライブ反映 ON"

                 else
                    "ライブ反映"
                )
            ]
        , button
            [ HA.class "btn btn-primary"
            , HE.onClick SaveClicked
            , HA.disabled (not model.dirty || model.current == Nothing || model.savingText /= Nothing)
            ]
            [ text
                (if model.savingText == Nothing then
                    "保存"

                 else
                    "保存中…"
                )
            ]
        ]


{-| 戻せる手数。0 の間は出さない — 何もしていない時に押せない物を置かない。
盤面・ドット絵が前面の時も出さない(その画面の ⌘Z は自前の履歴が持ち場で、
数が合わないため)。
-}
viewUndoCount : Model -> Html Msg
viewUndoCount model =
    let
        ( undoable, _ ) =
            EditHistory.depth model.history
    in
    if undoable == 0 || ownUndoFront model then
        text ""

    else
        button
            [ HA.class "undo-count btn btn-ghost btn-mini shrink-0 text-ink-faint hover:text-ink"
            , HA.title "元に戻す(⌘Z / Ctrl+Z)"
            , HE.onClick UndoPressed
            ]
            [ text ("↩ " ++ String.fromInt undoable) ]


{-| 右の JSON ペインの開閉。「常設が邪魔」への逃げ道を、道具の列に常に置く。 -}
viewJsonToggle : Model -> Html Msg
viewJsonToggle model =
    button
        [ HA.classList
            [ ( "json-toggle btn shrink-0", True )
            , ( "bg-accent text-white hover:bg-accent", model.jsonPaneOpen )
            ]
        , HA.title "右の JSON を出す / 畳む(⌘J / Ctrl+J)"
        , HE.onClick JsonPaneToggled
        ]
        [ text "⌨ JSON" ]


{-| モード切替(flix_ge_editor と同じ 3 連セグメント)。光るのは選択でなく
実際に出ているモード — スキーマ無しでコードに落ちている事実を隠さない。
-}
viewModeSeg : Model -> Html Msg
viewModeSeg model =
    let
        current =
            effectiveMode model

        modeButton mode labelText =
            button
                [ HA.classList
                    [ ( "mode-btn btn rounded-none border-0", True )
                    , ( "bg-accent text-white hover:bg-accent", current == mode )
                    ]
                , HE.onClick (ModeChosen mode)
                ]
                [ text labelText ]
    in
    span [ HA.class "mode-seg inline-flex shrink-0 divide-x divide-edge overflow-hidden rounded-sm border border-edge" ]
        [ modeButton VisualMode "ビジュアル"
        , modeButton SplitMode "分割"
        , modeButton CodeMode "コード"
        ]


{-| 開いているファイルの属するリソースグループ(ヘッダの現在地表示)。 -}
currentGroup : Model -> Maybe Api.ResourceGroup
currentGroup model =
    model.current
        |> Maybe.andThen
            (\path ->
                model.groups
                    |> List.filter (\g -> List.any (\f -> f.path == path) g.files)
                    |> List.head
            )


{-| このプロジェクトのゲームがいま走っているか(ミニプレイヤーの状態行と同じ判定)。 -}
projectGameRunning : Model -> Bool
projectGameRunning model =
    Maybe.withDefault (Journey.gameRunning model.journey) model.atelier.gameRunning


{-| いまゲームの画面に出ているパスの集まり(グループを問わない平集合)。
走っていない時は中身に関わらず空 — 止まったゲームの active-docs.json は
残るので、信じると「表示中」バッジが付きっぱなしになる。
-}
activePaths : Model -> List String
activePaths model =
    if projectGameRunning model then
        Dict.values model.activeDocs |> List.concat

    else
        []


{-| 開いているファイルの kind でゲームが別のファイルを表示中なら、その名前
(複数ならカンマ継ぎ)。active-docs が無い/kind に情報が無い時は言わない。
走っていない時も言わない(activePaths と同じ理由 — 残り香で騒がない)。
-}
activeMismatch : Model -> Maybe String
activeMismatch model =
    if not (projectGameRunning model) then
        Nothing

    else
        case ( model.current, currentGroup model ) of
            ( Just path, Just group ) ->
                case Dict.get group.id model.activeDocs of
                    Just (first :: rest) ->
                        if List.member path (first :: rest) then
                            Nothing

                        else
                            Just (String.join ", " (List.map baseName (first :: rest)))

                    _ ->
                        Nothing

            _ ->
                Nothing


baseName : String -> String
baseName path =
    String.split "/" path |> List.reverse |> List.head |> Maybe.withDefault path


{-| フォーム/エディタの下の境界の 1 行。開いたファイルが
素材(role:material の宣言)なら「素材を切り替える」への行き来リンク、
宣言された調整値(tuning)なら「値を変えるだけで完結」の一言(リンクなし)。
宣言外のファイル(すべてのファイル表示中に開ける)にはこの行自体を出さない。
題は宣言題(括弧前)だけを織り込む — 名詞を発明しない。
-}
viewRoleBoundary : Model -> Html Msg
viewRoleBoundary model =
    case model.current of
        Nothing ->
            text ""

        Just path ->
            case Atelier.materialTitleOf model.atelier path of
                Just title ->
                    let
                        count =
                            Atelier.slotCandidateCount model.atelier path
                    in
                    div [ HA.class "role-boundary flex min-h-8 shrink-0 items-center border-t border-edge bg-panel px-3" ]
                        [ if count > 0 then
                            button
                                [ HA.class "boundary-swap cursor-pointer text-[11px] text-accent hover:underline"
                                , HE.onClick (AtelierMsg Atelier.OpenPicks)
                                ]
                                [ text ("候補 " ++ String.fromInt count ++ " 件 — 別の" ++ title ++ "に切り替える →") ]

                          else
                            button
                                [ HA.class "boundary-swap cursor-pointer text-[11px] text-accent hover:underline"
                                , HE.onClick (AtelierMsg (Atelier.CreateForSlotClicked path))
                                ]
                                [ text ("別の" ++ title ++ "に切り替える(まず候補を作ります)→") ]
                        ]

                Nothing ->
                    case currentGroup model of
                        Just group ->
                            div [ HA.class "role-boundary flex min-h-8 shrink-0 items-center border-t border-edge bg-panel px-3" ]
                                [ span [ HA.class "boundary-tuning text-[11px] text-ink-faint" ]
                                    [ text
                                        (Atelier.titleBeforeParen (Maybe.withDefault group.id group.title)
                                            ++ "は値を変えるだけで完結します(切り替えるものはありません)"
                                        )
                                    ]
                                ]

                        Nothing ->
                            text ""


viewFilePane : Model -> Html Msg
viewFilePane model =
    let
        -- 宣言されたリソース(/resources)を見出し付きで先頭に。
        -- 宣言に無いファイル(旧 /files 由来)は既定では出さず、
        -- 「🗂 すべてのファイル」のトグルの間だけ「その他」に出す
        declared =
            declaredPaths model.groups

        others =
            List.filter (\p -> not (List.member p declared)) model.files

        showAll =
            Atelier.showAllFiles model.atelier

        -- ダッシュボード節(宣言がある時だけ)。グループ見出しの上に置く
        dashRows =
            if List.isEmpty model.dashboards then
                []

            else
                viewGroupHeading "ダッシュボード"
                    :: List.map
                        (viewDashboardRow (model.dashboard |> Maybe.map (\d -> d.decl.id)))
                        model.dashboards

        groupRows =
            model.groups
                |> List.concatMap
                    (\g ->
                        viewGroupHeadingFor g
                            :: List.map (viewFileRow model) (List.map .path g.files)
                    )

        -- 宣言が 1 件も無いプロジェクトに「その他」だけ出すと見出しが意味を
        -- 持たない(全部がその他)ので、その時はトグルを介さず素の一覧にする
        otherRows =
            if List.isEmpty others then
                []

            else if List.isEmpty model.groups then
                List.map (viewFileRow model) others

            else if showAll then
                viewGroupHeading "その他" :: List.map (viewFileRow model) others

            else
                []

        rows =
            dashRows ++ groupRows ++ otherRows

        -- 一覧の下の控えめなトグル。宣言も宣言外も両方ある時だけ意味を持つ
        filesToggle =
            if List.isEmpty model.groups || (List.isEmpty others && not showAll) then
                []

            else
                [ button
                    [ HA.class "files-toggle mx-3 mt-3 cursor-pointer text-left text-[11px] text-ink-faint hover:text-ink-soft"
                    , HE.onClick (AtelierMsg Atelier.FilesFilterToggled)
                    ]
                    [ text
                        (if showAll then
                            "宣言された素材だけに戻す"

                         else
                            "🗂 すべてのファイル"
                        )
                    ]
                ]
    in
    div
        [ HA.class "pane-files flex shrink-0 flex-col overflow-y-auto border-r border-edge bg-panel py-2"
        , HA.style "width" (String.fromInt model.leftPaneW ++ "px")
        ]
        (div [ HA.class "root px-3 pb-2 font-mono text-[10px] leading-relaxed break-all text-ink-faint" ] [ text model.root ]
            :: viewResourceWarnings model.resourceWarnings
            ++ (if List.isEmpty rows then
                    [ div [ HA.class "root px-3 pb-2 text-[11px] text-ink-faint" ] [ text "編集できる JSON が見つかりません" ] ]

                else
                    rows
               )
            ++ filesToggle
            ++ -- 手でファイルを作る道。すべてのファイル表示の中だけに置く
               -- (宣言がまだ無いプロジェクトでは素の一覧に出す — 最初の 1 個への道を塞がない)
               (if showAll || List.isEmpty model.groups then
                    [ button [ HA.class "new-resource btn mx-3 mt-3", HE.onClick WizardOpened ] [ text "+ 新しいファイル" ] ]

                else
                    []
               )
        )


{-| project.json の宣言と実ファイルのずれ(/resources の warnings)。0 件なら出さない。
握り潰すと「宣言したのに一覧に出ない」の原因を探せなくなるので、左レールの上に名指しで出す。
-}
viewResourceWarnings : List String -> List (Html Msg)
viewResourceWarnings warnings =
    if List.isEmpty warnings then
        []

    else
        [ div [ HA.class "resource-warnings mx-3 mb-2 rounded border border-danger/60 bg-danger/10 px-2 py-1.5" ]
            (warnings
                |> List.map
                    (\w -> div [ HA.class "text-[10px] leading-relaxed break-all text-danger" ] [ text w ])
            )
        ]


{-| ファイル行の頭に置く書類アイコン(currentColor で行の色に追従)。
「これは開けるファイル」を一目で示し、見出し(下の eyebrow)と区別する。 -}
fileIcon : Html Msg
fileIcon =
    Svg.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width "13"
        , SA.height "13"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.7"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.class "shrink-0 opacity-70"
        ]
        [ Svg.path [ SA.d "M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z" ] []
        , Svg.path [ SA.d "M14 3v5h5" ] []
        ]


{-| 見出し(グループの題)。クリックできる行と紛れないよう、字間を広げた小さな
eyebrow ラベルにする — 選択不可・上に間を空け、下のファイル行(アイコン付き)と役割を分ける。 -}
viewGroupHeading : String -> Html Msg
viewGroupHeading label =
    div [ HA.class "file-group select-none px-3 pt-3 pb-1 text-[10px] font-semibold uppercase tracking-[0.14em] text-ink-faint" ]
        [ text label ]


{-| 宣言グループの見出し。「+ 新規」は "*" を持つ宣言(何本でも置ける物)にだけ。
1 本しか置けない宣言(hitbox.json 等)に「新規」は意味を持たない。
-}
viewGroupHeadingFor : Api.ResourceGroup -> Html Msg
viewGroupHeadingFor group =
    div [ HA.class "file-group flex items-center gap-1 px-3 pt-3 pb-1" ]
        [ span [ HA.class "min-w-0 flex-1 truncate text-[10px] font-semibold uppercase tracking-[0.14em] text-ink-faint" ]
            [ text (Maybe.withDefault group.id group.title) ]
        , if String.contains "*" group.pattern then
            button
                [ HA.class "group-new btn btn-ghost btn-mini shrink-0 text-ink-faint hover:text-ink"
                , HA.title ("「" ++ Maybe.withDefault group.id group.title ++ "」に新しいファイルを作る")
                , HE.onClick (FileNewClicked group)
                ]
                [ text "＋ 新規" ]

          else
            text ""
        ]


viewFileRow : Model -> String -> Html Msg
viewFileRow model path =
    let
        current =
            model.current

        -- ゲームがいま画面に出しているファイルか(active-docs.json 由来)
        isActive =
            List.member path (activePaths model)

        renaming =
            model.fileRename |> Maybe.andThen (\r -> ifSame r path)
    in
    div
        [ HA.classList
            [ ( "file-row flex w-full shrink-0 items-center gap-1.5 px-3 text-left font-mono text-xs", True )
            , ( "selected bg-accent/15 text-ink", current == Just path )
            , ( "text-ink-soft hover:bg-white/5", current /= Just path )
            ]

        -- 右クリックは行のどこでも(IDE と同じ)
        , ContextMenu.onOpen (FileMenuOpened path)
        ]
        [ case renaming of
            Just r ->
                viewFileRenameBox r

            Nothing ->
                button
                    [ HA.class "file-open flex min-w-0 flex-1 cursor-pointer items-center gap-1.5 py-1 text-left hover:text-ink"
                    , HA.title
                        (if isActive then
                            path ++ "(いま画面に出ている)"

                         else
                            path
                        )
                    , HE.onClick (FileClicked path)
                    , onFileRowKeys path
                    ]
                    [ fileIcon
                    , span [ HA.class "min-w-0 flex-1 truncate" ] [ text path ]
                    ]
        , if isActive then
            -- ダークテーマで暗く沈む素の絵文字でなく、緑系バッジで「表示中」と一目に
            span
                [ HA.class "active-badge shrink-0 rounded-sm bg-emerald-500/25 px-1.5 py-px text-[10px] font-medium text-emerald-300 ring-1 ring-emerald-400/50"
                , HA.title "いま画面に出ています"
                ]
                [ text "🎮 表示中" ]

          else
            text ""
        ]


{-| その場編集の欄の名指し(カーソルを置く頼み事に使う)。同時に 1 つしか開かない。 -}
fileRenameBoxId : String
fileRenameBoxId =
    "file-rename-box"


ifSame : { path : String, text : String } -> String -> Maybe { path : String, text : String }
ifSame renaming path =
    if renaming.path == path then
        Just renaming

    else
        Nothing


{-| その場の名前変更(F2 / メニューの「名前の変更…」)。Enter で確定・Esc で取り消し。
欄から離れた時も確定にする — IDE と同じで、押し直しを強いない。
-}
viewFileRenameBox : { path : String, text : String } -> Html Msg
viewFileRenameBox renaming =
    input
        [ HA.class "file-rename field my-0.5 min-w-0 flex-1"
        , HA.id fileRenameBoxId
        , HA.type_ "text"
        , HA.value renaming.text
        , HA.autofocus True
        , HE.onInput FileRenameTyped
        , HE.onBlur FileRenameCommitted
        , HE.custom "keydown"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        case key of
                            "Enter" ->
                                D.succeed { message = FileRenameCommitted, stopPropagation = True, preventDefault = True }

                            "Escape" ->
                                D.succeed { message = FileRenameCancelled, stopPropagation = True, preventDefault = True }

                            _ ->
                                D.fail "他のキーは素通し"
                    )
            )
        ]
        []


{-| 行にカーソルがある時のキー。F2=名前の変更・Delete/Backspace=削除(IDE の作法)。 -}
onFileRowKeys : String -> Html.Attribute Msg
onFileRowKeys path =
    HE.custom "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "F2" ->
                            D.succeed { message = FileRenameStarted path, stopPropagation = True, preventDefault = True }

                        "Delete" ->
                            D.succeed { message = FileDeleteClicked path, stopPropagation = True, preventDefault = True }

                        "Backspace" ->
                            D.succeed { message = FileDeleteClicked path, stopPropagation = True, preventDefault = True }

                        _ ->
                            D.fail "他のキーは素通し"
                )
        )


{-| 行の右クリックメニュー(VS Code の並びに合わせる)。 -}
viewFileMenu : Model -> Html Msg
viewFileMenu model =
    case model.fileMenu of
        Just menu ->
            ContextMenu.view { onClose = FileMenuClosed }
                menu.anchor
                [ ContextMenu.item "複製" (FileDuplicateClicked menu.path)
                , ContextMenu.item "名前の変更…(F2)" (FileRenameStarted menu.path)
                , ContextMenu.separator
                , ContextMenu.danger "削除" (FileDeleteClicked menu.path)
                ]

        Nothing ->
            text ""


viewDashboardRow : Maybe String -> Api.Dashboard -> Html Msg
viewDashboardRow openId dash =
    button
        [ HA.classList
            [ ( "dash-row block w-full shrink-0 cursor-pointer truncate px-3 py-1 text-left text-xs", True )
            , ( "selected bg-accent/15 text-ink", openId == Just dash.id )
            , ( "text-ink-soft hover:bg-white/5 hover:text-ink", openId /= Just dash.id )
            ]
        , HE.onClick (DashboardClicked dash.id)
        ]
        [ text (Maybe.withDefault dash.id dash.title) ]



-- ダッシュボード(中央+右ペインの代わりに出す閲覧ボード)


{-| 左が主リソースのエントリ一覧、右が選択エントリの要約と ref ののぞき窓。
編集はここでは受けない — カードのジャンプで正本ファイルを開いてから行う。
-}
viewDashboard : Model -> DashState -> Html Msg
viewDashboard model dash =
    let
        heading =
            Maybe.withDefault dash.decl.id dash.decl.title

        -- 宣言だけあって /resources に居ないリソースは、黙って空にせず名指しで見せる
        missingUses =
            dash.decl.uses
                |> List.filter (\use -> not (List.any (\s -> s.resource == use) dash.slots))

        notes =
            List.map (\use -> "リソース \"" ++ use ++ "\" が見つかりません(project.json の resources を確認)") missingUses
                ++ (dash.slots
                        |> List.filterMap (\s -> s.dataError |> Maybe.map (\e -> s.path ++ ": " ++ e))
                   )

        loading =
            dash.slots |> List.any (\s -> s.dataReq /= Nothing)
    in
    div [ HA.class "pane-dash flex min-w-0 flex-1 flex-col bg-app" ]
        (div [ HA.class "dash-head flex h-9 shrink-0 items-center gap-2 border-b border-edge bg-panel px-3" ]
            [ span [ HA.class "dash-title text-xs font-semibold" ] [ text heading ]
            , if loading then
                span [ HA.class "text-[11px] text-ink-faint" ] [ text "読み込み中…" ]

              else
                text ""
            , span [ HA.class "spacer flex-1" ] []
            , button [ HA.class "dash-close btn", HE.onClick DashboardClosed ] [ text "閉じる" ]
            ]
            :: List.map
                (\note -> div [ HA.class "dash-note border-b border-edge px-3 py-1 text-[11px] text-danger" ] [ text note ])
                notes
            ++ [ viewDashBody model dash ]
        )


{-| 見開きの図鑑: 左がモンスター一覧(肖像サムネ付き)、右が選択エントリの
攻略本ページ。編集はここでは受けない — ジャンプで正本ファイルを開いてから行う。
-}
viewDashBody : Model -> DashState -> Html Msg
viewDashBody model dash =
    case Dashboards.find dash.decl.plugin of
        Nothing ->
            div [ HA.class "dash-note m-auto text-[11px] text-ink-faint" ]
                [ text ("ダッシュボードプラグイン \"" ++ dash.decl.plugin ++ "\" はこのエディタにありません") ]

        Just plugin ->
            let
                vm =
                    plugin.view
                        { uses = dash.decl.uses
                        , docs = dashDocs dash.slots
                        , selected = dash.selected
                        }
            in
            div [ HA.class "dash-body flex min-h-0 flex-1" ]
                [ div [ HA.class "dash-list w-56 shrink-0 overflow-y-auto border-r border-edge py-2" ]
                    (if List.isEmpty vm.entries then
                        [ div [ HA.class "px-3 text-[11px] text-ink-faint" ] [ text "エントリがありません" ] ]

                     else
                        vm.entries |> List.map (viewDashEntry model dash.selected)
                    )
                , div [ HA.class "dash-detail min-w-0 flex-1 overflow-y-auto p-4" ]
                    (viewDashDetail model vm.detail)
                ]


dashDocs : List DashSlot -> List Dashboards.SourceDoc
dashDocs slots =
    slots
        |> List.filterMap
            (\s ->
                s.doc
                    |> Maybe.map
                        (\doc -> { resource = s.resource, path = s.path, doc = doc, schema = s.schema })
            )


{-| 一覧の 1 行: 肖像サムネ+名前+id。 -}
viewDashEntry : Model -> Maybe String -> Dashboards.EntryItem -> Html Msg
viewDashEntry model selected item =
    let
        isOn =
            selected == Just item.entry.id
    in
    button
        [ HA.classList
            [ ( "dash-entry flex w-full shrink-0 cursor-pointer items-center gap-2 px-3 py-1 text-left", True )
            , ( "on bg-accent/15", isOn )
            , ( "hover:bg-white/5", not isOn )
            ]
        , HE.onClick (DashEntrySelected item.entry.id)
        ]
        [ case item.portrait of
            Just path ->
                viewPortrait model 28 path

            Nothing ->
                div [ HA.class "portrait-box portrait-blank", HA.style "width" "28px", HA.style "height" "28px" ] []
        , div [ HA.class "min-w-0 flex-1" ]
            [ div
                [ HA.classList
                    [ ( "truncate text-xs", True )
                    , ( "text-ink", isOn )
                    , ( "text-ink-soft", not isOn )
                    ]
                ]
                [ text (Maybe.withDefault item.entry.id item.name) ]
            , div [ HA.class "truncate font-mono text-[10px] text-ink-faint" ] [ text item.entry.id ]
            ]
        ]


{-| 攻略本の右ページ: 額装の肖像+名前(大)+フレーバー(引用風)+
メーター/%%/重み棒/のぞき窓のフィールド行。
-}
viewDashDetail : Model -> Maybe Dashboards.Detail -> List (Html Msg)
viewDashDetail model detail =
    case detail of
        Nothing ->
            [ div [ HA.class "dash-note text-[11px] leading-relaxed text-ink-faint" ]
                [ text "左の一覧からエントリを選んでください" ]
            ]

        Just d ->
            [ div [ HA.class "dash-page mx-auto w-full max-w-lg" ]
                (div [ HA.class "dash-page-head mb-3 flex items-start gap-4" ]
                    [ case d.portrait of
                        Just path ->
                            div [ HA.class "portrait-frame shrink-0" ] [ viewPortrait model 112 path ]

                        Nothing ->
                            text ""
                    , div [ HA.class "min-w-0 flex-1 pt-1" ]
                        [ div [ HA.class "dash-page-title text-lg font-semibold text-ink" ]
                            [ text (Maybe.withDefault d.entry.id d.title) ]
                        , div [ HA.class "mt-1 flex items-center gap-2" ]
                            [ span [ HA.class "entry-id font-mono text-[11px] text-ink-faint" ] [ text d.entry.id ]
                            , span [ HA.class "badge" ] [ text d.entry.resource ]
                            ]
                        , button
                            [ HA.class "dash-open btn btn-mini mt-2"
                            , HE.onClick
                                (DashJumped
                                    { path = d.entry.path
                                    , sectionKey = d.entry.resource
                                    , entry = ByKey d.entry.id
                                    }
                                )
                            ]
                            [ text "ファイルで開く" ]
                        ]
                    ]
                    :: (case d.flavor of
                            Just flavor ->
                                [ div [ HA.class "dash-flavor mb-4 border-l-2 border-accent/50 pl-3 text-xs leading-relaxed text-ink-soft" ]
                                    [ text flavor ]
                                ]

                            Nothing ->
                                []
                       )
                    ++ List.map (viewDashField model) d.fields
                )
            ]


viewDashField : Model -> Dashboards.FieldLine -> Html Msg
viewDashField model line =
    div [ HA.class "dash-field mb-3" ]
        [ div [ HA.class "form-label mb-1 text-[11px] text-ink-faint" ] [ text line.label ]
        , case line.value of
            Dashboards.Plain shown ->
                div [ HA.class "dash-value font-mono text-xs text-ink" ]
                    [ text
                        (if shown == "" then
                            "—"

                         else
                            shown
                        )
                    ]

            Dashboards.LongText long ->
                div [ HA.class "dash-value text-xs leading-relaxed text-ink-soft" ] [ text long ]

            Dashboards.Meter m ->
                viewMeter m.text ((m.value - m.min) / max 1.0e-9 (m.max - m.min))

            Dashboards.Percent p ->
                viewMeter p.text p.ratio

            Dashboards.WeightBars w ->
                div [ HA.class "dash-weights" ]
                    (w.entries
                        |> List.map
                            (\( key, value ) ->
                                div [ HA.class "mb-1 flex items-center gap-2" ]
                                    [ span [ HA.class "w-16 shrink-0 truncate font-mono text-[11px] text-ink-soft" ] [ text key ]
                                    , viewBar (value / max 1.0e-9 w.total)
                                    , span [ HA.class "w-10 shrink-0 text-right font-mono text-[11px] tabular-nums text-ink" ]
                                        [ text (weightText value) ]
                                    ]
                            )
                    )

            Dashboards.RefValue ref ->
                viewPeek model ref
        ]


{-| メーター 1 本(値の文字+バー)。ratio は 0〜1 に丸めて描く。 -}
viewMeter : String -> Float -> Html Msg
viewMeter shown ratio =
    div [ HA.class "flex items-center gap-2" ]
        [ viewBar ratio
        , span [ HA.class "w-12 shrink-0 text-right font-mono text-xs tabular-nums text-ink" ] [ text shown ]
        ]


viewBar : Float -> Html Msg
viewBar ratio =
    div [ HA.class "meter min-w-0 flex-1" ]
        [ div
            [ HA.class "meter-fill"
            , HA.style "width" (String.fromFloat (clamp 0 100 (ratio * 100)) ++ "%")
            ]
            []
        ]


weightText : Float -> String
weightText value =
    if value == toFloat (round value) then
        String.fromInt (round value)

    else
        String.fromFloat value


{-| ref ののぞき窓。参照先が引けたら要約カード(肖像サムネ+クリックで正本へ
ジャンプ)、引けなければ赤の「見つかりません」— 黙って隠すとぶら下がりに気づけない。
-}
viewPeek : Model -> { target : String, id : String, peek : Maybe Dashboards.Peek } -> Html Msg
viewPeek model ref =
    case ref.peek of
        Just peek ->
            button
                [ HA.class "peek-card block w-full cursor-pointer rounded border border-edge bg-panel p-2 text-left hover:border-accent/60 hover:bg-raised"
                , HE.onClick
                    (DashJumped
                        { path = peek.entry.path
                        , sectionKey = peek.entry.resource
                        , entry = ByKey peek.entry.id
                        }
                    )
                ]
                [ div [ HA.class "flex items-start gap-2.5" ]
                    (List.filterMap identity
                        [ peek.portrait |> Maybe.map (\path -> viewPortrait model 40 path)
                        , Just
                            (div [ HA.class "min-w-0 flex-1" ]
                                (div [ HA.class "peek-head mb-1 flex items-center gap-2" ]
                                    [ span [ HA.class "font-mono text-xs font-semibold text-accent" ] [ text ref.id ]
                                    , span [ HA.class "badge" ] [ text ref.target ]
                                    ]
                                    :: (peek.lines
                                            |> List.map
                                                (\( label_, value ) ->
                                                    div [ HA.class "peek-line font-mono text-[11px] text-ink-soft" ]
                                                        [ text (label_ ++ ": " ++ value) ]
                                                )
                                       )
                                )
                            )
                        ]
                    )
                ]

        Nothing ->
            div [ HA.class "peek-missing rounded border border-danger/60 bg-danger/10 p-2 font-mono text-[11px] text-danger" ]
                [ text ("\"" ++ ref.id ++ "\" は " ++ ref.target ++ " に見つかりません") ]


{-| 肖像 1 枚(エンジン焼き PNG の切り出し表示)。size は CSS px の一辺。
絵はサーバが焼いた物のまま — こちらは切り出しと拡縮だけ(唯一のレンダラの原則)。
-}
viewPortrait : Model -> Float -> String -> Html Msg
viewPortrait model size path =
    let
        sizePx =
            String.fromFloat size ++ "px"

        box extraClass attrs body =
            div
                ([ HA.class ("portrait-box " ++ extraClass)
                 , HA.style "width" sizePx
                 , HA.style "height" sizePx
                 ]
                    ++ attrs
                )
                body
    in
    case Dict.get path model.portraits of
        Just (PortraitReady image) ->
            let
                -- design px → CSS px(横幅合わせ。肖像は正方形の想定)
                k =
                    size / max 1.0e-9 image.crop.w

                px f =
                    String.fromFloat f ++ "px"
            in
            box ""
                [ HA.style "background-image" ("url(\"data:image/png;base64," ++ image.png ++ "\")")
                , HA.style "background-size" (px (image.imgW / image.scale * k) ++ " " ++ px (image.imgH / image.scale * k))
                , HA.style "background-position" (px (negate image.crop.x * k) ++ " " ++ px (negate image.crop.y * k))
                , HA.style "background-repeat" "no-repeat"
                , HA.title path
                ]
                []

        Just (PortraitFailed reason) ->
            box "portrait-failed" [ HA.title (path ++ ": " ++ reason) ] [ text "×" ]

        Just PortraitLoading ->
            box "portrait-loading" [] []

        Nothing ->
            box "portrait-blank" [] []


{-| フォーム上部の肖像カード(uiDoc フィールドを持つエントリを選んだ時)。 -}
viewEntryPortrait : Model -> Schema.Section -> D.Value -> List (Html Msg)
viewEntryPortrait model section entry =
    let
        portraitPath =
            section.fields
                |> List.filter (\( _, f ) -> f.type_ == Schema.TText && Schema.widgetIs "uiDoc" f.widget)
                |> List.head
                |> Maybe.andThen
                    (\( name, _ ) -> D.decodeValue (D.field name D.string) entry |> Result.toMaybe)
    in
    case portraitPath of
        Just path ->
            [ div [ HA.class "entry-portrait mb-2.5 flex justify-center" ]
                [ div [ HA.class "portrait-frame" ] [ viewPortrait model 96 path ] ]
            ]

        Nothing ->
            []


{-| 改名が他ファイルに及ぶ時の確認。開いている文書は保存するまで書き込まれないが、
他ファイルは確定と同時に保存されるので、その差をここで言葉にする。
-}
viewCrossRenameDialog : CrossRenamePlan -> Html Msg
viewCrossRenameDialog plan =
    Html.node "sl-dialog"
        [ HA.class "cross-rename"
        , HA.attribute "label" "他のファイルも書き換えます"
        , HA.attribute "open" ""
        ]
        [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
            (div [ HA.class "mb-2" ]
                [ text
                    ("\""
                        ++ plan.req.oldId
                        ++ "\" → \""
                        ++ plan.req.newId
                        ++ "\" への改名は、次のファイルの参照も書き換えて保存します:"
                    )
                ]
                :: (plan.files
                        |> List.map
                            (\f ->
                                div [ HA.class "cross-rename-file font-mono text-[11px] text-ink" ]
                                    [ text (f.path ++ " — " ++ String.fromInt f.count ++ " 箇所") ]
                            )
                   )
                ++ [ div [ HA.class "mt-2 text-[11px] text-ink-faint" ]
                        [ text "開いているファイルへの改名は、いつも通り保存するまで書き込まれません。" ]
                   ]
            )
        , div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
            [ button [ HA.class "btn", HE.onClick CrossRenameCancelled ] [ text "やめる" ]
            , button [ HA.class "btn btn-primary", HE.onClick CrossRenameConfirmed ] [ text "改名して保存" ]
            ]
        ]


viewEditorPane : Model -> Html Msg
viewEditorPane model =
    div [ HA.class "pane-editor flex min-w-0 flex-1 bg-app" ]
        (case model.current of
            Nothing ->
                [ div [ HA.class "empty m-auto text-ink-faint" ] [ text "左の一覧からファイルを選んでください" ] ]

            Just _ ->
                [ textarea
                    [ HA.class "flex-1 resize-none border-none bg-app p-3 font-mono text-xs leading-relaxed text-ink focus:outline-none"
                    , HA.value model.docText
                    , HA.spellcheck False
                    , HE.onInput DocChanged
                    ]
                    []
                ]
        )


-- ビジュアルモード(テーブル+フォームで完結する既定画面)


{-| タブ 1 枚。単一値のセクションは複数まとまって 1 枚になるので、タブとセクションは
1 対 1 ではない。key はクリックで開く識別子(group 名 または セクションキー)。
-}
type alias FormTab =
    { key : String
    , label : String
    , sections : List ( String, Schema.Section )
    }


defaultTabLabel : String
defaultTabLabel =
    "基本"


{-| 文書のメモ(規約で認めた note)。どの Doc でも同じ名前・同じ位置(いちばん最後)に
置く — 書いた順のままだと先頭に来ることが多く、開いた人が「まず何をすればいいか」を
見失う。 -}
noteKey : String
noteKey =
    "note"


noteTabLabel : String
noteTabLabel =
    "メモ"


{-| セクション列をタブへ束ねる。単一値(value/field)だけが束ね対象 —
一覧や入れ子を持つ種類は、操作(追加・削除・並べ替え)がタブ単位なので混ぜない。
束ね先は宣言した group、書いていなければ「基本」。並びは書いた順のまま。
-}
tabsOf : Schema.Schema -> List FormTab
tabsOf schema =
    let
        tabOf ( key, section ) =
            case section.kind of
                Schema.ValueKind _ ->
                    if key == noteKey then
                        ( noteTabLabel, noteTabLabel )

                    else
                        ( Maybe.withDefault defaultTabLabel section.group
                        , Maybe.withDefault defaultTabLabel section.group
                        )

                _ ->
                    ( key, Maybe.withDefault key section.label )

        add pair tabs =
            let
                ( tabKey, tabLabel ) =
                    tabOf pair
            in
            if List.any (\t -> t.key == tabKey) tabs then
                tabs
                    |> List.map
                        (\t ->
                            if t.key == tabKey then
                                { t | sections = pair :: t.sections }

                            else
                                t
                        )

            else
                { key = tabKey, label = tabLabel, sections = [ pair ] } :: tabs
    in
    let
        built =
            supportedSections schema
                |> List.foldl add []
                |> List.map (\t -> { t | sections = List.reverse t.sections })
                |> List.reverse
    in
    List.filter (\t -> t.key /= noteTabLabel) built
        ++ List.filter (\t -> t.key == noteTabLabel) built


{-| 表示中のタブ。lint や逆参照からの飛び先はセクションキーなので、
タブキーで当たらなければ「そのセクションを含むタブ」へ読み替える。
-}
activeTab : Schema.Schema -> Model -> Maybe FormTab
activeTab schema model =
    let
        tabs =
            tabsOf schema

        byKey k =
            tabs |> List.filter (\t -> t.key == k) |> List.head

        bySection k =
            tabs
                |> List.filter (\t -> t.sections |> List.any (\( sk, _ ) -> sk == k))
                |> List.head
    in
    case model.sectionKey of
        Just k ->
            case byKey k of
                Just t ->
                    Just t

                Nothing ->
                    case bySection k of
                        Just t ->
                            Just t

                        Nothing ->
                            List.head tabs

        Nothing ->
            List.head tabs


{-| タブの実効キー(未クリックは先頭タブ)。update 側の currentSection と
同じ決め方 — 操作が見えていない表に当たらないための対。
-}
activeSectionKey : Schema.Schema -> Model -> String
activeSectionKey schema model =
    activeTab schema model |> Maybe.map .key |> Maybe.withDefault ""


{-| そのタブが「単一値だけ」で出来ているなら、束ねた 1 枚の表として出すための
フィールド列。混ざっている(一覧や入れ子がある)タブは束ねない。
-}
bundledFields : FormTab -> Maybe (List ( String, Schema.Field ))
bundledFields tab =
    let
        picked =
            tab.sections
                |> List.filterMap
                    (\( key, section ) ->
                        case section.kind of
                            Schema.ValueKind field ->
                                Just ( key, field )

                            _ ->
                                Nothing
                    )
    in
    if not (List.isEmpty picked) && List.length picked == List.length tab.sections then
        Just picked

    else
        Nothing


{-| フォームに出すのは「今この場で実際にいじれる」セクションだけ。
未対応 kind はタブごと出さない(存在は件数の 1 行で伝える)。
-}
supportedSections : Schema.Schema -> List ( String, Schema.Section )
supportedSections schema =
    schema.sections
        |> List.filter
            (\( _, section ) ->
                case section.kind of
                    Schema.Unsupported _ ->
                        False

                    _ ->
                        True
            )


unsupportedSectionCount : Schema.Schema -> Int
unsupportedSectionCount schema =
    List.length schema.sections - List.length (supportedSections schema)


{-| フォームに出していない項目の存在をペイン最下部で 1 行だけ伝える(0 件なら無音)。 -}
viewUnsupportedFooter : Model -> List (Html Msg)
viewUnsupportedFooter model =
    case model.schemaState of
        SchemaReady schema ->
            let
                n =
                    unsupportedSectionCount schema
            in
            if n == 0 then
                []

            else
                [ div [ HA.class "unsupported-footer mt-3 text-[10px] leading-relaxed text-ink-faint" ]
                    [ text ("フォーム未対応の項目が " ++ String.fromInt n ++ " 件あります(テキスト編集で編集できます)") ]
                ]

        _ ->
            []


{-| ビジュアルの中央: セクションタブ+盤面カード+テーブル。テキストは出さない —
生 JSON は分割/コードモードの持ち場。
-}
viewVisualCenter : Model -> Html Msg
viewVisualCenter model =
    div [ HA.class "pane-visual flex min-w-0 flex-1 flex-col bg-app" ]
        (case ( model.current, model.schemaState, parsedDoc model ) of
            ( Nothing, _, _ ) ->
                [ div [ HA.class "empty m-auto text-ink-faint" ] [ text "左の一覧からファイルを選んでください" ] ]

            ( Just _, SchemaReady schema, Just doc ) ->
                viewVisualBody model schema doc

            _ ->
                -- effectiveMode の判定で、ここに残るのはスキーマ探索中だけ
                [ div [ HA.class "empty m-auto text-ink-faint" ] [ text "スキーマを探しています…" ] ]
        )


viewVisualBody : Model -> Schema.Schema -> D.Value -> List (Html Msg)
viewVisualBody model schema doc =
    let
        activeKey =
            activeSectionKey schema model

        -- 逆参照は文書全体を 1 回歩けば足りる(セクションごとに数え直さない)
        usageDict =
            EntryTable.usageDicts schema doc (crossSources model)
    in
    [ div [ HA.class "visual-tabs flex h-9 min-w-0 shrink-0 flex-nowrap items-center gap-1 overflow-x-auto border-b border-edge bg-panel px-3" ]
        (tabsOf schema |> List.map (viewTab activeKey))
    , div [ HA.class "visual-body flex min-h-0 flex-1 flex-col p-3" ]
        (viewPreviewCard model
            ++ (case activeTab schema model |> Maybe.andThen bundledFields of
                    -- 単一値だけのタブは案内 1 行だけ(設定の数だけ増やさない)
                    Just _ ->
                        [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ]
                            [ text "この一覧は右のフォームで編集します。" ]
                        ]

                    Nothing ->
                        activeTab schema model
                            |> Maybe.map .sections
                            |> Maybe.withDefault []
                            |> List.concatMap (viewVisualCenterSection model doc usageDict)
               )
        )
    ]


{-| ビジュアルの中央に出すセクション 1 枚ぶん(一覧の表か、盤面を持たない
種類の案内)。フォームは右ペインの持ち場なので、ここには出さない。
-}
viewVisualCenterSection : Model -> D.Value -> EntryTable.UsageDicts -> ( String, Schema.Section ) -> List (Html Msg)
viewVisualCenterSection model doc usageDict ( key, section ) =
    case section.kind of
        Schema.RecordKind ->
            if Schema.widgetIs "sfx" section.widget then
                [ Html.map SfxMsg
                    (SfxEditor.view
                        { sound = key
                        , label = Maybe.withDefault key section.label
                        , values = sfxValues key model
                        }
                        model.sfx
                    )
                ]

            else
                [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ]
                    [ text "この設定は一覧を持ちません。右のフォームで編集します。" ]
                ]

        Schema.ValueKind _ ->
            []

        Schema.Catalog ->
            [ EntryTable.viewCrudBar tableHandlers (tableState model) doc key section

            -- flex-1 で伸ばさない: 行が少ない表の下に空の枠を広げない
            , EntryTable.viewBox tableHandlers (tableState model) "min-h-0 shrink" doc key section (Just usageDict)
            ]
                ++ EntryTable.viewUsagePop tableHandlers (tableState model) key (Just usageDict)

        Schema.ListKind ->
            [ EntryTable.viewCrudBar tableHandlers (tableState model) doc key section
            , EntryTable.viewBox tableHandlers (tableState model) "min-h-0 shrink" doc key section Nothing
            ]

        -- supportedSections が濾すのでここへは来ない
        Schema.Unsupported _ ->
            []


{-| ビジュアルの右: 選択エントリのフォーム。未選択は案内カード —
空白の海に薄文字 1 行で放置しない。
-}
viewVisualSide : Model -> Html Msg
viewVisualSide model =
    div
        [ HA.class "pane-side shrink-0 overflow-y-auto border-l border-edge bg-panel p-3"
        , HA.style "width" (String.fromInt model.rightPaneW ++ "px")
        ]
        ((case ( model.current, model.schemaState, parsedDoc model ) of
            ( Just _, SchemaReady schema, Just doc ) ->
                viewVisualSideBody model schema doc

            _ ->
                [ viewGuideCard "ファイルを開く" "ファイルを開くと、ここに編集フォームが出ます。" ]
         )
            ++ viewKindGuide model
            ++ viewUnsupportedFooter model
        )


viewVisualSideBody : Model -> Schema.Schema -> D.Value -> List (Html Msg)
viewVisualSideBody model schema doc =
    let
        activeKey =
            activeSectionKey schema model

        guide =
            viewGuideCard "エントリを選んでください"
                "中央のテーブルの行(または盤面の点)をクリックすると、ここに編集フォームが出ます。"
    in
    case activeTab schema model |> Maybe.andThen bundledFields of
        -- 単一値だけのタブは、束ねた全部を 1 枚の表として出す(値ごとにタブを割らない)
        Just fields ->
            [ viewSideHead activeKey "値"
            , viewRows model doc (valueSectionsAsRecord fields) doc []
            ]

        Nothing ->
            case activeTab schema model |> Maybe.map .sections |> Maybe.andThen List.head of
                Nothing ->
                    [ guide ]

                Just ( key, section ) ->
                    FormHelp.section HelpToggled model.helpOpen key section
                        ++ viewVisualSideSection model doc guide key section


{-| 右ペインのセクション 1 枚ぶん(選択エントリのフォーム)。 -}
viewVisualSideSection : Model -> D.Value -> Html Msg -> String -> Schema.Section -> List (Html Msg)
viewVisualSideSection model doc guide key section =
    case section.kind of
        Schema.RecordKind ->
            [ viewSideHead key "設定"
            , viewRows model doc section (Doc.record key doc) [ KeySeg key ]
            ]

        Schema.ValueKind field ->
            [ viewSideHead key "値"
            , viewRows model doc (valueSectionsAsRecord [ ( key, field ) ]) doc []
            ]

        Schema.Catalog ->
            case Selection.catalogName model.entrySel key doc of
                Just name ->
                    viewEntryPortrait model section (Doc.catalog key doc |> Dict.get name |> Maybe.withDefault E.null)
                        ++ [ viewRenameHeader model key name
                           , viewRows model
                                doc
                                section
                                (Doc.catalog key doc |> Dict.get name |> Maybe.withDefault E.null)
                                [ KeySeg key, KeySeg name ]
                           ]

                Nothing ->
                    [ guide ]

        Schema.ListKind ->
            case Selection.listEntry model.entrySel key doc of
                Just ( i, entry ) ->
                    [ viewSideHead ("#" ++ String.fromInt i) key
                    , viewRows model doc section entry [ KeySeg key, IdxSeg i ]
                    ]

                Nothing ->
                    [ guide ]

        -- supportedSections が濾すのでここへは来ない
        Schema.Unsupported _ ->
            []


viewSideHead : String -> String -> Html Msg
viewSideHead name badgeText =
    div [ HA.class "side-head mb-2.5 flex items-center gap-2" ]
        [ span [ HA.class "entry-id font-mono text-xs font-semibold text-ink" ] [ text name ]
        , span [ HA.class "badge" ] [ text badgeText ]
        ]


viewGuideCard : String -> String -> Html Msg
viewGuideCard title body =
    div [ HA.class "guide-card rounded border border-dashed border-edge bg-well/40 px-3 py-6 text-center" ]
        [ div [ HA.class "mb-1 text-xs text-ink-soft" ] [ text title ]
        , div [ HA.class "text-[11px] leading-relaxed text-ink-faint" ] [ text body ]
        ]


{-| 開いているファイルの kind に応じた右ペインの案内。盤面プラグインを持つ
ファイルは絵と操作が主役なので出さない。
-}
viewKindGuide : Model -> List (Html Msg)
viewKindGuide model =
    case ( currentGroup model, currentPlugin model ) of
        ( Just group, Nothing ) ->
            if group.id == "ui" || group.id == "hitbox" then
                [ div [ HA.class "kind-guide editor-link mt-2.5 rounded border border-edge bg-well/40 px-3 py-2 text-[11px] leading-relaxed text-ink-faint" ]
                    [ text "ui / hitbox の見た目編集には専用エディタ flix_ge_editor もあります。同じ editor_server の EDITOR_WEB を flix_ge_editor の dist にして起動します(この画面と同じ http://127.0.0.1:8787)。" ]
                ]

            else if enginePreviewKind model == Nothing then
                [ div [ HA.class "kind-guide live-game mt-2.5 rounded border border-edge bg-well/40 px-3 py-2 text-[11px] leading-relaxed text-ink-faint" ]
                    [ text "🎮 走るゲームが本番プレビュー。このファイルは走るゲームが見ているので、保存すると即反映されます。" ]
                ]

            else
                []

        _ ->
            []



-- スキーマ駆動フォーム(右ペイン)


{-| 分割モードの右ペイン(幅を持つ側)。 -}
viewFormPane : Model -> Html Msg
viewFormPane model =
    viewFormPaneIn
        [ HA.class "pane-form shrink-0 overflow-y-auto border-l border-edge bg-panel p-3"
        , HA.style "width" (String.fromInt model.rightPaneW ++ "px")
        ]
        model


{-| つまみ系 2 ペインの左(伸びる側)。幅を持つのは右の JSON 側だけ —
両方が同じ幅の値を見ていると、境界を引いた向きと反対に動いてしまう。
-}
viewKnobFormPane : Model -> Html Msg
viewKnobFormPane model =
    viewFormPaneIn [ HA.class "pane-form min-w-0 flex-1 overflow-y-auto bg-panel p-3" ] model


viewFormPaneIn : List (Html.Attribute Msg) -> Model -> Html Msg
viewFormPaneIn attrs model =
    div attrs
        ((case ( model.current, model.schemaState ) of
            ( Nothing, _ ) ->
                [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ] [ text "ファイルを開くとフォームが出ます" ] ]

            ( Just _, SchemaNone ) ->
                []

            ( Just _, SchemaLoading ) ->
                [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ] [ text "スキーマを探しています…" ] ]

            -- 盤面/エンジン焼きプレビューはリソース宣言で決まり、スキーマの都合とは
            -- 独立なので、スキーマ待ち・欠け・壊れの間も出し続ける
            ( Just path, SchemaMissing ) ->
                viewPreviewCard model
                    ++ [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ]
                            [ text
                                (case schemaPlanFor model.groups path of
                                    Just schemaPath ->
                                        "スキーマ(" ++ schemaPath ++ ")が無いので、中央のテキストで編集してください"

                                    Nothing ->
                                        "この種類はスキーマ宣言が無いので、中央のテキストで編集してください"
                                )
                            ]
                       ]

            ( Just path, SchemaForeign info ) ->
                -- 意図的にフォーム化対象外(ドット絵等)。壊れではないので赤にしない。
                -- 代わりに、いま開いている Doc の絵(アトリエのプレビュー)をその場で見せる
                viewPreviewCard model
                    ++ [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ]
                            [ text "この種類はフォームになりません。ドット絵は アトリエ(候補づくり・切り替え)で扱うのが正です。ここでは中央のテキスト編集が使えます" ]
                       , if info.previewBroken then
                            div [ HA.class "form-note mt-2 text-[11px] text-ink-faint" ]
                                [ text "プレビュー準備中(アトリエを開くと自動で描き出されます)" ]

                         else
                            img
                                [ HA.src (model.serverBase ++ "/atelier/preview?file=" ++ Url.percentEncode path ++ "&p=" ++ Api.projectKey model.root)
                                , HA.alt ""
                                , HA.class "mt-2 w-full rounded border border-edge bg-black/50"
                                , HA.style "image-rendering" "pixelated"
                                , HE.on "error" (D.succeed ForeignPreviewFailed)
                                ]
                                []
                       ]

            ( Just _, SchemaBroken reason ) ->
                viewPreviewCard model
                    ++ [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ]
                            [ text "スキーマがこのエディタで読めません(壊れているか、未対応の書き方)。中央のテキスト編集は使えます。" ]
                       , div [ HA.class "form-error mt-2 text-[11px] break-all text-danger" ] [ text reason ]
                       ]

            ( Just _, SchemaReady schema ) ->
                viewPreviewCard model ++ viewForm model schema
         )
            ++ viewKindGuide model
            ++ viewUnsupportedFooter model
        )


{-| 盤面プレビューのカード(枠+タイトル+design 寸法)。失敗は枠の中の文言だけ —
プレビューが死んでいてもフォーム・テキスト編集は普段どおり使えると分かる形に。

絵(PNG)はサーバが焼いた物のまま。選択枠・掴み所・ドラッグ追従の円は
その上に % 配置で重ねる対話レイヤの div — 「エンジンが唯一のレンダラ」の原則は
絵の話で、選択やドラッグ中という対話の印はこちらの持ち場。
-}
viewPreviewCard : Model -> List (Html Msg)
viewPreviewCard model =
    let
        card title dims body =
            div [ HA.class "preview-card mb-2.5 w-full max-w-3xl shrink-0 overflow-hidden rounded border border-edge bg-panel" ]
                [ div [ HA.class "preview-title flex h-7 shrink-0 items-center gap-2 border-b border-edge px-2.5" ]
                    (span [ HA.class "text-[11px] font-semibold text-ink" ] [ text title ]
                        :: (case dims of
                                Just design ->
                                    [ span [ HA.class "font-mono text-[10px] text-ink-faint" ]
                                        [ text (String.fromInt (round design.w) ++ "×" ++ String.fromInt (round design.h)) ]
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                , body
                ]
    in
    case ( model.preview, currentPlugin model ) of
        ( PreviewShowing p, Just plugin ) ->
            [ card "盤面プレビュー"
                (Just p.design)
                (div [ HA.class "preview-box bg-app" ]
                    [ div
                        [ HA.classList [ ( "preview-stage", True ), ( "dragging", model.drag /= Nothing ) ] ]
                        (img
                            [ HA.class "block w-full cursor-crosshair [image-rendering:pixelated]"
                            , HA.src ("data:image/png;base64," ++ p.png)
                            , onPreviewClick p.design
                            ]
                            []
                            :: viewSelectedRect plugin model p
                            ++ List.map (viewHotspot p.design) (plugin.dragRects p.rects)
                            ++ viewDragGhost model.drag p.design
                        )
                    ]
                )
            ]

        ( PreviewShowing p, Nothing ) ->
            -- kind(ui/hitbox/fx)のエンジン焼き。眺めるだけの絵(掴み所なし)
            [ card "エンジンプレビュー"
                (Just p.design)
                (div [ HA.class "preview-box bg-app" ]
                    [ img
                        [ HA.class "block w-full [image-rendering:pixelated]"
                        , HA.src ("data:image/png;base64," ++ p.png)
                        ]
                        []
                    ]
                )
            ]

        ( PreviewFailed reason, _ ) ->
            [ card "プレビュー"
                Nothing
                (div [ HA.class "preview-box bg-app" ]
                    [ div [ HA.class "preview-error p-2 text-[11px] text-danger" ] [ text ("プレビューを描けません: " ++ reason) ] ]
                )
            ]

        _ ->
            []


{-| 表(または盤面)で選択中の行の rect を枠 div で示す。 -}
viewSelectedRect : Plugins.Plugin -> Model -> Api.Preview -> List (Html Msg)
viewSelectedRect plugin model p =
    case ( model.sectionKey, model.entrySel ) of
        ( Just key, Just (ByIndex i) ) ->
            if key == plugin.sectionKey then
                plugin.dragRects p.rects
                    |> List.filter (\( j, _ ) -> j == i)
                    |> List.head
                    |> Maybe.map (\( _, r ) -> [ percentDiv "preview-select" [] p.design (Plugins.inflate 3 r) ])
                    |> Maybe.withDefault []

            else
                []

        _ ->
            []


{-| 掴める 1 点の掴み所(透明・grab カーソル)。矩形より広いのは、点が数 px で
ぴったり掴むのが難しいため(クリックの nearLimit と同じ理由)。
-}
viewHotspot : Api.Design -> ( Int, Api.PreviewRect ) -> Html Msg
viewHotspot design ( i, r ) =
    percentDiv "preview-hotspot"
        [ onSpawnPress i { x = r.x + r.w / 2, y = r.y + r.h / 2 } ]
        design
        (Plugins.inflate 10 r)


{-| ドラッグ追従の円。サーバに焼き直させるのは mouseup 後の 1 回だけなので、
追従中の視覚はこの div が担う。
-}
viewDragGhost : Maybe DragState -> Api.Design -> List (Html Msg)
viewDragGhost drag design =
    case drag of
        Just d ->
            [ percentDiv "preview-ghost" [] design { x = d.pos.x - 6, y = d.pos.y - 6, w = 12, h = 12 } ]

        Nothing ->
            []


{-| design 座標の矩形を % で img に重ねる div(位置決めはどの層も同じ式)。 -}
percentDiv : String -> List (Html.Attribute Msg) -> Api.Design -> Api.PreviewRect -> Html Msg
percentDiv className attrs design r =
    let
        pct =
            Plugins.rectPercent design r
    in
    div
        ([ HA.class className
         , HA.style "left" (String.fromFloat pct.left ++ "%")
         , HA.style "top" (String.fromFloat pct.top ++ "%")
         , HA.style "width" (String.fromFloat pct.width ++ "%")
         , HA.style "height" (String.fromFloat pct.height ++ "%")
         ]
            ++ attrs
        )
        []


{-| mousedown でドラッグ開始。preventDefault はドラッグ中のテキスト選択・
ブラウザ標準の画像ドラッグを出さないため。表示幅は img と同幅の親
(preview-stage)から読む — 掴み所自身の幅は当たり範囲込みで基準にならない。
-}
onSpawnPress : Int -> Plugins.Point -> Html.Attribute Msg
onSpawnPress index center =
    HE.custom "mousedown"
        (D.map3
            (\cx cy cw ->
                { message = SpawnPressed { index = index, center = center, clientX = cx, clientY = cy, clientW = cw }
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (D.field "clientX" D.float)
            (D.field "clientY" D.float)
            (D.at [ "target", "parentElement", "clientWidth" ] D.float)
        )


{-| クリック位置を表示座標 → design 座標へ。img は width 100% の等比表示なので、
横も縦も同じ倍率(design.w / 表示幅)で割り戻せる。
-}
onPreviewClick : Api.Design -> Html.Attribute Msg
onPreviewClick design =
    HE.on "click"
        (D.map3
            (\offsetX offsetY clientW ->
                let
                    ratio =
                        design.w / max 1 clientW
                in
                PreviewClicked { x = offsetX * ratio, y = offsetY * ratio }
            )
            (D.field "offsetX" D.float)
            (D.field "offsetY" D.float)
            (D.at [ "target", "clientWidth" ] D.float)
        )


viewForm : Model -> Schema.Schema -> List (Html Msg)
viewForm model schema =
    case D.decodeString D.value model.docText of
        Err _ ->
            [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ] [ text "JSON が壊れているためフォームを出せません(テキストを直すと戻ります)" ] ]

        Ok doc ->
            let
                activeKey =
                    activeSectionKey schema model

                -- 逆参照は文書全体を 1 回歩けば足りる(セクションごとに数え直さない)
                usageDict =
                    EntryTable.usageDicts schema doc (crossSources model)
            in
            div [ HA.class "form-tabs mb-2.5 flex flex-nowrap gap-1 overflow-x-auto" ]
                (tabsOf schema |> List.map (viewTab activeKey))
                :: (case activeTab schema model of
                        Just tab ->
                            case bundledFields tab of
                                -- 単一値だけのタブは 1 枚の表にまとめる(値ごとにタブを割らない)
                                Just fields ->
                                    [ viewRows model doc (valueSectionsAsRecord fields) doc [] ]

                                Nothing ->
                                    tab.sections
                                        |> List.concatMap
                                            (\( key, section ) -> viewSection model doc usageDict key section)

                        Nothing ->
                            []
                   )


{-| タブ 1 つ。内部識別はタブキー(クリックで開くのはこれ)。
-}
viewTab : String -> FormTab -> Html Msg
viewTab activeKey tab =
    button
        [ HA.classList
            [ ( "form-tab h-6 shrink-0 cursor-pointer whitespace-nowrap rounded px-2 text-[11px]", True )
            , ( "on bg-raised text-ink", tab.key == activeKey )
            , ( "text-ink-soft hover:bg-white/5 hover:text-ink", tab.key /= activeKey )
            ]
        , HE.onClick (SectionClicked tab.key)
        ]
        [ text tab.label ]


viewSection : Model -> D.Value -> EntryTable.UsageDicts -> String -> Schema.Section -> List (Html Msg)
viewSection model doc usageDict key section =
    FormHelp.section HelpToggled model.helpOpen key section ++ viewSectionBody model doc usageDict key section


viewSectionBody : Model -> D.Value -> EntryTable.UsageDicts -> String -> Schema.Section -> List (Html Msg)
viewSectionBody model doc usageDict key section =
    case section.kind of
        Schema.RecordKind ->
            [ viewRows model doc section (Doc.record key doc) [ KeySeg key ] ]

        Schema.ValueKind field ->
            [ viewRows model doc (valueSectionsAsRecord [ ( key, field ) ]) doc [] ]

        Schema.Catalog ->
            EntryTable.view tableHandlers (tableState model) doc key section (Just usageDict)
                ++ (case Selection.catalogName model.entrySel key doc of
                        Just name ->
                            viewEntryPortrait model section (Doc.catalog key doc |> Dict.get name |> Maybe.withDefault E.null)
                                ++ [ viewRenameHeader model key name
                                   , viewRows model
                                        doc
                                        section
                                        (Doc.catalog key doc |> Dict.get name |> Maybe.withDefault E.null)
                                        [ KeySeg key, KeySeg name ]
                                   ]

                        Nothing ->
                            [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ] [ text "行をクリックすると下にフォームが出ます" ] ]
                   )

        Schema.ListKind ->
            EntryTable.view tableHandlers (tableState model) doc key section Nothing
                ++ (case Selection.listEntry model.entrySel key doc of
                        Just ( i, entry ) ->
                            [ viewRows model doc section entry [ KeySeg key, IdxSeg i ] ]

                        Nothing ->
                            [ div [ HA.class "form-note text-[11px] leading-relaxed text-ink-faint" ] [ text "行をクリックすると下にフォームが出ます" ] ]
                   )

        -- supportedSections が濾すのでここへは来ない
        Schema.Unsupported _ ->
            []


{-| catalog エントリ選択中の見出し: id 表示+「改名」ボタン、改名中はインライン入力
(Enter 確定 / Esc 破棄)。id はフォームの行でなくキーそのものなので、普段の
draft 経路(docEdit の値書き換え)とは別口。
-}
viewRenameHeader : Model -> String -> String -> Html Msg
viewRenameHeader model key name =
    let
        renaming =
            model.rename
                |> Maybe.andThen
                    (\r ->
                        if r.sectionKey == key && r.oldId == name then
                            Just r

                        else
                            Nothing
                    )
    in
    case renaming of
        Just r ->
            div [ HA.class "rename-row mb-2.5" ]
                [ input
                    [ HA.classList
                        [ ( "rename-input field w-full font-mono", True )
                        , ( "invalid border-danger", r.error /= Nothing )
                        ]
                    , HA.type_ "text"
                    , HA.value r.text
                    , HE.onInput RenameTyped
                    , commitCancelKeys RenameCommitted RenameCancelled
                    ]
                    []
                , case r.error of
                    Just reason ->
                        div [ HA.class "rename-error mt-1 text-[11px] text-danger" ] [ text reason ]

                    Nothing ->
                        div [ HA.class "rename-hint mt-1 text-[11px] text-ink-faint" ]
                            [ text "Enter で確定(この id への参照もまとめて書き換え)・Esc で破棄" ]
                ]

        Nothing ->
            div [ HA.class "rename-row mb-2.5 flex items-center gap-2" ]
                [ span [ HA.class "entry-id font-mono text-xs font-semibold text-ink" ] [ text name ]
                , button
                    [ HA.class "rename btn btn-mini"
                    , HE.onClick (RenameStarted { sectionKey = key, oldId = name })
                    ]
                    [ text "改名" ]
                ]


{-| Esc だけ破棄に使う(複数行入力は Enter を改行に譲る)。 -}
escCancels : Html.Attribute Msg
escCancels =
    HE.custom "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "Escape" then
                        D.succeed (draftKey DraftCancelled)

                    else
                        D.fail "他のキーは素通し"
                )
        )


{-| Enter=確定 / Esc=破棄。preventDefault は Esc がブラウザ側の取り消し
(input の値復元)を起こして draft とずれるのを止めるため。
-}
commitCancelKeys : Msg -> Msg -> Html.Attribute Msg
commitCancelKeys commitMsg cancelMsg =
    HE.custom "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            D.succeed (draftKey commitMsg)

                        "Escape" ->
                            D.succeed (draftKey cancelMsg)

                        _ ->
                            D.fail "他のキーは素通し"
                )
        )



-- 一覧の表(EntryTable)と選んだ 1 行のフォーム


{-| ビジュアルの右ペインに出す「選んだ 1 行のフォーム」。スキーマ駆動で、
中身は分割モードと同じ部品(viewRows)を使う — 同じ欄が画面ごとに違う顔に
ならないように。選択が無い/文書にその行が無いなら何も出さない(閉じる)。
-}
viewMapInspector : Model -> List (Html Msg)
viewMapInspector model =
    case ( MapEditor.selectedRow model.mapEd, parsedDoc model ) of
        ( Just ( key, index ), Just doc ) ->
            case Selection.mapTarget (sectionByKey model key) doc key index of
                Just target ->
                    [ div [ HA.class "map-inspector border-t border-edge" ]
                        [ div [ HA.class "flex items-center gap-1 px-3 pt-2 pb-1.5" ]
                            [ span [ HA.class "min-w-0 flex-1 truncate text-[11px] tracking-wider text-ink-faint" ]
                                [ text target.title ]
                            , case index of
                                Just _ ->
                                    button
                                        [ HA.class "btn btn-ghost btn-mini shrink-0 text-ink-faint hover:text-danger"
                                        , HA.title "この行を削除"
                                        , HE.onClick (MapMsg MapEditor.RemovePressed)
                                        ]
                                        [ text "✕" ]

                                Nothing ->
                                    text ""
                            ]
                        , div [ HA.class "px-2 pb-3" ]
                            [ viewRows model doc target.section target.entry target.path ]
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


{-| 表が押された時にどの便りを投げるか。EntryTable は Main の Msg を知らないので、
配線はこの 1 か所に集まる。
-}
tableHandlers : EntryTable.Handlers Msg
tableHandlers =
    { onSelect = EntryClicked
    , onSort = SortClicked
    , onFilter = FilterChanged
    , onAdd = AddClicked
    , onDuplicate = DuplicateClicked
    , onDelete = DeleteClicked
    , onRowOp = RowOpClicked
    , onRowDelete = RowDeleteClicked
    , onUsagesToggle = UsagesToggled
    , onUsageJump = UsageJumped
    , onExternalJump = DashJumped
    }


{-| 表の見え方に効く今の状態だけを渡す(Model 丸ごとは渡さない)。 -}
tableState : Model -> EntryTable.State
tableState model =
    { entrySel = model.entrySel
    , sort = model.tableSort
    , filter = model.tableFilter
    , usagesOpenFor = model.usagesOpenFor
    }


{-| キーのセクション(種類は問わない — 配列も単体も同じフォームに掛ける)。 -}
sectionByKey : Model -> String -> Maybe Schema.Section
sectionByKey model key =
    case model.schemaState of
        SchemaReady schema ->
            schema.sections
                |> List.filter (\( k, _ ) -> k == key)
                |> List.head
                |> Maybe.map Tuple.second

        _ ->
            Nothing


viewRows : Model -> D.Value -> Schema.Section -> D.Value -> List Seg -> Html Msg
viewRows model doc section entry basePath =
    div [ HA.class "form-rows" ]
        (SchemaForm.rows { doc = doc, textures = model.textures, others = crossSources model } section entry
            |> List.filter (rowIsEnabled model basePath)
            |> List.map (viewRow model basePath)
        )


{-| enabledWhen の最終判定。条件を満たさないフィールドはフォームに出さない —
「今この場で実際にいじれて、効く物」だけを見せる(条件元を切り替えた瞬間に現れる)。
兄弟フィールドに打ちかけ(draft)があればそちらを今の値として使う。
-}
rowIsEnabled : Model -> List Seg -> SchemaForm.Row -> Bool
rowIsEnabled model basePath row =
    case row.condition of
        Nothing ->
            True

        Just cond ->
            let
                siblingPath =
                    basePath ++ [ KeySeg cond.field ]

                shown =
                    case model.activeDraft of
                        Just d ->
                            if d.path == siblingPath then
                                Just d.text

                            else
                                cond.currentText

                        Nothing ->
                            cond.currentText
            in
            case shown of
                Just v ->
                    List.member v cond.allowedTexts

                Nothing ->
                    False


{-| ラベル左・部品右の 2 カラム(ラベル幅固定)。縦 1 カラムだと空白の海になる。
weights だけはラベル上置き — 中に行の並ぶ広い部品で、2 カラムの残り幅では
スライダーが潰れるため。
-}
viewRow : Model -> List Seg -> SchemaForm.Row -> Html Msg
viewRow model basePath row =
    let
        path =
            basePath ++ [ KeySeg row.name ]

        -- 縦積み(ラベル上・部品下)。狭い右ペインでは横並びよりこちらが読みやすく、
        -- ラベルを truncate せず全文を折り返して見せられる(情報はラベル側に多い)
        labelText =
            div [ HA.class "form-label mb-0.5 text-[11px] leading-tight break-words text-ink-soft", HA.title row.label ]
                [ text row.label
                , if row.required then
                    -- * より読める形で(デジタル庁のフォーム流儀)
                    span [ HA.class "required ml-1 inline-flex rounded-sm bg-danger/15 px-1 text-[9px] leading-4 text-danger" ] [ text "必須" ]

                  else
                    text ""
                , case row.unit of
                    -- 単位はラベルの隣。数字だけでは何を表すか読めない
                    Just u ->
                        span [ HA.class "unit ml-1 text-[10px] text-ink-faint" ] [ text ("(" ++ u ++ ")") ]

                    Nothing ->
                        text ""
                , FormHelp.toggle HelpToggled model.helpOpen helpKey row.help
                ]

        helpKey =
            pathKey path

        -- 効き目のひとこと。ラベルは名前、こちらは「上げると何がどうなるか」
        hintText =
            case row.hint of
                Just h ->
                    div [ HA.class "form-hint mb-0.5 text-[10px] leading-tight break-words text-ink-faint" ]
                        [ text h ]

                Nothing ->
                    text ""
    in
    div [ HA.class "form-row mb-2", HA.id (fieldDomId path) ]
        [ labelText
        , hintText
        , FormHelp.body model.helpOpen helpKey row.help
        , viewControl model path row.control
        ]


{-| その欄が打ちかけ中なら draft の文字、でなければ文書の値。
フォーカス中の欄に文書の値を流し込まない規則はこの 1 関数に集約。
-}
draftTextFor : Maybe ActiveDraft -> List Seg -> String -> String
draftTextFor activeDraft path docValue =
    case activeDraft of
        Just d ->
            if d.path == path then
                d.text

            else
                docValue

        Nothing ->
            docValue


isDrafting : Maybe ActiveDraft -> List Seg -> Bool
isDrafting activeDraft path =
    case activeDraft of
        Just d ->
            d.path == path

        Nothing ->
            False


viewControl : Model -> List Seg -> SchemaForm.Control -> Html Msg
viewControl model path control =
    case control of
        SchemaForm.NumberControl n ->
            let
                spec =
                    { isInt = n.isInt, min = n.min, max = n.max, step = n.step }

                docValue =
                    n.value |> Maybe.map (Draft.format spec) |> Maybe.withDefault ""

                seed =
                    { path = path, kind = NumberDraft spec, original = docValue }

                shown =
                    draftTextFor model.activeDraft path docValue

                -- スライダーも他欄と同じ draft 経路(DraftTyped)を通す。draft に
                -- text を載せ、liveTypedCommit がライブ反映 ON なら即 doc へ流す。
                -- sl-change(離した時)は確定。これで value=shown が draft を追い、
                -- doc 往復を待たずスライダー位置がその場で動く(1 手遅れが消える)
                onSliderTyped eventName =
                    HE.on eventName
                        (sliderValue |> D.map (\v -> DraftTyped seed (Draft.format spec v)))

                onSliderCommitted eventName =
                    HE.on eventName
                        (sliderValue
                            |> D.map (\_ -> DraftCommitted { release = True })
                        )

                rangeAttrs =
                    List.filterMap identity
                        [ n.min |> Maybe.map (String.fromFloat >> HA.attribute "min")
                        , n.max |> Maybe.map (String.fromFloat >> HA.attribute "max")
                        , Just (HA.attribute "step" (rangeStep n.step n.isInt))
                        ]

                numberBox widthClass =
                    viewDraftBox model.activeDraft
                        { seed = seed
                        , valid = \t -> Draft.parse spec t /= Nothing
                        , classes = "number-box " ++ widthClass
                        , withArrows = True
                        }

                -- % 併記(widget unit / percent)。打ちかけ中はその値で追従する
                note =
                    percentNote spec n.percentFactor shown
            in
            if n.slider then
                div [ HA.class "control-slider flex items-center gap-2" ]
                    ([ Html.node "sl-range"
                        ([ HA.class "slider"

                         -- 他欄と同じく draft 優先(shown)。打った値がその場でスライダー
                         -- 位置になる — doc の値(docValue)直渡しだと往復 1 回分遅れる
                         , HA.attribute "value"
                            (if shown == "" then
                                "0"

                             else
                                shown
                            )
                         , onSliderTyped "sl-input"
                         , onSliderCommitted "sl-change"
                         ]
                            ++ rangeAttrs
                        )
                        []
                     , numberBox "w-[72px] shrink-0"
                     ]
                        ++ note
                    )

            else if List.isEmpty note then
                numberBox "w-full"

            else
                div [ HA.class "control-number flex items-center gap-2" ]
                    (numberBox "min-w-0 flex-1" :: note)

        SchemaForm.TextControl value ->
            let
                docValue =
                    Maybe.withDefault "" value

                seed =
                    { path = path, kind = TextDraft, original = docValue }
            in
            input
                [ HA.class "field w-full"
                , HA.type_ "text"
                , HA.value (draftTextFor model.activeDraft path docValue)
                , HE.onFocus (DraftStarted seed)
                , HE.onInput (DraftTyped seed)
                , HE.onBlur (DraftCommitted { release = True })
                , onDraftKeys seed False
                ]
                []

        SchemaForm.MultilineControl value ->
            let
                docValue =
                    Maybe.withDefault "" value

                seed =
                    { path = path, kind = TextDraft, original = docValue }
            in
            -- Enter は改行に使うので確定キーにしない(blur 確定・Esc 破棄だけ)
            textarea
                [ HA.class "field multiline h-auto min-h-16 w-full resize-y py-1 leading-relaxed"
                , HA.spellcheck False
                , HA.value (draftTextFor model.activeDraft path docValue)
                , HE.onFocus (DraftStarted seed)
                , HE.onInput (DraftTyped seed)
                , HE.onBlur (DraftCommitted { release = True })
                , escCancels
                ]
                []

        SchemaForm.GridControl grid ->
            let
                docValue =
                    Maybe.withDefault "" grid.text

                seed =
                    { path = path, kind = GridDraft, original = docValue }
            in
            -- マップが絵として見えることが第一: 等幅・ソフトラップ無し(横は
            -- 内部スクロール)・行数ぶんの高さ(大きすぎる物は内部スクロール)。
            -- Enter は行追加に使うので確定キーにしない(blur 確定・Esc 破棄)
            textarea
                [ HA.class "field grid-box h-auto w-full resize-y overflow-auto py-1 font-mono text-[11px] leading-[1.35] whitespace-pre"
                , HA.attribute "wrap" "off"
                , HA.rows (clamp 8 24 grid.lineCount)
                , HA.spellcheck False
                , HA.value (draftTextFor model.activeDraft path docValue)
                , HE.onFocus (DraftStarted seed)
                , HE.onInput (DraftTyped seed)
                , HE.onBlur (DraftCommitted { release = True })
                , escCancels
                ]
                []

        SchemaForm.UiDocControl value ->
            let
                docValue =
                    Maybe.withDefault "" value

                seed =
                    { path = path, kind = TextDraft, original = docValue }
            in
            div [ HA.class "control-uidoc" ]
                [ div [ HA.class "flex items-start gap-2" ]
                    [ case value of
                        Just p ->
                            viewPortrait model 48 p

                        Nothing ->
                            div [ HA.class "portrait-box portrait-blank", HA.style "width" "48px", HA.style "height" "48px" ] []
                    , input
                        [ HA.class "field min-w-0 flex-1"
                        , HA.type_ "text"
                        , HA.placeholder "ui.json のパス"
                        , HA.value (draftTextFor model.activeDraft path docValue)
                        , HE.onFocus (DraftStarted seed)
                        , HE.onInput (DraftTyped seed)
                        , HE.onBlur (DraftCommitted { release = True })
                        , onDraftKeys seed False
                        ]
                        []
                    ]
                , case value of
                    Just p ->
                        div [ HA.class "uidoc-hint mt-1 font-mono text-[10px] text-ink-faint" ]
                            [ text ("絵の編集は flix_ge_editor で: " ++ p) ]

                    Nothing ->
                        text ""
                ]

        SchemaForm.BoolControl value ->
            input
                [ HA.type_ "checkbox"
                , HA.checked (value == Just True)
                , HE.onCheck (\b -> FieldEdited { op = SetOp, path = path, value = E.bool b, isInt = False })
                ]
                []

        SchemaForm.EnumControl e ->
            if e.segmented then
                div [ HA.class "segmented inline-flex flex-wrap gap-0.5 rounded-md border border-edge bg-well p-0.5" ]
                    (e.choices
                        |> List.map
                            (\choice ->
                                button
                                    [ HA.classList
                                        [ ( "cursor-pointer rounded px-2 py-0.5 text-[11px] leading-4", True )
                                        , ( "on bg-raised text-ink", e.selected == Just choice )
                                        , ( "text-ink-soft hover:text-ink", e.selected /= Just choice )
                                        ]
                                    , HE.onClick (FieldEdited { op = SetOp, path = path, value = E.string choice, isInt = False })
                                    ]
                                    [ text choice ]
                            )
                    )

            else
                viewSelect path e.choices e.selected

        SchemaForm.RefControl r ->
            viewSelect path r.choices r.selected

        SchemaForm.Vec2Control v ->
            viewVec2 model.activeDraft path v

        SchemaForm.ColorControl hex ->
            viewColor path hex

        SchemaForm.TextureControl t ->
            viewTexture model.activeDraft path t

        SchemaForm.ListTextControl items ->
            viewListText model path items

        SchemaForm.WeightsControl w ->
            viewWeights model path w

        SchemaForm.ReadOnlyControl raw ->
            div
                [ HA.class "read-only w-full truncate rounded border border-edge bg-well/50 px-2 py-1 font-mono text-[11px] leading-4 text-ink-soft"
                , HA.title "この形は読み取り表示のみ(\"#rrggbb\" の文字列にすると編集できます)"
                ]
                [ text raw ]

        SchemaForm.RawJsonControl current ->
            let
                seed =
                    { path = path, kind = RawJsonDraft, original = current }

                shown =
                    draftTextFor model.activeDraft path current

                invalid =
                    isDrafting model.activeDraft path
                        && (D.decodeString D.value shown |> Result.toMaybe) == Nothing

                -- 長い 1 行 JSON を折り返した後の見かけの行数を、幅を読めない view でも
                -- ざっくり見積もる(1 行 ≒ 48 文字)。改行があればその分も足す
                lineCount =
                    shown
                        |> String.split "\n"
                        |> List.map (\l -> max 1 ((String.length l + 47) // 48))
                        |> List.sum
            in
            -- 生 JSON(sprites の parts・shader の out など自由形)を複数行の等幅
            -- テキストエリアで編集する。grid と違い折り返しは on(長い 1 行 JSON は
            -- 折り返した方が読める)。行数連動の高さ・大きすぎる物は内部スクロール。
            -- Enter は改行に使うので確定キーにしない(blur 確定・Esc 破棄)
            textarea
                [ HA.classList
                    [ ( "raw-json field h-auto w-full resize-y overflow-auto py-1 font-mono text-[11px] leading-relaxed break-all whitespace-pre-wrap", True )
                    , ( "invalid border-danger", invalid )
                    ]
                , HA.rows (clamp 3 16 lineCount)
                , HA.spellcheck False
                , HA.value shown
                , HE.onFocus (DraftStarted seed)
                , HE.onInput (DraftTyped seed)
                , HE.onBlur (DraftCommitted { release = True })
                , escCancels
                ]
                []


{-| スライダーは操作=意図が明確なのでその場で確定(draft を挟まない)。
sl-range の value はプロパティが数値。文字列で返す系にも備えて両様で読む。
-}
sliderValue : D.Decoder Float
sliderValue =
    D.oneOf
        [ D.at [ "target", "value" ] D.float
        , D.at [ "target", "value" ] D.string
            |> D.andThen
                (\raw ->
                    case String.toFloat raw of
                        Just v ->
                            D.succeed v

                        Nothing ->
                            D.fail "数値になっていない入力は反映しない"
                )
        ]


{-| sl-range は step="any" を受け付けない(数値必須)ため、未指定の float は細かい固定刻み。 -}
rangeStep : Maybe Float -> Bool -> String
rangeStep step isInt =
    case ( step, isInt ) of
        ( Just s, _ ) ->
            String.fromFloat s

        ( Nothing, True ) ->
            "1"

        ( Nothing, False ) ->
            "0.01"


{-| 打ちかけ(draft)方式の数値・重み入力欄。
type="number" にしないのは、ブラウザ標準のスピナー(↑↓増減)とこちらの
step 処理が二重に効いて 1 押しで 2 目盛り動くため。数字キーボードは inputmode で出す。
赤枠は「今確定しても文書に入らない」の印。打ちかけ中だけ判定する。
-}
viewDraftBox :
    Maybe ActiveDraft
    ->
        { seed : DraftSeed
        , valid : String -> Bool
        , classes : String
        , withArrows : Bool
        }
    -> Html Msg
viewDraftBox activeDraft opts =
    let
        shown =
            draftTextFor activeDraft opts.seed.path opts.seed.original

        invalid =
            isDrafting activeDraft opts.seed.path && not (opts.valid shown)
    in
    input
        [ HA.type_ "text"
        , HA.attribute "inputmode" "decimal"
        , HA.classList
            [ ( "field " ++ opts.classes, True )
            , ( "invalid border-danger", invalid )
            ]
        , HA.value shown
        , HE.onFocus (DraftStarted opts.seed)
        , HE.onInput (DraftTyped opts.seed)
        , HE.onBlur (DraftCommitted { release = True })
        , onDraftKeys opts.seed opts.withArrows
        ]
        []


{-| % 併記(0.35 → 35%)。数字になっていない打ちかけの間は "–" で場所だけ保つ。 -}
percentNote : Draft.NumberSpec -> Maybe Float -> String -> List (Html Msg)
percentNote spec factor shown =
    case factor of
        Nothing ->
            []

        Just f ->
            [ span [ HA.class "unit-note w-9 shrink-0 text-right text-[11px] tabular-nums text-ink-faint" ]
                [ text
                    (case Draft.parse spec shown of
                        Just v ->
                            percentText (v * f)

                        Nothing ->
                            "–"
                    )
                ]
            ]


{-| 0.1 刻みまでで丸めた % 文字(0.35×100 の桁ゴミを見せない)。 -}
percentText : Float -> String
percentText p =
    let
        rounded =
            toFloat (round (p * 10)) / 10
    in
    (if rounded == toFloat (round rounded) then
        String.fromInt (round rounded)

     else
        String.fromFloat rounded
    )
        ++ "%"


{-| vec2 の [x][y] 2 連 draft 数値欄。軸ごとに文書の x / y キーへ書き戻す。 -}
viewVec2 : Maybe ActiveDraft -> List Seg -> { x : Maybe Float, y : Maybe Float } -> Html Msg
viewVec2 activeDraft path v =
    let
        axisSpec =
            { isInt = False, min = Nothing, max = Nothing, step = Nothing }

        axisBox name value =
            let
                docValue =
                    value |> Maybe.map (Draft.format axisSpec) |> Maybe.withDefault ""

                seed =
                    { path = path ++ [ KeySeg name ], kind = NumberDraft axisSpec, original = docValue }
            in
            [ span [ HA.class "text-[11px] text-ink-faint" ] [ text name ]
            , viewDraftBox activeDraft
                { seed = seed
                , valid = \t -> Draft.parse axisSpec t /= Nothing
                , classes = "vec2-" ++ name ++ " w-full"
                , withArrows = True
                }
            ]
    in
    div [ HA.class "control-vec2 grid grid-cols-[auto_minmax(0,1fr)_auto_minmax(0,1fr)] items-center gap-x-1.5" ]
        (axisBox "x" v.x ++ axisBox "y" v.y)


{-| 色(#rrggbb 文字列)。ピッカーで選ぶとその場で確定(スライダーと同じ流儀)。 -}
viewColor : List Seg -> Maybe String -> Html Msg
viewColor path hex =
    div [ HA.class "control-color flex items-center gap-2" ]
        [ Html.node "sl-color-picker"
            ([ HA.class "color-picker"
             , HA.attribute "size" "small"
             , HA.attribute "format" "hex"
             , HA.attribute "no-format-toggle" ""
             , HE.on "sl-change"
                (D.at [ "target", "value" ] D.string
                    |> D.map
                        (\v ->
                            FieldEdited { op = SetOp, path = path, value = E.string (String.toLower v), isInt = False }
                        )
                )
             ]
                ++ (hex |> Maybe.map (\h -> [ HA.attribute "value" h ]) |> Maybe.withDefault [])
            )
            []
        , span [ HA.class "color-hex font-mono text-[11px] text-ink-soft" ]
            [ text (Maybe.withDefault "(未設定)" hex) ]
        ]


{-| テクスチャ名。project.json の manifest 名を候補(datalist)に出しつつ
自由入力も通す — manifest が空でもただの文字欄として生きる。
-}
viewTexture : Maybe ActiveDraft -> List Seg -> { value : Maybe String, choices : List String } -> Html Msg
viewTexture activeDraft path t =
    let
        docValue =
            Maybe.withDefault "" t.value

        seed =
            { path = path, kind = TextDraft, original = docValue }

        listId =
            "textures-" ++ pathDomId path
    in
    div [ HA.class "control-texture w-full" ]
        [ input
            [ HA.class "texture-input field w-full font-mono"
            , HA.type_ "text"
            , HA.list listId
            , HA.placeholder "テクスチャ名"
            , HA.value (draftTextFor activeDraft path docValue)
            , HE.onFocus (DraftStarted seed)
            , HE.onInput (DraftTyped seed)
            , HE.onBlur (DraftCommitted { release = True })
            , onDraftKeys seed False
            ]
            []
        , datalist [ HA.id listId ] (t.choices |> List.map (\c -> option [ HA.value c ] []))
        ]


{-| datalist の id 用。パスは 1 画面に同じ物が 2 つ出ない(フォームは選択 1 件)。 -}
pathDomId : List Seg -> String
pathDomId path =
    path
        |> List.map
            (\seg ->
                case seg of
                    KeySeg key ->
                        key

                    IdxSeg i ->
                        String.fromInt i
            )
        |> String.join "-"


{-| 合計固定の複数連動スライダー(weights)。1 本動かすと他が比例配分され、
書き戻しは変わった行だけの applyDocEdits 1 バッチ(途中状態を見せない)。
-}
viewWeights : Model -> List Seg -> { config : Weights.Config, entries : List ( String, Float ) } -> Html Msg
viewWeights model path w =
    let
        config =
            w.config

        sum =
            List.sum (List.map Tuple.second w.entries)

        -- 手編集で合計が崩れた文書は隠さず黄色で見せる(スライダー 1 操作で直る)
        sumOk =
            round (sum * toFloat (10 ^ config.decimals)) == round (config.total * toFloat (10 ^ config.decimals))

        header =
            div [ HA.class "weights-head mb-1 flex items-center justify-between" ]
                [ span [ HA.class "text-[10px] tracking-wide text-ink-faint" ] [ text "重み配分" ]
                , span
                    [ HA.classList
                        [ ( "weights-sum text-[10px] tabular-nums", True )
                        , ( "text-ink-faint", sumOk )
                        , ( "text-warn", not sumOk )
                        ]
                    ]
                    [ text ("合計 " ++ Weights.format config sum ++ " / " ++ Weights.format config config.total) ]
                ]
    in
    div [ HA.class "control-weights w-full rounded border border-edge bg-well/40 p-1.5" ]
        (header
            :: List.map (viewWeightRow model path w) w.entries
            ++ [ viewWeightsAdd model path ]
        )


viewWeightRow : Model -> List Seg -> { config : Weights.Config, entries : List ( String, Float ) } -> ( String, Float ) -> Html Msg
viewWeightRow model path w ( key, value ) =
    let
        config =
            w.config

        docValue =
            Weights.format config value

        seed =
            { path = path ++ [ KeySeg key ]
            , kind = WeightsDraft { config = config, fieldPath = path, key = key, entries = w.entries }
            , original = docValue
            }

        -- 数値ボックスに打ちかけがあればスライダーもその値を映す(打った値が
        -- その場でつまみ位置になる)。weights は連動再配分なので draft でなく
        -- バッチ即書き(onSlider)のまま — value だけ draft 優先に揃える
        shown =
            draftTextFor model.activeDraft (path ++ [ KeySeg key ]) docValue

        -- スライダー: 動かした瞬間に配分し直して 1 バッチで確定(変化なしは流さない)
        onSlider eventName =
            HE.on eventName
                (sliderValue
                    |> D.andThen
                        (\v ->
                            case Weights.changedEntries w.entries (Weights.redistribute config key v w.entries) of
                                [] ->
                                    D.fail "変化なしは反映しない"

                                edits ->
                                    D.succeed (EditsQueued [ weightsBatchPayload config path edits ])
                        )
                )

        -- 行の削除 = キーの Remove + 残りの再配分バッチの 2 本組
        removeEdits =
            { op = RemoveOp, path = path ++ [ KeySeg key ], value = E.null, isInt = False }
                :: (case
                        Weights.changedEntries
                            (List.filter (\( k, _ ) -> k /= key) w.entries)
                            (Weights.removeKey config key w.entries)
                    of
                        [] ->
                            []

                        edits ->
                            [ weightsBatchPayload config path edits ]
                   )
    in
    div [ HA.class "weight-row mb-1 grid grid-cols-[56px_minmax(0,1fr)_44px_auto] items-center gap-x-1.5" ]
        [ span [ HA.class "weight-key truncate font-mono text-[11px] text-ink-soft", HA.title key ] [ text key ]
        , Html.node "sl-range"
            [ HA.class "slider"
            , HA.attribute "min" "0"
            , HA.attribute "max" (String.fromFloat config.total)
            , HA.attribute "step" (rangeStep Nothing (config.decimals == 0))
            , HA.attribute "value" shown
            , onSlider "sl-input"
            , onSlider "sl-change"
            ]
            []
        , viewDraftBox model.activeDraft
            { seed = seed
            , valid = \t -> Draft.parse (weightsSpec config) t /= Nothing
            , classes = "weight-box w-full text-right tabular-nums"
            , withArrows = False
            }
        , button
            [ HA.class "weight-remove btn btn-ghost btn-mini text-ink-faint hover:text-danger"
            , HA.title (key ++ " の行を削除(残りへ再配分)")
            , HE.onClick (EditsQueued removeEdits)
            ]
            [ text "✕" ]
        ]


{-| 行の追加(キー名のインライン入力・Enter 確定 / Esc 破棄)。 -}
viewWeightsAdd : Model -> List Seg -> Html Msg
viewWeightsAdd model path =
    case model.weightsAdd |> Maybe.andThen (matching path) of
        Just w ->
            div [ HA.class "weights-add mt-1" ]
                [ input
                    [ HA.classList
                        [ ( "weights-add-input field w-full font-mono", True )
                        , ( "invalid border-danger", w.error /= Nothing )
                        ]
                    , HA.type_ "text"
                    , HA.placeholder "新しいキー名"
                    , HA.value w.text
                    , HE.onInput WeightsAddTyped
                    , commitCancelKeys WeightsAddCommitted WeightsAddCancelled
                    ]
                    []
                , case w.error of
                    Just reason ->
                        div [ HA.class "mt-0.5 text-[11px] text-danger" ] [ text reason ]

                    Nothing ->
                        div [ HA.class "mt-0.5 text-[11px] text-ink-faint" ] [ text "Enter で追加(重み 0 で入ります)・Esc でやめる" ]
                ]

        Nothing ->
            button
                [ HA.class "weights-add-open btn btn-ghost btn-mini mt-0.5"
                , HE.onClick (WeightsAddOpened path)
                ]
                [ text "＋ 行を追加" ]


{-| 文字列の列(台詞など)。1 行 1 欄で縦に並べ、行ごとに ↑↓ と ✕、末尾に
「行を追加」。どの操作も「新しい列を丸ごと書く」1 本の編集に落ちる。
空行もそのまま持つ — 書いた空行を勝手に間引かない。
-}
viewListText : Model -> List Seg -> List String -> Html Msg
viewListText model path items =
    div [ HA.class "control-listtext w-full rounded border border-edge bg-well/40 p-1.5" ]
        ((if List.isEmpty items then
            [ div [ HA.class "mb-1 text-[11px] text-ink-faint" ] [ text "(まだ 1 行も無い)" ] ]

          else
            items |> List.indexedMap (viewListTextRow model path items)
         )
            ++ [ button
                    [ HA.class "listtext-add btn btn-ghost btn-mini mt-0.5"
                    , HE.onClick (FieldEdited (listTextPayload path (SchemaForm.applyListEdit SchemaForm.AddLine items)))
                    ]
                    [ text "＋ 行を追加" ]
               ]
        )


viewListTextRow : Model -> List Seg -> List String -> Int -> String -> Html Msg
viewListTextRow model path items index value =
    let
        seed =
            { path = path ++ [ IdxSeg index ]
            , kind = ListTextDraft { fieldPath = path, index = index, items = items }
            , original = value
            }

        listButton label title_ enabled edit =
            button
                [ HA.class "btn btn-ghost btn-mini shrink-0 text-ink-faint hover:text-ink"
                , HA.title title_
                , HA.disabled (not enabled)
                , HE.onClick (FieldEdited (listTextPayload path (SchemaForm.applyListEdit edit items)))
                ]
                [ text label ]
    in
    div [ HA.class "listtext-row mb-1 flex items-center gap-1" ]
        [ span [ HA.class "w-4 shrink-0 text-right font-mono text-[10px] text-ink-faint" ]
            [ text (String.fromInt (index + 1)) ]
        , input
            [ HA.class "field min-w-0 flex-1"
            , HA.type_ "text"
            , HA.value (draftTextFor model.activeDraft seed.path value)
            , HE.onFocus (DraftStarted seed)
            , HE.onInput (DraftTyped seed)
            , HE.onBlur (DraftCommitted { release = True })
            , onDraftKeys seed False
            ]
            []
        , listButton "↑" "1 つ上へ" (index > 0) (SchemaForm.MoveLine index -1)
        , listButton "↓" "1 つ下へ" (index < List.length items - 1) (SchemaForm.MoveLine index 1)
        , button
            [ HA.class "listtext-remove btn btn-ghost btn-mini shrink-0 text-ink-faint hover:text-danger"
            , HA.title "この行を削除"
            , HE.onClick (FieldEdited (listTextPayload path (SchemaForm.applyListEdit (SchemaForm.RemoveLine index) items)))
            ]
            [ text "✕" ]
        ]


matching : List Seg -> WeightsAddState -> Maybe WeightsAddState
matching path w =
    if w.path == path then
        Just w

    else
        Nothing


{-| 打ちかけ欄のキー運転。Enter=確定(focus は残す)・Esc=破棄、
数値欄(withArrows)だけ ↑↓=step 増減(Shift ×10)。
preventDefault は ↑↓ でカーソルが行頭へ飛ぶ・Enter が textarea に改行を
挿すのを止めるため。他のキーはデコーダ失敗で素通し。
-}
onDraftKeys : DraftSeed -> Bool -> Html.Attribute Msg
onDraftKeys seed withArrows =
    HE.custom "keydown"
        (D.map2 Tuple.pair (D.field "key" D.string) (D.field "shiftKey" D.bool)
            |> D.andThen
                (\( key, shift ) ->
                    case ( key, withArrows ) of
                        ( "Enter", _ ) ->
                            D.succeed (draftKey (DraftCommitted { release = False }))

                        ( "Escape", _ ) ->
                            D.succeed (draftKey DraftCancelled)

                        ( "ArrowUp", True ) ->
                            D.succeed (draftKey (DraftStepped seed { dir = 1, shift = shift }))

                        ( "ArrowDown", True ) ->
                            D.succeed (draftKey (DraftStepped seed { dir = -1, shift = shift }))

                        _ ->
                            D.fail "他のキーは素通し"
                )
        )


draftKey : Msg -> { message : Msg, stopPropagation : Bool, preventDefault : Bool }
draftKey msg =
    { message = msg, stopPropagation = False, preventDefault = True }


viewSelect : List Seg -> List String -> Maybe String -> Html Msg
viewSelect path choices selected =
    let
        -- 先頭の飾り("(未設定)")を選んでも値は書かない: 選択肢の実在チェックで弾く
        onChange =
            HE.on "change"
                (HE.targetValue
                    |> D.andThen
                        (\choice ->
                            if List.member choice choices then
                                D.succeed (FieldEdited { op = SetOp, path = path, value = E.string choice, isInt = False })

                            else
                                D.fail "選択肢にない値は反映しない"
                        )
                )
    in
    -- 色欄が 1 つの value で表示を握るのと同じく、select も value プロパティで
    -- 表示を doc に握らせる。option の selected だけだと、ブラウザの select は
    -- 選んだ見た目を保ったまま change を取りこぼす形があり、色と挙動がずれる
    -- (ライブ保存の往復で doc が 1 手遅れるときに顕在化する)。
    select
        [ HA.class "field w-full"
        , onChange
        , HA.property "value" (E.string (Maybe.withDefault "" selected))
        ]
        ((if selected == Nothing then
            [ option [ HA.value "" ] [ text "(未設定)" ] ]

          else
            []
         )
            ++ (choices
                    |> List.map
                        (\choice ->
                            option [ HA.value choice ] [ text choice ]
                        )
               )
        )


-- 「+ 新しいファイル」ウィザード(3 ペインの代わりに出す切替画面)


viewWizard : Model -> WizardState -> Html Msg
viewWizard model w =
    div [ HA.class "wizard flex-1 overflow-y-auto px-6 py-4" ]
        (List.concat
            [ [ div [ HA.class "wizard-head mb-4 flex items-center gap-3" ]
                    [ h2 [ HA.class "m-0 text-[13px] font-semibold" ] [ text "+ 新しいファイル" ]
                    , viewWizardSteps w.step
                    , span [ HA.class "spacer flex-1" ] []
                    , button
                        [ HA.class "btn", HE.onClick WizardClosed, HA.disabled (w.write /= WizNotStarted) ]
                        [ text "やめて戻る" ]
                    ]
              ]
            , case w.step of
                WizBasics ->
                    viewWizardBasics w.draft

                WizFields ->
                    viewWizardFields w.draft

                WizConfirm ->
                    viewWizardConfirm model w
            , viewWizardNav model w
            ]
        )


viewWizardSteps : WizardStep -> Html Msg
viewWizardSteps current =
    let
        pill step num title =
            button
                [ HA.classList
                    [ ( "wizard-step flex cursor-pointer items-center gap-1.5 rounded px-2 py-1 text-[11px]", True )
                    , ( "on text-ink", step == current )
                    , ( "text-ink-soft hover:bg-white/5 hover:text-ink", step /= current )
                    ]
                , HE.onClick (WizardStepChosen step)
                ]
                [ span
                    [ HA.class
                        ("flex h-4 w-4 items-center justify-center rounded-full text-[10px] leading-none "
                            ++ (if step == current then
                                    "bg-accent text-white"

                                else
                                    "bg-white/10 text-ink-soft"
                               )
                        )
                    ]
                    [ text num ]
                , text title
                ]

        arrow =
            span [ HA.class "text-ink-faint" ] [ text "›" ]
    in
    span [ HA.class "wizard-steps flex items-center gap-1" ]
        [ pill WizBasics "1" "基本"
        , arrow
        , pill WizFields "2" "フィールド"
        , arrow
        , pill WizConfirm "3" "確認と生成"
        ]


viewWizardBasics : Wizard.Draft -> List (Html Msg)
viewWizardBasics draft =
    [ viewWizardRow "名前 (id・半角英数)" <|
        input
            [ HA.class "field w-full", HA.type_ "text", HA.placeholder "半角英数の名前", HA.value draft.id, HE.onInput WizardIdChanged ]
            []
    , viewWizardRow "タイトル (日本語可・任意)" <|
        input
            [ HA.class "field w-full", HA.type_ "text", HA.placeholder "画面に出る名前", HA.value draft.title, HE.onInput WizardTitleChanged ]
            []
    , viewWizardRow "置き場所" <|
        input
            [ HA.class "field w-full font-mono", HA.type_ "text", HA.value (Wizard.dataPathOf draft), HE.onInput WizardPathChanged ]
            []
    , viewWizardRow "形" <|
        div [ HA.class "shape-cards flex gap-2" ]
            [ viewShapeCard draft.shape Wizard.ShapeCatalog "catalog" "名前で引く一覧(エントリ名がキー)"
            , viewShapeCard draft.shape Wizard.ShapeList "list" "並び順に意味がある列"
            , viewShapeCard draft.shape Wizard.ShapeRecord "record" "1 個だけの設定まとまり"
            ]
    ]


viewWizardRow : String -> Html Msg -> Html Msg
viewWizardRow labelText control =
    div [ HA.class "wizard-row my-3 max-w-xl" ]
        [ div [ HA.class "form-label mb-1 text-[11px] text-ink-soft" ] [ text labelText ]
        , control
        ]


viewShapeCard : Wizard.Shape -> Wizard.Shape -> String -> String -> Html Msg
viewShapeCard current shape name desc =
    button
        [ HA.classList
            [ ( "shape-card flex-1 cursor-pointer rounded border p-3 text-left", True )
            , ( "on border-accent bg-accent/10", current == shape )
            , ( "border-edge bg-well hover:bg-white/5", current /= shape )
            ]
        , HE.onClick (WizardShapeChosen shape)
        ]
        [ div
            [ HA.class
                ("shape-name mb-1 text-xs font-semibold "
                    ++ (if current == shape then
                            "text-accent"

                        else
                            "text-ink"
                       )
                )
            ]
            [ text name ]
        , div [ HA.class "shape-desc text-[11px] leading-relaxed text-ink-soft" ] [ text desc ]
        ]


viewWizardFields : Wizard.Draft -> List (Html Msg)
viewWizardFields draft =
    let
        total =
            List.length draft.fields
    in
    [ div [ HA.class "form-label mb-1 text-[11px] text-ink-soft" ] [ text "フィールド定義(上から order 順にフォームへ並びます)" ] ]
        ++ List.indexedMap (viewWizardFieldRow total) draft.fields
        ++ [ button [ HA.class "add-field btn mt-2", HE.onClick WizardFieldAdded ] [ text "+ フィールド追加" ] ]


viewWizardFieldRow : Int -> Int -> Wizard.FieldDraft -> Html Msg
viewWizardFieldRow total i f =
    div [ HA.class "wizard-field my-1.5 flex items-center gap-1.5" ]
        [ span [ HA.class "move flex gap-0.5" ]
            [ button [ HA.class "btn btn-mini", HE.onClick (WizardFieldMoved i -1), HA.disabled (i == 0) ] [ text "↑" ]
            , button [ HA.class "btn btn-mini", HE.onClick (WizardFieldMoved i 1), HA.disabled (i == total - 1) ] [ text "↓" ]
            ]
        , input
            [ HA.class "f-name field w-[150px] shrink-0"
            , HA.type_ "text"
            , HA.placeholder "フィールド名"
            , HA.value f.name
            , HE.onInput (\s -> WizardFieldChanged i { f | name = s })
            ]
            []
        , viewTypeSelect i f
        , viewFieldExtra i f
        , input
            [ HA.class "f-label field w-[150px] shrink-0"
            , HA.type_ "text"
            , HA.placeholder "表示名(日本語)"
            , HA.value f.label
            , HE.onInput (\s -> WizardFieldChanged i { f | label = s })
            ]
            []
        , label [ HA.class "f-required flex items-center gap-1 text-[11px] whitespace-nowrap text-ink-soft" ]
            [ input
                [ HA.type_ "checkbox"
                , HA.checked f.required
                , HE.onCheck (\b -> WizardFieldChanged i { f | required = b })
                ]
                []
            , text "必須"
            ]
        , button [ HA.class "btn", HE.onClick (WizardFieldRemoved i) ] [ text "削除" ]
        ]


viewTypeSelect : Int -> Wizard.FieldDraft -> Html Msg
viewTypeSelect i f =
    select
        [ HA.class "field w-24 shrink-0"
        , HE.on "change"
            (HE.targetValue
                |> D.andThen
                    (\name ->
                        case Wizard.typeChoiceFromName name of
                            Just choice ->
                                D.succeed (WizardFieldChanged i { f | type_ = choice })

                            Nothing ->
                                D.fail "選択肢にない type は反映しない"
                    )
            )
        ]
        (Wizard.typeChoices
            |> List.map
                (\choice ->
                    let
                        name =
                            Wizard.typeChoiceName choice
                    in
                    option [ HA.value name, HA.selected (choice == f.type_) ] [ text name ]
                )
        )


{-| type ごとの追加入力。enum は値の列・ref は参照先・数値は min/max、それ以外は無し。 -}
viewFieldExtra : Int -> Wizard.FieldDraft -> Html Msg
viewFieldExtra i f =
    div [ HA.class "f-extra flex min-w-0 flex-1 items-center gap-1.5" ]
        (case f.type_ of
            Wizard.CEnum ->
                [ input
                    [ HA.class "field min-w-0 flex-1"
                    , HA.type_ "text"
                    , HA.placeholder "値をカンマ区切りで並べます"
                    , HA.value f.enumValues
                    , HE.onInput (\s -> WizardFieldChanged i { f | enumValues = s })
                    ]
                    []
                ]

            Wizard.CRef ->
                [ input
                    [ HA.class "field min-w-0 flex-1"
                    , HA.type_ "text"
                    , HA.placeholder "参照先のセクション名"
                    , HA.value f.refTarget
                    , HE.onInput (\s -> WizardFieldChanged i { f | refTarget = s })
                    ]
                    []
                ]

            Wizard.CInt ->
                viewMinMax i f

            Wizard.CFloat ->
                viewMinMax i f

            _ ->
                [ span [ HA.class "dim text-ink-faint" ] [ text "—" ] ]
        )


viewMinMax : Int -> Wizard.FieldDraft -> List (Html Msg)
viewMinMax i f =
    [ input
        [ HA.class "f-minmax field w-[90px] shrink-0"
        , HA.type_ "text"
        , HA.placeholder "min(任意)"
        , HA.value f.minText
        , HE.onInput (\s -> WizardFieldChanged i { f | minText = s })
        ]
        []
    , input
        [ HA.class "f-minmax field w-[90px] shrink-0"
        , HA.type_ "text"
        , HA.placeholder "max(任意)"
        , HA.value f.maxText
        , HE.onInput (\s -> WizardFieldChanged i { f | maxText = s })
        ]
        []
    ]


viewWizardConfirm : Model -> WizardState -> List (Html Msg)
viewWizardConfirm model w =
    let
        errors =
            wizardErrors model w.draft
    in
    if not (List.isEmpty errors) then
        [ Html.node "sl-alert"
            [ HA.class "wizard-errors notice my-3 block max-w-xl"
            , HA.attribute "variant" "warning"
            , HA.attribute "open" ""
            ]
            (div [] [ text "直すところがあります(直すと生成プレビューが出ます):" ]
                :: List.map (\e -> div [] [ text ("・" ++ e) ]) errors
            )
        ]

    else
        [ div [ HA.class "wizard-previews flex flex-wrap items-start gap-3" ]
            [ viewGenPreview ("① スキーマ → " ++ Wizard.schemaPathOf w.draft) (Wizard.schemaText w.draft)
            , viewGenPreview ("② データ雛形 → " ++ Wizard.dataPathOf w.draft) (Wizard.dataText w.draft)
            , viewGenPreview "③ project.json — \"editor\".\"resources\" 末尾へ追記(他のキー・注釈はそのまま)"
                (Wizard.declText w.draft)
            ]
        ]


viewGenPreview : String -> String -> Html Msg
viewGenPreview name content =
    div [ HA.class "wizard-preview min-w-[280px] flex-1" ]
        [ div [ HA.class "name mb-1 font-mono text-[11px] text-ink-faint" ] [ text name ]
        , pre [ HA.class "m-0 max-h-[340px] overflow-auto rounded border border-edge bg-app p-2 font-mono text-[11px] leading-relaxed" ] [ text content ]
        ]


viewWizardNav : Model -> WizardState -> List (Html Msg)
viewWizardNav model w =
    let
        busy =
            w.write /= WizNotStarted

        back step =
            button [ HA.class "btn", HE.onClick (WizardStepChosen step), HA.disabled busy ] [ text "← 戻る" ]

        next step =
            button [ HA.class "btn", HE.onClick (WizardStepChosen step) ] [ text "次へ →" ]
    in
    [ div [ HA.class "wizard-nav mt-5 flex items-center gap-2" ]
        (List.concat
            [ case w.step of
                WizBasics ->
                    [ next WizFields ]

                WizFields ->
                    [ back WizBasics, next WizConfirm ]

                WizConfirm ->
                    [ back WizFields
                    , button
                        [ HA.class "create btn btn-primary"
                        , HE.onClick WizardCreateClicked
                        , HA.disabled (busy || not (List.isEmpty (wizardErrors model w.draft)))
                        ]
                        [ text (wizardCreateLabel w.write) ]
                    ]
            , case w.error of
                Just message ->
                    [ span [ HA.class "wizard-error text-[11px] text-danger" ] [ text message ] ]

                Nothing ->
                    []
            ]
        )
    ]


wizardCreateLabel : WizardWrite -> String
wizardCreateLabel write =
    case write of
        WizNotStarted ->
            "作成"

        WizPutSchema _ ->
            "スキーマを書き込み中…"

        WizPutData _ ->
            "データ雛形を書き込み中…"

        WizGetProject _ ->
            "project.json を読み込み中…"

        WizEditProject _ ->
            "project.json へ追記中…"

        WizPutProject _ ->
            "project.json を書き込み中…"



-- 問題パネル(下部バー)


{-| 問題一覧は文書から計算し直す(数百エントリ規模なら全計算で足りる)。
問題があっても保存は止めない — バーは知らせるだけ。

lazy に包むのは、検査が文書全体を舐めるから — ドット絵の一筆はセルを跨ぐたびに
view が回るので、素の view のたびだと 89KB の検査がカーソルに付いて回る。
種(文書・スキーマ・横断辞書)が動かない限り前の結果でよい。
-}
viewProblemBar : Model -> Html Msg
viewProblemBar model =
    case ( model.current, model.schemaState ) of
        ( Just _, SchemaReady schema ) ->
            case model.docValue of
                Just doc ->
                    HL.lazy4 viewProblemBarBody model.crossSlots schema doc model.problemsOpen

                Nothing ->
                    div [ HA.class "problem-bar shrink-0 border-t border-edge bg-panel" ]
                        [ div [ HA.class "problem-head muted px-3 py-1.5 text-[11px] text-ink-faint" ] [ text "JSON が読めないため検査できません" ] ]

        _ ->
            text ""


viewProblemBarBody : List CrossSlot -> Schema.Schema -> D.Value -> Bool -> Html Msg
viewProblemBarBody slots schema doc open =
    viewProblems (Lint.checkAcross (crossSourcesOf slots) schema doc) open


viewProblems : List Lint.Problem -> Bool -> Html Msg
viewProblems problems open =
    let
        count =
            List.length problems

        head =
            button
                [ HA.classList
                    [ ( "problem-head flex w-full cursor-pointer items-center gap-2 px-3 py-1.5 text-left text-[11px]", True )
                    , ( "ok text-ink-faint", count == 0 )
                    , ( "warn text-ink-soft hover:text-ink", count > 0 )
                    ]
                , HE.onClick ProblemBarToggled
                ]
                (if count == 0 then
                    [ text "✓ 問題なし" ]

                 else
                    [ span [ HA.class "badge bg-warn/15 font-semibold text-warn" ] [ text (String.fromInt count) ]
                    , text "件の問題"
                    ]
                )
    in
    div [ HA.class "problem-bar shrink-0 border-t border-edge bg-panel" ]
        (head
            :: (if open && count > 0 then
                    [ div [ HA.class "problem-list max-h-40 overflow-y-auto border-t border-edge" ]
                        (problems
                            |> List.map
                                (\p ->
                                    button
                                        [ HA.class "problem-item block w-full cursor-pointer px-3 py-1 text-left text-[11px] text-ink-soft hover:bg-white/5 hover:text-ink"
                                        , HE.onClick (ProblemClicked p)
                                        ]
                                        [ text p.message ]
                                )
                        )
                    ]

                else
                    []
               )
        )


{-| dirty のまま移動しようとしたときの 2 択。保存競合ダイアログと同じ作法
(sl-dialog+btn/btn-primary)で、答えるまで移動を進めない。
-}
viewDiscardDialog : Html Msg
viewDiscardDialog =
    Html.node "sl-dialog"
        [ HA.class "discard"
        , HA.attribute "label" "未保存の編集があります"
        , HA.attribute "open" ""
        ]
        [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
            [ text "このファイルの編集はまだ保存していません。移動すると編集は失われます。" ]
        , div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
            [ button [ HA.class "btn", HE.onClick NavStayed ] [ text "やめる" ]

            -- 編集を捨てる危険操作なので primary でなく danger
            , button [ HA.class "btn btn-danger", HE.onClick NavDiscarded ] [ text "破棄して開く" ]
            ]
        ]


viewConflictDialog : Html Msg
viewConflictDialog =
    Html.node "sl-dialog"
        [ HA.class "conflict"
        , HA.attribute "label" "外部で変更あり"
        , HA.attribute "open" ""
        ]
        [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
            [ text "このファイルは別の場所で変更されています。再読み込みしてください(自分の編集で構わず上書きもできます)。" ]
        , div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
            [ button [ HA.class "btn", HE.onClick ReloadChosen ] [ text "再読込(自分の編集を捨てる)" ]

            -- 相手の変更を潰す危険操作なので primary でなく danger
            , button [ HA.class "btn btn-danger", HE.onClick OverwriteChosen ] [ text "構わず上書き" ]
            ]
        ]


{-| catalog エントリ追加の小ダイアログ。id を決めるだけ — 値はスキーマの
default/零値の雛形が入る(細部は追加後に右のフォームで直す)。
-}
viewAddDialog : AddDialogState -> Html Msg
viewAddDialog dialog =
    Html.node "sl-dialog"
        [ HA.class "add-entry"
        , HE.on "sl-request-close" (D.succeed AddCancelled)
        , HA.attribute "label" ("エントリを追加 — " ++ dialog.sectionKey)
        , HA.attribute "open" ""
        ]
        [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
            [ text "新しいエントリの id を入れてください。値はスキーマの既定値で入ります。" ]
        , input
            [ HA.classList
                [ ( "add-id field mt-2 w-full font-mono", True )
                , ( "invalid border-danger", dialog.error /= Nothing )
                ]
            , HA.type_ "text"
            , HA.placeholder "新しい id"
            , HA.value dialog.text
            , HE.onInput AddIdTyped
            , commitCancelKeys AddConfirmed AddCancelled
            ]
            []
        , case dialog.error of
            Just reason ->
                div [ HA.class "add-error mt-1 text-[11px] text-danger" ] [ text reason ]

            Nothing ->
                div [ HA.class "mt-1 text-[11px] text-ink-faint" ] [ text "Enter で追加・Esc でやめる" ]
        , div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
            [ button [ HA.class "btn", HE.onClick AddCancelled ] [ text "やめる" ]
            , button [ HA.class "btn btn-primary", HE.onClick AddConfirmed ] [ text "追加する" ]
            ]
        ]


{-| 使用中エントリの削除確認。件数と使用元を見せる — 黙って参照を宙に浮かせない。 -}
viewDeleteDialog : DeleteConfirmState -> Html Msg
viewDeleteDialog confirm =
    let
        name =
            case confirm.entry of
                ByKey key ->
                    "\"" ++ key ++ "\""

                ByIndex i ->
                    "#" ++ String.fromInt i
    in
    Html.node "sl-dialog"
        [ HA.class "delete-entry"
        , HE.on "sl-request-close" (D.succeed DeleteCancelled)
        , HA.attribute "label" "削除の確認"
        , HA.attribute "open" ""
        ]
        [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
            [ text
                (name
                    ++ " は "
                    ++ String.fromInt (List.length confirm.sites)
                    ++ " 箇所から使われています。削除すると参照が宙に浮きます。"
                )
            ]
        , div [ HA.class "delete-sites mt-2 max-h-40 overflow-y-auto rounded border border-edge bg-well p-2" ]
            (confirm.sites
                |> List.map
                    (\site ->
                        div [ HA.class "py-0.5 font-mono text-[11px] text-ink-soft" ]
                            [ text (Refs.siteLabel site) ]
                    )
            )
        , div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
            [ button [ HA.class "btn", HE.onClick DeleteCancelled ] [ text "やめる" ]

            -- 参照を宙に浮かせる危険操作なので primary でなく danger
            , button [ HA.class "btn btn-danger", HE.onClick DeleteConfirmed ] [ text "削除する" ]
            ]
        ]



-- 配線


{-| ⌘⇧F(開く / 閉じる)と、開いている間の Esc(閉じる)。 -}
searchKeyDecoder : Bool -> D.Decoder Msg
searchKeyDecoder isOpen =
    D.map4 (\key meta ctrl shift -> { key = key, meta = meta, ctrl = ctrl, shift = shift })
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "shiftKey" D.bool)
        |> D.andThen
            (\k ->
                if String.toLower k.key == "f" && (k.meta || k.ctrl) && k.shift then
                    D.succeed SearchToggled

                else if String.toLower k.key == "j" && (k.meta || k.ctrl) then
                    D.succeed JsonPaneToggled

                else if k.key == "Escape" && isOpen then
                    D.succeed SearchClosed

                else
                    D.fail "他のキーは素通し"
            )


{-| ⌘Z / Ctrl+Z(⇧付きと Ctrl+Y はやり直す)。文字を打っている欄の中では
渡さない — 欄の中の ⌘Z は「打った文字を戻す」ブラウザ本来の働きに任せる。
-}
historyKeyDecoder : D.Decoder Msg
historyKeyDecoder =
    D.map5 (\key meta ctrl shift tag -> { key = key, meta = meta, ctrl = ctrl, shift = shift, tag = tag })
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "shiftKey" D.bool)
        (D.oneOf [ D.at [ "target", "tagName" ] D.string, D.succeed "" ])
        |> D.andThen
            (\k ->
                if isTypingTag k.tag then
                    D.fail "欄の中は素通し"

                else
                    case ( String.toLower k.key, k.meta || k.ctrl, k.shift ) of
                        ( "z", True, False ) ->
                            D.succeed UndoPressed

                        ( "z", True, True ) ->
                            D.succeed RedoPressed

                        ( "y", True, _ ) ->
                            D.succeed RedoPressed

                        _ ->
                            D.fail "他のキーは素通し"
            )


{-| 文字を打つ場所か(素の欄と、Shoelace の欄部品)。 -}
isTypingTag : String -> Bool
isTypingTag tag =
    let
        name =
            String.toUpper tag
    in
    List.member name [ "INPUT", "TEXTAREA", "SELECT" ] || String.startsWith "SL-" name


{-| 文書全体の mousemove/mouseup はドラッグ中だけ購読する —
常時購読は全マウス操作で update が走るだけで得る物がない。
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ apiResponse GotApiResponse
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (D.map2 (\x y -> DragMoved { x = x, y = y })
                            (D.field "clientX" D.float)
                            (D.field "clientY" D.float)
                        )
                    , Browser.Events.onMouseUp (D.succeed DragEnded)
                    ]

            Nothing ->
                Sub.none
        , case model.paneDrag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (D.map PaneMoved (D.field "clientX" D.float))
                    , Browser.Events.onMouseUp (D.succeed PaneReleased)
                    ]

            Nothing ->
                Sub.none

        -- ドット絵の一筆はグリッドの外で離しても確定させる(描いている間だけ生きる)
        , if PixelEditor.strokeActive model.pixel then
            Browser.Events.onMouseUp (D.succeed (PixelMsg PixelEditor.StrokeEnded))

          else
            Sub.none

        -- マップの一筆も同じ(グリッドの外で離しても確定させる)
        , if MapEditor.strokeActive model.mapEd then
            Browser.Events.onMouseUp (D.succeed (MapMsg MapEditor.StrokeEnded))

          else
            Sub.none

        -- 「いま画面に出ている Doc」(ゲームが書く debug/active-docs.json)の定期確認
        , if model.screen == Editing then
            Time.every 2000 (\_ -> ActivePollTick)

          else
            Sub.none

        -- 採用オーバーレイの段送り(採用しました → 反映の報せ)。待ちの間だけ生きる
        , if Atelier.needsTick model.atelier then
            Time.every 1000 (\_ -> AtelierOverlayTick)

          else
            Sub.none

        -- 開いているファイルの見張り。外で変わったら、編集を始める前に気付ける
        , if model.current /= Nothing then
            Time.every 2000 (\_ -> FileWatchTick)

          else
            Sub.none

        -- ドット絵のコマ送り。動かしている間だけ時計を回す
        , if PixelEditor.isPlaying model.pixel then
            Time.every 140 (\_ -> PixelMsg PixelEditor.PlayTicked)

          else
            Sub.none

        -- 「✓ コピーしました」の戻し(2 秒)。コピー直後だけ生きる
        , if Atelier.needsCopyReset model.atelier then
            Time.every 2000 (\_ -> AtelierMsg Atelier.CopyResetTick)

          else
            Sub.none

        -- 右クリックメニューが開いている間だけ Esc で閉じる(外側クリックは受け皿の仕事)
        , if model.fileMenu /= Nothing then
            Browser.Events.onKeyDown
                (D.field "key" D.string
                    |> D.andThen
                        (\key ->
                            if key == "Escape" then
                                D.succeed FileMenuClosed

                            else
                                D.fail "他のキーは素通し"
                        )
                )

          else
            Sub.none

        -- ⌘⇧F で横断検索を開く / 閉じる。編集画面に居る間だけ(欄の中でも効く —
        -- 探すのは文字を打つ場所からでも呼びたい操作なので、⌘Z と扱いを分ける)
        , if model.screen == Editing then
            Browser.Events.onKeyDown (searchKeyDecoder (SearchView.isOpen model.search))

          else
            Sub.none

        -- ⌘Z / ⇧⌘Z(戻す・やり直す)。編集画面で、前面の編集器が自前の undo を
        -- 持っていない間だけ生かす — 盤面・ドット絵の ⌘Z はそちらの持ち場
        , if model.screen == Editing && model.current /= Nothing && not (ownUndoFront model) then
            Browser.Events.onKeyDown historyKeyDecoder

          else
            Sub.none

        -- プレビュー拡大(lightbox)が開いている間だけ Esc で閉じる購読を生かす
        , if Atelier.lightboxOpen model.atelier then
            Browser.Events.onKeyDown
                (D.field "key" D.string
                    |> D.andThen
                        (\key ->
                            if key == "Escape" then
                                D.succeed (AtelierMsg Atelier.LightboxClosed)

                            else
                                D.fail "他のキーは素通し"
                        )
                )

          else
            Sub.none

        -- ミニプレイヤーの拡大が開いている間だけ Esc で閉じる購読を生かす
        , if model.miniZoom /= Nothing then
            Browser.Events.onKeyDown
                (D.field "key" D.string
                    |> D.andThen
                        (\key ->
                            if key == "Escape" then
                                D.succeed MiniZoomClosed

                            else
                                D.fail "他のキーは素通し"
                        )
                )

          else
            Sub.none

        -- ゲーム起動(make debug)を待つ間だけログと状態を追う(2 秒間隔)
        , if model.screen == Editing && Atelier.isLaunchPolling model.atelier then
            Time.every 2000 (\_ -> LaunchPollTick)

          else
            Sub.none

        -- ホームとアトリエに居る間、見た目の検査を進めて知らせ・実況・
        -- ミニプレイヤーの追従を養う。平時は 8 秒間隔(サーバ負荷を無闇に
        -- 上げない)だが、エンジンが描き直している間だけ 2 秒間隔 —
        -- 焼き上がり(絵の差し替わり)への気づきを待たせないため。
        -- この口を持たないサーバ(404)では回さない
        , if model.screen == Editing && (model.tab == HomeTab || model.tab == AtelierTab) && model.changesAvailable then
            Time.every
                (if model.changesBaking then
                    2000

                 else
                    8000
                )
                (\_ -> ChangesPollTick)

          else
            Sub.none

        -- ミニプレイヤーの「✓ 差し替わりました」の戻し(2 秒)。点いている間だけ生きる
        , if model.miniSwapNotice then
            Time.every 2000 (\_ -> MiniSwapNoticeExpired)

          else
            Sub.none

        -- サーバがプレビューを焼いている間だけ候補を取り直す(2 秒間隔)。
        -- baking=false が届いた瞬間に購読ごと消え、その応答が最終形
        , if model.screen == Editing && Atelier.isBaking model.atelier then
            Time.every 2000 (\_ -> AtelierBakePollTick)

          else
            Sub.none

        -- 描き出し中はアトリエ表示中だけログ末尾も追う(1 秒間隔)。
        -- 進捗パネル(インジケーター+一言+末尾数行)の材料
        , if model.screen == Editing && model.tab == AtelierTab && Atelier.isBaking model.atelier then
            Time.every 1000 (\_ -> RunnerPollTick)

          else
            Sub.none

        -- 新しいゲームのひな形づくりを待つ間だけログを追う(2 秒間隔 —
        -- ゲーム起動と同じ拍)。止まったら購読ごと消える
        , if model.screen == Picker && NewGame.isPolling model.newGame then
            Time.every 2000 (\_ -> ProjectNewPollTick)

          else
            Sub.none

        -- プロジェクト選択画面では走っているゲームを定期問い合わせして起動中バッジを追従
        , if model.screen == Picker then
            Time.every 3000 (\_ -> RunningGamesPollTick)

          else
            Sub.none
        ]


main : Program () Model Msg
main =
    let
        caps =
            { toPort = apiRequest
            , noticeExpired = NoticeExpired
            , autosaveFired = AutosaveFired
            , delayFired = SfxWaitTick
            , searchDebounced = SearchDebounced
            }
    in
    Browser.element
        { init = init >> Tuple.mapSecond (Effect.perform caps)
        , update = \msg -> update msg >> Tuple.mapSecond (Effect.perform caps)
        , view = view
        , subscriptions = subscriptions
        }
