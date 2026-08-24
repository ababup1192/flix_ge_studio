module Widgets.Timeline exposing
    ( ClipSpec
    , Config
    , Handle(..)
    , Limit
    , Model
    , Msg(..)
    , Out(..)
    , PhaseSpec
    , TrackSpec(..)
    , clipSpan
    , clipStartSeconds
    , init
    , isDragging
    , pushForward
    , rulerSecondsOf
    , sectionOf
    , specsFrom
    , totalSecondsOf
    , update
    , valueOf
    , view
    )

{-| 固定トラック型のタイムライン。演出のテンポを 1 本の帯で調整する。

トラックの構成はスキーマの widget 宣言が決め、ここは値を動かすだけ:

  - `{"timeline": {...}}` … フェーズの帯 1 本。境目を掴んで隣り合う出来事の
    位置(0〜1 の割合)を動かす。帯全体の右端で総尺も動かせる
  - `{"clips": {...}}` … ワンショット演出のバー。右端を掴んで長さ
    (1 ビートに対する割合)を動かす。start 宣言があるクリップは左端も掴めて、
    トリガーから再生までの待ち(同じく割合)をスライドで動かせる

元データはいつも文書の数値で、ここは「帯の上の操作 → 数値 1 つの書き換え」に
写すだけ(SfxEditor と同じ立場)。保存・履歴・debounce は親の queueEdit 経路に
任せ、独自機構は持たない。プレビューも持たない — 保存は watchFile が実機へ
即反映するので、絵の確認は実機でやる。

値の押し出し(境目の順序が破れたときに前へ直す)は書き戻さない。ゲーム側が
読むときにやる決まりなので、ここは生の値を保存し、**表示だけ** pushForward で
整える(行き過ぎても元へ戻せる)。

-}

import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D



-- 宣言(スキーマの widget 値を読んだ結果)


{-| トラック 1 本ぶんの宣言。名前の束で、値はまだ知らない。
-}
type TrackSpec
    = PhaseTrack { section : String, total : TotalSpec, phases : List PhaseSpec }
    | ClipTrack { section : String, total : TotalSpec, scenes : List SceneSpec, items : List ClipSpec }


{-| トラックの総尺。multiply のフィールド値を掛けると秒になる。
to はその右端を掴んだとき書き戻す先(無ければ右端は掴めない)。
-}
type alias TotalSpec =
    { multiply : List String
    , to : Maybe String
    }


{-| フェーズ 1 区間。to はその区間の**終わり**の位置を持つフィールド名。
Nothing は最後の区間(宣言の "to": "end")で、終わりは総尺に固定 — 掴めない。
-}
type alias PhaseSpec =
    { label : String
    , to : Maybe String
    , wait : Bool
    , description : Maybe String
    }


{-| クリップ 1 本。length は 1 ビートに対する割合のフィールド名。
start はトリガーから再生までの待ちのフィールド名(同じく割合。無ければ
左端 0 固定で掴めない)。
capSeconds は上限秒のフィールド名(実際の長さは短い方)。
restLabel は「残りが別の動きになる」ときの名前。
echo は「値 = 繰り返しの遅れ幅」の印(効果テキスト。実体はビートいっぱい
再生され、2 回目以降が length ぶんずつ遅れる — 寸法線で編集する)。
-}
type alias ClipSpec =
    { label : String
    , length : String
    , start : Maybe String
    , capSeconds : Maybe String
    , restLabel : Maybe String
    , echo : Bool
    }


{-| 絵コンテの場面 1 つ。items はこの場面で同時に走るクリップの
length 名(items 宣言の中の物を指す)。
-}
type alias SceneSpec =
    { label : String
    , items : List String
    }


{-| (セクションキー, widget 値) の列から宣言を集める。読めない宣言は無視 —
呼び側は素のフォームへ倒す(Weights と同じ fail-open)。
最後以外の区間に "end" が居る宣言は書き間違いなので、トラックごと落とす。
-}
specsFrom : List ( String, Maybe D.Value ) -> List TrackSpec
specsFrom sections =
    sections
        |> List.filterMap
            (\( key, widget ) ->
                widget
                    |> Maybe.andThen
                        (\w ->
                            D.decodeValue (trackDecoder key) w
                                |> Result.toMaybe
                        )
            )
        |> List.filter endsAreValid


trackDecoder : String -> D.Decoder TrackSpec
trackDecoder key =
    D.oneOf
        [ D.field "timeline"
            (D.map2 (\total phases -> PhaseTrack { section = key, total = total, phases = phases })
                (D.field "totalSeconds" totalDecoder)
                (D.field "phases" (D.list phaseDecoder))
            )
        , D.field "clips"
            (D.map3 (\total scenes items -> ClipTrack { section = key, total = total, scenes = scenes, items = items })
                (D.field "totalSeconds" totalDecoder)
                -- scenes が無ければ空 = 素のバーの並び(絵コンテにしない)
                (D.oneOf [ D.field "scenes" (D.list sceneDecoder), D.succeed [] ])
                (D.field "items" (D.list clipDecoder))
            )
        ]


totalDecoder : D.Decoder TotalSpec
totalDecoder =
    D.map2 TotalSpec
        (D.field "multiply" (D.list D.string))
        (opt "to" D.string)


phaseDecoder : D.Decoder PhaseSpec
phaseDecoder =
    D.map4 PhaseSpec
        (D.field "label" D.string)
        -- "end" は予約語(最後の区間の終わり = 総尺)。フィールド名として持ち回らない
        (D.field "to" D.string
            |> D.map
                (\name ->
                    if name == "end" then
                        Nothing

                    else
                        Just name
                )
        )
        (D.oneOf [ D.field "wait" D.bool, D.succeed False ])
        (opt "description" D.string)


clipDecoder : D.Decoder ClipSpec
clipDecoder =
    D.map6 ClipSpec
        (D.field "label" D.string)
        (D.field "length" D.string)
        (opt "start" D.string)
        (opt "capSeconds" D.string)
        (opt "restLabel" D.string)
        (D.oneOf [ D.field "echo" D.bool, D.succeed False ])


sceneDecoder : D.Decoder SceneSpec
sceneDecoder =
    D.map2 SceneSpec
        (D.field "label" D.string)
        (D.field "items" (D.list D.string))


{-| "end"(= to Nothing)が許されるのは最後の区間だけ。
-}
endsAreValid : TrackSpec -> Bool
endsAreValid spec =
    case spec of
        PhaseTrack track ->
            case List.reverse track.phases of
                [] ->
                    False

                _ :: earlier ->
                    List.all (\p -> p.to /= Nothing) earlier

        ClipTrack track ->
            not (List.isEmpty track.items)



-- 描くのに要る材料(親が組む)


{-| values / fields のキーは「セクション.フィールド」。root のフィールド
(kind "field" のセクション)は素の名前 — 総尺の multiply が root を指すため。
-}
type alias Config =
    { specs : List TrackSpec
    , values : Dict String Float
    , fields : Dict String Limit

    -- セクションキー → 表示名(スキーマの label)。無ければキーのまま出す
    , labels : Dict String String
    }


type alias Limit =
    { min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float
    , default : Maybe Float
    }


{-| 文書のいまの値。無ければスキーマの default へ倒す(ゲーム側 loader と
同じ挙動 — 編集途中で欄が消えていても帯は描き続ける)。
-}
valueOf : Config -> String -> Maybe Float
valueOf config name =
    case Dict.get name config.values of
        Just v ->
            Just v

        Nothing ->
            Dict.get name config.fields |> Maybe.andThen .default


{-| multiply の積(秒)。引けない名前が 1 つでもあれば 0 —
呼び側は 0 のトラックを描かない。
-}
totalSecondsOf : Config -> TrackSpec -> Float
totalSecondsOf config spec =
    let
        names =
            (totalOf spec).multiply
    in
    names
        |> List.foldl
            (\name acc ->
                Maybe.map2 (*) (valueOf config name) acc
            )
            (Just 1)
        |> Maybe.map (Basics.max 0)
        |> Maybe.withDefault 0


totalOf : TrackSpec -> TotalSpec
totalOf spec =
    case spec of
        PhaseTrack track ->
            track.total

        ClipTrack track ->
            track.total


{-| 物差しの長さ(秒)。一番長いトラックに、右端(TotalEnd)を伸ばすための
余白を足す。1 回のドラッグで伸ばせるのはこの余白まで — 離すと物差しが
組み直されるので、掴み直して続きができる。
-}
rulerSecondsOf : Config -> Float
rulerSecondsOf config =
    case storyboardOf config of
        -- 絵コンテ: ターンの頭 + 場面のビート列 + ゴースト 1 つを連結した長さ
        Just sb ->
            (sb.turnSeconds + sb.beatSeconds * toFloat (List.length sb.scenes + 1)) * 1.05

        Nothing ->
            config.specs
                |> List.map (totalSecondsOf config)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (*) 1.15


{-| 絵コンテの材料。scenes 宣言を持つ clips トラックがあるときだけ組み立てる。
場面の items が指し損ねたクリップは、末尾の追加場面(label 空)に集める —
宣言の書き漏れでクリップが消えない(fail-open)。
-}
storyboardOf : Config -> Maybe Storyboard
storyboardOf config =
    config.specs
        |> List.filterMap
            (\spec ->
                case spec of
                    ClipTrack t ->
                        if List.isEmpty t.scenes then
                            Nothing

                        else
                            Just t

                    PhaseTrack _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.andThen
            (\clipTrack ->
                let
                    beat =
                        totalSecondsOf config (ClipTrack clipTrack)

                    phase =
                        config.specs
                            |> List.filterMap
                                (\spec ->
                                    case spec of
                                        PhaseTrack t ->
                                            Just t

                                        ClipTrack _ ->
                                            Nothing
                                )
                            |> List.head
                in
                if beat <= 0 then
                    Nothing

                else
                    Just
                        { phase = phase
                        , clipSection = clipTrack.section
                        , clipTotal = clipTrack.total
                        , scenes = resolveScenes clipTrack
                        , turnSeconds =
                            phase
                                |> Maybe.map (\t -> totalSecondsOf config (PhaseTrack t))
                                |> Maybe.withDefault 0
                        , beatSeconds = beat
                        }
            )


type alias Storyboard =
    { phase : Maybe { section : String, total : TotalSpec, phases : List PhaseSpec }
    , clipSection : String
    , clipTotal : TotalSpec
    , scenes : List { label : String, clips : List ClipSpec }
    , turnSeconds : Float
    , beatSeconds : Float
    }


resolveScenes :
    { section : String, total : TotalSpec, scenes : List SceneSpec, items : List ClipSpec }
    -> List { label : String, clips : List ClipSpec }
resolveScenes track =
    let
        clipFor name =
            track.items |> List.filter (\c -> c.length == name) |> List.head

        resolved =
            track.scenes
                |> List.map
                    (\scene ->
                        { label = scene.label
                        , clips = scene.items |> List.filterMap clipFor
                        }
                    )
                |> List.filter (\scene -> not (List.isEmpty scene.clips))

        used =
            resolved |> List.concatMap .clips |> List.map .length

        leftovers =
            track.items |> List.filter (\c -> not (List.member c.length used))
    in
    if List.isEmpty leftovers then
        resolved

    else
        resolved ++ [ { label = "", clips = leftovers } ]


{-| 掴んだ瞬間に凍結する「面いっぱいの秒数」。絵コンテでは下段の編集モードが
面を独り占めしていて、ハンドルの種類からどのペインの物か決まる —
ターンの頭の物差しか、ビートの物差しか。どちらも右端を伸ばす余白 +15%。
絵コンテでなければ従来どおり全体の物差し。
-}
pressRulerFor : Config -> Handle -> Float
pressRulerFor config handle =
    case storyboardOf config of
        Nothing ->
            rulerSecondsOf config

        Just sb ->
            case handle of
                Boundary _ ->
                    sb.turnSeconds * 1.15

                TotalEnd t ->
                    if Just t.target == totalEndTarget sb.clipTotal then
                        sb.beatSeconds * 1.15

                    else
                        sb.turnSeconds * 1.15

                ClipStart _ ->
                    sb.beatSeconds * 1.15

                ClipEnd _ ->
                    sb.beatSeconds * 1.15


{-| 表示用の位置ならし。各値を 0..1 に収めてから、前から max の連鎖で
単調にする(ゲーム側 turnCueOf と同じ式)。生の値が順序を破っている間も
帯が負の幅にならない。書き戻しには使わない。
-}
pushForward : List Float -> List Float
pushForward positions =
    positions
        |> List.foldl
            (\v ( acc, floor_ ) ->
                let
                    pushed =
                        Basics.max floor_ (clamp 0 1 v)
                in
                ( pushed :: acc, pushed )
            )
            ( [], 0 )
        |> Tuple.first
        |> List.reverse


{-| クリップの実際の長さ(秒)。length × ビート秒と上限秒の短い方。
capped は上限で切られているか(切られている間、右端をそれ以上
伸ばしても絵は変わらない)。
-}
clipSpan : Config -> { section : String, beatSeconds : Float } -> ClipSpec -> Maybe { seconds : Float, capped : Bool }
clipSpan config track clip =
    valueOf config (track.section ++ "." ++ clip.length)
        |> Maybe.map
            (\ratio ->
                let
                    raw =
                        clamp 0 1 ratio * track.beatSeconds

                    cap =
                        clip.capSeconds
                            |> Maybe.andThen (\name -> valueOf config (track.section ++ "." ++ name))
                in
                case cap of
                    Just capSec ->
                        if capSec < raw then
                            { seconds = capSec, capped = True }

                        else
                            { seconds = raw, capped = False }

                    Nothing ->
                        { seconds = raw, capped = False }
            )


{-| クリップの開始までの待ち(秒)。start 宣言が無ければ 0 = トリガーの瞬間。
-}
clipStartSeconds : Config -> { section : String, beatSeconds : Float } -> ClipSpec -> Float
clipStartSeconds config track clip =
    clip.start
        |> Maybe.andThen (\name -> valueOf config (track.section ++ "." ++ name))
        |> Maybe.map (\ratio -> clamp 0 1 ratio * track.beatSeconds)
        |> Maybe.withDefault 0



-- 状態とドラッグ


type alias Model =
    { drag : Maybe Drag

    -- 横方向の倍率(1 = 全体が収まる)。無段階 — ピンチ / Ctrl+ホイールの
    -- 増分をそのまま掛ける
    , zoom : Float

    -- 絵コンテで下段に開いている場面(編集モード)。Nothing = 何も開いていない
    , openPane : Maybe Pane
    }


{-| 下段の編集モードで開ける物。上段の箱と 1 対 1。
-}
type Pane
    = TurnPane
    | ScenePane Int


{-| 凍結するのは物差しだけ(TotalEnd を伸ばすと物差し自体が伸びて、掴んだ点が
指から逃げる — 正のフィードバックを断つ)。トラックの秒数や multiply の値は
Moved のたびに Config から導く。ドラッグ中に文書が外から変わっても、
古い値を基準に書き戻さない。
-}
type alias Drag =
    { handle : Handle
    , rulerSeconds : Float
    }


{-| 掴んだ物 = 書き戻し先の名指し。幾何は持たない。
ClipEnd の afterStart は開始の待ちのフィールド名 — グリップは(開始 + 長さ)の
位置に立つので、書き戻すときに開始ぶんを引いて長さへ戻す。
ClipStart は左端(開始の待ち)。動かしても長さは変えない(スライド式)。
-}
type Handle
    = Boundary { section : String, target : String }
    | ClipStart { section : String, target : String }
    | ClipEnd { section : String, target : String, afterStart : Maybe String }
    | TotalEnd { target : String }


type Msg
    = Pressed Handle Point
    | Moved Point
    | Released
      -- ピンチ / Ctrl+ホイール。Point は掴んだ点(スクロールの中心に残す)、
      -- Float は掛ける倍率(ホイールの増分から作るので無段階)
    | ZoomBy Point Float
    | ZoomHome
      -- 上段の箱をクリック(Nothing = 閉じる ×)
    | PaneSelected (Maybe Pane)


type alias Point =
    { fx : Float, fy : Float }


type Out
    = Silent
    | Edited { path : List String, value : Float }
      -- ズーム後の横スクロール合わせを外(Main の Effect)へ頼む
    | Zoomed { anchorFx : Float, ratio : Float }
    | ZoomReset


init : Model
init =
    { drag = Nothing, zoom = 1, openPane = Nothing }


isDragging : Model -> Bool
isDragging model =
    model.drag /= Nothing


update : Config -> Msg -> Model -> ( Model, Out )
update config msg model =
    case msg of
        -- 押しただけでは書かない(うっかりクリックで境目が飛ばないように)。
        -- 書くのは動かし始めてから — SfxEditor の縁の掴みと同じ流儀
        Pressed handle _ ->
            ( { model | drag = Just { handle = handle, rulerSeconds = pressRulerFor config handle } }
            , Silent
            )

        Moved point ->
            case model.drag of
                Nothing ->
                    ( model, Silent )

                Just drag ->
                    ( model, editFor config drag point )

        Released ->
            ( { model | drag = Nothing }, Silent )

        ZoomBy point factor ->
            let
                next =
                    clamp 1 8 (model.zoom * factor)
            in
            if next == model.zoom then
                ( model, Silent )

            else
                ( { model | zoom = next }
                , Zoomed { anchorFx = point.fx, ratio = next / model.zoom }
                )

        ZoomHome ->
            ( { model | zoom = 1 }, ZoomReset )

        PaneSelected pane ->
            ( { model | openPane = pane }, Silent )


{-| 掴んだ位置(物差しに対する割合)を、書き戻す値 1 つに写す。
丸めた結果が今の値と同じなら Silent(step が自然なスロットルになる)。
-}
editFor : Config -> Drag -> Point -> Out
editFor config drag point =
    let
        seconds =
            clamp 0 1 point.fx * drag.rulerSeconds
    in
    case drag.handle of
        Boundary at ->
            ratioEdit config at seconds

        ClipStart at ->
            ratioEdit config at seconds

        ClipEnd at ->
            -- グリップの位置 = 開始 + 長さ。開始ぶんを引いてから割合へ戻す
            ratioEdit config
                { section = at.section, target = at.target }
                (seconds - clipStartOffset config at)

        TotalEnd at ->
            totalEdit config at seconds


{-| ClipEnd の書き戻しで引く、開始の待ち(秒)。start 宣言が無ければ 0。
-}
clipStartOffset : Config -> { section : String, target : String, afterStart : Maybe String } -> Float
clipStartOffset config at =
    at.afterStart
        |> Maybe.andThen (\name -> valueOf config (at.section ++ "." ++ name))
        |> Maybe.map (\ratio -> clamp 0 1 ratio * trackSecondsIn config at.section)
        |> Maybe.withDefault 0


{-| 境目・クリップ右端: 秒 → そのトラックの総尺に対する割合(0〜1)。
-}
ratioEdit : Config -> { section : String, target : String } -> Float -> Out
ratioEdit config at seconds =
    let
        trackSeconds =
            trackSecondsIn config at.section

        key =
            at.section ++ "." ++ at.target
    in
    if trackSeconds <= 0 then
        Silent

    else
        emitIfChanged config key [ at.section, at.target ] (unitClamped config key (seconds / trackSeconds))


{-| 帯全体の右端: 秒 → to のフィールド値(総尺 ÷ 残りの積)。
-}
totalEdit : Config -> { target : String } -> Float -> Out
totalEdit config at seconds =
    case totalTrackFor config at.target of
        Nothing ->
            Silent

        Just spec ->
            let
                others =
                    othersProduct config (totalOf spec) at.target
            in
            if others <= 0 then
                Silent

            else
                emitIfChanged config at.target [ at.target ] (limitClamped config at.target (seconds / others))


{-| target を右端に持つトラック。逆算が成立するのは multiply に target が
ちょうど 1 回現れるときだけ(view も同じ条件でしかハンドルを出さない)。
-}
totalTrackFor : Config -> String -> Maybe TrackSpec
totalTrackFor config target =
    config.specs
        |> List.filter (\spec -> totalEndTarget (totalOf spec) == Just target)
        |> List.head


{-| 右端を掴めるトラックの、書き戻し先。to が multiply にちょうど 1 回
現れないときは Nothing(総尺 = to値 × 残りの積、の逆算が壊れる)。
-}
totalEndTarget : TotalSpec -> Maybe String
totalEndTarget total =
    total.to
        |> Maybe.andThen
            (\to ->
                if List.length (List.filter ((==) to) total.multiply) == 1 then
                    Just to

                else
                    Nothing
            )


othersProduct : Config -> TotalSpec -> String -> Float
othersProduct config total target =
    dropFirst target total.multiply
        |> List.foldl
            (\name acc -> Maybe.map2 (*) (valueOf config name) acc)
            (Just 1)
        |> Maybe.withDefault 0


dropFirst : a -> List a -> List a
dropFirst wanted items =
    case items of
        [] ->
            []

        first :: rest ->
            if first == wanted then
                rest

            else
                first :: dropFirst wanted rest


trackSecondsIn : Config -> String -> Float
trackSecondsIn config section =
    config.specs
        |> List.filter (\spec -> sectionOf spec == section)
        |> List.head
        |> Maybe.map (totalSecondsOf config)
        |> Maybe.withDefault 0


labelOf : Config -> String -> String
labelOf config section =
    Dict.get section config.labels |> Maybe.withDefault section


{-| そのトラックの宣言が住むセクション(親が「いま開いているタブに出すか」を
判じる材料)。
-}
sectionOf : TrackSpec -> String
sectionOf spec =
    case spec of
        PhaseTrack track ->
            track.section

        ClipTrack track ->
            track.section


{-| スキーマの min/max と 0..1 の交差で clamp して step で丸める(割合の欄)。
-}
unitClamped : Config -> String -> Float -> Float
unitClamped config key v =
    let
        limit =
            limitFor config key

        lo =
            Basics.max 0 (Maybe.withDefault 0 limit.min)

        hi =
            Basics.min 1 (Maybe.withDefault 1 limit.max)
    in
    roundBy limit.step (safeClamp lo hi v)


{-| スキーマの min/max だけで clamp して step で丸める(root の欄)。
-}
limitClamped : Config -> String -> Float -> Float
limitClamped config key v =
    let
        limit =
            limitFor config key

        lo =
            Maybe.withDefault v limit.min

        hi =
            Maybe.withDefault v limit.max
    in
    roundBy limit.step (safeClamp lo hi v)


limitFor : Config -> String -> Limit
limitFor config key =
    Dict.get key config.fields
        |> Maybe.withDefault { min = Nothing, max = Nothing, step = Nothing, default = Nothing }


{-| min > max の壊れたスキーマは範囲を無視する(clamp は lo > hi のとき
常に lo を返してしまい、値が片側へ張り付く)。
-}
safeClamp : Float -> Float -> Float -> Float
safeClamp lo hi v =
    if lo > hi then
        v

    else
        clamp lo hi v


{-| step が無い欄の既定は 0.01(割合の粒度として schema の実物と同じ)。

`目盛り数 × step` で組み立てず、整数の分子分母から 1 回の除算で作る —
掛け算は 0.58 を 0.5800000000000001 に汚し、文書にそのまま書かれてしまう
(Weights が units で計算するのと同じ理由)。

-}
roundBy : Maybe Float -> Float -> Float
roundBy step v =
    let
        s =
            Maybe.withDefault 0.01 step
    in
    if s <= 0 then
        v

    else
        let
            den =
                10 ^ decimalsOf s

            num =
                Basics.round (s * toFloat den)
        in
        toFloat (Basics.round (v / s) * num) / toFloat den


{-| step の小数の桁数(0.01 → 2・0.5 → 1・1 → 0)。4 桁あれば schema の実物に足りる。
-}
decimalsOf : Float -> Int
decimalsOf s =
    List.range 0 4
        |> List.filter
            (\n ->
                let
                    scaled =
                        s * toFloat (10 ^ n)
                in
                abs (scaled - toFloat (Basics.round scaled)) < 1.0e-9
            )
        |> List.head
        |> Maybe.withDefault 4


emitIfChanged : Config -> String -> List String -> Float -> Out
emitIfChanged config key path v =
    if valueOf config key == Just v then
        Silent

    else
        Edited { path = path, value = v }



-- 描画


view : Config -> Model -> Html Msg
view config model =
    let
        ruler =
            case model.drag of
                -- ドラッグ中は掴んだ瞬間の物差しのまま(右端を動かすと物差しが
                -- 伸びて、掴んだ点が指から逃げるため)
                Just drag ->
                    drag.rulerSeconds

                Nothing ->
                    rulerSecondsOf config
    in
    div [ HA.class "tl-editor" ]
        [ div [ HA.class "tl-head" ]
            [ span [ HA.class "tl-title" ] [ text "タイムライン" ]
            , viewZoomState model
            , span
                [ HA.class "tl-note"
                , HA.title "保存すると watchFile が実機へ即反映する。絵の確認は実機で。"
                ]
                [ text "保存 → 実機に即反映" ]
            ]
        , div
            -- 横スクロールの窓。ズームで面が窓より広くなったぶんはここで送る
            [ HA.class "tl-viewport", HA.id viewportId ]
            [ div
                -- pointer 面。行が薄いので、move はここで受ける(行から縦に
                -- ずれても途絶えない)。横 padding 0 でトラック面と左右端をそろえ、
                -- fx の意味を一致させる。幅はズーム倍率ぶん伸ばすだけで、
                -- fx(面に対する割合)の意味は変わらない
                (HA.classList
                    [ ( "tl-surface", True )

                    -- ドラッグ中はセグメントの pointer-events を切る(move の
                    -- target を面そのものに保ち、offsetX の基準を揺らさない)
                    , ( "tl-drag", isDragging model )
                    ]
                    :: HA.style "width" (String.fromFloat (model.zoom * 100) ++ "%")
                    :: onZoomWheel
                    :: dragAttrs model
                )
                (viewTicks ruler
                    :: (case storyboardOf config of
                            -- 絵コンテ: 代表的な 1 ターンを 1 本の物差しに並べる
                            Just sb ->
                                viewStoryboard config ruler model.zoom model sb

                            -- 素の形: トラックを縦に並べる(scenes 宣言の無い Doc)
                            Nothing ->
                                viewBeatLine config ruler
                                    ++ (config.specs |> List.concatMap (viewTrack config ruler model.zoom))
                       )
                )
            ]
        ]


{-| スクロールの窓の id。Main が Effect(ZoomViewport / ResetViewport)で
同じ窓を名指しする。
-}
viewportId : String
viewportId =
    "tl-viewport"


{-| ズーム中だけ「×1.4 全体へ戻る」を出す。等倍のときは操作の案内だけ。
-}
viewZoomState : Model -> Html Msg
viewZoomState model =
    if model.zoom > 1.01 then
        Html.button
            [ HA.class "tl-zoom-home"
            , HA.type_ "button"
            , HE.onClick ZoomHome
            ]
            [ text ("×" ++ String.fromFloat (toFloat (round (model.zoom * 10)) / 10) ++ " 全体へ戻る") ]

    else
        span [ HA.class "tl-note" ] [ text "ピンチ / Ctrl+ホイールで拡大" ]


{-| ピンチ(ブラウザには ctrlKey つきの wheel として届く) / Ctrl+ホイール。
増分 deltaY をそのまま指数に写すので段が無く、なめらかに効く。
Ctrl 無しのホイールは受けない(ページのスクロールのまま)。
-}
onZoomWheel : Html.Attribute Msg
onZoomWheel =
    HE.custom "wheel"
        (D.field "ctrlKey" D.bool
            |> D.andThen
                (\ctrl ->
                    if ctrl then
                        D.map2
                            (\point deltaY ->
                                { message = ZoomBy point (2 ^ (-deltaY / 200))
                                , preventDefault = True
                                , stopPropagation = False
                                }
                            )
                            pointDecoder
                            (D.field "deltaY" D.float)

                    else
                        D.fail "ズームは ctrlKey つきの wheel だけ受ける"
                )
        )


dragAttrs : Model -> List (Html.Attribute Msg)
dragAttrs model =
    if model.drag == Nothing then
        [ HE.on "pointerup" (D.succeed Released) ]

    else
        [ HE.on "pointermove" (D.map Moved pointDecoder)
        , HE.on "pointerup" (D.succeed Released)
        , HE.on "pointercancel" (D.succeed Released)
        ]


{-| 絵コンテ(上下分割)。上段 = 箱だけの流れ図(常に表示・読む専用)。
箱をクリックすると下段にその場面単体の編集モードが開く。
見た目の文法は 2 値 — 実色 + グリップ = 掴める / 減光 = 自動で決まる・説明。
-}
viewStoryboard : Config -> Float -> Float -> Model -> Storyboard -> List (Html Msg)
viewStoryboard config ruler zoom model sb =
    viewFlowRow config ruler model.openPane sb
        :: div [ HA.class "tl-split" ] []
        :: (case model.openPane of
                Nothing ->
                    [ div [ HA.class "tl-pane-hint" ]
                        [ text "上の箱をクリックすると、その場面の編集モードがここに開く" ]
                    ]

                Just pane ->
                    viewPane config zoom pane sb
           )


{-| 上段: 箱だけの流れ図。幅は秒数に比例。掴める物は無い(選ぶだけ)。
-}
viewFlowRow : Config -> Float -> Maybe Pane -> Storyboard -> Html Msg
viewFlowRow config ruler openPane sb =
    let
        box pane secs name badge sub mini =
            div
                [ HA.classList
                    [ ( "tl-box", True )
                    , ( "tl-box-selected", openPane == Just pane )
                    ]
                , HA.style "width" (percent (secs / ruler))
                , HE.onClick (PaneSelected (Just pane))
                ]
                (div [ HA.class "tl-box-name" ]
                    (text name
                        :: (if badge then
                                [ span
                                    [ HA.class "tl-scene-badge"
                                    , HA.title "このビートがいつ・何回来るかは戦況しだい(型の例)"
                                    ]
                                    [ text "例" ]
                                ]

                            else
                                []
                           )
                    )
                    :: div [ HA.class "tl-box-sub" ] [ text sub ]
                    :: mini
                )

        turnBox =
            case sb.phase of
                Just track ->
                    [ box TurnPane
                        sb.turnSeconds
                        (labelOf config track.section)
                        False
                        (secondsText sb.turnSeconds ++ " ・ " ++ String.fromInt (List.length track.phases) ++ " 区間")
                        [ div [ HA.class "tl-box-mini" ]
                            [ div [ HA.style "width" "100%" ] [] ]
                        ]
                    ]

                Nothing ->
                    []

        sceneBoxes =
            sb.scenes
                |> List.indexedMap
                    (\index scene ->
                        box (ScenePane index)
                            sb.beatSeconds
                            (sceneNameOf config sb scene)
                            True
                            (secondsText sb.beatSeconds ++ " ・ 演出 " ++ String.fromInt (List.length scene.clips) ++ " 本")
                            [ div [ HA.class "tl-box-mini" ]
                                (scene.clips
                                    |> List.map
                                        (\clip ->
                                            let
                                                startFrac =
                                                    clipStartSeconds config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip
                                                        / sb.beatSeconds

                                                frac =
                                                    clipSpan config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip
                                                        |> Maybe.map (\sp -> sp.seconds / sb.beatSeconds)
                                                        |> Maybe.withDefault 0

                                                room =
                                                    Basics.max 0 (1 - startFrac)
                                            in
                                            div
                                                [ HA.style "margin-left" (percent startFrac)
                                                , HA.style "width"
                                                    (percent
                                                        (if clip.echo then
                                                            room

                                                         else
                                                            Basics.min frac room
                                                        )
                                                    )
                                                ]
                                                []
                                        )
                                )
                            ]
                    )

        ghostBox =
            div
                [ HA.class "tl-box tl-box-ghost"
                , HA.style "width" (percent (sb.beatSeconds / ruler))
                , HA.title "ビートは場のカードと攻撃の数だけ続く(順番も回数も戦況しだい)。終わったら次のターンの頭へ"
                ]
                [ div [ HA.class "tl-box-name" ] [ text "…ビートが続く" ] ]
    in
    div [ HA.class "tl-flow" ] (turnBox ++ sceneBoxes ++ [ ghostBox ])


sceneNameOf : Config -> Storyboard -> { label : String, clips : List ClipSpec } -> String
sceneNameOf config sb scene =
    if scene.label == "" then
        labelOf config sb.clipSection

    else
        scene.label


{-| 下段: 選んだ場面単体の編集モード。面いっぱいがそのペインの物差し
(ペインの秒数 × 1.15。右端を伸ばす余白ぶん)になる。
-}
viewPane : Config -> Float -> Pane -> Storyboard -> List (Html Msg)
viewPane config zoom pane sb =
    let
        header name badge =
            div [ HA.class "tl-pane-head" ]
                (span [ HA.class "tl-pane-title" ] [ text ("編集モード: " ++ name) ]
                    :: (if badge then
                            [ span [ HA.class "tl-scene-badge" ] [ text "例" ] ]

                        else
                            []
                       )
                    ++ [ span
                            [ HA.class "tl-pane-close"
                            , HE.onClick (PaneSelected Nothing)
                            ]
                            [ text "閉じる ×" ]
                       ]
                )
    in
    case pane of
        TurnPane ->
            case sb.phase of
                Nothing ->
                    []

                Just track ->
                    header (labelOf config track.section) False
                        :: viewTicks (sb.turnSeconds * 1.15)
                        :: viewTurnPaneLane config zoom sb track

        ScenePane index ->
            case sb.scenes |> List.drop index |> List.head of
                Nothing ->
                    []

                Just scene ->
                    header (sceneNameOf config sb scene) True
                        :: viewTicks (sb.beatSeconds * 1.15)
                        :: viewScenePaneLanes config zoom sb scene


{-| ターンの頭のペイン。フェーズの帯 1 本(座標はペインの物差しに対する割合)。
-}
viewTurnPaneLane :
    Config
    -> Float
    -> Storyboard
    -> { section : String, total : TotalSpec, phases : List PhaseSpec }
    -> List (Html Msg)
viewTurnPaneLane config zoom sb track =
    let
        paneRuler =
            sb.turnSeconds * 1.15

        fxOf seconds =
            seconds / paneRuler

        rawEnds =
            track.phases
                |> List.map
                    (\p ->
                        p.to
                            |> Maybe.andThen (\name -> valueOf config (track.section ++ "." ++ name))
                            |> Maybe.withDefault 1
                    )

        ends =
            pushForward rawEnds

        starts =
            0 :: ends

        segments =
            List.map3
                (\phase start end ->
                    let
                        width =
                            fxOf (Basics.max 0 (end - start) * sb.turnSeconds)
                    in
                    div
                        [ HA.classList
                            [ ( "tl-phase", True )
                            , ( "tl-wait", phase.wait )
                            ]
                        , HA.style "left" (percent (fxOf (start * sb.turnSeconds)))
                        , HA.style "width" (percent width)
                        , HA.title (Maybe.withDefault phase.label phase.description)
                        ]
                        (if width * 100 * zoom >= labelMinPercent then
                            [ span [ HA.class "tl-phase-label" ] [ text phase.label ] ]

                         else
                            []
                        )
                )
                track.phases
                starts
                ends

        boundaryHandles =
            List.map2
                (\phase end ->
                    phase.to
                        |> Maybe.map
                            (\name ->
                                ( fxOf (end * sb.turnSeconds)
                                , Boundary { section = track.section, target = name }
                                , secondsText (end * sb.turnSeconds)
                                )
                            )
                )
                track.phases
                ends
                |> List.filterMap identity

        totalHandle =
            totalEndTarget track.total
                |> Maybe.map
                    (\target ->
                        [ ( fxOf sb.turnSeconds
                          , TotalEnd { target = target }
                          , "全体 " ++ secondsText sb.turnSeconds
                          )
                        ]
                    )
                |> Maybe.withDefault []

        handles =
            boundaryHandles ++ totalHandle
    in
    [ div
        [ HA.class "tl-track tl-track-phase"
        , onPointerDown (pickNearest handles)
        ]
        (div
            [ HA.class "tl-frame"
            , HA.style "width" (percent (fxOf sb.turnSeconds))
            ]
            []
            :: segments
            ++ List.indexedMap (\i handle -> viewGrip { alt = modBy 2 i == 1, warn = False } handle) handles
        )
    ]


{-| 場面のペイン。同時に走る演出のレーンを縦に積む(縦の重なり = 同時進行)。
枠 = 1 ビート。ビートの右端(紫)はどの場面のペインでも掴める — 共有の値なので、
動かすと他の場面もターンの頭も一緒に伸び縮みする。
-}
viewScenePaneLanes :
    Config
    -> Float
    -> Storyboard
    -> { label : String, clips : List ClipSpec }
    -> List (Html Msg)
viewScenePaneLanes config zoom sb scene =
    let
        paneRuler =
            sb.beatSeconds * 1.15

        beatFx =
            sb.beatSeconds / paneRuler

        beatHandle =
            totalEndTarget sb.clipTotal
                |> Maybe.map
                    (\target ->
                        [ ( beatFx
                          , TotalEnd { target = target }
                          , "1 ビート " ++ secondsText sb.beatSeconds
                          )
                        ]
                    )
                |> Maybe.withDefault []

        spanOf clip =
            clipSpan config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip

        -- 「次のビートまで待ち」は、余りが一番広いレーンに 1 か所だけ
        idleLane =
            scene.clips
                |> List.indexedMap
                    (\i clip ->
                        if clip.echo then
                            ( i, 0 )

                        else
                            ( i
                            , spanOf clip
                                |> Maybe.map
                                    (\sp ->
                                        Basics.max 0
                                            (sb.beatSeconds
                                                - clipStartSeconds config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip
                                                - sp.seconds
                                            )
                                    )
                                |> Maybe.withDefault 0
                            )
                    )
                |> List.sortBy (\( _, remain ) -> -remain)
                |> List.head
                |> Maybe.andThen
                    (\( i, remain ) ->
                        if remain / sb.beatSeconds >= 0.3 then
                            Just i

                        else
                            Nothing
                    )
    in
    scene.clips
        |> List.indexedMap
            (\i clip ->
                viewPaneLane config
                    zoom
                    sb
                    { beatHandle = beatHandle
                    , showBeatGrip = i == 0
                    , idle = idleLane == Just i
                    }
                    clip
            )
        |> List.concat


viewPaneLane :
    Config
    -> Float
    -> Storyboard
    -> { beatHandle : List ( Float, Handle, String ), showBeatGrip : Bool, idle : Bool }
    -> ClipSpec
    -> List (Html Msg)
viewPaneLane config zoom sb opts clip =
    case clipSpan config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip of
        Nothing ->
            []

        Just span_ ->
            let
                paneRuler =
                    sb.beatSeconds * 1.15

                fxOf seconds =
                    seconds / paneRuler

                startSec =
                    clipStartSeconds config { section = sb.clipSection, beatSeconds = sb.beatSeconds } clip

                -- 開始の待ちと長さの合計がビートを超えたら長さの側を縮める
                -- (ゲーム側の読み出しと同じ決まり)
                spanSec =
                    Basics.min span_.seconds (Basics.max 0 (sb.beatSeconds - startSec))

                startFx =
                    fxOf startSec

                clipFx =
                    fxOf spanSec

                endFx =
                    startFx + clipFx

                beatFx =
                    fxOf sb.beatSeconds

                gripLabel =
                    if span_.capped then
                        secondsText spanSec ++ " (上限)"

                    else
                        valueOf config (sb.clipSection ++ "." ++ clip.length)
                            |> Maybe.map (\r -> secondsText spanSec ++ " (" ++ String.fromInt (round (r * 100)) ++ "%)")
                            |> Maybe.withDefault (secondsText spanSec)

                handle =
                    ( endFx
                    , ClipEnd { section = sb.clipSection, target = clip.length, afterStart = clip.start }
                    , gripLabel
                    )

                -- 左端(開始の待ち)。start 宣言があるクリップだけ掴める
                startHandle =
                    clip.start
                        |> Maybe.map
                            (\name ->
                                [ ( startFx
                                  , ClipStart { section = sb.clipSection, target = name }
                                  , "開始 " ++ secondsText startSec
                                  )
                                ]
                            )
                        |> Maybe.withDefault []

                -- 開始までの斜線(ウェイトの文法)。待ちが 0 の間は描かない
                startWait =
                    if startSec > 0 then
                        [ div
                            [ HA.class "tl-clip tl-wait"
                            , HA.style "width" (percent startFx)
                            , HA.title ("トリガーから " ++ secondsText startSec ++ " 待ってから始まる(左端のグリップで動かす)")
                            ]
                            []
                        ]

                    else
                        []

                nameInside w =
                    w * 100 * zoom >= labelMinPercent

                nameOf w label =
                    if nameInside w then
                        [ span [ HA.class "tl-phase-label" ] [ text label ] ]

                    else
                        []

                body =
                    if clip.echo then
                        [ div
                            [ HA.class "tl-clip"
                            , HA.style "left" (percent startFx)
                            , HA.style "width" (percent (Basics.max 0 (beatFx - startFx)))
                            , HA.title (clip.label ++ "。1 行目はビートいっぱい表示される")
                            ]
                            (nameOf (beatFx - startFx) clip.label)
                        , div
                            [ HA.class "tl-clip tl-derived"
                            , HA.style "left" (percent endFx)
                            , HA.style "width" (percent (Basics.max 0 (beatFx - endFx)))
                            , HA.title ("2 行目 — " ++ secondsText spanSec ++ " 遅れて出る(行数は効果の数しだい)")
                            ]
                            []
                        ]

                    else
                        div
                            [ HA.class "tl-clip"
                            , HA.style "left" (percent startFx)
                            , HA.style "width" (percent clipFx)
                            , HA.title
                                (clip.capSeconds
                                    |> Maybe.andThen (\name -> valueOf config (sb.clipSection ++ "." ++ name))
                                    |> Maybe.map (\cap -> clip.label ++ "。上限 " ++ secondsText cap ++ " で切られている(超える値は左のフォームで)")
                                    |> Maybe.withDefault clip.label
                                )
                            ]
                            (nameOf clipFx clip.label)
                            :: (case clip.restLabel of
                                    Just label ->
                                        [ div
                                            [ HA.class "tl-clip tl-derived"
                                            , HA.style "left" (percent endFx)
                                            , HA.style "width" (percent (Basics.max 0 (beatFx - endFx)))
                                            , HA.title (label ++ " — " ++ clip.label ++ " の残りで自動で決まる")
                                            ]
                                            (nameOf (beatFx - endFx) label)
                                        ]

                                    Nothing ->
                                        []
                               )

                nameOutside =
                    if clip.echo || nameInside clipFx || clip.restLabel /= Nothing then
                        []

                    else
                        [ span
                            [ HA.class "tl-phase-label tl-label-out"
                            , HA.style "left" (percent endFx)
                            ]
                            [ text clip.label ]
                        ]

                idle =
                    if opts.idle && clip.restLabel == Nothing && not clip.echo then
                        [ div
                            [ HA.class "tl-idle"
                            , HA.style "left" (percent endFx)
                            , HA.style "width" (percent (Basics.max 0 (beatFx - endFx)))
                            , HA.title "演出はここで終わり。ビートの残りは何も動かない(ビートの長さはテンポ側が決める)"
                            ]
                            [ text "次のビートまで待ち" ]
                        ]

                    else
                        []

                beatGrip =
                    if opts.showBeatGrip then
                        [ div
                            [ HA.class "tl-grip tl-grip-beat"
                            , HA.style "left" (percent beatFx)
                            ]
                            [ div [ HA.class "tl-grip-dot" ] []
                            , div [ HA.class "tl-grip-label" ]
                                [ text ("1 ビート " ++ secondsText sb.beatSeconds) ]
                            ]
                        ]

                    else
                        []
            in
            [ div
                [ HA.class "tl-track tl-lane"
                , onPointerDown (pickNearest (handle :: startHandle ++ opts.beatHandle))
                ]
                (div
                    [ HA.class "tl-frame"
                    , HA.style "width" (percent beatFx)
                    ]
                    []
                    :: startWait
                    ++ body
                    ++ idle
                    ++ nameOutside
                    ++ beatGrip
                    ++ (startHandle |> List.map (viewGrip { alt = True, warn = False }))
                    ++ [ viewGrip { alt = False, warn = span_.capped } handle ]
                )
            ]


{-| 1 ビートの線。全行を貫く縦の点線で、ワンショットの枠の右端はこの線に一致する
(ワンショットは必ず 1 ビートの中で終わる、を線が語る)。ビートの長さは
クリップのトラックの総尺(beatSeconds の掛け算)から取る。
-}
viewBeatLine : Config -> Float -> List (Html Msg)
viewBeatLine config ruler =
    config.specs
        |> List.filterMap
            (\spec ->
                case spec of
                    ClipTrack _ ->
                        Just (totalSecondsOf config spec)

                    PhaseTrack _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.andThen
            (\beat ->
                if ruler <= 0 || beat <= 0 then
                    Nothing

                else
                    Just
                        [ div
                            [ HA.class "tl-beatline"
                            , HA.style "left" (percent (beat / ruler))
                            ]
                            [ span [] [ text ("1 ビート " ++ secondsText beat) ] ]
                        ]
            )
        |> Maybe.withDefault []


{-| 秒の目盛り。0.1 / 0.25 / 0.5 / 1s から「4〜8 本になる間隔」を選ぶ。
-}
viewTicks : Float -> Html Msg
viewTicks ruler =
    let
        gap =
            [ 0.1, 0.25, 0.5, 1 ]
                |> List.filter (\g -> ruler / g <= 8)
                |> List.head
                |> Maybe.withDefault 1

        count =
            if ruler <= 0 then
                0

            else
                floor (ruler / gap)
    in
    div [ HA.class "tl-ticks" ]
        (List.range 0 count
            |> List.map
                (\i ->
                    let
                        sec =
                            toFloat i * gap
                    in
                    div
                        [ HA.class "tl-tick"
                        , HA.style "left" (percent (sec / Basics.max 0.001 ruler))
                        ]
                        [ span [] [ text (secondsText sec) ] ]
                )
        )


viewTrack : Config -> Float -> Float -> TrackSpec -> List (Html Msg)
viewTrack config ruler zoom spec =
    let
        trackSeconds =
            totalSecondsOf config spec
    in
    if ruler <= 0 || trackSeconds <= 0 then
        [ div [ HA.class "tl-row-label" ] [ text (sectionOf spec) ]
        , div [ HA.class "tl-empty" ]
            [ text "総尺のフィールドが読めないため帯を出せません" ]
        ]

    else
        case spec of
            PhaseTrack track ->
                viewPhaseTrack config ruler zoom trackSeconds track

            ClipTrack track ->
                viewClipTrack config ruler zoom trackSeconds track


viewPhaseTrack :
    Config
    -> Float
    -> Float
    -> Float
    -> { section : String, total : TotalSpec, phases : List PhaseSpec }
    -> List (Html Msg)
viewPhaseTrack config ruler zoom trackSeconds track =
    let
        -- 各区間の終わりの位置(割合)。最後(to Nothing)は 1.0 固定
        rawEnds =
            track.phases
                |> List.map
                    (\p ->
                        p.to
                            |> Maybe.andThen (\name -> valueOf config (track.section ++ "." ++ name))
                            |> Maybe.withDefault 1
                    )

        ends =
            pushForward rawEnds

        starts =
            0 :: ends

        widthOf ratio =
            ratio * trackSeconds / ruler

        segments =
            List.map3
                (\phase start end ->
                    let
                        width =
                            widthOf (Basics.max 0 (end - start))
                    in
                    div
                        [ HA.classList
                            [ ( "tl-phase", True )
                            , ( "tl-wait", phase.wait )
                            ]
                        , HA.style "left" (percent (widthOf start))
                        , HA.style "width" (percent width)
                        , HA.title (Maybe.withDefault phase.label phase.description)
                        ]
                        -- 頭の数文字が入る幅なら書く(CSS の text-overflow が … で切る)。
                        -- それ以下は隠す — フェーズは隣が密着していて右へ
                        -- はみ出す空きが無いので、ホバーの title に任せる
                        (if width * 100 * zoom >= labelMinPercent then
                            [ span [ HA.class "tl-phase-label" ] [ text phase.label ] ]

                         else
                            []
                        )
                )
                track.phases
                starts
                ends

        boundaryHandles =
            List.map2
                (\phase end ->
                    phase.to
                        |> Maybe.map
                            (\name ->
                                ( widthOf end
                                , Boundary { section = track.section, target = name }
                                , secondsText (end * trackSeconds)
                                )
                            )
                )
                track.phases
                ends
                |> List.filterMap identity

        totalHandle =
            totalEndTarget track.total
                |> Maybe.map
                    (\target ->
                        [ ( trackSeconds / ruler
                          , TotalEnd { target = target }
                          , "全体 " ++ secondsText trackSeconds
                          )
                        ]
                    )
                |> Maybe.withDefault []

        handles =
            boundaryHandles ++ totalHandle
    in
    -- 総尺は右端のグリップが「全体 1.28s」と言うので、ここでは繰り返さない
    [ div [ HA.class "tl-row-label" ]
        [ text (labelOf config track.section) ]
    , div
        [ HA.class "tl-track tl-track-phase"
        , onPointerDown (pickNearest handles)
        ]
        -- 枠は総尺の所で切る(枠の外 = 時間の外。何も描かない)。当たり判定は
        -- トラック全幅のままなので、fx の座標系は変わらない
        -- 秒ラベルは隣どうしが近いと重なるので、上下 2 段に互い違いで置く
        (div
            [ HA.class "tl-frame"
            , HA.style "width" (percent (trackSeconds / ruler))
            ]
            []
            :: segments
            ++ List.indexedMap (\i handle -> viewGrip { alt = modBy 2 i == 1, warn = False } handle) handles
        )
    ]


viewClipTrack :
    Config
    -> Float
    -> Float
    -> Float
    -> { section : String, total : TotalSpec, scenes : List SceneSpec, items : List ClipSpec }
    -> List (Html Msg)
viewClipTrack config ruler zoom beatSeconds track =
    let
        -- 「1 ビート」の線そのものを掴んで beatSeconds を動かす。
        -- 逆算が成立するとき(宣言の to が multiply に 1 回)だけ掴める
        beatHandle =
            totalEndTarget track.total
                |> Maybe.map
                    (\target ->
                        ( beatSeconds / ruler
                        , TotalEnd { target = target }
                        , "1 ビート " ++ secondsText beatSeconds
                        )
                    )
    in
    -- ワンショットの起点はターンの頭のバーの中の位置ではなく「トリガーの瞬間」
    -- (カードの発動・被弾)。トリガーは戦況しだいで毎回違う時刻に起きるので、
    -- 横位置は描かず(描くと嘘になる)、全部左端 0 = トリガーとして長さだけ見せる
    div [ HA.class "tl-group-note" ]
        [ text (labelOf config track.section ++ " — トリガー(カードの発動・被弾)ごとに 1 回だけ再生。トリガーが来る時刻は実行時に決まるので、ここで編集するのはトリガーからの待ち(左端)と長さ(右端)。枠は 1 ビート") ]
        :: (track.items
                |> List.indexedMap
                    (\index clip ->
                        case clipSpan config { section = track.section, beatSeconds = beatSeconds } clip of
                            -- 値も default も無いクリップは行ごと出さない(fail-open)
                            Nothing ->
                                []

                            Just span_ ->
                                viewClipRow config
                                    ruler
                                    zoom
                                    beatSeconds
                                    track.section
                                    -- 取っ手の絵は先頭の行にだけ出す(線は全行を貫いて
                                    -- いるので十分)。掴みはどの行からでも効く
                                    { drag = beatHandle, showGrip = index == 0 }
                                    clip
                                    span_
                    )
                |> List.concat
           )


viewClipRow :
    Config
    -> Float
    -> Float
    -> Float
    -> String
    -> { drag : Maybe ( Float, Handle, String ), showGrip : Bool }
    -> ClipSpec
    -> { seconds : Float, capped : Bool }
    -> List (Html Msg)
viewClipRow config ruler zoom beatSeconds section beat clip span_ =
    let
        widthOf sec =
            sec / ruler

        beatHandles =
            beat.drag |> Maybe.map List.singleton |> Maybe.withDefault []

        beatGrip =
            case ( beat.showGrip, beat.drag ) of
                ( True, Just ( fx, _, _ ) ) ->
                    [ div
                        [ HA.class "tl-grip tl-grip-beat"
                        , HA.style "left" (percent fx)
                        ]
                        [ div [ HA.class "tl-grip-dot" ] [] ]
                    ]

                _ ->
                    []

        startSeconds =
            clipStartSeconds config { section = section, beatSeconds = beatSeconds } clip

        -- 開始の待ちと長さの合計がビートを超えたら長さの側を縮める
        spanSeconds =
            Basics.min span_.seconds (Basics.max 0 (beatSeconds - startSeconds))

        -- 秒に「ビートに対する割合」を併記する(下のフォームの数値と照合できる)。
        -- 上限で切られている間は割合を動かしても絵が変わらないので、代わりに(上限)と言う
        gripLabel =
            if span_.capped then
                secondsText spanSeconds ++ " (上限)"

            else
                valueOf config (section ++ "." ++ clip.length)
                    |> Maybe.map (\r -> secondsText spanSeconds ++ " (" ++ String.fromInt (round (r * 100)) ++ "%)")
                    |> Maybe.withDefault (secondsText spanSeconds)

        handle =
            ( widthOf (startSeconds + spanSeconds)
            , ClipEnd { section = section, target = clip.length, afterStart = clip.start }
            , gripLabel
            )

        startHandle =
            clip.start
                |> Maybe.map
                    (\name ->
                        [ ( widthOf startSeconds
                          , ClipStart { section = section, target = name }
                          , "開始 " ++ secondsText startSeconds
                          )
                        ]
                    )
                |> Maybe.withDefault []

        startWait =
            if startSeconds > 0 then
                [ div
                    [ HA.class "tl-clip tl-wait"
                    , HA.style "width" (percent (widthOf startSeconds))
                    , HA.title ("トリガーから " ++ secondsText startSeconds ++ " 待ってから始まる(左端のグリップで動かす)")
                    ]
                    []
                ]

            else
                []

        capTitle =
            clip.capSeconds
                |> Maybe.andThen (\name -> valueOf config (section ++ "." ++ name))
                |> Maybe.map (\cap -> "上限 " ++ secondsText cap ++ " で切られている(超える値は左のフォームで)")
                |> Maybe.withDefault ""

        clipWidth =
            widthOf spanSeconds

        clipEnd =
            widthOf (startSeconds + spanSeconds)

        -- 名前は 3 段: 入るなら全文 / 途中まで入るなら … で省略(CSS の
        -- text-overflow) / … すら入らない狭さならバーの右の空きへはみ出す
        -- (このタイムラインは枠の右が必ず空いているので置ける)
        nameInside =
            clipWidth * 100 * zoom >= labelMinPercent

        clipName =
            if nameInside then
                [ span [ HA.class "tl-phase-label" ] [ text clip.label ] ]

            else
                []

        nameOutside =
            if nameInside then
                []

            else
                [ span
                    [ HA.class "tl-phase-label tl-label-out"
                    , HA.style "left" (percent clipEnd)
                    ]
                    [ text clip.label ]
                ]

        rest =
            case clip.restLabel of
                Just label ->
                    [ div
                        [ HA.class "tl-rest"
                        , HA.style "left" (percent clipEnd)
                        , HA.style "width" (percent (widthOf (Basics.max 0 (beatSeconds - startSeconds - spanSeconds))))
                        , HA.title label
                        ]
                        [ span [ HA.class "tl-phase-label" ] [ text label ] ]
                    ]

                Nothing ->
                    []
    in
    [ div
        [ HA.class "tl-track tl-track-clip"
        , onPointerDown (pickNearest (handle :: startHandle ++ beatHandles))
        ]
        -- 枠 = 1 ビート(ワンショットはこの中で終わる)。枠の外には何も描かない
        (div
            [ HA.class "tl-frame"
            , HA.style "width" (percent (widthOf beatSeconds))
            ]
            []
            :: startWait
            ++ (div
                    [ HA.class "tl-clip"
                    , HA.style "left" (percent (widthOf startSeconds))
                    , HA.style "width" (percent clipWidth)
                    , HA.title
                        (if capTitle == "" then
                            clip.label

                         else
                            clip.label ++ "。" ++ capTitle
                        )
                    ]
                    clipName
                    :: rest
               )
            ++ nameOutside
            ++ beatGrip
            ++ (startHandle |> List.map (viewGrip { alt = True, warn = False }))
            ++ [ viewGrip { alt = False, warn = span_.capped } handle ]
        )
    ]


{-| 名前を塗りの中に出す最小幅(物差しに対する %)。「… を添えて頭の 3 文字」が
読める幅の見積もり(1 文字 ≈ 1.7%)。
-}
labelMinPercent : Float
labelMinPercent =
    1.7 * 4


{-| 押した位置に一番近いハンドルを掴む。同率は後ろが勝つ — 押し出しで
0 幅に重なった境目は、後ろを動かせば広げ直せる。
-}
pickNearest : List ( Float, Handle, String ) -> Point -> Msg
pickNearest handles point =
    handles
        |> List.foldl
            (\( fx, handle, _ ) best ->
                case best of
                    Nothing ->
                        Just ( fx, handle )

                    Just ( bestFx, _ ) ->
                        if abs (point.fx - fx) <= abs (point.fx - bestFx) then
                            Just ( fx, handle )

                        else
                            best
            )
            Nothing
        |> Maybe.map (\( _, handle ) -> Pressed handle point)
        |> Maybe.withDefault Released


{-| グリップ 1 本。alt = 秒ラベルを上の段に逃がす(隣と互い違いにして重なりを断つ)。
warn = 上限で切られている間、取っ手ごと黄色にする — 「この取っ手はいま右へ
動かしても効かない」を取っ手自身の色で言う。
-}
viewGrip : { alt : Bool, warn : Bool } -> ( Float, Handle, String ) -> Html Msg
viewGrip opts ( fx, handle, label ) =
    div
        [ HA.classList
            [ ( "tl-grip", True )
            , ( "tl-grip-alt", opts.alt )
            , ( "tl-grip-warn", opts.warn )
            , ( "tl-grip-total", isTotal handle )
            ]
        , HA.style "left" (percent fx)
        ]
        [ div [ HA.class "tl-grip-dot" ] []
        , div [ HA.class "tl-grip-label" ] [ text label ]
        ]


isTotal : Handle -> Bool
isTotal handle =
    case handle of
        TotalEnd _ ->
            True

        _ ->
            False


onPointerDown : (Point -> Msg) -> Html.Attribute Msg
onPointerDown toMsg =
    HE.on "pointerdown" (D.map toMsg pointDecoder)


{-| 押した場所を「その要素の中の割合」で読む(SfxEditor と同じ)。
セグメント(帯)はホバーの title を出すために pointer-events を持つので、
その上で押すと offsetX はセグメント基準になる — segmentShift で track 基準へ戻す。
-}
pointDecoder : D.Decoder Point
pointDecoder =
    D.map5 (\x y w h dx -> { fx = safeDiv (x + dx) w, fy = safeDiv y h })
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)
        (D.at [ "currentTarget", "clientWidth" ] D.float)
        (D.at [ "currentTarget", "clientHeight" ] D.float)
        segmentShift


{-| target がトラック直下の絶対配置の子(offsetParent = tl-track)のときだけ、
その子の left を足して offsetX をトラック基準に直す。target がトラック自身や
tl-surface のときは offsetParent がトラックではないので 0 のまま。
-}
segmentShift : D.Decoder Float
segmentShift =
    D.oneOf
        [ D.at [ "target", "offsetParent", "className" ] D.string
            |> D.andThen
                (\cls ->
                    if String.contains "tl-track" cls then
                        D.at [ "target", "offsetLeft" ] D.float

                    else
                        D.succeed 0
                )
        , D.succeed 0
        ]


safeDiv : Float -> Float -> Float
safeDiv a b =
    if b <= 0 then
        0

    else
        clamp 0 1 (a / b)


percent : Float -> String
percent fx =
    String.fromFloat (clamp 0 100 (fx * 100)) ++ "%"


{-| 秒の表示は 2 桁で足りる(step 0.01 × 総尺 1〜3 秒の粒度)。
-}
secondsText : Float -> String
secondsText sec =
    String.fromFloat (toFloat (Basics.round (sec * 100)) / 100) ++ "s"



-- 小さな組み立て部品


opt : String -> D.Decoder a -> D.Decoder (Maybe a)
opt name dec =
    D.oneOf [ D.field name (D.nullable dec), D.succeed Nothing ]
