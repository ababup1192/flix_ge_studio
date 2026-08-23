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
  - `{"clips": {...}}` … ワンショット演出のバー。起点はトリガー(左端 0 固定)で、
    右端を掴んで長さ(1 ビートに対する割合)を動かす

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
    | ClipTrack { section : String, total : TotalSpec, items : List ClipSpec }


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
capSeconds は上限秒のフィールド名(実際の長さは短い方)。
restLabel は「残りが別の動きになる」ときの名前(残りを別色で塗る)。
-}
type alias ClipSpec =
    { label : String
    , length : String
    , capSeconds : Maybe String
    , restLabel : Maybe String
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
            (D.map2 (\total items -> ClipTrack { section = key, total = total, items = items })
                (D.field "totalSeconds" totalDecoder)
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
    D.map4 ClipSpec
        (D.field "label" D.string)
        (D.field "length" D.string)
        (opt "capSeconds" D.string)
        (opt "restLabel" D.string)


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
    config.specs
        |> List.map (totalSecondsOf config)
        |> List.maximum
        |> Maybe.withDefault 0
        |> (*) 1.15


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



-- 状態とドラッグ


type alias Model =
    { drag : Maybe Drag

    -- 横方向の倍率(1 = 全体が収まる)。無段階 — ピンチ / Ctrl+ホイールの
    -- 増分をそのまま掛ける
    , zoom : Float
    }


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
-}
type Handle
    = Boundary { section : String, target : String }
    | ClipEnd { section : String, target : String }
    | TotalEnd { target : String }


type Msg
    = Pressed Handle Point
    | Moved Point
    | Released
      -- ピンチ / Ctrl+ホイール。Point は掴んだ点(スクロールの中心に残す)、
      -- Float は掛ける倍率(ホイールの増分から作るので無段階)
    | ZoomBy Point Float
    | ZoomHome


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
    { drag = Nothing, zoom = 1 }


isDragging : Model -> Bool
isDragging model =
    model.drag /= Nothing


update : Config -> Msg -> Model -> ( Model, Out )
update config msg model =
    case msg of
        -- 押しただけでは書かない(うっかりクリックで境目が飛ばないように)。
        -- 書くのは動かし始めてから — SfxEditor の縁の掴みと同じ流儀
        Pressed handle _ ->
            ( { model | drag = Just { handle = handle, rulerSeconds = rulerSecondsOf config } }
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

        ClipEnd at ->
            ratioEdit config at seconds

        TotalEnd at ->
            totalEdit config at seconds


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
                    :: viewBeatLine config ruler
                    ++ (config.specs |> List.concatMap (viewTrack config ruler model.zoom))
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
            ++ List.indexedMap (\i handle -> viewGrip (modBy 2 i == 1) handle) handles
        )
    ]


viewClipTrack :
    Config
    -> Float
    -> Float
    -> Float
    -> { section : String, total : TotalSpec, items : List ClipSpec }
    -> List (Html Msg)
viewClipTrack config ruler zoom beatSeconds track =
    -- ワンショットの起点はターンの頭のバーの中の位置ではなく「トリガーの瞬間」
    -- (カードの発動・被弾)。トリガーは戦況しだいで毎回違う時刻に起きるので、
    -- 横位置は描かず(描くと嘘になる)、全部左端 0 = トリガーとして長さだけ見せる
    div [ HA.class "tl-group-note" ]
        [ text (labelOf config track.section ++ " — トリガー(カードの発動・被弾)ごとに 1 回だけ再生。始まる時刻は実行時に決まるので、ここでは長さだけを編集する。枠は 1 ビート") ]
        :: (track.items
                |> List.concatMap
                    (\clip ->
                        case clipSpan config { section = track.section, beatSeconds = beatSeconds } clip of
                            -- 値も default も無いクリップは行ごと出さない(fail-open)
                            Nothing ->
                                []

                            Just span_ ->
                                viewClipRow config ruler zoom beatSeconds track.section clip span_
                    )
           )


viewClipRow :
    Config
    -> Float
    -> Float
    -> Float
    -> String
    -> ClipSpec
    -> { seconds : Float, capped : Bool }
    -> List (Html Msg)
viewClipRow config ruler zoom beatSeconds section clip span_ =
    let
        widthOf sec =
            sec / ruler

        -- 秒に「ビートに対する割合」を併記する(下のフォームの数値と照合できる)。
        -- 上限で切られている間は割合を動かしても絵が変わらないので、代わりに(上限)と言う
        gripLabel =
            if span_.capped then
                secondsText span_.seconds ++ " (上限)"

            else
                valueOf config (section ++ "." ++ clip.length)
                    |> Maybe.map (\r -> secondsText span_.seconds ++ " (" ++ String.fromInt (round (r * 100)) ++ "%)")
                    |> Maybe.withDefault (secondsText span_.seconds)

        handle =
            ( widthOf span_.seconds
            , ClipEnd { section = section, target = clip.length }
            , gripLabel
            )

        capTitle =
            clip.capSeconds
                |> Maybe.andThen (\name -> valueOf config (section ++ "." ++ name))
                |> Maybe.map (\cap -> "上限 " ++ secondsText cap ++ " で切られている(超える値は左のフォームで)")
                |> Maybe.withDefault ""

        clipWidth =
            widthOf span_.seconds

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
                    , HA.style "left" (percent clipWidth)
                    ]
                    [ text clip.label ]
                ]

        rest =
            case clip.restLabel of
                Just label ->
                    [ div
                        [ HA.class "tl-rest"
                        , HA.style "left" (percent clipWidth)
                        , HA.style "width" (percent (widthOf (Basics.max 0 (beatSeconds - span_.seconds))))
                        , HA.title label
                        ]
                        [ span [ HA.class "tl-phase-label" ] [ text label ] ]
                    ]

                Nothing ->
                    []
    in
    [ div
        [ HA.class "tl-track tl-track-clip"
        , onPointerDown (pickNearest [ handle ])
        ]
        -- 枠 = 1 ビート(ワンショットはこの中で終わる)。枠の外には何も描かない
        (div
            [ HA.class "tl-frame"
            , HA.style "width" (percent (widthOf beatSeconds))
            ]
            []
            :: div
                [ HA.classList [ ( "tl-clip", True ), ( "tl-capped", span_.capped ) ]
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
            ++ nameOutside
            ++ [ viewGrip False handle ]
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
-}
viewGrip : Bool -> ( Float, Handle, String ) -> Html Msg
viewGrip alt ( fx, handle, label ) =
    div
        [ HA.classList
            [ ( "tl-grip", True )
            , ( "tl-grip-alt", alt )
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
