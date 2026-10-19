module VideoView exposing
    ( Card(..)
    , Listing(..)
    , Model
    , Msg(..)
    , Out(..)
    , State(..)
    , Video
    , gotList
    , gotRerun
    , gotSfx
    , init
    , listDecoder
    , listFailed
    , loading
    , marksOf
    , parseSfx
    , update
    , view
    )

{-| ギャラリーの「動画」のタブ — ゲームが撮った動画(debug/video/ の mp4)を一覧で見る。

いちばんのねらいは、途中までしか撮れていない動画を一覧の印で見つけること
(誰も開かなければ気づけない)。撮れたか・基準と違うかの判定は engine の道具
(bin/fge videos status)が持ち、ここは届いた state を印にするだけ。

1 枚のカードに出るのは 4 つ。

  - 動画(撮れていない物は最後の画面)。クリックで再生
  - 題と一言 … その動画を撮った flow の先頭のコメント。flow で撮っていない物は
    ファイル名で代用し、「説明がまだありません」と静かに添える
  - 印 … 撮れていない(失敗・途中で止まった)/ 基準と違う(make compare)/ 撮影中
  - 流し直す … flow で撮った動画だけ。起動中のゲームへ同じ flow を流す(ビルド不要)

再生中は、鳴った音(sfx.txt)を再生の帯の上に印で出す。同じコマに 2 回鳴った音は
重ねて濃く出るので、二重に鳴った所と鳴らなくなった所が目で分かる。

サーバ往復は Main の仕事(Out で頼む)。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, img, span, text, video)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Url


{-| 撮れたか(道具の state をそのまま型にした物)。 -}
type State
    = Done
    | Failed
    | Stuck
    | Running
    | NoRecord


{-| 動画 1 本ぶん。パスはどれもゲームの根から(無ければ空)。 -}
type alias Video =
    { name : String
    , state : State
    , make : String
    , stage : String
    , failure : String
    , frames : Int
    , fps : Float
    , mp4 : String
    , mp4Mtime : Int
    , lastFrame : String
    , sfx : String
    , flow : String
    , title : String
    , desc : String
    , compare : String
    , compareDetail : String
    }


{-| 一覧の様子。

  - Loading … 取りに行った(開いた直後)
  - Unavailable … 道具が古い・口の無いサーバ(理由を案内に出す)
  - Listed … here = この Mac に置き場があるか(debug/ は git の外なので、撮った Mac にしか無い)

-}
type Listing
    = Loading
    | Unavailable String
    | Listed { here : Bool, videos : List Video }


{-| カードごとの流し直しの様子。 -}
type Card
    = Idle
    | Rerunning
    | RerunFailed String


type alias Model =
    { listing : Listing
    , cards : Dict String Card

    -- 再生中の動画(Nothing = 閉じている)
    , playing : Maybe String

    -- 動画の名前 → 鳴った音(コマ番号・名前)。再生を開いた時に取りに行く
    , sfx : Dict String (List ( Int, String ))

    -- 再生中の動画の長さと今の位置(秒。video 要素が知らせる)
    , duration : Float
    , position : Float
    }


init : Model
init =
    { listing = Loading, cards = Dict.empty, playing = Nothing, sfx = Dict.empty, duration = 0, position = 0 }


loading : Model -> Model
loading model =
    { model | listing = Loading }


gotList : Listing -> Model -> Model
gotList listing model =
    { model | listing = listing }


listFailed : Model -> Model
listFailed model =
    { model | listing = Unavailable "このサーバは動画の口を持っていません。Studio を新しくしてください。" }


{-| sfx.txt の中身が届いた。 -}
gotSfx : String -> String -> Model -> Model
gotSfx name body model =
    { model | sfx = Dict.insert name (parseSfx body) model.sfx }


{-| 流し直しの答え(Ok = 流れた・Err = 理由)。 -}
gotRerun : String -> Result String () -> Model -> Model
gotRerun name result model =
    { model
        | cards =
            Dict.insert name
                (case result of
                    Ok () ->
                        Idle

                    Err why ->
                        RerunFailed why
                )
                model.cards
    }


type Msg
    = CardClicked String
    | PlayerClosed
    | RefreshClicked
    | RerunClicked String
    | DurationKnown Float
    | PositionMoved Float


type Out
    = OutNone
    | OutRefresh
    | OutFetchSfx String String
    | OutRerun String


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        CardClicked name ->
            let
                opened =
                    { model | playing = Just name, duration = 0, position = 0 }
            in
            case videoNamed model name of
                Just found ->
                    if found.sfx /= "" && not (Dict.member name model.sfx) then
                        ( opened, OutFetchSfx name found.sfx )

                    else
                        ( opened, OutNone )

                Nothing ->
                    ( model, OutNone )

        PlayerClosed ->
            ( { model | playing = Nothing }, OutNone )

        RefreshClicked ->
            ( { model | listing = Loading }, OutRefresh )

        RerunClicked name ->
            if Dict.get name model.cards == Just Rerunning then
                ( model, OutNone )

            else
                ( { model | cards = Dict.insert name Rerunning model.cards }, OutRerun name )

        DurationKnown seconds ->
            ( { model | duration = seconds }, OutNone )

        PositionMoved seconds ->
            ( { model | position = seconds }, OutNone )


videoNamed : Model -> String -> Maybe Video
videoNamed model name =
    case model.listing of
        Listed listed ->
            listed.videos |> List.filter (\v -> v.name == name) |> List.head

        _ ->
            Nothing


{-| sfx.txt(「コマ番号 音の名前」の並び。# の行と空行は飛ばす)を読む。 -}
parseSfx : String -> List ( Int, String )
parseSfx body =
    body
        |> String.lines
        |> List.filterMap
            (\line ->
                case String.words line of
                    frame :: name :: _ ->
                        if String.startsWith "#" frame then
                            Nothing

                        else
                            String.toInt frame |> Maybe.map (\n -> ( n, name ))

                    _ ->
                        Nothing
            )


{-| 帯の上の印: (コマ番号, 名前, 同じコマに同じ名前が鳴った数)。コマ順。 -}
marksOf : List ( Int, String ) -> List ( Int, String, Int )
marksOf sounds =
    sounds
        |> List.foldl (\key acc -> Dict.update key (\n -> Just (Maybe.withDefault 0 n + 1)) acc) Dict.empty
        |> Dict.toList
        |> List.map (\( ( frame, name ), count ) -> ( frame, name, count ))



-- デコーダ


listDecoder : D.Decoder Listing
listDecoder =
    D.field "available" D.bool
        |> D.andThen
            (\available ->
                if available then
                    D.map2 (\here videos -> Listed { here = here, videos = videos })
                        (withDefault True (D.field "here" D.bool))
                        (D.field "videos" (D.list videoDecoder))

                else
                    D.map Unavailable (withDefault "" (D.field "reason" D.string))
            )


videoDecoder : D.Decoder Video
videoDecoder =
    D.succeed Video
        |> field "name" D.string ""
        |> field "state" stateDecoder NoRecord
        |> field "make" D.string ""
        |> field "stage" D.string ""
        |> field "failure" D.string ""
        |> field "frames" D.int 0
        |> field "fps" D.float 0
        |> field "mp4" D.string ""
        |> field "mp4Mtime" D.int 0
        |> field "lastFrame" D.string ""
        |> field "sfx" D.string ""
        |> field "flow" D.string ""
        |> field "title" D.string ""
        |> field "desc" D.string ""
        |> field "compare" D.string ""
        |> field "compareDetail" D.string ""


field : String -> D.Decoder a -> a -> D.Decoder (a -> b) -> D.Decoder b
field key decoder fallback =
    D.map2 (|>) (withDefault fallback (D.field key decoder))


stateDecoder : D.Decoder State
stateDecoder =
    D.string
        |> D.map
            (\s ->
                case s of
                    "ok" ->
                        Done

                    "failed" ->
                        Failed

                    "stuck" ->
                        Stuck

                    "running" ->
                        Running

                    _ ->
                        NoRecord
            )


withDefault : a -> D.Decoder a -> D.Decoder a
withDefault fallback decoder =
    D.oneOf [ decoder, D.succeed fallback ]



-- 画面


type alias Ctx =
    { serverBase : String, projectKey : String }


view : Ctx -> Model -> Html Msg
view ctx model =
    div [ HA.class "videos flex min-h-0 flex-1 flex-col overflow-y-auto px-6 pb-8 pt-5" ]
        [ viewHead model
        , viewBody ctx model
        , viewPlayer ctx model
        ]


isBroken : Video -> Bool
isBroken v =
    v.state == Failed || v.state == Stuck


viewHead : Model -> Html Msg
viewHead model =
    let
        videos =
            case model.listing of
                Listed listed ->
                    listed.videos

                _ ->
                    []

        broken =
            List.length (List.filter isBroken videos)
    in
    div [ HA.class "mb-4 flex items-baseline gap-3" ]
        [ span [ HA.class "text-sm font-semibold text-ink" ]
            [ text ("動画 " ++ String.fromInt (List.length videos)) ]
        , if broken > 0 then
            span [ HA.class "video-broken-count rounded-sm bg-red-500/25 px-1.5 text-[11px] text-ink" ]
                [ text ("撮れていない " ++ String.fromInt broken) ]

          else
            text ""
        , span [ HA.class "text-[11px] text-ink-faint" ]
            [ text "検証や撮影で撮った動画(debug/video/)です。クリックで再生します" ]
        , button
            [ HA.class "videos-refresh ml-auto cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
            , HE.onClick RefreshClicked
            ]
            [ text
                (if model.listing == Loading then
                    "⏳ 読み込んでいます…"

                 else
                    "更新"
                )
            ]
        ]


viewBody : Ctx -> Model -> Html Msg
viewBody ctx model =
    case model.listing of
        Loading ->
            viewNote "読み込んでいます…"

        Unavailable reason ->
            viewNote reason

        Listed listed ->
            if not listed.here then
                viewNote "この Mac では撮っていません(動画は撮った Mac の debug/video/ にだけあります。debug/ は git の外です)。"

            else if List.isEmpty listed.videos then
                viewNote "まだ動画がありません。ゲームの動画の口(make 〜-video)で撮ると、ここに並びます。"

            else
                -- 撮れていない物を先に(気づいてほしい物が上)
                div [ HA.class "grid grid-cols-[repeat(auto-fill,minmax(240px,1fr))] gap-4" ]
                    (List.map (viewCard ctx model)
                        (List.filter isBroken listed.videos ++ List.filter (not << isBroken) listed.videos)
                    )


viewNote : String -> Html Msg
viewNote message =
    div [ HA.class "text-xs text-ink-soft" ] [ text message ]


fileUrl : Ctx -> String -> String
fileUrl ctx path =
    ctx.serverBase ++ "/videos/file?p=" ++ ctx.projectKey ++ "&path=" ++ Url.percentEncode path


viewCard : Ctx -> Model -> Video -> Html Msg
viewCard ctx model v =
    div
        [ HA.classList
            [ ( "video-card d-card d-card-border overflow-hidden rounded-lg bg-panel", True )
            , ( "border-red-500/70", isBroken v )
            , ( "border-edge", not (isBroken v) )
            ]
        ]
        [ viewThumb ctx v
        , div [ HA.class "flex min-w-0 flex-col gap-1 px-3 py-2" ]
            [ div [ HA.class "truncate text-xs font-semibold text-ink", HA.title v.name ]
                [ text (headingOf v) ]
            , viewDesc v
            , viewMarks v
            , viewRerun v (Dict.get v.name model.cards |> Maybe.withDefault Idle)
            ]
        ]


{-| 撮れていない動画は最後の画面を出す(どこで止まったかが一目で分かる)。
撮れた動画は mp4 の頭のコマ(video 要素の preload=metadata)。
-}
viewThumb : Ctx -> Video -> Html Msg
viewThumb ctx v =
    if (isBroken v || v.mp4 == "") && v.lastFrame /= "" then
        div [ HA.class "relative" ]
            [ img
                [ HA.class "video-last-frame block w-full bg-well"
                , HA.src (fileUrl ctx v.lastFrame)
                , HA.alt v.name
                , HA.attribute "loading" "lazy"
                , HA.title "最後に描けた画面"
                ]
                []
            , span [ HA.class "absolute left-1 top-1 rounded-sm bg-black/70 px-1 text-[10px] text-white" ]
                [ text "最後の画面" ]
            ]

    else if v.mp4 /= "" then
        video
            [ HA.class "video-thumb block w-full cursor-pointer bg-well"
            , HA.src (fileUrl ctx v.mp4 ++ "&t=" ++ String.fromInt v.mp4Mtime)
            , HA.attribute "preload" "metadata"
            , HA.attribute "muted" ""
            , HA.title "クリックで再生"
            , HE.onClick (CardClicked v.name)
            ]
            []

    else
        div [ HA.class "flex h-24 items-center justify-center bg-well text-[11px] text-ink-faint" ]
            [ text "mp4 がありません" ]


headingOf : Video -> String
headingOf v =
    if v.title == "" then
        v.name

    else
        v.title


viewDesc : Video -> Html Msg
viewDesc v =
    if v.desc == "" && v.title == "" then
        div
            [ HA.class "text-[11px] text-ink-faint"
            , HA.title "flow(e2e/<名前>.flow)で撮った動画は、その先頭のコメントが題と一言になります"
            ]
            [ text "説明がまだありません" ]

    else if v.desc == "" then
        text ""

    else
        div [ HA.class "text-[11px] leading-snug text-ink-soft" ] [ text v.desc ]


{-| 気に留めてほしい事だけ出す。撮れていて基準どおりの動画には何も付けない。 -}
viewMarks : Video -> Html Msg
viewMarks v =
    let
        marks =
            List.filterMap identity
                [ case v.state of
                    Failed ->
                        Just ( "撮れていない", failedWhy v, "bg-red-500/30" )

                    Stuck ->
                        Just ( "撮れていない", "撮っていた make(" ++ orDash v.make ++ ")が途中で居なくなりました(running のまま)", "bg-red-500/30" )

                    Running ->
                        Just ( "撮影中", "make(" ++ orDash v.make ++ ")が撮っています", "bg-white/10" )

                    NoRecord ->
                        Just ( "記録なし", "撮れたかの記録(.video.json)を書く前に撮った動画です", "bg-white/5" )

                    Done ->
                        Nothing
                , if v.compare == "diff" then
                    Just ( "基準と違う", "make compare: " ++ v.compareDetail, "bg-accent/25" )

                  else
                    Nothing
                ]
    in
    if List.isEmpty marks then
        text ""

    else
        div [ HA.class "flex flex-col gap-1" ]
            (marks
                |> List.map
                    (\( label, why, tone ) ->
                        div [ HA.class "flex items-start gap-1" ]
                            [ span [ HA.class ("video-mark shrink-0 rounded-sm px-1 text-[10px] text-ink " ++ tone) ]
                                [ text label ]
                            , span [ HA.class "text-[10px] leading-snug text-ink-faint" ] [ text why ]
                            ]
                    )
            )


failedWhy : Video -> String
failedWhy v =
    let
        stage =
            case v.stage of
                "frames" ->
                    "連番を描く段で止まりました"

                "mix" ->
                    "mp4 に束ねる段で止まりました"

                _ ->
                    "止まりました"
    in
    if v.failure == "" then
        stage ++ "(make " ++ orDash v.make ++ ")"

    else
        stage ++ ": " ++ v.failure


orDash : String -> String
orDash s =
    if s == "" then
        "-"

    else
        s


viewRerun : Video -> Card -> Html Msg
viewRerun v card =
    if v.flow == "" then
        text ""

    else
        div [ HA.class "flex items-center gap-2" ]
            [ button
                [ HA.class "video-rerun d-btn d-btn-xs"
                , HA.disabled (card == Rerunning)
                , HA.title ("起動中のゲームへ " ++ v.flow ++ " を流し直します(ビルドしません)")
                , HE.onClick (RerunClicked v.name)
                ]
                [ text
                    (if card == Rerunning then
                        "流しています…"

                     else
                        "ゲームで流し直す"
                    )
                ]
            , case card of
                RerunFailed why ->
                    span [ HA.class "text-[10px] text-red-300" ] [ text why ]

                _ ->
                    text ""
            ]



-- 再生


viewPlayer : Ctx -> Model -> Html Msg
viewPlayer ctx model =
    case model.playing |> Maybe.andThen (videoNamed model) of
        Nothing ->
            text ""

        Just v ->
            div [ HA.class "video-player fixed inset-0 z-50 flex flex-col items-center justify-center gap-3 bg-black/85" ]
                [ div [ HA.class "flex w-[90vw] max-w-[1200px] items-baseline gap-3" ]
                    [ span [ HA.class "text-xs text-white" ] [ text (headingOf v) ]
                    , span [ HA.class "font-mono text-[11px] text-white/60" ] [ text v.mp4 ]
                    , button
                        [ HA.class "ml-auto cursor-pointer text-xs text-white/70 hover:text-white"
                        , HE.onClick PlayerClosed
                        ]
                        [ text "閉じる ✕" ]
                    ]
                , viewSfxStrip model v
                , video
                    [ HA.class "max-h-[75vh] w-[90vw] max-w-[1200px] bg-black"
                    , HA.src (fileUrl ctx v.mp4 ++ "&t=" ++ String.fromInt v.mp4Mtime)
                    , HA.controls True
                    , HA.autoplay True
                    , HE.on "loadedmetadata" (D.map DurationKnown (D.at [ "target", "duration" ] D.float))
                    , HE.on "timeupdate" (D.map PositionMoved (D.at [ "target", "currentTime" ] D.float))
                    ]
                    []
                ]


{-| 再生の帯の上の音の印。横の位置 = コマ ÷ fps ÷ 動画の長さ。同じコマに同じ音が
2 回以上鳴った所は赤く濃く出す(×2 の字つき)。今の位置の縦線も引く。
-}
viewSfxStrip : Model -> Video -> Html Msg
viewSfxStrip model v =
    let
        note message =
            div [ HA.class "w-[90vw] max-w-[1200px] text-[11px] text-white/50" ] [ text message ]
    in
    if v.sfx == "" then
        note "この動画には音の表(sfx.txt)がありません"

    else if v.fps <= 0 || model.duration <= 0 then
        note
            (if v.fps <= 0 then
                "コマの速さ(fps)の記録が無いので、音の印を帯に置けません"

             else
                "音の印を読み込んでいます…"
            )

    else
        case Dict.get v.name model.sfx of
            Nothing ->
                note "音の印を読み込んでいます…"

            Just sounds ->
                let
                    at seconds =
                        String.fromFloat (clamp 0 100 (seconds / model.duration * 100)) ++ "%"
                in
                div [ HA.class "video-sfx-strip relative h-10 w-[90vw] max-w-[1200px] rounded-sm bg-white/5" ]
                    (div
                        [ HA.class "absolute bottom-0 top-0 w-px bg-white/70"
                        , HA.style "left" (at model.position)
                        ]
                        []
                        :: List.map
                            (\( frame, name, count ) ->
                                div
                                    [ HA.classList
                                        [ ( "video-sfx-mark absolute bottom-0 w-[3px] -translate-x-1/2", True )
                                        , ( "h-4 bg-sky-300/80", count == 1 )
                                        , ( "h-8 bg-red-400", count > 1 )
                                        ]
                                    , HA.style "left" (at (toFloat frame / v.fps))
                                    , HA.title
                                        (name
                                            ++ " @"
                                            ++ String.fromInt frame
                                            ++ (if count > 1 then
                                                    " ×" ++ String.fromInt count ++ "(同じコマに 2 回以上)"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                                    (if count > 1 then
                                        [ span [ HA.class "absolute -top-4 left-1 text-[9px] text-red-300" ] [ text ("×" ++ String.fromInt count) ] ]

                                     else
                                        []
                                    )
                            )
                            (marksOf sounds)
                    )
