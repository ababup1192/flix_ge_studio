module FileVerbs exposing
    ( Dialog
    , Handlers
    , Kind(..)
    , duplicateOf
    , forDelete
    , forDuplicate
    , forNew
    , problem
    , renameProblem
    , renameTarget
    , schemaPathOf
    , targetPath
    , view
    )

{-| ファイルそのものへの動詞(新規 / 複製 / 名前を変える / 消す)の聞き方。

「どこへ何という名前で作るか」は宣言の pattern が決める。人が打つのは名前だけで、
置き場と拡張子はこちらが埋める — 打ち間違いでプロジェクトの変な所にファイルが
生えるのを、入力の時点で起こらなくする(サーバ側の門番はその後ろの二重の守り)。

断りの理由は押した後に出す(打っている間は赤くしない)— 改名・エントリ追加と
同じ流儀。

-}

import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Skeleton


type alias Dialog =
    { kind : Kind
    , text : String
    , error : Maybe String
    }


{-| 何を聞いているか。pattern を抱いて回るのは、名前から実際のパスを組むのに
要るのがそれだけだから(グループの素性は聞き終わったら要らない)。
-}
type Kind
    = NewFile { groupId : String, groupLabel : String, pattern : String, schemaPath : Maybe String }
    | Duplicate { path : String, pattern : String }
    | Delete { path : String }


type alias Handlers msg =
    { onTyped : String -> msg
    , onConfirmed : msg
    , onCancelled : msg
    }


forNew : { groupId : String, groupLabel : String, pattern : String, schemaPath : Maybe String } -> Dialog
forNew group =
    { kind = NewFile group, text = "", error = Nothing }


{-| 複製・改名の初期値は元の名前(pattern の飾りを外した所)。全部打ち直させない。 -}
forDuplicate : String -> String -> Dialog
forDuplicate pattern path =
    { kind = Duplicate { path = path, pattern = pattern }
    , text = Skeleton.bareNameOf pattern path
    , error = Nothing
    }


forDelete : String -> Dialog
forDelete path =
    { kind = Delete { path = path }, text = "", error = Nothing }


{-| 打った名前が落ちる先のパス。消す時は対象そのもの。 -}
targetPath : Dialog -> String
targetPath dialog =
    case dialog.kind of
        NewFile g ->
            Skeleton.pathFor g.pattern dialog.text

        Duplicate d ->
            Skeleton.pathFor d.pattern dialog.text

        Delete d ->
            d.path


{-| 複製元(複製の時だけ)。 -}
duplicateOf : Dialog -> Maybe String
duplicateOf dialog =
    case dialog.kind of
        Duplicate d ->
            Just d.path

        _ ->
            Nothing


{-| 押してよいか(Nothing = 通す)。空名・既にある名前・元と同じ名前を、
サーバへ行く前に断る。理由は画面にそのまま出す。
-}
problem : List String -> Dialog -> Maybe String
problem existing dialog =
    let
        path =
            targetPath dialog
    in
    case dialog.kind of
        Delete _ ->
            Nothing

        _ ->
            if String.trim dialog.text == "" then
                Just "名前が空です"

            else if List.member path existing then
                Just ("\"" ++ path ++ "\" は既にあります")

            else
                Nothing


view : Handlers msg -> List String -> Dialog -> Html msg
view handlers existing dialog =
    let
        path =
            targetPath dialog

        blocked =
            problem existing dialog
    in
    -- ✕ ボタン・Esc・外側クリックはどれも sl-request-close で届く(部品が自分で
    -- 閉じても Elm 側が開いたままだと、次に開けなくなる)
    Html.node "sl-dialog"
        [ HA.class "file-verb"
        , HA.attribute "label" (label dialog.kind)
        , HA.attribute "open" ""
        , HE.on "sl-request-close" (D.succeed handlers.onCancelled)
        ]
        (case dialog.kind of
            Delete d ->
                [ div [ HA.class "text-xs leading-relaxed text-ink-soft" ]
                    [ text ("\"" ++ d.path ++ "\" を消します。元には戻せません。") ]
                , footer handlers "消す" "btn btn-danger" False
                ]

            _ ->
                [ div [ HA.class "mb-2 text-[11px] leading-relaxed text-ink-soft" ]
                    [ text (guide dialog.kind) ]
                , input
                    [ HA.class "verb-name field w-full"
                    , HA.type_ "text"
                    , HA.placeholder "名前(拡張子は要りません)"
                    , HA.value dialog.text
                    , HE.onInput handlers.onTyped
                    ]
                    []
                , div [ HA.class "verb-path mt-1.5 font-mono text-[11px] text-ink-faint" ]
                    [ text path ]
                , case ( dialog.error, blocked ) of
                    ( Just reason, _ ) ->
                        div [ HA.class "verb-error mt-1.5 text-[11px] text-danger" ] [ text reason ]

                    ( Nothing, Just reason ) ->
                        div [ HA.class "verb-error mt-1.5 text-[11px] text-danger" ] [ text reason ]

                    _ ->
                        text ""
                , footer handlers (confirmLabel dialog.kind) "btn btn-primary" (blocked /= Nothing)
                ]
        )


footer : Handlers msg -> String -> String -> Bool -> Html msg
footer handlers confirm confirmClass disabled =
    div [ HA.attribute "slot" "footer", HA.class "flex justify-end gap-2" ]
        [ button [ HA.class "btn", HE.onClick handlers.onCancelled ] [ text "やめる" ]
        , button
            [ HA.class confirmClass
            , HA.disabled disabled
            , HE.onClick handlers.onConfirmed
            ]
            [ text confirm ]
        ]


label : Kind -> String
label kind =
    case kind of
        NewFile _ ->
            "新しいファイル"

        Duplicate _ ->
            "ファイルを複製"

        Delete _ ->
            "ファイルを消す"


confirmLabel : Kind -> String
confirmLabel kind =
    case kind of
        NewFile _ ->
            "作る"

        Duplicate _ ->
            "複製する"

        Delete _ ->
            "消す"


guide : Kind -> String
guide kind =
    case kind of
        NewFile g ->
            "「" ++ g.groupLabel ++ "」に新しいファイルを作ります。中身はスキーマの欄を全部書いた骨格です。"

        Duplicate d ->
            "\"" ++ d.path ++ "\" の中身をそのまま、別の名前で作ります。"

        Delete _ ->
            ""


{-| 新規のときだけ、骨格を組むのに要るスキーマの場所。 -}
schemaPathOf : Kind -> Maybe String
schemaPathOf kind =
    case kind of
        NewFile g ->
            g.schemaPath

        _ ->
            Nothing


{-| その場の名前変更が落ちる先。 -}
renameTarget : String -> String -> String
renameTarget pattern text =
    Skeleton.pathFor pattern text


{-| その場の名前変更を断る理由(Nothing = 通す)。空・重複はサーバへ行く前に断る。
元と同じ名前は「やめた」と同じ扱いにしたいので、ここでは断らず呼び側が畳む。
-}
renameProblem : List String -> { pattern : String, path : String, text : String } -> Maybe String
renameProblem existing item =
    let
        target =
            renameTarget item.pattern item.text
    in
    if String.trim item.text == "" then
        Just "名前が空です"

    else if target == item.path then
        Nothing

    else if List.member target existing then
        Just ("\"" ++ target ++ "\" は既にあります")

    else
        Nothing
