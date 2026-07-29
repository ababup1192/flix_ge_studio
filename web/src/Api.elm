module Api exposing
    ( Changes
    , Dashboard
    , Design
    , Envelope
    , FileContent
    , Files
    , Health
    , HealthResult(..)
    , Preview
    , PreviewRect
    , PreviewResult(..)
    , ProjectEntry
    , ProjectSwitch(..)
    , Projects
    , PutFileResult(..)
    , ResourceFile
    , ResourceGroup
    , Resources
    , SearchHit
    , SearchResults
    , SpriteColors
    , changesDecoder
    , envelopeDecoder
    , fileContentDecoder
    , filesDecoder
    , healthResultDecoder
    , noSpriteColors
    , previewResultDecoder
    , projectKey
    , projectSwitchDecoder
    , projectsDecoder
    , putFileResultDecoder
    , resourcesDecoder
    , runningGamesDecoder
    , searchResultsDecoder
    , spriteColorsDecoder
    )

{-| editor_server 応答のデコーダ集。サーバが契約の正で、ずれたらこちらを直す。 -}

import Dict exposing (Dict)
import Edit exposing (Seg(..))
import Json.Decode as D
import Set exposing (Set)


type alias Design =
    { w : Float, h : Float }


type alias Health =
    { dir : String, title : String, design : Design, version : String }


{-| GET /health は「サーバは居るがプロジェクトが選べていない/読めない」ときも
HTTP 200 の {ok:false, error} で返る。ok の真偽で画面遷移(Editing / Picker)が
分かれるので、成功と失敗を 1 つのデコーダで受ける。
-}
type HealthResult
    = HealthOk Health
    | HealthErr String


{-| リソースエディタはファイル種別を区別しない(どれも「スキーマ付き JSON」候補)ので、
GET /files の ui / hitbox / palette をならして 1 本のパス列にする。
専用の /resources が生えたらそちらへ乗り換える。
-}
type alias Files =
    { root : String, paths : List String }


{-| mtime は保存の ifMtime(競合検出)に渡す控え。読む側が使わない文脈
(スキーマ・横断辞書)もあるので、欠けを失敗にしない。
-}
type alias FileContent =
    { path : String, content : String, mtime : Maybe Int }


{-| PUT /file の結果。409(競合)も {ok:false, currentMtime} の JSON が契約なので、
成功・競合・その他失敗を 1 つのデコーダで受ける(/health と同じ流儀)。
baking はサーバが保存の瞬間に検査(焼き)を蹴れたかの知らせ —
無い(旧サーバ)は False に倒す(従来どおりポーリング任せ)。
-}
type PutFileResult
    = PutOk { mtime : Maybe Int, baking : Bool }
    | PutConflict { currentMtime : Int }
    | PutErr String


{-| GET /changes — 監視対象ファイルの mtime(ミリ秒)一覧と集計 token。
C0 では未使用(外部変更の追従ポーリングは次の段)。
-}
type alias Changes =
    { token : String, files : Dict String Int }


{-| プロジェクト候補 1 件。title は project.json の題名(サーバが読めなければディレクトリ名)。 -}
type alias ProjectEntry =
    { dir : String, title : String }


{-| GET /projects — current は選択中(未選択は null)、recent は最近開いた順、
found は探索起点(既定 $HOME/Desktop)で見つかった project.json 持ちディレクトリ。
-}
type alias Projects =
    { current : Maybe String
    , recent : List ProjectEntry
    , found : List ProjectEntry
    }


{-| POST /project の結果。400 でも {ok:false, error} の JSON が契約なので、
成功/失敗を 1 つのデコーダで受けて、失敗文言は候補カードの赤メッセージに使う。
-}
type ProjectSwitch
    = SwitchOk { dir : String, title : String, design : Design }
    | SwitchErr String


{-| POST /preview/items の矩形 1 個(design 座標)。サーバは [x,y,w,h] の配列で
返すが、位置計算の並びミス(w と h の取り違え等)を型で防ぐため record に起こす。
-}
type alias PreviewRect =
    { x : Float, y : Float, w : Float, h : Float }


type alias Preview =
    { png : String
    , width : Int
    , height : Int
    , scale : Float
    , design : Design
    , rects : Dict String PreviewRect
    , warnings : List String
    }


{-| POST /preview/items も HTTP 200 の {ok:false, error} があり得る契約なので、
成功/失敗を 1 つのデコーダで受ける(/health と同じ流儀)。
-}
type PreviewResult
    = PreviewOk Preview
    | PreviewErr String


type alias Envelope =
    { id : Int, kind : String, ok : Bool, body : D.Value }



-- 小さな組み立て部品


{-| フィールドが無い / null をどちらも Nothing として受ける -}
opt : String -> D.Decoder a -> D.Decoder (Maybe a)
opt name dec =
    D.oneOf [ D.field name (D.nullable dec), D.succeed Nothing ]


withDefault : a -> D.Decoder a -> D.Decoder a
withDefault fallback dec =
    D.oneOf [ dec, D.succeed fallback ]


{-| error は素の文字列(400 応答)と {message}(プロジェクト未選択応答)の両形がある。 -}
errorTextDecoder : D.Decoder String
errorTextDecoder =
    D.field "error" (D.oneOf [ D.string, D.field "message" D.string ])



-- デコーダ本体


designDecoder : D.Decoder Design
designDecoder =
    D.map2 Design (D.field "w" D.float) (D.field "h" D.float)


healthResultDecoder : D.Decoder HealthResult
healthResultDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map HealthOk
                        (D.map4 Health
                            (D.field "dir" D.string)
                            (D.field "title" D.string)
                            (D.field "design" designDecoder)
                            (D.field "version" D.string)
                        )

                else
                    D.map HealthErr errorTextDecoder
            )


filesDecoder : D.Decoder Files
filesDecoder =
    D.map2 Files
        (D.field "root" D.string)
        (D.map3 (\ui hitbox palette -> dedupe (ui ++ hitbox ++ palette))
            (withDefault [] (D.field "ui" (D.list pathEntryDecoder)))
            (withDefault [] (D.field "hitbox" (D.list D.string)))
            (withDefault [] (D.field "palette" (D.list D.string)))
        )


{-| GET /resources のグループ 1 件。plugin はサーバが素通しした文字列のままで、
どのプラグイン実装に対応するかの解決はエディタ側(Plugins.find)の仕事。
-}
type alias ResourceGroup =
    { id : String

    -- 宣言の照合パターン("assets/*.map.json" 等)。新しいファイルの置き場と
    -- 名前の決まりでもあるので、一覧を出す側だけでなく「+ 新規」も見る
    , pattern : String
    , title : Maybe String
    , plugin : Maybe String
    , files : List ResourceFile
    }


{-| schema はサーバが実在確認済みのパス。Nothing = サーバはスキーマを見つけて
いないので、開く側は隣接規約(*.schema.json)を自分で試す。
-}
type alias ResourceFile =
    { path : String, schema : Maybe String }


{-| ダッシュボード宣言 1 件。plugin と uses は素通し文字列のままで、
実装との突き合わせはエディタ側(Dashboards.find / groups の id 照合)の仕事。
-}
type alias Dashboard =
    { id : String
    , title : Maybe String
    , plugin : String
    , uses : List String
    }


type alias Resources =
    { groups : List ResourceGroup
    , dashboards : List Dashboard

    -- 宣言と実ファイルのずれ(壊れ JSON・schema 不在・一致ゼロ等)の人間可読文
    , warnings : List String
    }


{-| GET /resources — project.json の "editor" 宣言に一致した実ファイル。
root はサーバも返すが /files 側の root と同じ値なので読まない。
dashboards はキーごと無い旧応答も空として読める形にしておく。
-}
resourcesDecoder : D.Decoder Resources
resourcesDecoder =
    D.map3 Resources
        (D.field "resources" (D.list resourceGroupDecoder))
        (withDefault [] (D.field "dashboards" (D.list dashboardDecoder)))
        (withDefault [] (D.field "warnings" (D.list D.string)))


dashboardDecoder : D.Decoder Dashboard
dashboardDecoder =
    D.map4 Dashboard
        (D.field "id" D.string)
        (opt "title" D.string)
        (D.field "plugin" D.string)
        (D.field "uses" (D.list D.string))


resourceGroupDecoder : D.Decoder ResourceGroup
resourceGroupDecoder =
    D.map5 ResourceGroup
        (D.field "id" D.string)
        (withDefault "" (D.field "pattern" D.string))
        (opt "title" D.string)
        (opt "plugin" D.string)
        (D.field "files" (D.list resourceFileDecoder))


resourceFileDecoder : D.Decoder ResourceFile
resourceFileDecoder =
    D.map2 ResourceFile
        (D.field "path" D.string)
        (opt "schema" D.string)


{-| ui はメタ情報付き {path, ...} の列、hitbox/palette は素の文字列の列。
こちらはパスしか使わないので両形を同じ文字列に落とす。
-}
pathEntryDecoder : D.Decoder String
pathEntryDecoder =
    D.oneOf [ D.field "path" D.string, D.string ]


{-| 順序を保ったまま重複を落とす(palette が ui と同じファイルを指すことがある)。 -}
dedupe : List String -> List String
dedupe paths =
    List.foldl
        (\p acc ->
            if List.member p acc then
                acc

            else
                acc ++ [ p ]
        )
        []
        paths


fileContentDecoder : D.Decoder FileContent
fileContentDecoder =
    D.map3 FileContent
        (D.field "path" D.string)
        (D.field "content" D.string)
        (opt "mtime" D.int)


putFileResultDecoder : D.Decoder PutFileResult
putFileResultDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map2 (\mtime baking -> PutOk { mtime = mtime, baking = baking })
                        (opt "mtime" D.int)
                        (withDefault False (D.field "baking" D.bool))

                else
                    D.oneOf
                        [ D.map (\m -> PutConflict { currentMtime = m }) (D.field "currentMtime" D.int)
                        , D.map PutErr (withDefault "不明なエラー" errorTextDecoder)
                        ]
            )


changesDecoder : D.Decoder Changes
changesDecoder =
    D.map2 Changes
        (D.field "token" D.string)
        (D.field "files" (D.dict D.int))


projectsDecoder : D.Decoder Projects
projectsDecoder =
    D.map3 Projects
        (opt "current" D.string)
        (D.field "recent" (D.list projectEntryDecoder))
        (D.field "found" (D.list projectEntryDecoder))


projectEntryDecoder : D.Decoder ProjectEntry
projectEntryDecoder =
    D.map2 ProjectEntry
        (D.field "dir" D.string)
        (D.field "title" D.string)


{-| Tauri の list_running_games (デスクトップだけで中身が入る)。
Rust は生の {pid, cwd} を返すだけなので、こちらは各ゲームの作業ディレクトリ
(= プロジェクト root) の一覧だけ取り出す。cwd 空(取得失敗)は捨てる。
ブラウザや取得失敗時は games が空で、突き合わせが空振り = バッジが出ないだけ。
-}
runningGamesDecoder : D.Decoder (List String)
runningGamesDecoder =
    D.field "games" (D.list (D.field "cwd" D.string))
        |> D.map (List.filter (\cwd -> cwd /= ""))


{-| プロジェクト root(絶対パス)から短い識別子(数字だけ)を作る。
golden/ や atelier/ のファイル名(title.png 等)はプロジェクト間で同名に
なり得るので、絵の URL にこれを混ぜてブラウザキャッシュの混線
(前のプロジェクトの絵が映る)を断つ。サーバは未知のクエリを無視するので
サーバ側の変更は要らない。root をそのまま使わないのは長い・記号が混ざるため
(djb2 の畳み込み。root が違えばまず違う値になれば足りる)。
-}
projectKey : String -> String
projectKey root =
    root
        |> String.foldl (\c acc -> modBy 16777213 (acc * 33 + Char.toCode c)) 5381
        |> String.fromInt


projectSwitchDecoder : D.Decoder ProjectSwitch
projectSwitchDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map3
                        (\dir title design -> SwitchOk { dir = dir, title = title, design = design })
                        (D.field "dir" D.string)
                        (D.field "title" D.string)
                        (D.field "design" designDecoder)

                else
                    D.map SwitchErr errorTextDecoder
            )


{-| POST /sprite/colors の応答。table は legend の「値 → #rrggbb」、unresolved は
サーバが実色に解けなかった legend の値(ゲームが場面ごとに決める色など)。
-}
type alias SpriteColors =
    { table : Dict String String
    , unresolved : Set String
    }


{-| 何も分かっていない状態(応答が届く前・取れなかったとき)。 -}
noSpriteColors : SpriteColors
noSpriteColors =
    { table = Dict.empty, unresolved = Set.empty }


{-| POST /sprite/colors の読み取り。ok:false・欄の欠け・形違いは空 — 画面側が
従来の仮色に倒す(fail-open)。unresolved を返さない旧サーバでも表だけは読める。
-}
spriteColorsDecoder : D.Decoder SpriteColors
spriteColorsDecoder =
    D.map2 SpriteColors
        (D.oneOf [ D.field "colors" (D.dict D.string), D.succeed Dict.empty ])
        (D.oneOf [ D.field "unresolved" (D.list D.string) |> D.map Set.fromList, D.succeed Set.empty ])


previewResultDecoder : D.Decoder PreviewResult
previewResultDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map PreviewOk previewDecoder

                else
                    D.map PreviewErr (withDefault "不明なエラー" errorTextDecoder)
            )


previewDecoder : D.Decoder Preview
previewDecoder =
    D.succeed Preview
        |> andMap (D.field "png" D.string)
        |> andMap (D.field "width" D.int)
        |> andMap (D.field "height" D.int)
        |> andMap (D.field "scale" D.float)
        |> andMap (D.field "design" designDecoder)
        -- /preview/hitbox と /preview/fx は掴める矩形を返さない(眺めるだけの絵)
        -- ので、rects 無しも空として読める形にしておく
        |> andMap (withDefault Dict.empty (D.field "rects" (D.dict previewRectDecoder)))
        |> andMap (withDefault [] (D.field "warnings" (D.list D.string)))


previewRectDecoder : D.Decoder PreviewRect
previewRectDecoder =
    D.list D.float
        |> D.andThen
            (\xs ->
                case xs of
                    [ x, y, w, h ] ->
                        D.succeed { x = x, y = y, w = w, h = h }

                    _ ->
                        D.fail "rect は [x,y,w,h] の 4 要素で書く"
            )


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


envelopeDecoder : D.Decoder Envelope
envelopeDecoder =
    D.map4 Envelope
        (D.field "id" D.int)
        (D.field "kind" D.string)
        (D.field "ok" D.bool)
        (D.field "body" D.value)


{-| GET /search の当たり 1 件。path は文書の中の場所(キーと添字が混ざる)、
value はその文字列値まるごと(置換はこれを元に組む)、excerpt は当たりの前後。
-}
type alias SearchHit =
    { file : String
    , path : List Seg
    , value : String
    , excerpt : String
    }


{-| files はファイル名が一致したパス(中身は見ていない)、hits は中身の当たり。
total / filesTotal は打ち切り前の総数で、truncated が真なら「他 N 件」を出す。
-}
type alias SearchResults =
    { files : List String
    , filesTotal : Int
    , hits : List SearchHit
    , total : Int
    , truncated : Bool
    }


searchResultsDecoder : D.Decoder SearchResults
searchResultsDecoder =
    D.map5 SearchResults
        (withDefault [] (D.field "files" (D.list D.string)))
        (withDefault 0 (D.field "filesTotal" D.int))
        (D.field "results" (D.list searchHitDecoder))
        (withDefault 0 (D.field "total" D.int))
        (withDefault False (D.field "truncated" D.bool))


searchHitDecoder : D.Decoder SearchHit
searchHitDecoder =
    D.map4 SearchHit
        (D.field "file" D.string)
        (D.field "path" (D.list segDecoder))
        (withDefault "" (D.field "value" D.string))
        (withDefault "" (D.field "excerpt" D.string))


segDecoder : D.Decoder Seg
segDecoder =
    D.oneOf [ D.map KeySeg D.string, D.map IdxSeg D.int ]
