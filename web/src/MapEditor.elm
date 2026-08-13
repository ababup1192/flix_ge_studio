module MapEditor exposing
    ( AddKind(..)
    , Addable
    , Doc
    , Edit(..)
    , GroupKind(..)
    , Layer(..)
    , Mark
    , Model
    , Msg(..)
    , Out(..)
    , addColumn
    , addRow
    , defaultChar
    , fromDoc
    , init
    , layerAlpha
    , paletteChips
    , release
    , select
    , selectedRow
    , strokeActive
    , update
    , view
    )

{-| マップ(\*.map.json)のクイック編集。骨格はピクセルエディタと同じ:
左に道具(ペン・消しゴム・バケツ・スポイト・戻す/やり直す)、中央にセルの
グリッド、右にレイヤー+パレット。

エディタ内はベタ塗り — セルは地形 1 文字 = 1 色で塗るだけで、角の丸みなど
本物の見た目は再現しない(本物はミニプレイヤーが映す)。

文書は 2 レイヤーに見る(データ形式は不変 — 表示と編集対象の切り替えだけ):

  - 配置(手前)… map Doc のトップレベルで x,y を持つ物(単体オブジェクトと
    オブジェクト配列)を機械的に見つけた物。ラベルは JSON キーそのまま。
    配列に x,y を持たない行(triggers の on:enter など)が混ざっていたら、
    その行だけセルに置かずパレットの件数に回す(編集は分割モードのフォーム)。
  - 地形 … rows に塗る文字。terrain Doc(entries[].{char,name,fill})が
    読めればそこから、無ければ rows に実際に出る文字+'.' を候補にする
    (fail-open)。色は #rrggbb ならそのまま、それ以外はパレット内の並び順から
    導く仮色('.' だけは無彩色寄りの暗色)。

選択中のレイヤーだけが編集対象で、パレットもそのレイヤーの中身だけを見せる。
非選択レイヤーはキャンバスで淡く、👁 を消すと描かない。👁 と選択は
セッション内だけの状態(Doc には保存しない)。

文書の元データはここに置かない — 描画は毎回親から渡される Doc から導き、
一筆の途中だけ rows の作業コピー(working)が表示に勝つ。確定は 1 操作 =
1 つの Edit として親へ返し、親の編集直列(docEdit → dirty → 保存)に乗せる。

-}

import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import PixelEditor exposing (floodAt, goldenHue, paintAt, pickAt)
import Summary
import Svg
import Svg.Attributes as SA



-- 文書(親が渡す読み取り結果)


type alias Doc =
    { rows : List String
    , groups : List PlaceGroup
    , terrain : List Swatch
    }


{-| 配置 1 キーぶん。単体({x,y} オブジェクト)か配列かで動かし方が変わる。
-}
type alias PlaceGroup =
    { key : String
    , kind : GroupKind
    }


type GroupKind
    = Single ( Int, Int )
    | Many
        { add : AddKind

        -- セルを見ない行(on:enter 等)も雛形から足せるか
        , roomAdd : Bool
        , points : List Mark
        , offRows : List OffRow
        }


{-| セルに置かれていない行(x,y を持たない行)。グリッドには描けないので、
パレットのチップから一覧で開いて選ぶ。summary は中身の見当をつけるための
1 行の見出し(スキーマは知らないので、値そのものから作る)。
-}
type alias OffRow =
    { index : Int, summary : String }


{-| 親が渡す「雛形から足せる配列」の宣言。room = セルを見ない行(x,y を書かない
行)も作れるか。スキーマを読むのは親の仕事なので、ここは結果だけ受ける。
-}
type alias Addable =
    { key : String, room : Bool }


{-| 空きセルのクリックで行を足せるか。

  - XyOnly … 要素が x,y だけ(herbs/lamps 型)。{x,y} の行をその場で足す(従来)。
  - FromSchema … x,y 以外のフィールドも要る配列(triggers/props 型)だが、
    スキーマに宣言があるので雛形(default で埋めた行)の x,y をクリック先にして足せる。
  - NoAdd … スキーマにも無い配列。足すと欠けた行ができるので、移動・削除のみ。

-}
type AddKind
    = XyOnly
    | FromSchema
    | NoAdd


{-| 配列のうち「セルに置かれている 1 要素」。index は元の配列の添字
(x,y を持たない行を飛ばしても書き戻し先がずれないように、詰めた順ではなく
ドキュメントの添字をそのまま覚える)。
-}
type alias Mark =
    { index : Int, at : ( Int, Int ) }


type alias Swatch =
    { ch : Char
    , name : String
    , css : String
    }


{-| rows の既定の文字(消しゴム・広げた新セルの埋め草)。
-}
defaultChar : Char
defaultChar =
    '.'


{-| パース済み map Doc の読み取り。rows(文字列配列・空でない)が無い文書は
Nothing — 呼び側は従来のフォーム/コード表示へ倒す(壊さない)。
terrainDoc は読めた terrain Doc(無ければ Nothing で rows から導く)。
addables は「雛形から 1 行足せる配列」(スキーマに x,y つき list セクションの
宣言がある物)。ここはスキーマを知らないので、結果だけ親から受ける。
-}
fromDoc : List Addable -> Maybe D.Value -> D.Value -> Maybe Doc
fromDoc schemaKeys terrainDoc value =
    case D.decodeValue (D.field "rows" (D.list D.string)) value of
        Ok rows ->
            if List.isEmpty rows then
                Nothing

            else
                Just
                    { rows = rows
                    , groups = placeGroups schemaKeys value
                    , terrain = terrainSwatches terrainDoc rows
                    }

        Err _ ->
            Nothing


{-| トップレベルから x,y 持ちを機械的に拾う。順序は JSON のキー順のまま。
-}
placeGroups : List Addable -> D.Value -> List PlaceGroup
placeGroups schemaKeys value =
    D.decodeValue (D.keyValuePairs D.value) value
        |> Result.withDefault []
        |> List.filterMap
            (\( key, v ) ->
                case D.decodeValue pointDecoder v of
                    Ok point ->
                        Just { key = key, kind = Single point }

                    Err _ ->
                        D.decodeValue (D.list D.value) v
                            |> Result.toMaybe
                            |> Maybe.andThen (manyKind schemaKeys key)
            )


{-| オブジェクトの配列なら配置グループ。x,y を持つ要素だけをセルに置き、
持たない行(triggers の on:enter のような、セルを見ない行)はグリッドに描かず
一覧(offRows)に回す — 混ざっているからとグループごと落とすと、
同じキーの他の印まで見えなくなる。
空配列と、文字列など非オブジェクトの配列(rows 等)は配置ではない(fail-open)。
-}
manyKind : List Addable -> String -> List D.Value -> Maybe PlaceGroup
manyKind addables key items =
    let
        rows =
            items
                |> List.indexedMap
                    (\index v ->
                        case D.decodeValue pointDecoder v of
                            Ok at ->
                                Ok { index = index, at = at }

                            Err _ ->
                                Err { index = index, summary = Summary.line v }
                    )
    in
    if List.isEmpty items || not (List.all isObjectValue items) then
        Nothing

    else
        Just
            { key = key
            , kind =
                Many
                    { add = addKind addables key items
                    , roomAdd =
                        addables |> List.any (\a -> a.key == key && a.room)
                    , points = rows |> List.filterMap Result.toMaybe
                    , offRows = rows |> List.filterMap errOf
                    }
            }


errOf : Result e a -> Maybe e
errOf result =
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing


addKind : List Addable -> String -> List D.Value -> AddKind
addKind addables key items =
    if List.all xyOnlyValue items then
        XyOnly

    else if List.any (\a -> a.key == key) addables then
        FromSchema

    else
        NoAdd


pointDecoder : D.Decoder ( Int, Int )
pointDecoder =
    D.map2 Tuple.pair (D.field "x" D.int) (D.field "y" D.int)


isObjectValue : D.Value -> Bool
isObjectValue v =
    D.decodeValue (D.keyValuePairs D.value) v |> Result.toMaybe |> (/=) Nothing


xyOnlyValue : D.Value -> Bool
xyOnlyValue v =
    D.decodeValue (D.keyValuePairs D.value) v
        |> Result.toMaybe
        |> Maybe.map (List.all (\( k, _ ) -> k == "x" || k == "y"))
        |> Maybe.withDefault False



-- 地形パレットの導出


{-| terrain Doc(entries[].{char,name,fill})が読めればそこから。
無い・壊れている・entries が空なら rows に実際に出る文字+'.'(fail-open)。
名詞は発明しない — Doc の name か文字そのものだけ。
-}
terrainSwatches : Maybe D.Value -> List String -> List Swatch
terrainSwatches terrainDoc rows =
    case terrainDoc |> Maybe.andThen docSwatches of
        Just swatches ->
            swatches

        Nothing ->
            usedChars rows
                |> List.indexedMap
                    (\index ch -> { ch = ch, name = String.fromChar ch, css = provisionalCss index ch })


docSwatches : D.Value -> Maybe (List Swatch)
docSwatches value =
    D.decodeValue (D.field "entries" (D.list entryDecoder)) value
        |> Result.toMaybe
        |> Maybe.map (List.filterMap identity)
        |> Maybe.andThen
            (\entries ->
                if List.isEmpty entries then
                    Nothing

                else
                    Just (List.indexedMap entrySwatch entries)
            )


{-| entry 1 件の生読み。char が 1 文字でない行は黙って除く。
-}
entryDecoder : D.Decoder (Maybe { ch : Char, name : String, fill : Maybe String })
entryDecoder =
    D.map3
        (\charText name fill ->
            case String.toList charText of
                [ ch ] ->
                    Just
                        { ch = ch
                        , name = Maybe.withDefault (String.fromChar ch) name
                        , fill = fill
                        }

                _ ->
                    Nothing
        )
        (D.field "char" D.string)
        (D.maybe (D.field "name" D.string))
        (D.maybe (D.field "fill" D.string))


{-| fill が #rrggbb ならその色、意味色キー(@…)等はここでは実色に解けない
ので並び順から仮色(表示のためだけ)。
-}
entrySwatch : Int -> { ch : Char, name : String, fill : Maybe String } -> Swatch
entrySwatch index entry =
    { ch = entry.ch
    , name = entry.name
    , css =
        case entry.fill of
            Just f ->
                if isHexColor f then
                    f

                else
                    provisionalCss index entry.ch

            Nothing ->
                provisionalCss index entry.ch
    }


{-| rows に出る文字を出現順に(重複なし)。'.' は消しゴムの行き先なので必ず入れる。
-}
usedChars : List String -> List Char
usedChars rows =
    let
        distinct =
            rows
                |> List.concatMap String.toList
                |> List.foldl
                    (\c acc ->
                        if List.member c acc then
                            acc

                        else
                            acc ++ [ c ]
                    )
                    []
    in
    if List.member defaultChar distinct then
        distinct

    else
        distinct ++ [ defaultChar ]


{-| パレット内の並び順から導く仮色。色相を黄金角(約137.5°)ずつ離すので、
少数の候補どうしが必ず大きく離れる(文字ハッシュ由来の偶然の近さがない)。
'.'(既定の文字=消しゴムが戻す先)だけは無彩色寄りの暗色に固定 — 意味の
発明ではなく、既定文字の表示上の扱い。
-}
provisionalCss : Int -> Char -> String
provisionalCss index ch =
    if ch == defaultChar then
        "hsl(0 0% 30%)"

    else
        "hsl(" ++ String.fromInt (goldenHue index) ++ " 45% 45%)"


isHexColor : String -> Bool
isHexColor value =
    case String.uncons value of
        Just ( '#', rest ) ->
            (String.length rest == 3 || String.length rest == 6)
                && List.all Char.isHexDigit (String.toList rest)

        _ ->
            False


hashHue : String -> Int
hashHue name =
    name
        |> String.toList
        |> List.foldl (\c acc -> modBy 360 (acc * 31 + Char.toCode c)) 7



-- 状態


type Tool
    = Pen
    | Eraser
    | Bucket
    | Dropper


{-| 編集対象のレイヤー。配置(手前)か地形か。表示と編集対象の切り替えだけで、
文書のデータ形式は変わらない。
-}
type Layer
    = PlaceLayer
    | TerrainLayer


{-| いま塗る/置く物。地形(rows の文字)か配置(グループのキー)か。
選択中のレイヤーと、そのレイヤーが覚えているチップから解ける。
-}
type Brush
    = Terrain Char
    | Place String


type Axis
    = Cols
    | Rows


{-| 確定待ちの確認。消す物が空でない時だけここを経由する。
-}
type Confirm
    = RemovePointConfirm { key : String, index : Int }
    | ShrinkConfirm Axis


{-| 戻す/やり直すの 1 操作。rows はスナップショット(ピクセルエディタと同じ)、
配置は逆操作を覚える。追加の取り消し・削除のやり直しは「そのグループの末尾」を
消す(追加は常に末尾へ入るため。適用時に長さから添字を解く)。
-}
type Step
    = RowsStep { before : List String, after : List String }
    | MoveStep { key : String, index : Maybe Int, from : ( Int, Int ), to : ( Int, Int ) }
    | AddStep { key : String, point : ( Int, Int ), fromSchema : Bool }
      -- point = 消した行の印(セルを見ない行は Nothing。戻す先が無い)
    | RemoveStep { key : String, point : Maybe ( Int, Int ), index : Int }


type alias Model =
    { tool : Tool

    -- いま編集中のレイヤー(パレットの中身・淡さ・バケツの可否を決める)
    , layer : Layer

    -- キャンバスに描くか(👁)。セッション内だけ・Doc には保存しない
    , shown : { terrain : Bool, place : Bool }

    -- 各レイヤーが覚えているチップ。Nothing は「まだ選んでいない」=
    -- そのレイヤーの先頭(地形の先頭スワッチ・配置の先頭グループ)
    , terrainCh : Maybe Char
    , placeKey : Maybe String

    -- 選択中の 1 行(次のクリックで動かす対象・インスペクタが映す行)。
    -- 添字はドキュメント配列の添字(Mark.index)で、単体グループは Nothing
    , picked : Maybe ( String, Maybe Int )

    -- 「セルを見ない行」の一覧を開いているグループ(パレットのチップ 1 つぶん)
    , expanded : Maybe String
    , cellPx : Int
    , hover : Maybe ( Int, Int )

    -- 一筆の途中(Just = 塗っている)。before は戻す用の一筆前
    , stroke : Maybe { ch : Char, before : List String }

    -- 表示が文書に勝つ rows の作業コピー(一筆の途中と、確定後のエコー待ちの間)
    , working : Maybe (List String)
    , confirm : Maybe Confirm
    , undo : List Step
    , redo : List Step
    }


init : Model
init =
    { tool = Pen
    , layer = TerrainLayer
    , shown = { terrain = True, place = True }
    , terrainCh = Nothing
    , placeKey = Nothing
    , picked = Nothing
    , expanded = Nothing
    , cellPx = baseCellPx
    , hover = Nothing
    , stroke = Nothing
    , working = Nothing
    , confirm = Nothing
    , undo = []
    , redo = []
    }


{-| 一筆の途中か(親がグローバル mouseup を購読する間だけ True)。
-}
strokeActive : Model -> Bool
strokeActive model =
    model.stroke /= Nothing


{-| 一筆・作業コピー・選択を手放して文書の値へ戻る(モード切替等、テキスト側で
文書が動き得る時に親が呼ぶ)。道具・ズーム・履歴は保つ。
-}
release : Model -> Model
release model =
    { model | stroke = Nothing, working = Nothing, hover = Nothing, picked = Nothing, confirm = Nothing }



-- 更新


type Msg
    = ToolChosen Tool
    | LayerChosen Layer
    | LayerToggled Layer
    | TerrainChosen Char
    | PlaceChosen String
    | ZoomStepped Int
    | CellPressed Int Int Int
    | CellEntered Int Int
    | GridLeft
    | StrokeEnded
    | GrowPressed Axis
    | ShrinkPressed Axis
    | ConfirmAccepted
    | ConfirmDismissed
    | UndoPressed
    | RedoPressed
      -- パレットのチップ「(+部屋 N)」の開閉
    | GroupToggled String
      -- セルを見ない行を一覧から選ぶ(インスペクタが映す)
    | OffRowChosen String Int
      -- セルを見ない行(on:enter 等)を雛形から 1 行足す
    | RoomRowPressed String
      -- 選択中の行を消す(インスペクタの削除。確認へ進む)
    | RemovePressed
      -- contextmenu を抑えるためだけの空打ち(右クリック=消しゴムを活かす)
    | Swallowed


{-| 編集 1 件。親が docEdit の payload へ翻訳する:
rows は配列丸ごと 1 本、配置は該当 path(villagers.0.x 等)。
index はドキュメント配列の添字そのまま(印だけ詰めた順ではない)。
-}
type Edit
    = RowsEdited (List String)
    | PointMoved { key : String, index : Maybe Int, x : Int, y : Int }
      -- fromSchema = スキーマの雛形(default 済みの全フィールド)から作る行か。
      -- False は従来どおり {x,y} だけの行
    | PointAdded { key : String, x : Int, y : Int, fromSchema : Bool }
      -- セルを見ない行(x,y を書かない雛形)を 1 行。hadRoom = 既に同じ配列に
      -- そういう行があったか(親が注意書きを出すため)
    | RoomRowAdded { key : String, hadRoom : Bool }
    | PointRemoved { key : String, index : Int }


type Out
    = Silent
    | Edited Edit
    | Noticed String


update : Doc -> Msg -> Model -> ( Model, Out )
update doc msg model =
    case msg of
        Swallowed ->
            ( model, Silent )

        ToolChosen tool ->
            -- 配置レイヤーの間はバケツ無効(ボタンも押せないが、ショートカットもここで塞ぐ)
            if tool == Bucket && model.layer == PlaceLayer then
                ( model, Silent )

            else
                ( { model | tool = tool }, Silent )

        LayerChosen layer ->
            -- レイヤーを切り替えると編集対象が変わる。移動途中の選択は持ち越さない。
            -- 配置レイヤーへ移った時、バケツのままなら押せない道具なのでペンへ倒す。
            ( { model
                | layer = layer
                , picked = Nothing
                , tool =
                    if layer == PlaceLayer && model.tool == Bucket then
                        Pen

                    else
                        model.tool
              }
            , Silent
            )

        LayerToggled layer ->
            ( { model | shown = toggleShown layer model.shown }, Silent )

        TerrainChosen ch ->
            -- 消しゴム・スポイト中の色選びは「その色で塗りたい」なのでペンへ
            ( { model
                | terrainCh = Just ch
                , layer = TerrainLayer
                , picked = Nothing
                , tool =
                    if model.tool == Eraser || model.tool == Dropper then
                        Pen

                    else
                        model.tool
              }
            , Silent
            )

        PlaceChosen key ->
            -- 配置に効く道具はペンだけ(バケツ等は残しても迷うだけ)
            ( { model | placeKey = Just key, layer = PlaceLayer, picked = Nothing, tool = Pen }, Silent )

        GroupToggled key ->
            ( { model
                | expanded =
                    if model.expanded == Just key then
                        Nothing

                    else
                        Just key
              }
            , Silent
            )

        OffRowChosen key index ->
            -- セルを見ない行は動かせない(印が無い)。選ぶ=インスペクタで開く
            ( { model | placeKey = Just key, layer = PlaceLayer, picked = Just ( key, Just index ) }, Silent )

        RoomRowPressed key ->
            case groupKind doc key of
                Just (Many many) ->
                    if many.roomAdd then
                        ( { model | placeKey = Just key, layer = PlaceLayer, expanded = Just key }
                        , Edited (RoomRowAdded { key = key, hadRoom = not (List.isEmpty many.offRows) })
                        )

                    else
                        ( model, Silent )

                _ ->
                    ( model, Silent )

        RemovePressed ->
            case model.picked of
                Just ( key, Just index ) ->
                    ( { model | confirm = Just (RemovePointConfirm { key = key, index = index }) }, Silent )

                _ ->
                    ( model, Noticed "この行は消せません(動かすだけにしてあります)" )

        ZoomStepped dir ->
            ( { model | cellPx = zoomStep dir model.cellPx }, Silent )

        CellEntered x y ->
            let
                m1 =
                    { model | hover = Just ( x, y ) }
            in
            case ( m1.stroke, m1.working ) of
                ( Just stroke, Just rows ) ->
                    ( { m1 | working = Just (paintAt stroke.ch ( x, y ) rows) }, Silent )

                _ ->
                    ( m1, Silent )

        GridLeft ->
            ( { model | hover = Nothing }, Silent )

        CellPressed x y buttonId ->
            press doc ( x, y ) buttonId model

        StrokeEnded ->
            endStroke model

        GrowPressed axis ->
            commitRows (grow axis (shownRows doc model)) doc model

        ShrinkPressed axis ->
            shrinkPress axis doc model

        ConfirmAccepted ->
            acceptConfirm doc model

        ConfirmDismissed ->
            ( { model | confirm = Nothing }, Silent )

        UndoPressed ->
            case model.undo of
                [] ->
                    ( model, Silent )

                step :: rest ->
                    applyStep doc True step { model | undo = rest, redo = step :: model.redo }

        RedoPressed ->
            case model.redo of
                [] ->
                    ( model, Silent )

                step :: rest ->
                    applyStep doc False step { model | redo = rest, undo = step :: model.undo }


{-| 押した瞬間の分岐。右ボタンは道具に依らず消しゴム(定番ツールの作法)。
-}
press : Doc -> ( Int, Int ) -> Int -> Model -> ( Model, Out )
press doc cell buttonId model =
    if buttonId == 2 || (buttonId == 0 && model.tool == Eraser) then
        erasePress doc cell model

    else if buttonId /= 0 then
        ( model, Silent )

    else
        case model.tool of
            Eraser ->
                -- 上で処理済み(ここには来ない)
                ( model, Silent )

            Pen ->
                case activeBrush doc model of
                    Terrain ch ->
                        ( startStroke ch cell doc model, Silent )

                    Place key ->
                        placePress doc key cell model

            Bucket ->
                case activeBrush doc model of
                    Terrain ch ->
                        commitRows (floodAt ch cell (shownRows doc model)) doc model

                    Place _ ->
                        ( model, Silent )

            Dropper ->
                case pickAt cell (shownRows doc model) of
                    Just ch ->
                        ( { model | terrainCh = Just ch, tool = Pen, picked = Nothing }, Silent )

                    Nothing ->
                        ( model, Silent )


{-| 消しゴム。配置の印の上ならその要素の削除(確認つき。配列要素だけ —
単体オブジェクトは消すと文書が欠けるので動かすだけ)。地形セルなら '.' へ戻す一筆。
-}
erasePress : Doc -> ( Int, Int ) -> Model -> ( Model, Out )
erasePress doc cell model =
    case pointAt doc cell of
        Just ( key, Just index ) ->
            ( { model | confirm = Just (RemovePointConfirm { key = key, index = index }) }, Silent )

        Just ( _, Nothing ) ->
            ( model, Noticed "この印は消せません(動かすだけにしてあります)" )

        Nothing ->
            ( startStroke defaultChar cell doc model, Silent )


{-| 配置ブラシでのクリック。単体はクリック先へ即移動。配列は
印をクリック=選ぶ → 別セルをクリック=移動。足せる配列(AddKind)は
何も選んでいない空セルへのクリック=追加。
-}
placePress : Doc -> String -> ( Int, Int ) -> Model -> ( Model, Out )
placePress doc key cell model =
    case groupKind doc key of
        Nothing ->
            ( model, Silent )

        Just (Single point) ->
            -- 単体は動かすだけ。同じセルをもう一度なら「選ぶ」だけになる
            -- (インスペクタで中身を書くための選択)
            if point == cell then
                ( { model | picked = Just ( key, Nothing ) }, Silent )

            else
                commitMove { key = key, index = Nothing, from = point, to = cell }
                    { model | picked = Just ( key, Nothing ) }

        Just (Many many) ->
            let
                hitIndex =
                    many.points
                        |> List.filter (\mark -> mark.at == cell)
                        |> List.head
                        |> Maybe.map .index
            in
            case ( model.picked, hitIndex ) of
                ( _, Just index ) ->
                    -- 印の上: 選ぶ(同じ印をもう一度なら選択解除)
                    ( { model
                        | picked =
                            if model.picked == Just ( key, Just index ) then
                                Nothing

                            else
                                Just ( key, Just index )
                      }
                    , Silent
                    )

                ( Just ( pickedKey, Just index ), Nothing ) ->
                    if pickedKey == key then
                        case many.points |> List.filter (\mark -> mark.index == index) |> List.head of
                            Just mark ->
                                commitMove { key = key, index = Just index, from = mark.at, to = cell } model

                            Nothing ->
                                -- 印を持たない行(セルを見ない行)を選んでいた: 解除だけ
                                ( { model | picked = Nothing }, Silent )

                    else
                        ( { model | picked = Nothing }, Silent )

                ( Just ( _, Nothing ), Nothing ) ->
                    ( { model | picked = Nothing }, Silent )

                ( Nothing, Nothing ) ->
                    case many.add of
                        XyOnly ->
                            -- 続けて何個も置けるよう、足した行は選ばない(従来どおり)
                            commitAdd { key = key, point = cell, fromSchema = False, select = Nothing } model

                        FromSchema ->
                            -- 雛形から生まれた行は中身をフォームで書くので、
                            -- どれが生まれたか分かるよう選んでおく(添字は配列末尾)
                            commitAdd
                                { key = key
                                , point = cell
                                , fromSchema = True
                                , select = Just (List.length many.points + List.length many.offRows)
                                }
                                model

                        NoAdd ->
                            ( model, Noticed "動かしたい印をクリックで選んでください" )


commitMove : { key : String, index : Maybe Int, from : ( Int, Int ), to : ( Int, Int ) } -> Model -> ( Model, Out )
commitMove move model =
    ( { model
        | undo = pushHistory (MoveStep move) model.undo
        , redo = []
      }
    , Edited (PointMoved { key = move.key, index = move.index, x = Tuple.first move.to, y = Tuple.second move.to })
    )


commitAdd : { key : String, point : ( Int, Int ), fromSchema : Bool, select : Maybe Int } -> Model -> ( Model, Out )
commitAdd add model =
    let
        ( x, y ) =
            add.point
    in
    ( { model
        | undo = pushHistory (AddStep { key = add.key, point = add.point, fromSchema = add.fromSchema }) model.undo
        , redo = []
        , picked = add.select |> Maybe.map (\i -> ( add.key, Just i ))
      }
    , Edited (PointAdded { key = add.key, x = x, y = y, fromSchema = add.fromSchema })
    )


{-| 配列 1 行の削除。印のある行も、セルを見ない行(インスペクタからの削除)も
同じ道を通る。文書に無い添字なら何もしない。
-}
commitRemove : Doc -> { key : String, index : Int } -> Model -> ( Model, Out )
commitRemove doc target model =
    let
        point =
            groupMarks doc target.key
                |> List.filter (\mark -> mark.index == target.index)
                |> List.head
                |> Maybe.map .at

        isRoomRow =
            groupOffRows doc target.key |> List.any (\row -> row.index == target.index)
    in
    if point == Nothing && not isRoomRow then
        ( model, Silent )

    else
        ( { model
            | undo = pushHistory (RemoveStep { key = target.key, point = point, index = target.index }) model.undo
            , redo = []

            -- 添字がずれるので選択は持ち越さない
            , picked = Nothing
          }
        , Edited (PointRemoved { key = target.key, index = target.index })
        )


startStroke : Char -> ( Int, Int ) -> Doc -> Model -> Model
startStroke ch cell doc model =
    let
        rows =
            shownRows doc model
    in
    { model
        | stroke = Just { ch = ch, before = rows }
        , working = Just (paintAt ch cell rows)
    }


endStroke : Model -> ( Model, Out )
endStroke model =
    case ( model.stroke, model.working ) of
        ( Just stroke, Just rows ) ->
            if rows == stroke.before then
                ( { model | stroke = Nothing }, Silent )

            else
                ( { model
                    | stroke = Nothing
                    , undo = pushHistory (RowsStep { before = stroke.before, after = rows }) model.undo
                    , redo = []
                  }
                , Edited (RowsEdited rows)
                )

        _ ->
            ( { model | stroke = Nothing }, Silent )


{-| 一筆を介さない rows の即時確定(バケツ・広げる/狭める)。
-}
commitRows : List String -> Doc -> Model -> ( Model, Out )
commitRows rows doc model =
    let
        before =
            shownRows doc model
    in
    if rows == before then
        ( model, Silent )

    else
        ( { model
            | working = Just rows
            , undo = pushHistory (RowsStep { before = before, after = rows }) model.undo
            , redo = []
          }
        , Edited (RowsEdited rows)
        )


{-| 狭める。消える端に既定の文字以外・配置の印があれば確認を挟む。
-}
shrinkPress : Axis -> Doc -> Model -> ( Model, Out )
shrinkPress axis doc model =
    let
        rows =
            shownRows doc model
    in
    if not (shrinkable axis rows) then
        ( model, Silent )

    else if shrinkTouches axis rows doc.groups then
        ( { model | confirm = Just (ShrinkConfirm axis) }, Silent )

    else
        commitRows (shrink axis rows) doc model


acceptConfirm : Doc -> Model -> ( Model, Out )
acceptConfirm doc model =
    case model.confirm of
        Just (RemovePointConfirm target) ->
            commitRemove doc target { model | confirm = Nothing }

        Just (ShrinkConfirm axis) ->
            commitRows (shrink axis (shownRows doc model)) doc { model | confirm = Nothing }

        Nothing ->
            ( model, Silent )


{-| 戻す/やり直すの適用。配置の追加/削除の巻き戻しは末尾基準
(追加は常に末尾へ入るため)。対象が既に無ければ何もしない。
-}
applyStep : Doc -> Bool -> Step -> Model -> ( Model, Out )
applyStep doc isUndo step model =
    let
        m1 =
            { model | stroke = Nothing, picked = Nothing }
    in
    case step of
        RowsStep snap ->
            let
                rows =
                    if isUndo then
                        snap.before

                    else
                        snap.after
            in
            ( { m1 | working = Just rows }, Edited (RowsEdited rows) )

        MoveStep move ->
            let
                ( x, y ) =
                    if isUndo then
                        move.from

                    else
                        move.to
            in
            ( m1, Edited (PointMoved { key = move.key, index = move.index, x = x, y = y }) )

        AddStep add ->
            if isUndo then
                removeLast doc add.key m1

            else
                ( m1
                , Edited
                    (PointAdded
                        { key = add.key
                        , x = Tuple.first add.point
                        , y = Tuple.second add.point
                        , fromSchema = add.fromSchema
                        }
                    )
                )

        RemoveStep removed ->
            if isUndo then
                -- 消した行の中身までは覚えていないので、戻るのは印(x,y)だけ。
                -- 雛形から生まれる配列なら、欠けた行にならないよう雛形で戻す
                case removed.point of
                    Just ( x, y ) ->
                        ( m1
                        , Edited
                            (PointAdded
                                { key = removed.key
                                , x = x
                                , y = y
                                , fromSchema = addKindOf doc removed.key == FromSchema
                                }
                            )
                        )

                    Nothing ->
                        -- セルを見ない行は印が無く、戻す手がかりを持っていない
                        ( m1, Noticed "セルを見ない行は戻せません(中身を覚えていません)" )

            else
                removeLast doc removed.key m1


{-| 追加は配列末尾へ入るので、その取り消しで消すのは印の中で添字が一番後ろの物
(x,y を持たない行には手を出さない)。
-}
removeLast : Doc -> String -> Model -> ( Model, Out )
removeLast doc key model =
    case groupMarks doc key |> List.map .index |> List.maximum of
        Just index ->
            ( model, Edited (PointRemoved { key = key, index = index }) )

        Nothing ->
            ( model, Silent )


pushHistory : Step -> List Step -> List Step
pushHistory step history =
    -- 覚えるのは直近 50 操作まで(スナップショットを無限に抱えない)
    step :: List.take 49 history



-- 純ロジック(広げる・狭める)


{-| 右端に 1 列足す。全行を「一番長い行+1」まで既定の文字で埋める —
不揃いの行もここで揃う。
-}
addColumn : List String -> List String
addColumn rows =
    let
        w =
            maxWidth rows
    in
    List.map (String.padRight (w + 1) defaultChar) rows


{-| 下端に 1 行足す(一番長い行と同じ幅、既定の文字で)。
-}
addRow : List String -> List String
addRow rows =
    rows ++ [ String.repeat (maxWidth rows) (String.fromChar defaultChar) ]


grow : Axis -> List String -> List String
grow axis =
    case axis of
        Cols ->
            addColumn

        Rows ->
            addRow


shrink : Axis -> List String -> List String
shrink axis rows =
    case axis of
        Cols ->
            List.map (String.left (maxWidth rows - 1)) rows

        Rows ->
            List.take (List.length rows - 1) rows


shrinkable : Axis -> List String -> Bool
shrinkable axis rows =
    case axis of
        Cols ->
            maxWidth rows > 1

        Rows ->
            List.length rows > 1


{-| 狭めると消える端に、既定の文字以外か配置の印があるか(=確認が要るか)。
-}
shrinkTouches : Axis -> List String -> List PlaceGroup -> Bool
shrinkTouches axis rows groups =
    let
        ( edge, cells ) =
            case axis of
                Cols ->
                    ( \( x, _ ) -> x >= maxWidth rows - 1
                    , rows |> List.concatMap (String.toList >> List.drop (maxWidth rows - 1))
                    )

                Rows ->
                    ( \( _, y ) -> y >= List.length rows - 1
                    , rows |> List.drop (List.length rows - 1) |> List.concatMap String.toList
                    )
    in
    List.any (\c -> c /= defaultChar) cells
        || List.any edge (List.concatMap allPoints groups)


maxWidth : List String -> Int
maxWidth rows =
    rows |> List.map String.length |> List.maximum |> Maybe.withDefault 0



-- 読み(選択の解決)


shownRows : Doc -> Model -> List String
shownRows doc model =
    Maybe.withDefault doc.rows model.working


{-| 選択中レイヤーで塗る/置く物。覚えているチップが文書から消えていたら
そのレイヤーの先頭へ倒す(fail-open)。配置に候補が無ければ地形の先頭へ。
-}
activeBrush : Doc -> Model -> Brush
activeBrush doc model =
    case model.layer of
        TerrainLayer ->
            case model.terrainCh |> Maybe.andThen (\ch -> ifTrue (List.any (\sw -> sw.ch == ch) doc.terrain) ch) of
                Just ch ->
                    Terrain ch

                Nothing ->
                    firstTerrain doc

        PlaceLayer ->
            case model.placeKey |> Maybe.andThen (\key -> ifTrue (List.any (\g -> g.key == key) doc.groups) key) of
                Just key ->
                    Place key

                Nothing ->
                    doc.groups
                        |> List.head
                        |> Maybe.map (.key >> Place)
                        |> Maybe.withDefault (firstTerrain doc)


firstTerrain : Doc -> Brush
firstTerrain doc =
    doc.terrain
        |> List.head
        |> Maybe.map (.ch >> Terrain)
        |> Maybe.withDefault (Terrain defaultChar)


{-| 👁 の反転。レイヤー 1 枚ぶん。
-}
toggleShown : Layer -> { terrain : Bool, place : Bool } -> { terrain : Bool, place : Bool }
toggleShown layer shown =
    case layer of
        TerrainLayer ->
            { shown | terrain = not shown.terrain }

        PlaceLayer ->
            { shown | place = not shown.place }


{-| レイヤーの淡さ。0 = 隠す(👁 オフ)、1 = 選択中、0.45 = 非選択(淡く)。
-}
layerAlpha : Layer -> Model -> Float
layerAlpha layer model =
    if not (isShown layer model) then
        0

    else if model.layer == layer then
        1

    else
        0.45


isShown : Layer -> Model -> Bool
isShown layer model =
    case layer of
        TerrainLayer ->
            model.shown.terrain

        PlaceLayer ->
            model.shown.place


{-| パレットに並ぶチップの見出し(そのレイヤーの中身だけ)。地形は 1 文字、
配置は JSON キー。パレットの表示と、レイヤー↔中身の対応の検査に使う。
-}
paletteChips : Layer -> Doc -> List String
paletteChips layer doc =
    case layer of
        TerrainLayer ->
            doc.terrain |> List.map (.ch >> String.fromChar)

        PlaceLayer ->
            doc.groups |> List.map .key


groupKind : Doc -> String -> Maybe GroupKind
groupKind doc key =
    doc.groups
        |> List.filter (\g -> g.key == key)
        |> List.head
        |> Maybe.map .kind


addKindOf : Doc -> String -> AddKind
addKindOf doc key =
    case groupKind doc key of
        Just (Many many) ->
            many.add

        _ ->
            NoAdd


{-| そのキーの、セルに置かれている要素(元の添字つき)。単体は添字を持たないので空。
-}
groupMarks : Doc -> String -> List Mark
groupMarks doc key =
    case groupKind doc key of
        Just (Many many) ->
            many.points

        _ ->
            []


groupOffRows : Doc -> String -> List OffRow
groupOffRows doc key =
    case groupKind doc key of
        Just (Many many) ->
            many.offRows

        _ ->
            []


{-| いま選んでいる 1 行(キーと、配列なら添字)。親のインスペクタが映す行 —
どの行を書いているかの元データはここ 1 つに置く。
-}
selectedRow : Model -> Maybe ( String, Maybe Int )
selectedRow model =
    model.picked


{-| 親から選択を張り直す(足した行をそのままインスペクタで開くため)。
-}
select : ( String, Maybe Int ) -> Model -> Model
select picked model =
    { model | picked = Just picked }


kindPoints : GroupKind -> List ( Int, Int )
kindPoints kind =
    case kind of
        Single point ->
            [ point ]

        Many many ->
            List.map .at many.points


allPoints : PlaceGroup -> List ( Int, Int )
allPoints group =
    kindPoints group.kind


{-| セルの上の配置。配列要素(Just index)を優先し、次に単体(Nothing)。
消しゴムの当たり判定に使う。
-}
pointAt : Doc -> ( Int, Int ) -> Maybe ( String, Maybe Int )
pointAt doc cell =
    let
        manyHits =
            doc.groups
                |> List.concatMap
                    (\g ->
                        case g.kind of
                            Many many ->
                                many.points
                                    |> List.filter (\mark -> mark.at == cell)
                                    |> List.map (\mark -> ( g.key, Just mark.index ))

                            Single _ ->
                                []
                    )

        singleHits =
            doc.groups
                |> List.concatMap
                    (\g ->
                        case g.kind of
                            Single point ->
                                if point == cell then
                                    [ ( g.key, Nothing ) ]

                                else
                                    []

                            Many _ ->
                                []
                    )
    in
    List.head (manyHits ++ singleHits)


ifTrue : Bool -> a -> Maybe a
ifTrue cond value =
    if cond then
        Just value

    else
        Nothing


{-| ズーム 100% のセル辺(px)。
-}
baseCellPx : Int
baseCellPx =
    22


zoomLevels : List Int
zoomLevels =
    [ 14, 18, baseCellPx, 28, 34 ]


zoomStep : Int -> Int -> Int
zoomStep dir current =
    let
        index =
            zoomLevels
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, v ) -> v == current)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 2
    in
    zoomLevels
        |> List.drop (clamp 0 (List.length zoomLevels - 1) (index + dir))
        |> List.head
        |> Maybe.withDefault current



-- 表示


{-| toSelf はここの Msg を親の Msg へ包む物、inspector は「選んだ 1 行のフォーム」
(スキーマを読む親が描く。ここは中身を知らず、右ペインの置き場所だけ用意する)。
-}
view : (Msg -> msg) -> List (Html msg) -> Doc -> Model -> Html msg
view toSelf inspector doc model =
    div [ HA.class "map-editor relative flex min-w-0 flex-1 flex-col bg-app" ]
        [ Html.map toSelf (viewTopStatus doc model)
        , div [ HA.class "relative flex min-h-0 min-w-0 flex-1" ]
            ([ Html.map toSelf (viewTools doc model)
             , Html.map toSelf (viewCenter doc model)
             , viewSide toSelf inspector doc model
             ]
                ++ (case model.confirm of
                        Just confirm ->
                            [ Html.map toSelf (viewConfirm confirm) ]

                        Nothing ->
                            []
                   )
            )
        ]


{-| 上部ステータス行: 選択中レイヤー名・えらび中のチップ名・道具・座標。
-}
viewTopStatus : Doc -> Model -> Html Msg
viewTopStatus doc model =
    div [ HA.class "map-topstatus flex h-8 shrink-0 items-center gap-3.5 border-b border-edge bg-panel px-3 text-[12px]" ]
        [ statusField "レイヤー" (layerName model.layer) True
        , statusSep
        , statusField "えらび中" (brushName doc model) False
        , statusSep
        , statusField "道具" (toolName model.tool) False
        , span [ HA.class "ml-auto font-mono text-ink-faint tabular-nums" ]
            [ text
                (case model.hover of
                    Just ( x, y ) ->
                        "x " ++ String.fromInt x ++ " , y " ++ String.fromInt y

                    Nothing ->
                        ""
                )
            ]
        ]


statusField : String -> String -> Bool -> Html Msg
statusField key value accent =
    span []
        [ span [ HA.class "text-ink-faint" ] [ text (key ++ " ") ]
        , span
            [ HA.classList
                [ ( "font-semibold", True )
                , ( "text-accent", accent )
                , ( "text-ink", not accent )
                ]
            ]
            [ text value ]
        ]


statusSep : Html Msg
statusSep =
    span [ HA.class "text-edge" ] [ text "|" ]


layerName : Layer -> String
layerName layer =
    case layer of
        TerrainLayer ->
            "地形"

        PlaceLayer ->
            "配置"


toolName : Tool -> String
toolName tool =
    case tool of
        Pen ->
            "ペン"

        Eraser ->
            "消しゴム"

        Bucket ->
            "バケツ"

        Dropper ->
            "スポイト"


{-| えらび中のチップ名(地形は Doc の name、配置は JSON キー)。
-}
brushName : Doc -> Model -> String
brushName doc model =
    case activeBrush doc model of
        Terrain ch ->
            doc.terrain
                |> List.filter (\sw -> sw.ch == ch)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault (String.fromChar ch)

        Place key ->
            key


viewTools : Doc -> Model -> Html Msg
viewTools doc model =
    div [ HA.class "map-tools flex w-14 shrink-0 flex-col items-center gap-1.5 border-r border-edge bg-panel py-2" ]
        [ toolButton model Pen False "ペン(P)" iconPen
        , toolButton model Eraser False "消しゴム(E)" iconEraser
        , toolButton model Bucket (model.layer == PlaceLayer) "バケツ(B)" iconBucket
        , toolButton model Dropper False "スポイト(I)" iconDropper
        , div [ HA.class "my-1 h-px w-8 shrink-0 bg-edge" ] []
        , historyButton "戻す(⌘Z / Ctrl+Z)" UndoPressed (List.isEmpty model.undo) iconUndo
        , historyButton "やり直す(⇧⌘Z / Ctrl+Y)" RedoPressed (List.isEmpty model.redo) iconRedo
        ]


toolButton : Model -> Tool -> Bool -> String -> Html Msg -> Html Msg
toolButton model tool disabled label body =
    button
        [ HA.classList
            [ ( "map-tool flex h-10 w-10 shrink-0 cursor-pointer items-center justify-center rounded border", True )
            , ( "border-accent bg-accent/10 text-accent", model.tool == tool )
            , ( "border-transparent text-ink-soft hover:bg-white/5 hover:text-ink", model.tool /= tool )
            , ( "cursor-default opacity-40 hover:bg-transparent", disabled )
            ]
        , HA.title label
        , HA.disabled disabled
        , HE.onClick (ToolChosen tool)
        ]
        [ body ]


historyButton : String -> Msg -> Bool -> Html Msg -> Html Msg
historyButton label msg disabled body =
    button
        [ HA.class "map-tool flex h-10 w-10 shrink-0 cursor-pointer items-center justify-center rounded border border-transparent text-ink-soft hover:bg-white/5 hover:text-ink disabled:cursor-default disabled:opacity-40 disabled:hover:bg-transparent"
        , HA.title label
        , HA.disabled disabled
        , HE.onClick msg
        ]
        [ body ]


viewCenter : Doc -> Model -> Html Msg
viewCenter doc model =
    let
        rows =
            shownRows doc model
    in
    div [ HA.class "map-center flex min-w-0 flex-1 flex-col" ]
        [ div
            [ HA.class "map-stage flex min-h-0 flex-1 cursor-crosshair overflow-auto p-4 select-none"

            -- ショートカットはグリッドが焦点の時だけ(入力欄の ⌘Z を横取りしない)
            , HA.tabindex 0
            , HE.custom "keydown" keyDecoder
            , HE.custom "contextmenu"
                (D.succeed { message = Swallowed, stopPropagation = True, preventDefault = True })
            ]
            [ div [ HA.class "m-auto" ]
                [ div [ HA.class "flex items-start" ]
                    [ viewGridWithMarks doc model rows
                    , viewColButtons rows
                    ]
                , viewRowButtons rows
                ]
            ]
        , viewStatus model rows
        ]


{-| グリッド+配置の印。地形の色と配置の印はそれぞれのレイヤーの淡さで描く
(👁 オフのレイヤーは描かない)。クリックを受けるセルは常に敷いておく —
淡くしたり隠したりしても、別レイヤーを触るための当たり判定は残す。
-}
viewGridWithMarks : Doc -> Model -> List String -> Html Msg
viewGridWithMarks doc model rows =
    let
        terrainA =
            layerAlpha TerrainLayer model

        colors =
            if terrainA <= 0 then
                Dict.empty

            else
                doc.terrain
                    |> List.map (\sw -> ( sw.ch, sw.css ))
                    |> Dict.fromList

        marks =
            if layerAlpha PlaceLayer model <= 0 then
                []

            else
                [ div
                    [ HA.class "pointer-events-none absolute inset-0"
                    , HA.style "opacity" (String.fromFloat (layerAlpha PlaceLayer model))
                    ]
                    (List.concatMap (viewGroupMarks model) doc.groups)
                ]
    in
    div
        [ HA.class "map-grid relative border border-edge shadow-[0_2px_10px_rgb(0_0_0/0.4)]"
        , HE.onMouseLeave GridLeft
        ]
        ((rows |> List.indexedMap (viewGridRow colors terrainA model.cellPx))
            ++ marks
        )


viewGridRow : Dict.Dict Char String -> Float -> Int -> Int -> String -> Html Msg
viewGridRow colors terrainA cellPx y row =
    div [ HA.class "flex" ]
        (row |> String.toList |> List.indexedMap (\x ch -> viewCell colors terrainA cellPx x y ch))


viewCell : Dict.Dict Char String -> Float -> Int -> Int -> Int -> Char -> Html Msg
viewCell colors terrainA cellPx x y ch =
    div
        ([ HA.class "map-cell shrink-0"
         , HA.style "width" (String.fromInt cellPx ++ "px")
         , HA.style "height" (String.fromInt cellPx ++ "px")
         , HE.custom "mousedown"
            (D.field "button" D.int
                |> D.map (\b -> { message = CellPressed x y b, stopPropagation = True, preventDefault = False })
            )
         , HE.onMouseEnter (CellEntered x y)
         ]
            ++ (case Dict.get ch colors of
                    Just css ->
                        [ HA.style "background-color" css
                        , HA.style "opacity" (String.fromFloat terrainA)
                        ]

                    Nothing ->
                        [ HA.class "px-checker" ]
               )
        )
        []


{-| 配置 1 グループぶんの印(キー頭文字の小さなバッジ)。
-}
viewGroupMarks : Model -> PlaceGroup -> List (Html Msg)
viewGroupMarks model group =
    case group.kind of
        Single point ->
            [ viewMark model group.key (model.picked == Just ( group.key, Nothing )) point ]

        Many many ->
            many.points
                |> List.map
                    (\mark ->
                        viewMark model group.key (model.picked == Just ( group.key, Just mark.index )) mark.at
                    )


viewMark : Model -> String -> Bool -> ( Int, Int ) -> Html Msg
viewMark model key isPicked ( x, y ) =
    div
        [ HA.class "pointer-events-none absolute flex items-center justify-center"
        , HA.style "left" (String.fromInt (x * model.cellPx) ++ "px")
        , HA.style "top" (String.fromInt (y * model.cellPx) ++ "px")
        , HA.style "width" (String.fromInt model.cellPx ++ "px")
        , HA.style "height" (String.fromInt model.cellPx ++ "px")
        ]
        [ span
            [ HA.classList
                [ ( "flex h-4 w-4 items-center justify-center rounded-full border bg-panel font-mono text-[9px] leading-none text-ink", True )
                , ( "border-accent ring-2 ring-accent/70", isPicked )
                ]
            , HA.style "border-color" (markCss key)
            ]
            [ text (String.left 1 key) ]
        ]


markCss : String -> String
markCss key =
    "hsl(" ++ String.fromInt (hashHue key) ++ " 70% 65%)"


{-| 右端の +/−(列)。
-}
viewColButtons : List String -> Html Msg
viewColButtons rows =
    div [ HA.class "ml-1.5 flex flex-col gap-1" ]
        [ growButton "列を足す" (GrowPressed Cols)
        , shrinkButton "右端の列を消す" (shrinkable Cols rows) (ShrinkPressed Cols)
        ]


{-| 下端の +/−(行)。
-}
viewRowButtons : List String -> Html Msg
viewRowButtons rows =
    div [ HA.class "mt-1.5 flex gap-1" ]
        [ growButton "行を足す" (GrowPressed Rows)
        , shrinkButton "下端の行を消す" (shrinkable Rows rows) (ShrinkPressed Rows)
        ]


growButton : String -> Msg -> Html Msg
growButton label msg =
    button
        [ HA.class "btn btn-mini h-6 w-6 justify-center"
        , HA.title label
        , HE.onClick msg
        ]
        [ text "+" ]


shrinkButton : String -> Bool -> Msg -> Html Msg
shrinkButton label enabled msg =
    button
        [ HA.class "btn btn-mini h-6 w-6 justify-center"
        , HA.title label
        , HA.disabled (not enabled)
        , HE.onClick msg
        ]
        [ text "−" ]


viewStatus : Model -> List String -> Html Msg
viewStatus model rows =
    div [ HA.class "map-status flex h-8 shrink-0 items-center gap-2 border-t border-edge bg-panel px-3 text-[11px] text-ink-soft" ]
        [ button
            [ HA.class "btn btn-mini"
            , HA.title "ズーム −"
            , HA.disabled (Just model.cellPx == List.head zoomLevels)
            , HE.onClick (ZoomStepped -1)
            ]
            [ text "−" ]
        , span [ HA.class "w-10 text-center font-mono" ]
            [ text (String.fromInt (model.cellPx * 100 // baseCellPx) ++ "%") ]
        , button
            [ HA.class "btn btn-mini"
            , HA.title "ズーム +"
            , HA.disabled (Just model.cellPx == List.head (List.reverse zoomLevels))
            , HE.onClick (ZoomStepped 1)
            ]
            [ text "+" ]
        , span [ HA.class "ml-2 font-mono text-ink-faint" ]
            [ text (String.fromInt (maxWidth rows) ++ "×" ++ String.fromInt (List.length rows)) ]
        , span [ HA.class "spacer flex-1" ] []
        , case model.picked of
            Just ( key, _ ) ->
                span [ HA.class "text-ink-faint" ] [ text (key ++ " — 動かす先をクリック") ]

            Nothing ->
                text ""
        , span [ HA.class "w-16 text-right font-mono text-ink-faint" ]
            [ text
                (case model.hover of
                    Just ( x, y ) ->
                        String.fromInt x ++ ", " ++ String.fromInt y

                    Nothing ->
                        ""
                )
            ]
        ]


{-| 右ペイン: レイヤーパネル(上=配置・手前、下=地形)/ 選択中レイヤーのパレット /
インスペクタ(親が描く「選んだ 1 行のフォーム」)。下 2 つは 1 本の
スクロール域に入れる — 縦に長くなっても、区切りは既存の枠線と見出しで見せる。
-}
viewSide : (Msg -> msg) -> List (Html msg) -> Doc -> Model -> Html msg
viewSide toSelf inspector doc model =
    div [ HA.class "map-side flex w-56 shrink-0 flex-col border-l border-edge bg-panel" ]
        [ Html.map toSelf
            (div [ HA.class "border-b border-edge pb-2" ]
                [ paneTitle "レイヤー"
                , viewLayerRow model PlaceLayer
                , viewLayerRow model TerrainLayer
                ]
            )
        , div [ HA.class "flex min-h-0 flex-1 flex-col overflow-y-auto" ]
            (Html.map toSelf
                (div []
                    [ paneTitle (layerName model.layer)
                    , div [ HA.class "map-palette px-2 pb-3" ] [ viewPalette doc model ]
                    ]
                )
                :: inspector
            )
        ]


paneTitle : String -> Html Msg
paneTitle label =
    div [ HA.class "px-3 pt-2 pb-1.5 text-[11px] tracking-wider text-ink-faint" ] [ text label ]


{-| レイヤー 1 行。👁 表示切替と選択。選択中は行をハイライト。
-}
viewLayerRow : Model -> Layer -> Html Msg
viewLayerRow model layer =
    let
        selected =
            model.layer == layer

        shown =
            isShown layer model
    in
    div
        [ HA.classList
            [ ( "map-layer mx-1.5 flex cursor-pointer items-center gap-2 rounded border-l-2 px-2 py-1.5", True )
            , ( "border-accent bg-white/5", selected )
            , ( "border-transparent hover:bg-white/5", not selected )
            ]
        , HE.onClick (LayerChosen layer)
        ]
        [ button
            [ HA.classList
                [ ( "flex h-6 w-6 shrink-0 items-center justify-center rounded text-sm hover:bg-white/10", True )
                , ( "opacity-30", not shown )
                ]
            , HA.title
                (if shown then
                    "隠す"

                 else
                    "見せる"
                )
            , HE.custom "click"
                (D.succeed { message = LayerToggled layer, stopPropagation = True, preventDefault = False })
            ]
            [ text "👁" ]
        , span
            [ HA.classList
                [ ( "flex-1", True )
                , ( "font-semibold text-ink", selected )
                , ( "text-ink-soft", not selected )
                ]
            ]
            [ text (layerName layer) ]
        ]


{-| パレットは選択中レイヤーの中身だけ。
-}
viewPalette : Doc -> Model -> Html Msg
viewPalette doc model =
    case model.layer of
        TerrainLayer ->
            div [ HA.class "flex flex-wrap gap-1.5 px-1" ]
                (doc.terrain |> List.map (viewSwatch doc model))

        PlaceLayer ->
            div [ HA.class "flex flex-col gap-1 px-1" ]
                (doc.groups |> List.map (viewPlaceEntry doc model))


viewSwatch : Doc -> Model -> Swatch -> Html Msg
viewSwatch doc model swatch =
    let
        selected =
            model.tool /= Eraser && activeBrush doc model == Terrain swatch.ch
    in
    -- 名前はボタンの中に出す。色と 1 文字だけでは「どれが何か」が分からず、
    -- 名前を当てるのに 1 つずつ指を乗せて確かめることになる。
    button
        [ HA.classList
            [ ( "flex h-7 shrink-0 cursor-pointer items-center gap-1.5 rounded-sm border pr-2 text-[11px] text-ink", True )
            , ( "border-accent ring-2 ring-accent/60", selected )
            , ( "border-edge hover:border-ink-faint", not selected )
            ]
        , HA.title swatch.name
        , HE.onClick (TerrainChosen swatch.ch)
        ]
        [ span
            [ HA.class "flex h-full w-7 shrink-0 items-center justify-center rounded-l-sm font-mono text-[10px] text-white/80"
            , HA.style "background-color" swatch.css
            ]
            [ text (String.fromChar swatch.ch) ]
        , span [ HA.class "whitespace-nowrap" ] [ text swatch.name ]
        ]


{-| 配置チップ 1 つ。セルに置かれた印の数と、セルを見ない行の数(+部屋 N)を出す。
「+部屋 N」は押すと一覧が開き、行を選ぶとインスペクタで中身を書ける。
雛形からその種の行を作れる配列には「＋ 部屋の行」も添える。
-}
viewPlaceEntry : Doc -> Model -> PlaceGroup -> Html Msg
viewPlaceEntry doc model group =
    let
        selected =
            activeBrush doc model == Place group.key

        count =
            List.length (allPoints group)

        ( offRows, roomAdd ) =
            case group.kind of
                Many many ->
                    ( many.offRows, many.roomAdd )

                Single _ ->
                    ( [], False )

        open =
            model.expanded == Just group.key

        countText =
            case group.kind of
                Single _ ->
                    ""

                Many _ ->
                    String.fromInt count
    in
    div [ HA.class "place-entry" ]
        ([ button
            [ HA.classList
                [ ( "flex w-full cursor-pointer items-center gap-2 rounded border px-2 py-1 text-left text-xs", True )
                , ( "border-accent bg-accent/10 text-accent", selected )
                , ( "border-edge text-ink-soft hover:border-ink-faint hover:text-ink", not selected )
                ]
            , HA.title group.key
            , HE.onClick (PlaceChosen group.key)
            ]
            [ span
                [ HA.class "flex h-4 w-4 shrink-0 items-center justify-center rounded-full border bg-panel font-mono text-[9px] leading-none text-ink"
                , HA.style "border-color" (markCss group.key)
                ]
                [ text (String.left 1 group.key) ]
            , span [ HA.class "min-w-0 flex-1 truncate font-mono" ] [ text group.key ]
            , span [ HA.class "shrink-0 font-mono text-[10px] text-ink-faint" ] [ text countText ]
            ]
         ]
            ++ (if List.isEmpty offRows then
                    []

                else
                    [ button
                        [ HA.class "place-room-toggle mt-0.5 flex w-full cursor-pointer items-center gap-1 px-2 text-left text-[10px] text-ink-faint hover:text-ink"
                        , HA.title "セルを見ない行(on:enter など)。えらぶとこの下のフォームで書けます"
                        , HE.onClick (GroupToggled group.key)
                        ]
                        [ text
                            ((if open then
                                "▾ "

                              else
                                "▸ "
                             )
                                ++ "+部屋 "
                                ++ String.fromInt (List.length offRows)
                            )
                        ]
                    ]
               )
            ++ (if open then
                    offRows |> List.map (viewOffRow model group.key)

                else
                    []
               )
            ++ (if roomAdd then
                    [ button
                        [ HA.class "place-room-add mt-0.5 ml-2 cursor-pointer text-[10px] text-ink-faint hover:text-accent"
                        , HA.title "セルを見ない行(on:enter)を 1 行足す。部屋に入った拍の言葉に使います"
                        , HE.onClick (RoomRowPressed group.key)
                        ]
                        [ text "＋ 部屋の行(enter)" ]
                    ]

                else
                    []
               )
        )


{-| セルを見ない行 1 行(一覧の中身)。見出しは値から作った要約。
-}
viewOffRow : Model -> String -> OffRow -> Html Msg
viewOffRow model key row =
    let
        isPicked =
            model.picked == Just ( key, Just row.index )
    in
    button
        [ HA.classList
            [ ( "place-room-row mt-0.5 flex w-full cursor-pointer items-center gap-1 rounded border px-2 py-0.5 text-left text-[10px]", True )
            , ( "border-accent bg-accent/10 text-accent", isPicked )
            , ( "border-edge text-ink-soft hover:border-ink-faint hover:text-ink", not isPicked )
            ]
        , HA.title row.summary
        , HE.onClick (OffRowChosen key row.index)
        ]
        [ span [ HA.class "shrink-0 font-mono text-ink-faint" ] [ text ("#" ++ String.fromInt row.index) ]
        , span [ HA.class "min-w-0 flex-1 truncate" ] [ text row.summary ]
        ]


viewConfirm : Confirm -> Html Msg
viewConfirm confirm =
    let
        ( message, okLabel ) =
            case confirm of
                RemovePointConfirm target ->
                    ( "「" ++ target.key ++ "」の行をひとつ削除します。よろしいですか?", "削除する" )

                ShrinkConfirm Cols ->
                    ( "右端の列は空ではありません。消しますか?", "消す" )

                ShrinkConfirm Rows ->
                    ( "下端の行は空ではありません。消しますか?", "消す" )
    in
    div [ HA.class "absolute inset-0 z-20 flex items-center justify-center bg-black/40" ]
        [ div [ HA.class "w-72 rounded border border-edge bg-panel p-4 shadow-lg" ]
            [ div [ HA.class "mb-3 text-xs text-ink" ] [ text message ]
            , div [ HA.class "flex justify-end gap-2" ]
                [ button [ HA.class "btn", HE.onClick ConfirmDismissed ] [ text "やめる" ]
                , button [ HA.class "btn btn-danger", HE.onClick ConfirmAccepted ] [ text okLabel ]
                ]
            ]
        ]



-- ショートカット(グリッドが焦点の時だけ届く keydown)


keyDecoder : D.Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
keyDecoder =
    D.map4
        (\key meta ctrl shift -> { key = key, meta = meta, ctrl = ctrl, shift = shift })
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "shiftKey" D.bool)
        |> D.andThen
            (\k ->
                case shortcutMsg k of
                    Just msg ->
                        D.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    Nothing ->
                        D.fail "他のキーは素通し"
            )


shortcutMsg : { key : String, meta : Bool, ctrl : Bool, shift : Bool } -> Maybe Msg
shortcutMsg k =
    case ( String.toLower k.key, k.meta || k.ctrl, k.shift ) of
        ( "z", True, False ) ->
            Just UndoPressed

        ( "z", True, True ) ->
            Just RedoPressed

        ( "y", True, _ ) ->
            Just RedoPressed

        ( "p", False, _ ) ->
            Just (ToolChosen Pen)

        ( "e", False, _ ) ->
            Just (ToolChosen Eraser)

        ( "b", False, _ ) ->
            Just (ToolChosen Bucket)

        ( "i", False, _ ) ->
            Just (ToolChosen Dropper)

        _ ->
            Nothing



-- アイコン(SVG 線画・20px — ピクセルエディタと同じ形)


icon : List (Svg.Svg Msg) -> Html Msg
icon paths =
    Svg.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width "20"
        , SA.height "20"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.6"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        paths


iconPen : Html Msg
iconPen =
    icon [ Svg.path [ SA.d "M17 3a2.85 2.83 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5Z" ] [] ]


iconEraser : Html Msg
iconEraser =
    icon
        [ Svg.path [ SA.d "m7 21-4.3-4.3c-1-1-1-2.5 0-3.4l9.6-9.6c1-1 2.5-1 3.4 0l5.6 5.6c1 1 1 2.5 0 3.4L13 21" ] []
        , Svg.path [ SA.d "M22 21H7" ] []
        , Svg.path [ SA.d "m5 11 9 9" ] []
        ]


iconBucket : Html Msg
iconBucket =
    icon
        [ Svg.path [ SA.d "m19 11-8-8-8.6 8.6a2 2 0 0 0 0 2.8l5.2 5.2c.8.8 2 .8 2.8 0L19 11Z" ] []
        , Svg.path [ SA.d "m5 2 5 5" ] []
        , Svg.path [ SA.d "M2 13h15" ] []
        , Svg.path [ SA.d "M22 20a2 2 0 1 1-4 0c0-1.6 1.7-2.4 2-4 .3 1.6 2 2.4 2 4Z" ] []
        ]


iconDropper : Html Msg
iconDropper =
    icon
        [ Svg.path [ SA.d "m2 22 1-1h3l9-9" ] []
        , Svg.path [ SA.d "M3 21v-3l9-9" ] []
        , Svg.path [ SA.d "m15 6 3.4-3.4a2.1 2.1 0 1 1 3 3L18 9l.4.4a2.1 2.1 0 1 1-3 3l-3.8-3.8a2.1 2.1 0 1 1 3-3l.4.4Z" ] []
        ]


iconUndo : Html Msg
iconUndo =
    icon
        [ Svg.path [ SA.d "M9 14 4 9l5-5" ] []
        , Svg.path [ SA.d "M4 9h10.5a5.5 5.5 0 0 1 5.5 5.5 5.5 5.5 0 0 1-5.5 5.5H11" ] []
        ]


iconRedo : Html Msg
iconRedo =
    icon
        [ Svg.path [ SA.d "m15 14 5-5-5-5" ] []
        , Svg.path [ SA.d "M20 9H9.5A5.5 5.5 0 0 0 4 14.5 5.5 5.5 0 0 0 9.5 20H13" ] []
        ]
