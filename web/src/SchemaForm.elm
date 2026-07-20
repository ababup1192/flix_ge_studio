module SchemaForm exposing (Context, Control(..), Row, rows)

{-| スキーマのセクション+選択エントリの値 → フォーム行モデル。

ここは純ロジック(Html を作らない)。どの type がどの見た目になるかの決定を
描画側に散らさず 1 箇所に集める。未対応 type は生 JSON 行に倒す —
どんなスキーマでも「編集できない項目」を作らないため。

-}

import Json.Decode as D
import Json.Encode as E
import Schema exposing (Field, FieldType(..), Section)
import Sources
import Widgets.Weights as Weights


type alias Row =
    { name : String
    , label : String
    , required : Bool
    , control : Control

    -- enabledWhen 宣言の評価材料。currentText は兄弟フィールドの今の値
    -- (無ければ default)を比較用文字列にした物。draft(打ちかけ)を優先する
    -- 最終判定は activeDraft を知る描画側の仕事
    , condition : Maybe Condition
    }


type alias Condition =
    { field : String
    , fieldLabel : String

    -- 兄弟フィールドがこの列のどれかに一致すれば有効(equals は 1 要素・in は複数)
    , allowedTexts : List String
    , currentText : Maybe String
    }


type Control
    = NumberControl
        { isInt : Bool
        , value : Maybe Float
        , min : Maybe Float
        , max : Maybe Float
        , step : Maybe Float

        -- min と max が両方あるときだけ slider(可動域が決まらない slider は作れない)
        , slider : Bool

        -- % 併記の倍率(widget "unit" = 100・"percent" = 1・無指定 = Nothing)
        , percentFactor : Maybe Float
        }
    | TextControl (Maybe String)
      -- widget "multiline" の text(解説などの長文。textarea で出す)
    | MultilineControl (Maybe String)
      -- type "grid"(ASCII マップ)。text は行を改行で継いだ物・lineCount は行数(高さの目安)
    | GridControl { text : Maybe String, lineCount : Int }
      -- widget "uiDoc" の text(ui.json のパス。エンジン焼きのサムネを添えて出す)
    | UiDocControl (Maybe String)
    | BoolControl (Maybe Bool)
    | EnumControl { choices : List String, selected : Maybe String, segmented : Bool }
    | RefControl { target : String, choices : List String, selected : Maybe String }
    | Vec2Control { x : Maybe Float, y : Maybe Float }
    | ColorControl (Maybe String)
    | TextureControl { value : Maybe String, choices : List String }
    | WeightsControl { config : Weights.Config, entries : List ( String, Float ) }
      -- 対応の形でない値({hsv} の色等)は壊さず見せるだけ(v1 は変換を持たない)
    | ReadOnlyControl String
      -- 未対応 type(list / record / custom)や対応外の値の形の生 JSON 1 行
    | RawJsonControl String


{-| doc は ref の選択肢(参照先 catalog の一覧)を引くため、
textures は texture 欄の候補(project.json の manifest 名)のために渡す。
others は参照先セクションが別ファイルに住む時の横断辞書(同一文書優先)。
-}
type alias Context =
    { doc : D.Value
    , textures : List String
    , others : List Sources.SourceDoc
    }


rows : Context -> Section -> D.Value -> List Row
rows ctx section entry =
    section.fields
        |> List.sortBy (\( _, field ) -> Maybe.withDefault orderLast field.order)
        |> List.map (\( name, field ) -> row ctx section entry name field)


{-| order 未指定は末尾へ(同順位どうしは sortBy の安定性で書いた順が保たれる) -}
orderLast : Int
orderLast =
    2 ^ 30


row : Context -> Section -> D.Value -> String -> Field -> Row
row ctx section entry name field =
    { name = name
    , label = Maybe.withDefault name field.label
    , required = field.required
    , control = control ctx entry name field
    , condition = field.enabledWhen |> Maybe.map (condition section entry)
    }


condition : Section -> D.Value -> { field : String, allowed : List D.Value } -> Condition
condition section entry cond =
    let
        siblingField =
            section.fields
                |> List.filter (\( n, _ ) -> n == cond.field)
                |> List.head
                |> Maybe.map Tuple.second
    in
    { field = cond.field
    , fieldLabel =
        siblingField
            |> Maybe.andThen .label
            |> Maybe.withDefault cond.field
    , allowedTexts = List.map comparableText cond.allowed
    , currentText =
        case D.decodeValue (D.field cond.field D.value) entry of
            Ok v ->
                Just (comparableText v)

            Err _ ->
                -- 値が書かれていなければ default が効いている(フォームの表示と同じ規則)
                siblingField
                    |> Maybe.andThen .default
                    |> Maybe.map comparableText
    }


{-| 値の比較用文字列。文字列はそのまま、他は素の JSON 1 行 —
enum(文字列)が主用途で、数値・bool も同じ規則で比べられる。
-}
comparableText : D.Value -> String
comparableText v =
    case D.decodeValue D.string v of
        Ok s ->
            s

        Err _ ->
            E.encode 0 v


control : Context -> D.Value -> String -> Field -> Control
control ctx entry name field =
    let
        -- 現在値が無ければ default を見せる(書き戻すまでは文書に入らない)
        currentOr : D.Decoder a -> Maybe a
        currentOr dec =
            case D.decodeValue (D.field name dec) entry of
                Ok v ->
                    Just v

                Err _ ->
                    field.default
                        |> Maybe.andThen (\d -> D.decodeValue dec d |> Result.toMaybe)
    in
    case field.type_ of
        TInt ->
            numberControl True field (currentOr D.float)

        TFloat ->
            numberControl False field (currentOr D.float)

        TText ->
            if Schema.widgetIs "multiline" field.widget then
                MultilineControl (currentOr D.string)

            else if Schema.widgetIs "uiDoc" field.widget then
                UiDocControl (currentOr D.string)

            else
                TextControl (currentOr D.string)

        TBool ->
            BoolControl (currentOr D.bool)

        TGrid ->
            -- 文字列の列だけマップとして見せる。それ以外の形は壊さず生 JSON へ —
            -- 改行 join/split の書き戻しが刺さらない形を編集可能に見せない
            case currentOr D.value of
                Just raw ->
                    case D.decodeValue (D.list D.string) raw of
                        Ok rows_ ->
                            GridControl { text = Just (String.join "\n" rows_), lineCount = List.length rows_ }

                        Err _ ->
                            rawJson (Just raw)

                Nothing ->
                    GridControl { text = Nothing, lineCount = 0 }

        TEnum choices ->
            EnumControl
                { choices = choices
                , selected = currentOr D.string
                , segmented = Schema.widgetIs "segmented" field.widget
                }

        TRef target ->
            RefControl
                { target = target
                , choices = Sources.refChoices target ctx.doc ctx.others
                , selected = currentOr D.string
                }

        TVec2 ->
            -- {x,y} の形だけ 2 連数値欄。それ以外(配列等)は生 JSON へ倒す —
            -- 軸の書き戻し(x/y キーへの set)が刺さらない形を編集可能に見せない
            case currentOr D.value of
                Just raw ->
                    if isObject raw then
                        Vec2Control
                            { x = D.decodeValue (D.field "x" D.float) raw |> Result.toMaybe
                            , y = D.decodeValue (D.field "y" D.float) raw |> Result.toMaybe
                            }

                    else
                        rawJson (Just raw)

                Nothing ->
                    Vec2Control { x = Nothing, y = Nothing }

        TColor ->
            case currentOr D.value of
                Just raw ->
                    case D.decodeValue D.string raw of
                        Ok hex ->
                            ColorControl (Just hex)

                        Err _ ->
                            ReadOnlyControl (E.encode 0 raw)

                Nothing ->
                    ColorControl Nothing

        TTexture ->
            case currentOr D.value of
                Just raw ->
                    case D.decodeValue D.string raw of
                        Ok value ->
                            TextureControl { value = Just value, choices = ctx.textures }

                        Err _ ->
                            rawJson (Just raw)

                Nothing ->
                    TextureControl { value = Nothing, choices = ctx.textures }

        TCustom "weights" ->
            case ( Weights.configFrom field.widget, currentOr D.value ) of
                ( Just config, Just raw ) ->
                    case weightEntries raw of
                        Just entries ->
                            WeightsControl { config = config, entries = entries }

                        Nothing ->
                            rawJson (Just raw)

                ( Just config, Nothing ) ->
                    WeightsControl { config = config, entries = [] }

                ( Nothing, raw ) ->
                    -- widget に total が無ければ配分のしようがない → 生 JSON へ
                    rawJson raw

        _ ->
            rawJson (currentOr D.value)


rawJson : Maybe D.Value -> Control
rawJson value =
    RawJsonControl
        (value
            |> Maybe.map (E.encode 0)
            |> Maybe.withDefault ""
        )


numberControl : Bool -> Field -> Maybe Float -> Control
numberControl isInt field value =
    NumberControl
        { isInt = isInt
        , value = value
        , min = field.min
        , max = field.max
        , step = field.step
        , slider = field.min /= Nothing && field.max /= Nothing
        , percentFactor =
            if Schema.widgetIs "unit" field.widget then
                Just 100

            else if Schema.widgetIs "percent" field.widget then
                Just 1

            else
                Nothing
        }


isObject : D.Value -> Bool
isObject raw =
    D.decodeValue (D.keyValuePairs D.value) raw
        |> Result.toMaybe
        |> (/=) Nothing


{-| キー→数値のオブジェクトだけ受ける("//" 注釈キーは文書の流儀どおり読み飛ばす)。
1 つでも数値でない値が混ざれば Nothing — 黙って行を落とすと編集で消えたように見える。
-}
weightEntries : D.Value -> Maybe (List ( String, Float ))
weightEntries raw =
    D.decodeValue (D.keyValuePairs D.value) raw
        |> Result.toMaybe
        |> Maybe.andThen
            (\pairs ->
                let
                    real =
                        pairs |> List.filter (\( k, _ ) -> not (String.startsWith "//" k))

                    floats =
                        real
                            |> List.filterMap
                                (\( k, v ) ->
                                    D.decodeValue D.float v
                                        |> Result.toMaybe
                                        |> Maybe.map (Tuple.pair k)
                                )
                in
                if List.length floats == List.length real then
                    Just floats

                else
                    Nothing
            )
