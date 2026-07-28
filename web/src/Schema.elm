module Schema exposing
    ( Field
    , FieldType(..)
    , Schema
    , Section
    , SectionKind(..)
    , decodeString
    , isJsonSchema
    , widgetIs
    )

{-| スキーマ方言(◯◯.schema.json)のデコーダ。

widget はエディタ専用の自由な JSON なので、形を決めずに Value のまま持つ。
未知の type タグは黙って飲み込まず Err にする — スキーマの書き間違いを
「フォームに出ない項目」として見過ごすと気づけないため。
ただし未知の「kind」だけは Err にしない(Unsupported として持つ) — kind は
サーバ側 Doc 規約の語彙で先に増えるので、1 セクション未対応なだけで
文書全体を「壊れている」扱いにしない。

-}

import Json.Decode as D


type alias Schema =
    { version : Maybe Int

    -- タブの並びは書いた順が意図なので Dict にせず対のまま持つ
    , sections : List ( String, Section )
    }


{-| ValueKind はセクション全体が単一の値(kind "value" / "field")で、
セクション宣言そのものがフィールド宣言を兼ねる。
Unsupported はこのエディタがまだフォームにできない kind(名前を持ち回って
画面が「何が未対応か」を言えるようにする)。スキーマが悪いのではない。
-}
type SectionKind
    = Catalog
    | ListKind
    | RecordKind
    | ValueKind Field
    | Unsupported String


type alias Section =
    { kind : SectionKind

    -- タブの表示名(無ければ呼び側がキー名にフォールバック)
    , label : Maybe String

    -- 同じ group 名を書いた単一値のセクションは、1 枚のタブに束ねて表として並ぶ。
    -- 設定が数十個ある文書で「1 値 = 1 タブ」になるのを防ぐための、見せ方だけの指定。
    -- 書かなければ「基本」タブへ束ねる。セクションの種類は変えないので、
    -- 検査・参照解決・表・書き戻しはこの指定を一切見なくてよい。
    , group : Maybe String

    -- セクションまるごとの見せ方の指定({"sfx": {…}} 等)。読めない指定は
    -- 素の自動フォームに倒す — 宣言が先行してもエディタが壊れないように。
    , widget : Maybe D.Value
    , fields : List ( String, Field )
    }


type alias Field =
    { type_ : FieldType
    , label : Maybe String

    -- 値の単位(秒・px・度・%)。数字だけでは何を表すか読めないので、欄の右に添える
    , unit : Maybe String

    -- ひとこと説明。「上げると何がどうなるか」を書く場所(ラベルは名前、こちらは効き目)
    , hint : Maybe String
    , order : Maybe Int
    , widget : Maybe D.Value
    , required : Bool
    , default : Maybe D.Value
    , min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float

    -- 同じ record 内の兄弟フィールドが特定の値のときだけ効くフィールドの宣言
    -- (例: sides は shape が "ngon" のときだけ)。allowed は許される値の列で、
    -- {equals: v}(単一)も {in: [..]}(複数)も同じ列に落として持つ。値は形を
    -- 決めず Value のまま
    , enabledWhen : Maybe { field : String, allowed : List D.Value }
    }


type FieldType
    = TText
    | TInt
    | TFloat
    | TBool
    | TColor
    | TVec2
    | TTexture
      -- 自由形 JSON(hash 参照オブジェクト等)。フォームは生 JSON 行で受ける
    | TJson
      -- ASCII マップ(1 要素 = 1 行の文字列列)。等幅・複数行の専用エディタで受ける
    | TGrid
    | TEnum (List String)
    | TRef String
    | TList FieldType
    | TRecord (List ( String, Field ))
    | TCustom String


{-| widget 指定が特定の名前(素の文字列形)か。フォームとダッシュボードが
同じ判定を使う — 片方だけ別解釈になると同じ欄が画面ごとに違う顔になる。
-}
widgetIs : String -> Maybe D.Value -> Bool
widgetIs name widget =
    (widget |> Maybe.andThen (\w -> D.decodeValue D.string w |> Result.toMaybe))
        == Just name


{-| 失敗理由は 1 行の文字列に畳む — 右ペインにそのまま出せる形が欲しいだけで、
D.Error の木構造を呼び側に持ち回らせない。
-}
decodeString : String -> Result String Schema
decodeString text =
    D.decodeString schemaDecoder text
        |> Result.mapError (\e -> "スキーマが読めません: " ++ errorToLine e)


{-| draft-07 など JSON-Schema 形式か(トップに "$schema" か "properties")。
sections 方言のパースに失敗した時の振り分け材料 — JSON-Schema 形式は
「壊れている」のではなく「意図的にフォーム化対象外の種類」(ドット絵等)なので、
赤エラーでなく穏やかな案内に倒す。JSON として読めない物は False(本当に壊れ)。
-}
isJsonSchema : String -> Bool
isJsonSchema text =
    D.decodeString
        (D.map2 (\a b -> a /= Nothing || b /= Nothing)
            (D.maybe (D.field "$schema" D.value))
            (D.maybe (D.field "properties" D.value))
        )
        text
        |> Result.withDefault False



-- デコーダ本体


schemaDecoder : D.Decoder Schema
schemaDecoder =
    D.map2 Schema
        (opt "version" D.int)
        (D.field "sections" (pairsSkippingNotes sectionDecoder))


sectionDecoder : D.Decoder Section
sectionDecoder =
    D.map4 (\label group widget body -> { body | label = label, group = group, widget = widget })
        (opt "label" D.string)
        (opt "group" D.string)
        (opt "widget" D.value)
        sectionBodyDecoder


{-| kind ごとの本体(label は sectionDecoder が一括で載せる)。 -}
sectionBodyDecoder : D.Decoder Section
sectionBodyDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\name ->
                case name of
                    "catalog" ->
                        D.map (section Catalog) fieldsDecoder

                    "list" ->
                        D.map (section ListKind) fieldsOrItemDecoder

                    "record" ->
                        D.map (section RecordKind) fieldsDecoder

                    "map" ->
                        -- 名前 → 中身の辞書。編集操作(キー追加/改名/削除+選択キーの
                        -- 中身フォーム)は catalog と同じなので Catalog に畳む
                        D.map (section Catalog) fieldsOrItemDecoder

                    "value" ->
                        valueSectionDecoder

                    "field" ->
                        valueSectionDecoder

                    _ ->
                        -- 未対応 kind は中身を読まない(形が分からない物を
                        -- 半端に読んで壊れ扱いしない)。テキスト編集が持ち場
                        D.succeed (section (Unsupported name) [])
            )


{-| label 抜きの Section を組む(label は sectionDecoder が後載せする)。 -}
section : SectionKind -> List ( String, Field ) -> Section
section kind fields =
    { kind = kind, label = Nothing, group = Nothing, widget = Nothing, fields = fields }


fieldsDecoder : D.Decoder (List ( String, Field ))
fieldsDecoder =
    D.field "fields" (pairsSkippingNotes fieldDecoder)


{-| kind "value" / "field": セクションのオブジェクト自体が 1 個のフィールド宣言
(type/min/max/default…)なので、そのまま fieldDecoder で読む。
-}
valueSectionDecoder : D.Decoder Section
valueSectionDecoder =
    D.map (\field -> section (ValueKind field) []) fieldDecoder


{-| list / map の中身の宣言は fields(平置き)と item(入れ子セクション)の 2 方言。
どちらも無い宣言は空フォーム(要素の出し入れだけできる)。ある時の書き間違いは
普段どおり Err にする(oneOf で握り潰さない)。
-}
fieldsOrItemDecoder : D.Decoder (List ( String, Field ))
fieldsOrItemDecoder =
    D.value
        |> D.andThen
            (\raw ->
                case D.decodeValue (D.field "fields" D.value) raw of
                    Ok fieldsRaw ->
                        runNamed "fields" (pairsSkippingNotes fieldDecoder) fieldsRaw

                    Err _ ->
                        case D.decodeValue (D.field "item" D.value) raw of
                            Ok itemRaw ->
                                runNamed "item" (D.field "fields" (pairsSkippingNotes fieldDecoder)) itemRaw

                            Err _ ->
                                D.succeed []
            )


fieldDecoder : D.Decoder Field
fieldDecoder =
    D.succeed Field
        |> andMap (D.field "type" (D.lazy (\_ -> fieldTypeDecoder)))
        |> andMap (opt "label" D.string)
        |> andMap (opt "unit" D.string)
        |> andMap (opt "hint" D.string)
        |> andMap (opt "order" D.int)
        |> andMap (opt "widget" D.value)
        |> andMap (D.oneOf [ D.field "required" D.bool, D.succeed False ])
        |> andMap (opt "default" D.value)
        |> andMap (opt "min" D.float)
        |> andMap (opt "max" D.float)
        |> andMap (opt "step" D.float)
        |> andMap enabledWhenDecoder


{-| enabledWhen は {field, equals: v}(単一)か {field, in: [..]}(複数)。
どちらも許される値の列 allowed に落とす。field が無い/値の指定が無い等の未知形は
「条件なし(常に有効)」に倒す — 条件の読み違いでフィールドを触れなくする方が害が大きい。
-}
enabledWhenDecoder : D.Decoder (Maybe { field : String, allowed : List D.Value })
enabledWhenDecoder =
    D.oneOf
        [ D.field "enabledWhen"
            (D.map2 (\f allowed -> Just { field = f, allowed = allowed })
                (D.field "field" D.string)
                (D.oneOf
                    [ D.field "in" (D.list D.value)
                    , D.field "equals" (D.map List.singleton D.value)

                    -- field はあるが値の指定が読めない → 常に有効へ倒す(空列 = 誰にも一致しない
                    -- ではなく、判定側が「条件なし」と見なせるよう Nothing に落とす)
                    ]
                )
            )
            |> D.andThen
                (\parsed ->
                    case parsed of
                        Just c ->
                            if List.isEmpty c.allowed then
                                D.succeed Nothing

                            else
                                D.succeed (Just c)

                        Nothing ->
                            D.succeed Nothing
                )
        , D.succeed Nothing
        ]


{-| type は文字列("int" 等)とタグ 1 つのオブジェクト({"enum": [...]} 等)の 2 形。
oneOf で受けると失敗時にどちらの文言を出すか選べないので、値の形を先に見て分岐する。
-}
fieldTypeDecoder : D.Decoder FieldType
fieldTypeDecoder =
    D.value
        |> D.andThen
            (\raw ->
                case D.decodeValue D.string raw of
                    Ok name ->
                        scalarType name

                    Err _ ->
                        taggedType raw
            )


scalarType : String -> D.Decoder FieldType
scalarType name =
    case name of
        "text" ->
            D.succeed TText

        -- サーバ側 Doc 規約の別名(shader.schema.json の kind field が使う)
        "string" ->
            D.succeed TText

        "int" ->
            D.succeed TInt

        "float" ->
            D.succeed TFloat

        "bool" ->
            D.succeed TBool

        "color" ->
            D.succeed TColor

        "vec2" ->
            D.succeed TVec2

        "texture" ->
            D.succeed TTexture

        "json" ->
            D.succeed TJson

        "grid" ->
            D.succeed TGrid

        _ ->
            D.fail ("未知の type \"" ++ name ++ "\"")


taggedType : D.Value -> D.Decoder FieldType
taggedType raw =
    case D.decodeValue (D.keyValuePairs D.value) raw of
        Ok [ ( tag, body ) ] ->
            case tag of
                "enum" ->
                    runOn (D.list D.string) body |> D.map TEnum

                "ref" ->
                    runOn D.string body |> D.map TRef

                "list" ->
                    runOn (D.lazy (\_ -> fieldTypeDecoder)) body |> D.map TList

                "record" ->
                    runOn (pairsSkippingNotes (D.lazy (\_ -> fieldDecoder))) body |> D.map TRecord

                "custom" ->
                    runOn D.string body |> D.map TCustom

                _ ->
                    D.fail ("未知の type タグ \"" ++ tag ++ "\"")

        Ok _ ->
            D.fail "type のオブジェクト形はタグ 1 つだけで書く"

        Err _ ->
            D.fail "type は文字列かタグ 1 つのオブジェクトで書く"



-- 小さな組み立て部品


{-| オブジェクトを書いた順のまま対にする。"//" で始まるキーは注釈として読み飛ばす。
D.keyValuePairs に直接デコーダを渡すと注釈の値までデコードして失敗するので、
先に Value で受けてから選り分ける。
-}
pairsSkippingNotes : D.Decoder a -> D.Decoder (List ( String, a ))
pairsSkippingNotes dec =
    D.keyValuePairs D.value
        |> D.andThen
            (\pairs ->
                pairs
                    |> List.filter (\( key, _ ) -> not (String.startsWith "//" key))
                    |> List.foldr
                        (\( key, raw ) acc ->
                            D.map2 (\a rest -> ( key, a ) :: rest) (runNamed key dec raw) acc
                        )
                        (D.succeed [])
            )


{-| decodeValue を挟むと外側のパス情報が切れるので、キー名を失敗文言へ手で継ぎ足す。 -}
runNamed : String -> D.Decoder a -> D.Value -> D.Decoder a
runNamed key dec raw =
    case D.decodeValue dec raw of
        Ok a ->
            D.succeed a

        Err e ->
            D.fail (key ++ " → " ++ errorToLine e)


runOn : D.Decoder a -> D.Value -> D.Decoder a
runOn dec raw =
    case D.decodeValue dec raw of
        Ok a ->
            D.succeed a

        Err e ->
            D.fail (errorToLine e)


{-| elm/json の複数行エラーを「場所 → 理由」の 1 行に。理由の英語文(型不一致等)は
翻訳せずそのまま — 自前の fail 文言(日本語)が主役で、そちらを潰さないことが目的。
-}
errorToLine : D.Error -> String
errorToLine error =
    case error of
        D.Field name inner ->
            name ++ " → " ++ errorToLine inner

        D.Index i inner ->
            String.fromInt i ++ " 番目 → " ++ errorToLine inner

        D.OneOf [] ->
            "どの形にも合いません"

        D.OneOf (first :: _) ->
            errorToLine first

        D.Failure message _ ->
            message
                |> String.lines
                |> List.head
                |> Maybe.withDefault message


opt : String -> D.Decoder a -> D.Decoder (Maybe a)
opt name dec =
    D.oneOf [ D.field name (D.nullable dec), D.succeed Nothing ]


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)
