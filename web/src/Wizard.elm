module Wizard exposing
    ( Draft
    , FieldDraft
    , PathSeg(..)
    , Shape(..)
    , TypeChoice(..)
    , dataPathOf
    , dataText
    , declEdit
    , declText
    , emptyDraft
    , emptyField
    , moveField
    , schemaPathOf
    , schemaText
    , shapeName
    , typeChoiceFromName
    , typeChoiceName
    , typeChoices
    , validate
    )

{-| 「+ 新規リソース」ウィザードの純ロジック(Html を作らない)。

入力の下書き(Draft)から 3 点セットの中身 — ①データ雛形 ②スキーマ(新方言)
③project.json への追記編集 — を導く。ここに閉じるのは、生成物の形を
テストで固定するため。書き込みの往復や画面は Main が持つ。

WhyNot: 既存 json のドロップ推論(ファイルを落として形を当てる)は次段。
推論の当たり外れの UI(候補の提示・訂正)ごと設計する話で、v1 の手入力とは
別物なのでここには入れない。

-}

import Json.Encode as E


{-| フィールドの下書き。数値でない type の min/max・enum でない type の
enumValues 等は画面が隠すだけでデータとしては持ち続ける — type を
切り替えて戻したとき入力が消えない。
-}
type alias FieldDraft =
    { name : String
    , type_ : TypeChoice
    , enumValues : String -- カンマ区切りの生入力
    , refTarget : String
    , label : String
    , required : Bool
    , minText : String
    , maxText : String
    }


type alias Draft =
    { id : String
    , title : String

    -- Nothing = 既定(assets/<id>.json)に従う。触ったら Just で固定
    , path : Maybe String
    , shape : Shape
    , fields : List FieldDraft
    }


type Shape
    = ShapeCatalog
    | ShapeList
    | ShapeRecord


{-| list / record を今回のウィザードが扱う範囲(スカラ+enum+ref)に絞った型。
スキーマ方言の list / record / custom は雛形の零値が一意に決まらないので出さない。
-}
type TypeChoice
    = CText
    | CInt
    | CFloat
    | CBool
    | CColor
    | CVec2
    | CTexture
    | CEnum
    | CRef


emptyDraft : Draft
emptyDraft =
    { id = ""
    , title = ""
    , path = Nothing
    , shape = ShapeCatalog
    , fields = [ emptyField ]
    }


emptyField : FieldDraft
emptyField =
    { name = ""
    , type_ = CText
    , enumValues = ""
    , refTarget = ""
    , label = ""
    , required = False
    , minText = ""
    , maxText = ""
    }


typeChoices : List TypeChoice
typeChoices =
    [ CText, CInt, CFloat, CBool, CColor, CVec2, CTexture, CEnum, CRef ]


typeChoiceName : TypeChoice -> String
typeChoiceName choice =
    case choice of
        CText ->
            "text"

        CInt ->
            "int"

        CFloat ->
            "float"

        CBool ->
            "bool"

        CColor ->
            "color"

        CVec2 ->
            "vec2"

        CTexture ->
            "texture"

        CEnum ->
            "enum"

        CRef ->
            "ref"


typeChoiceFromName : String -> Maybe TypeChoice
typeChoiceFromName name =
    typeChoices
        |> List.filter (\c -> typeChoiceName c == name)
        |> List.head


shapeName : Shape -> String
shapeName shape =
    case shape of
        ShapeCatalog ->
            "catalog"

        ShapeList ->
            "list"

        ShapeRecord ->
            "record"



-- 置き場所


dataPathOf : Draft -> String
dataPathOf draft =
    case draft.path of
        Just p ->
            p

        Nothing ->
            "assets/" ++ draft.id ++ ".json"


schemaPathOf : Draft -> String
schemaPathOf draft =
    let
        path =
            dataPathOf draft
    in
    if String.endsWith ".json" path then
        String.dropRight 5 path ++ ".schema.json"

    else
        path ++ ".schema.json"



-- 並べ替え(ステップ 2 の ↑↓)


{-| i 番目のフィールドを delta(±1)だけ動かす。端からはみ出す指定はそのまま返す。 -}
moveField : Int -> Int -> List FieldDraft -> List FieldDraft
moveField i delta fields =
    let
        j =
            i + delta
    in
    case ( List.drop i fields |> List.head, List.drop j fields |> List.head ) of
        ( Just a, Just b ) ->
            if j < 0 then
                fields

            else
                fields
                    |> List.indexedMap
                        (\k f ->
                            if k == i then
                                b

                            else if k == j then
                                a

                            else
                                f
                        )

        _ ->
            fields



-- 検査(作成前に全部まとめて知らせる)


validate : List String -> Draft -> List String
validate existingPaths draft =
    List.concat
        [ idErrors draft.id
        , pathErrors existingPaths draft
        , fieldListErrors draft.fields
        ]


idErrors : String -> List String
idErrors id =
    if id == "" then
        [ "リソース名(id)を入力してください" ]

    else if not (isAsciiName id) then
        [ "リソース名(id)は半角英数で入力してください(先頭は英字): " ++ id ]

    else
        []


pathErrors : List String -> Draft -> List String
pathErrors existingPaths draft =
    let
        path =
            dataPathOf draft

        shapeOk =
            String.endsWith ".json" path
                && not (String.endsWith ".schema.json" path)
                && not (String.startsWith "/" path)
                && not (List.member ".." (String.split "/" path))

        collisions =
            [ path, schemaPathOf draft ]
                |> List.filter (\p -> List.member p existingPaths)
    in
    (if shapeOk then
        []

     else
        [ "置き場所はプロジェクト内の .json で指定してください(.schema.json・絶対パス・.. は不可): " ++ path ]
    )
        ++ List.map (\p -> "既にあるファイルと重なります: " ++ p) collisions


fieldListErrors : List FieldDraft -> List String
fieldListErrors fields =
    let
        names =
            List.map .name fields

        duplicates =
            names
                |> List.filter (\n -> n /= "" && List.length (List.filter ((==) n) names) > 1)
                |> dedupe
    in
    (if List.isEmpty fields then
        [ "フィールドを 1 つ以上定義してください" ]

     else
        []
    )
        ++ List.map (\n -> "フィールド名が重複しています: " ++ n) duplicates
        ++ List.concatMap fieldErrors fields


fieldErrors : FieldDraft -> List String
fieldErrors field =
    let
        named message =
            "フィールド「" ++ field.name ++ "」: " ++ message
    in
    List.concat
        [ if field.name == "" then
            [ "名前が空のフィールドがあります" ]

          else if not (isAsciiName field.name) then
            [ "フィールド名は半角英数で入力してください(先頭は英字): " ++ field.name ]

          else
            []
        , case field.type_ of
            CEnum ->
                if List.isEmpty (enumList field.enumValues) then
                    [ named "enum の値をカンマ区切りで 1 つ以上入れてください" ]

                else
                    []

            CRef ->
                if String.trim field.refTarget == "" then
                    [ named "ref の参照先セクション名を入れてください" ]

                else
                    []

            _ ->
                []
        , if isNumeric field.type_ then
            numberRangeErrors named field

          else
            []
        ]


numberRangeErrors : (String -> String) -> FieldDraft -> List String
numberRangeErrors named field =
    let
        parsed label textValue =
            if String.trim textValue == "" then
                Ok Nothing

            else
                case String.toFloat (String.trim textValue) of
                    Just v ->
                        Ok (Just v)

                    Nothing ->
                        Err (named (label ++ " が数値になっていません: " ++ textValue))
    in
    case ( parsed "min" field.minText, parsed "max" field.maxText ) of
        ( Ok (Just lo), Ok (Just hi) ) ->
            if lo > hi then
                [ named "min が max を超えています" ]

            else
                []

        ( a, b ) ->
            List.filterMap errOnly [ a, b ]


errOnly : Result e (Maybe a) -> Maybe e
errOnly result =
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing


isAsciiName : String -> Bool
isAsciiName s =
    case String.uncons s of
        Just ( first, _ ) ->
            Char.isAlpha first && String.all Char.isAlphaNum s

        Nothing ->
            False


isNumeric : TypeChoice -> Bool
isNumeric choice =
    choice == CInt || choice == CFloat


enumList : String -> List String
enumList raw =
    raw
        |> String.split ","
        |> List.map String.trim
        |> List.filter ((/=) "")


dedupe : List String -> List String
dedupe xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        xs



-- ①データ雛形


dataText : Draft -> String
dataText draft =
    let
        zeroEntry =
            E.object (draft.fields |> List.map (\f -> ( f.name, zeroValue f )))

        body =
            case draft.shape of
                ShapeCatalog ->
                    E.object [ ( "sample", zeroEntry ) ]

                ShapeList ->
                    E.list identity [ zeroEntry ]

                ShapeRecord ->
                    zeroEntry
    in
    E.encode 2
        (E.object
            [ ( "//", E.string "ウィザード生成・自由に編集可" )
            , ( draft.id, body )
            ]
        )
        ++ "\n"


{-| 型の零値。default 入力はウィザードに無いので、雛形はこれで埋める。 -}
zeroValue : FieldDraft -> E.Value
zeroValue field =
    case field.type_ of
        CText ->
            E.string ""

        CInt ->
            E.int 0

        CFloat ->
            E.int 0

        CBool ->
            E.bool False

        CColor ->
            E.string "#ffffff"

        CVec2 ->
            E.object [ ( "x", E.int 0 ), ( "y", E.int 0 ) ]

        CTexture ->
            E.string ""

        CEnum ->
            E.string (enumList field.enumValues |> List.head |> Maybe.withDefault "")

        CRef ->
            E.string ""



-- ②スキーマ(新方言)


schemaText : Draft -> String
schemaText draft =
    E.encode 2
        (E.object
            [ ( "version", E.int 1 )
            , ( "sections"
              , E.object
                    [ ( draft.id
                      , E.object
                            [ ( "kind", E.string (shapeName draft.shape) )
                            , ( "fields"
                              , E.object
                                    (draft.fields
                                        |> List.indexedMap (\i f -> ( f.name, fieldJson (i + 1) f ))
                                    )
                              )
                            ]
                      )
                    ]
              )
            ]
        )
        ++ "\n"


fieldJson : Int -> FieldDraft -> E.Value
fieldJson order field =
    E.object
        (List.concat
            [ [ ( "type", typeJson field ) ]
            , if String.trim field.label == "" then
                []

              else
                [ ( "label", E.string (String.trim field.label) ) ]
            , [ ( "order", E.int order ) ]
            , if field.required then
                [ ( "required", E.bool True ) ]

              else
                []
            , if isNumeric field.type_ then
                List.filterMap identity
                    [ numberField "min" field.minText
                    , numberField "max" field.maxText
                    ]

              else
                []
            ]
        )


numberField : String -> String -> Maybe ( String, E.Value )
numberField key textValue =
    String.toFloat (String.trim textValue)
        |> Maybe.map (\v -> ( key, E.float v ))


typeJson : FieldDraft -> E.Value
typeJson field =
    case field.type_ of
        CEnum ->
            E.object [ ( "enum", E.list E.string (enumList field.enumValues) ) ]

        CRef ->
            E.object [ ( "ref", E.string (String.trim field.refTarget) ) ]

        other ->
            E.string (typeChoiceName other)



-- ③project.json への追記編集(適用は jsonc 側 = docEdit ポート)


type PathSeg
    = Key String
    | Idx Int


{-| "editor"."resources" 末尾へ 1 エントリ足す編集の導出。path は追記先の
配列そのもの(末尾挿入は jsonc 側 applyDocAppend の役目)。schema はサーバの
sibling 規約(pattern の .json → .schema.json)がそのまま当たるので書かない。
-}
declEdit : Draft -> { path : List PathSeg, value : E.Value }
declEdit draft =
    { path = [ Key "editor", Key "resources" ]
    , value = declValue draft
    }


declValue : Draft -> E.Value
declValue draft =
    E.object
        (List.concat
            [ [ ( "id", E.string draft.id )
              , ( "pattern", E.string (dataPathOf draft) )
              ]
            , if String.trim draft.title == "" then
                []

              else
                [ ( "title", E.string (String.trim draft.title) ) ]
            ]
        )


{-| ステップ 3 で見せる「追記されるエントリ」の文字列。 -}
declText : Draft -> String
declText draft =
    E.encode 2 (declValue draft)
