module Dashboards exposing
    ( Args
    , Detail
    , EntryItem
    , EntryRef
    , FieldLine
    , FieldValue(..)
    , Peek
    , Plugin
    , SourceDoc
    , ViewModel
    , find
    , generic
    )

{-| ダッシュボード(複数リソースをまたぐ閲覧)の純ロジックとプラグインの束。

盤面プレビューの Plugins と別枠なのは、渡る物が違うため — あちらは開いている
1 文書、こちらは宣言(uses)に並んだ全リソースの全文書。実装は汎用 1 個
(generic)だけで、見せ方はスキーマの宣言から導く: uiDoc は肖像・multiline は
フレーバーテキスト・min/max 付き数値はメーター・unit は %・weights は重み棒・
ref はプレビュー。ゲーム固有のプラグインは 2 例目の実需が出るまで作らない。

編集の口は持たない(閲覧まで)。書き換えたい時のジャンプ先(元のファイルの
パスとエントリ)を値に含めるのはそのため。

-}

import Dict
import Doc
import Json.Decode as D
import Json.Encode as E
import Schema exposing (FieldType(..))
import Table
import Widgets.Weights as Weights


{-| 読み込み済みの文書 1 件。resource はセクション名でもある
(1 ファイル 1 セクション・セクション名=リソース id の規約)。
-}
type alias SourceDoc =
    { resource : String
    , path : String
    , doc : D.Value
    , schema : Maybe Schema.Schema
    }


{-| ホスト(Main)がプラグインへ渡す一式。uses は宣言の並びのまま
(先頭が主リソース)。
-}
type alias Args =
    { uses : List String
    , docs : List SourceDoc
    , selected : Maybe String
    }


{-| エントリの居場所。ジャンプ(そのファイルを開いて選択)に必要な分だけ。 -}
type alias EntryRef =
    { resource : String
    , path : String
    , id : String
    }


{-| 一覧の 1 行: 居場所+見せ物(表示名とサムネ用の肖像パス)。 -}
type alias EntryItem =
    { entry : EntryRef
    , name : Maybe String
    , portrait : Maybe String
    }


{-| フィールド 1 行の見せ方。スキーマの宣言(widget / min / max / type)から決まる。 -}
type FieldValue
    = Plain String
      -- multiline text(フレーバー以外の長文)
    | LongText String
      -- min/max 付き数値(メーターバー)
    | Meter { value : Float, min : Float, max : Float, text : String }
      -- widget unit / percent の数値。ratio は 0〜1(バーの長さ)
    | Percent { ratio : Float, text : String }
      -- custom weights(重み棒の読み取り表示)
    | WeightBars { total : Float, entries : List ( String, Float ) }
    | RefValue { target : String, id : String, peek : Maybe Peek }


{-| プレビューの中身: 参照先エントリの居場所・肖像・要約行。 -}
type alias Peek =
    { entry : EntryRef
    , portrait : Maybe String
    , lines : List ( String, String )
    }


type alias FieldLine =
    { label : String
    , value : FieldValue
    }


{-| 攻略本の右ページ。title(名前)・portrait(肖像)・flavor(解説)は
額装の主役なので fields から抜き出して別枠にする。
-}
type alias Detail =
    { entry : EntryRef
    , title : Maybe String
    , portrait : Maybe String
    , flavor : Maybe String
    , fields : List FieldLine
    }


type alias ViewModel =
    { entries : List EntryItem
    , detail : Maybe Detail
    }


{-| プラグイン 1 件 = 文書一式 → 表示モデル。Html を作らないのは、
クリックの意味(選択・ジャンプ)を Main のメッセージに握らせたままにするため。
-}
type alias Plugin =
    { id : String
    , view : Args -> ViewModel
    }


plugins : List Plugin
plugins =
    [ generic ]


find : String -> Maybe Plugin
find id =
    plugins
        |> List.filter (\p -> p.id == id)
        |> List.head


{-| 汎用既定: 主リソース(uses の先頭)のエントリ一覧と、選択エントリの
攻略本ページ。ref フィールドは参照先エントリの要約(プレビュー)に展開する。
-}
generic : Plugin
generic =
    { id = "generic"
    , view =
        \args ->
            let
                main =
                    args.uses |> List.head |> Maybe.withDefault ""
            in
            { entries = entriesOf main args.docs
            , detail =
                args.selected
                    |> Maybe.andThen
                        (\id ->
                            lookup main id args.docs
                                |> Maybe.map (\( d, entry ) -> detailOf args.docs d id entry)
                        )
            }
    }


{-| リソースの全ファイルを横断したエントリ一覧(各ファイルの文書順のまま)。 -}
entriesOf : String -> List SourceDoc -> List EntryItem
entriesOf resource docs =
    docs
        |> List.filter (\d -> d.resource == resource)
        |> List.concatMap
            (\d ->
                let
                    dict =
                        Doc.catalog resource d.doc

                    section =
                        sectionOf d
                in
                Doc.catalogKeys resource d.doc
                    |> List.map
                        (\id ->
                            let
                                entry =
                                    Dict.get id dict |> Maybe.withDefault E.null
                            in
                            { entry = { resource = resource, path = d.path, id = id }
                            , name = titleFieldOf section |> Maybe.andThen (\name -> stringField name entry)
                            , portrait = portraitFieldOf section |> Maybe.andThen (\name -> stringField name entry)
                            }
                        )
            )


{-| id のエントリを持つ文書を探す(複数ファイルにまたがる時は先勝ち)。 -}
lookup : String -> String -> List SourceDoc -> Maybe ( SourceDoc, D.Value )
lookup resource id docs =
    docs
        |> List.filter (\d -> d.resource == resource)
        |> List.filterMap
            (\d ->
                Doc.catalog resource d.doc
                    |> Dict.get id
                    |> Maybe.map (\entry -> ( d, entry ))
            )
        |> List.head


{-| その文書の中身を定義するセクション(セクション名=リソース id の項)。 -}
sectionOf : SourceDoc -> Maybe Schema.Section
sectionOf d =
    d.schema
        |> Maybe.andThen
            (\schema ->
                schema.sections
                    |> List.filter (\( key, _ ) -> key == d.resource)
                    |> List.head
                    |> Maybe.map Tuple.second
            )


{-| 攻略本ページの導出。スキーマがあれば額装 3 点(名前・肖像・解説)を抜き出し、
残りを宣言順のフィールド行に。無ければ書いてある順の生のキーで並べる —
どちらでも閲覧は成立させる。
-}
detailOf : List SourceDoc -> SourceDoc -> String -> D.Value -> Detail
detailOf docs d id entry =
    let
        ref =
            { resource = d.resource, path = d.path, id = id }
    in
    case sectionOf d of
        Just section ->
            let
                titleName =
                    titleFieldOf (Just section)

                portraitName =
                    portraitFieldOf (Just section)

                flavorName =
                    flavorFieldOf (Just section)

                framed =
                    List.filterMap identity [ titleName, portraitName, flavorName ]
            in
            { entry = ref
            , title = titleName |> Maybe.andThen (\name -> stringField name entry)
            , portrait = portraitName |> Maybe.andThen (\name -> stringField name entry)
            , flavor = flavorName |> Maybe.andThen (\name -> stringField name entry)
            , fields =
                orderedFields section
                    |> List.filter (\( name, _ ) -> not (List.member name framed))
                    |> List.map
                        (\( name, field ) ->
                            { label = Maybe.withDefault name field.label
                            , value = valueOf docs field name entry
                            }
                        )
            }

        Nothing ->
            { entry = ref
            , title = Nothing
            , portrait = Nothing
            , flavor = Nothing
            , fields =
                rawLines entry
                    |> List.map (\( name, shown ) -> { label = name, value = Plain shown })
            }


{-| フィールドをフォーム・表と同じ order 順で(未指定は末尾・同順位は書いた順)。 -}
orderedFields : Schema.Section -> List ( String, Schema.Field )
orderedFields section =
    section.fields
        |> List.sortBy (\( _, field ) -> Maybe.withDefault (2 ^ 30) field.order)


{-| 額装 3 点の抜き出し規約: 名前=最初の素の text(uiDoc / multiline を除く)、
肖像=最初の widget uiDoc、解説=最初の widget multiline。
-}
titleFieldOf : Maybe Schema.Section -> Maybe String
titleFieldOf section =
    firstField section
        (\field ->
            field.type_
                == TText
                && not (Schema.widgetIs "uiDoc" field.widget)
                && not (Schema.widgetIs "multiline" field.widget)
        )


portraitFieldOf : Maybe Schema.Section -> Maybe String
portraitFieldOf section =
    firstField section (\field -> field.type_ == TText && Schema.widgetIs "uiDoc" field.widget)


flavorFieldOf : Maybe Schema.Section -> Maybe String
flavorFieldOf section =
    firstField section (\field -> field.type_ == TText && Schema.widgetIs "multiline" field.widget)


firstField : Maybe Schema.Section -> (Schema.Field -> Bool) -> Maybe String
firstField section matches =
    section
        |> Maybe.map orderedFields
        |> Maybe.withDefault []
        |> List.filter (\( _, field ) -> matches field)
        |> List.head
        |> Maybe.map Tuple.first


{-| フィールド 1 個の見せ方をスキーマの宣言から決める。値が宣言と違う形の時は
Plain(生の 1 行)へ倒す — 壊れた値でページ全体を止めない。
-}
valueOf : List SourceDoc -> Schema.Field -> String -> D.Value -> FieldValue
valueOf docs field name entry =
    let
        cellText_ =
            cellText name entry

        number =
            D.decodeValue (D.field name D.float) entry |> Result.toMaybe
    in
    case field.type_ of
        TRef target ->
            case stringField name entry of
                Just id ->
                    RefValue { target = target, id = id, peek = peekOf docs target id }

                Nothing ->
                    Plain cellText_

        TText ->
            case ( Schema.widgetIs "multiline" field.widget, stringField name entry ) of
                ( True, Just long ) ->
                    LongText long

                _ ->
                    Plain cellText_

        TCustom "weights" ->
            case ( Weights.configFrom field.widget, weightPairs name entry ) of
                ( Just config, Just entries ) ->
                    WeightBars { total = config.total, entries = entries }

                _ ->
                    Plain cellText_

        _ ->
            case ( number, percentFactor field ) of
                ( Just v, Just factor ) ->
                    Percent
                        { ratio = clamp 0 1 (v * factor / 100)
                        , text = percentText (v * factor)
                        }

                ( Just v, Nothing ) ->
                    case ( field.min, field.max ) of
                        ( Just lo, Just hi ) ->
                            Meter { value = v, min = lo, max = hi, text = cellText_ }

                        _ ->
                            Plain cellText_

                _ ->
                    Plain cellText_


{-| % 併記の倍率(SchemaForm の numberControl と同じ規約)。 -}
percentFactor : Schema.Field -> Maybe Float
percentFactor field =
    if Schema.widgetIs "unit" field.widget then
        Just 100

    else if Schema.widgetIs "percent" field.widget then
        Just 1

    else
        Nothing


percentText : Float -> String
percentText shown =
    let
        rounded =
            toFloat (round (shown * 10)) / 10
    in
    (if rounded == toFloat (round rounded) then
        String.fromInt (round rounded)

     else
        String.fromFloat rounded
    )
        ++ "%"


{-| プレビュー: 参照先エントリの要約(Nothing = ぶら下がり)。肖像(uiDoc)は
行でなくサムネの席へ。参照先の文書にもスキーマが無いことはあり得るので、
要約行の作り方は detailOf と同じ 2 段構え。
-}
peekOf : List SourceDoc -> String -> String -> Maybe Peek
peekOf docs target id =
    lookup target id docs
        |> Maybe.map
            (\( d, entry ) ->
                let
                    section =
                        sectionOf d

                    portraitName =
                        portraitFieldOf section
                in
                { entry = { resource = target, path = d.path, id = id }
                , portrait = portraitName |> Maybe.andThen (\name -> stringField name entry)
                , lines =
                    case section of
                        Just s ->
                            Table.columns s
                                |> List.filter (\col -> Just col.name /= portraitName)
                                |> List.map (\col -> ( col.label, cellText col.name entry ))

                        Nothing ->
                            rawLines entry
                }
            )


{-| スキーマ無しの生の要約行(書いてある順・"//" アノテーションは飛ばす)。 -}
rawLines : D.Value -> List ( String, String )
rawLines entry =
    D.decodeValue (D.keyValuePairs D.value) entry
        |> Result.withDefault []
        |> List.filter (\( name, _ ) -> not (String.startsWith "//" name))
        |> List.map (\( name, _ ) -> ( name, cellText name entry ))


cellText : String -> D.Value -> String
cellText name entry =
    (Table.cell name entry).text


stringField : String -> D.Value -> Maybe String
stringField name entry =
    D.decodeValue (D.field name D.string) entry |> Result.toMaybe


{-| キー→数値のオブジェクトだけ受ける("//" アノテーションキーは文書の流儀どおり読み飛ばす)。
数値でない値が混ざれば Nothing(Plain に倒す)。
-}
weightPairs : String -> D.Value -> Maybe (List ( String, Float ))
weightPairs name entry =
    D.decodeValue (D.field name (D.keyValuePairs D.value)) entry
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
