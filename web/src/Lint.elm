module Lint exposing (Problem, Target(..), check, checkAcross)

{-| スキーマと文書を突き合わせて問題の一覧を作る純関数。

問題があっても保存は止めない — ここは「気づかせる」だけで、直すかどうか・いつ直すかは
書き手に任せる(作業途中の不完全な状態で保存できないエディタは使われなくなる)。

WhyNot: 重複 id の検出はここではやらない。パース済みの JSON では同名キーが後勝ちで
1 つに畳まれていて、この層からはもう見えない。生テキストを走査できる jsonc 側
(docEdit)に足すのが次段の仕事。

-}

import Dict
import Doc
import Json.Decode as D
import Json.Encode as E
import Schema exposing (FieldType(..))
import Sources


{-| 問題の指すエントリ。catalog は id・list は元配列の添字(record セクションは Nothing)。 -}
type Target
    = AtKey String
    | AtIndex Int


type alias Problem =
    { sectionKey : String
    , entry : Maybe Target
    , field : Maybe String
    , message : String
    }


check : Schema.Schema -> D.Value -> List Problem
check =
    checkAcross []


{-| 横断辞書つきの検査。ref の参照先セクションが別ファイルに住むプロジェクト
(enemies の weapon → weapons.json)は、others を渡すとぶら下がり誤検知が消える。
-}
checkAcross : List Sources.SourceDoc -> Schema.Schema -> D.Value -> List Problem
checkAcross others schema doc =
    schema.sections
        |> List.concatMap (\( key, section ) -> checkSection others doc key section)


checkSection : List Sources.SourceDoc -> D.Value -> String -> Schema.Section -> List Problem
checkSection others doc key section =
    case section.kind of
        Schema.RecordKind ->
            checkEntry others doc key Nothing (Doc.record key doc) section

        Schema.Catalog ->
            let
                dict =
                    Doc.catalog key doc
            in
            Doc.catalogKeys key doc
                |> List.concatMap
                    (\name ->
                        checkEntry others
                            doc
                            key
                            (Just (AtKey name))
                            (Dict.get name dict |> Maybe.withDefault E.null)
                            section
                    )

        Schema.ListKind ->
            Doc.list key doc
                |> List.indexedMap (\i entry -> checkEntry others doc key (Just (AtIndex i)) entry section)
                |> List.concat

        -- 単一値セクションの検査は持たない(型・可動域は編集フォーム側が守る)
        Schema.ValueKind _ ->
            []

        -- フォーム未対応 kind は検査しない(fields を持たず、指摘しても場所を示せない)
        Schema.Unsupported _ ->
            []


checkEntry : List Sources.SourceDoc -> D.Value -> String -> Maybe Target -> D.Value -> Schema.Section -> List Problem
checkEntry others doc sectionKey target entry section =
    let
        loc =
            locLabel sectionKey target

        knownNames =
            List.map Tuple.first section.fields

        fieldProblems =
            section.fields
                |> List.concatMap (\( name, field ) -> checkField others doc loc sectionKey target entry name field)

        unknownKeyProblems =
            entryKeys entry
                |> List.filter (\k -> not (List.member k knownNames))
                |> List.map
                    (\k ->
                        { sectionKey = sectionKey
                        , entry = target
                        , field = Just k
                        , message = loc ++ " にスキーマに無いキー \"" ++ k ++ "\" があります"
                        }
                    )
    in
    fieldProblems ++ unknownKeyProblems


{-| 文言の場所書き(「spawns #3」「routes "wavy"」)。message を 1 行で自己完結させ、
一覧でそのまま読める形にする。
-}
locLabel : String -> Maybe Target -> String
locLabel sectionKey target =
    case target of
        Nothing ->
            sectionKey

        Just (AtKey name) ->
            sectionKey ++ " \"" ++ name ++ "\""

        Just (AtIndex i) ->
            sectionKey ++ " #" ++ String.fromInt i


{-| "//" 始まりは注釈キー(スキーマ・文書共通の流儀)なので検査対象にしない。 -}
entryKeys : D.Value -> List String
entryKeys entry =
    D.decodeValue (D.keyValuePairs D.value) entry
        |> Result.withDefault []
        |> List.map Tuple.first
        |> List.filter (\k -> not (String.startsWith "//" k))


checkField : List Sources.SourceDoc -> D.Value -> String -> String -> Maybe Target -> D.Value -> String -> Schema.Field -> List Problem
checkField others doc loc sectionKey target entry name field =
    case D.decodeValue (D.field name D.value) entry of
        Err _ ->
            if field.required then
                [ { sectionKey = sectionKey
                  , entry = target
                  , field = Just name
                  , message =
                        loc
                            ++ " の "
                            ++ name
                            ++ " が書かれていません"
                            ++ (if field.default /= Nothing then
                                    "(既定値あり)"

                                else
                                    ""
                               )
                  }
                ]

            else
                []

        Ok value ->
            refProblems others doc loc sectionKey target name field value
                ++ rangeProblems loc sectionKey target name field value


{-| ぶら下がり参照: ref の値が参照先 catalog(同一文書 → 無ければ横断辞書)に無い。
値が文字列でない時はここでは黙る — 型の間違いまで背負うと文言が濁る。
-}
refProblems : List Sources.SourceDoc -> D.Value -> String -> String -> Maybe Target -> String -> Schema.Field -> D.Value -> List Problem
refProblems others doc loc sectionKey target name field value =
    case ( field.type_, D.decodeValue D.string value ) of
        ( TRef refTarget, Ok chosen ) ->
            if List.member chosen (Sources.refChoices refTarget doc others) then
                []

            else
                [ { sectionKey = sectionKey
                  , entry = target
                  , field = Just name
                  , message = loc ++ " の " ++ name ++ " \"" ++ chosen ++ "\" は " ++ refTarget ++ " に居ません"
                  }
                ]

        _ ->
            []


rangeProblems : String -> String -> Maybe Target -> String -> Schema.Field -> D.Value -> List Problem
rangeProblems loc sectionKey target name field value =
    case ( D.decodeValue D.float value, ( field.min, field.max ) ) of
        ( _, ( Nothing, Nothing ) ) ->
            []

        ( Err _, _ ) ->
            []

        ( Ok v, ( minVal, maxVal ) ) ->
            let
                below =
                    minVal |> Maybe.map (\m -> v < m) |> Maybe.withDefault False

                above =
                    maxVal |> Maybe.map (\m -> v > m) |> Maybe.withDefault False
            in
            if below || above then
                [ { sectionKey = sectionKey
                  , entry = target
                  , field = Just name
                  , message =
                        loc
                            ++ " の "
                            ++ name
                            ++ " は "
                            ++ numberText v
                            ++ " で範囲("
                            ++ rangeText minVal maxVal
                            ++ ")の外です"
                  }
                ]

            else
                []


rangeText : Maybe Float -> Maybe Float -> String
rangeText minVal maxVal =
    case ( minVal, maxVal ) of
        ( Just lo, Just hi ) ->
            numberText lo ++ "〜" ++ numberText hi

        ( Just lo, Nothing ) ->
            numberText lo ++ " 以上"

        ( Nothing, Just hi ) ->
            numberText hi ++ " 以下"

        ( Nothing, Nothing ) ->
            ""


numberText : Float -> String
numberText n =
    if n == toFloat (round n) then
        String.fromInt (round n)

    else
        String.fromFloat n
