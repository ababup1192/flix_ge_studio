module Refs exposing
    ( DocEdit(..)
    , Entry(..)
    , PathSeg(..)
    , UsageSite
    , refRewriteEdits
    , renameEdits
    , renameProblem
    , usageKey
    , usages
    )

{-| 逆参照(どの id がどこから使われているか)と、id 改名の編集列を導く純関数。

対象は開いている 1 文書の中だけ。文書横断(別ファイルからの ref)は次段の
ダッシュボード基盤と一緒にやる — その時はこの Dict のキー("セクション/id")の
前にファイル名を足す形で広げられるよう、キーは文字列 1 本にしてある。

-}

import Dict exposing (Dict)
import Doc
import Json.Decode as D
import Json.Encode as E
import Schema exposing (FieldType(..))


{-| 使用箇所のエントリの指し先。catalog は id・list は添字(record セクションは Nothing)。 -}
type Entry
    = AtKey String
    | AtIndex Int


{-| 参照している側の場所。elem は {"list": {"ref": ...}} の何番目か(単値の ref は Nothing)。 -}
type alias UsageSite =
    { sectionKey : String
    , entry : Maybe Entry
    , fieldName : String
    , elem : Maybe Int
    }


{-| 文書の 1 段(docEdit のパスと同じ意味)。 -}
type PathSeg
    = Key String
    | Idx Int


{-| docEdit へ渡す編集 1 件。SetString は値の書き換え・RenameKey はキー自体の改名。 -}
type DocEdit
    = RenameKey (List PathSeg) String
    | SetString (List PathSeg) String


{-| 逆参照インデックスのキー。id に "/" が入ると別セクションと衝突し得るが、
id は半角英数の運用(ウィザードの検証もそう)なので背負わない。
-}
usageKey : String -> String -> String
usageKey sectionKey id =
    sectionKey ++ "/" ++ id


{-| 文書全体を 1 回歩いて「セクション/id → 使われている場所の列」を作る。
場所は文書に書いてある順のまま(一覧をそのまま出せる形)。
-}
usages : Schema.Schema -> D.Value -> Dict String (List UsageSite)
usages schema doc =
    schema.sections
        |> List.concatMap (\( key, section ) -> sectionSites doc key section)
        |> List.foldr
            (\( key, site ) acc ->
                Dict.update key (\found -> Just (site :: Maybe.withDefault [] found)) acc
            )
            Dict.empty


sectionSites : D.Value -> String -> Schema.Section -> List ( String, UsageSite )
sectionSites doc key section =
    case section.kind of
        Schema.RecordKind ->
            entrySites key Nothing section (Doc.record key doc)

        Schema.Catalog ->
            let
                dict =
                    Doc.catalog key doc
            in
            Doc.catalogKeys key doc
                |> List.concatMap
                    (\name ->
                        entrySites key
                            (Just (AtKey name))
                            section
                            (Dict.get name dict |> Maybe.withDefault E.null)
                    )

        Schema.ListKind ->
            Doc.list key doc
                |> List.indexedMap (\i entry -> entrySites key (Just (AtIndex i)) section entry)
                |> List.concat

        -- 単一値セクションは今の方言で ref を持たない(数える場所も無い)
        Schema.ValueKind _ ->
            []

        -- フォーム未対応 kind の中身は読まない(fields も持っていない)
        Schema.Unsupported _ ->
            []


entrySites : String -> Maybe Entry -> Schema.Section -> D.Value -> List ( String, UsageSite )
entrySites sectionKey entry section value =
    section.fields
        |> List.concatMap (\( name, field ) -> fieldSites sectionKey entry name field.type_ value)


{-| ref を持てるのは単値の ref と list の ref の 2 形だけ。それより深い入れ子
(list の list・record 内の ref)は追わない — 今のスキーマ方言のフォームが
出せるのも 1 段までで、数えても指し示せない場所になるため。
-}
fieldSites : String -> Maybe Entry -> String -> FieldType -> D.Value -> List ( String, UsageSite )
fieldSites sectionKey entry fieldName type_ entryValue =
    case type_ of
        TRef target ->
            D.decodeValue (D.field fieldName D.string) entryValue
                |> Result.toMaybe
                |> Maybe.map
                    (\chosen ->
                        [ ( usageKey target chosen
                          , { sectionKey = sectionKey, entry = entry, fieldName = fieldName, elem = Nothing }
                          )
                        ]
                    )
                |> Maybe.withDefault []

        TList (TRef target) ->
            D.decodeValue (D.field fieldName (D.list D.value)) entryValue
                |> Result.withDefault []
                |> List.indexedMap
                    (\j raw ->
                        D.decodeValue D.string raw
                            |> Result.toMaybe
                            |> Maybe.map
                                (\chosen ->
                                    ( usageKey target chosen
                                    , { sectionKey = sectionKey, entry = entry, fieldName = fieldName, elem = Just j }
                                    )
                                )
                    )
                |> List.filterMap identity

        _ ->
            []


{-| 改名を拒む理由(Nothing = 通してよい)。重なり検査は今の文書の catalog キーと。 -}
renameProblem : D.Value -> { sectionKey : String, oldId : String, newId : String } -> Maybe String
renameProblem doc { sectionKey, oldId, newId } =
    if String.trim newId == "" then
        Just "id が空です"

    else if newId == oldId then
        Just "元の id と同じです"

    else if List.member newId (Doc.catalogKeys sectionKey doc) then
        Just ("\"" ++ newId ++ "\" は既にあります")

    else
        Nothing


{-| 改名 1 回ぶんの編集列: 先頭がキー自体の改名、続いて参照している全フィールドの
値書き換え。1 バッチで当てる前提の並び — 途中だけ当てると参照が割れる。
-}
renameEdits : Schema.Schema -> D.Value -> { sectionKey : String, oldId : String, newId : String } -> List DocEdit
renameEdits schema doc req =
    RenameKey [ Key req.sectionKey, Key req.oldId ] req.newId
        :: refRewriteEdits schema doc req


{-| 参照の値書き換えだけの編集列(キー改名は含まない)。改名される id が別ファイルに
住む時、参照している側の文書にはこちらを当てる — キーはその文書に無い。
-}
refRewriteEdits : Schema.Schema -> D.Value -> { sectionKey : String, oldId : String, newId : String } -> List DocEdit
refRewriteEdits schema doc { sectionKey, oldId, newId } =
    usages schema doc
        |> Dict.get (usageKey sectionKey oldId)
        |> Maybe.withDefault []
        |> List.map (\site -> SetString (sitePath site) newId)


sitePath : UsageSite -> List PathSeg
sitePath site =
    List.concat
        [ [ Key site.sectionKey ]
        , case site.entry of
            Just (AtKey name) ->
                [ Key name ]

            Just (AtIndex i) ->
                [ Idx i ]

            Nothing ->
                []
        , [ Key site.fieldName ]
        , case site.elem of
            Just j ->
                [ Idx j ]

            Nothing ->
                []
        ]
