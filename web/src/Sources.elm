module Sources exposing
    ( ExternalUsage
    , SourceDoc
    , externalUsages
    , refChoices
    )

{-| 文書横断の参照解決(開いている文書の外に住む catalog を引く)純ロジック。

enemies の weapon が weapons.json を指すように、ref の参照先セクションは
別ファイルに住み得る。ここは「同一文書 → 無ければ横断辞書」の 2 段解決と、
逆向き(他文書からこの文書への参照)の索引だけを持つ — どのファイルを
いつ読むか(取得)はホスト(Main)の仕事。

-}

import Dict exposing (Dict)
import Doc
import Json.Decode as D
import Refs
import Schema


{-| 読み込み済みの他文書 1 件。resource はセクション名でもある
(1 ファイル 1 セクション・セクション名=リソース id の規約)。
-}
type alias SourceDoc =
    { resource : String
    , path : String
    , doc : D.Value
    , schema : Maybe Schema.Schema
    }


{-| ref の選択肢の 2 段解決: 開いている文書に target セクションのエントリが
あればそれが正、無ければ横断辞書から引く。両方を混ぜないのは、選ばれた id が
どのファイルの持ち物か(改名や lint の当て先)を曖昧にしないため。
-}
refChoices : String -> D.Value -> List SourceDoc -> List String
refChoices target doc others =
    case Doc.catalogKeys target doc of
        [] ->
            others
                |> List.filter (\d -> d.resource == target)
                |> List.concatMap (\d -> Doc.catalogKeys target d.doc)
                |> dedupe

        own ->
            own


{-| 他文書からの使用箇所 1 件。site の指し先はその文書(path)の中の場所。 -}
type alias ExternalUsage =
    { path : String
    , resource : String
    , site : Refs.UsageSite
    }


{-| 他文書を各自のスキーマで歩き、「セクション/id → 他文書からの使用箇所」を作る。
スキーマの無い文書は数えない(どのフィールドが ref かが分からない)。
-}
externalUsages : List SourceDoc -> Dict String (List ExternalUsage)
externalUsages others =
    others
        |> List.concatMap
            (\d ->
                case d.schema of
                    Just schema ->
                        Refs.usages schema d.doc
                            |> Dict.toList
                            |> List.concatMap
                                (\( key, sites ) ->
                                    sites
                                        |> List.map
                                            (\site ->
                                                ( key, { path = d.path, resource = d.resource, site = site } )
                                            )
                                )

                    Nothing ->
                        []
            )
        |> List.foldr
            (\( key, usage ) acc ->
                Dict.update key (\found -> Just (usage :: Maybe.withDefault [] found)) acc
            )
            Dict.empty


{-| 順序を保ったまま重複を落とす(同じリソースが複数ファイルに割れている時)。 -}
dedupe : List String -> List String
dedupe items =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        items
