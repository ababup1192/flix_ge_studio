module DocKind exposing (isKnob, playableSound, soundsFrom)

{-| 開いている文書が「どういう種類の物か」を型で判じる問い(純ロジック)。

ファイル名やそのゲーム独自の言葉では判じない。名前は人が付けた見出しで、
別のゲームでは別の言葉になる。判じるのは宣言の形だけ:

  - 盤面(grid 欄を持つ)… 絵で編集する物。専用エディタの担当
  - ドット絵 … 同じく専用エディタの担当(判定は呼び側が持つ)
  - それ以外 … つまみと一覧をフォームで触る物。2 ペイン常設の担当

音が鳴らせるかも同じ流儀で決める: project.json が「この名前の音がある」と
宣言していれば鳴らせる。文書の中の名前(セクション名・エントリ名)がその宣言に
一致した時だけ ▶ を出す。

-}

import Json.Decode as D
import Schema exposing (Schema)


{-| つまみ系の文書(2 ペイン常設の対象)か。

盤面とドット絵は自前の絵エディタが主役なので外す。フォームにできるセクションが
1 つも無い文書(未対応 kind だけ)も外す — 触れる物が無い所に 2 ペインを出さない。

-}
isKnob : { schema : Maybe Schema, isSprite : Bool, supported : Int } -> Bool
isKnob doc =
    case ( doc.schema, doc.isSprite ) of
        ( Just schema, False ) ->
            not (Schema.hasGridField schema) && doc.supported > 0

        _ ->
            False


{-| project.json の sounds 宣言にある名前(= 焼かれた WAV がある物)。 -}
soundsFrom : String -> List String
soundsFrom content =
    D.decodeString (D.field "sounds" (D.list (D.field "name" D.string))) content
        |> Result.withDefault []


{-| いま選んでいる場所の名前が、宣言された音のどれかなら「鳴らせる」。
効果音のつまみ(セクション名 = 音の名前)も、曲の一覧(エントリ名 = 曲の名前)も
同じ 1 本の規則で拾える。
-}
playableSound : List String -> Maybe String -> Maybe String
playableSound declared name =
    case name of
        Just n ->
            if List.member n declared then
                Just n

            else
                Nothing

        Nothing ->
            Nothing
