module Edit exposing
    ( Op(..)
    , Payload
    , Seg(..)
    , encodeSeg
    , fromRefsSeg
    , pathKey
    )

{-| 正本(jsonc テキスト)への編集 1 件の語彙。

パス(どこを)と op(どう書き換えるか)は、フォーム・盤面・改名・履歴の
どれもが同じ言葉で話す必要がある。Main に置くと編集を扱う側が Main を
呼び返すことになるので、語彙だけをここに切り出す。

書き換えの実体(jsonc の最小編集)は JS 側(docEdit.ts)の持ち場で、
ここは「何をどこへ」を運ぶ封筒の形だけを決める。

-}

import Json.Encode as E
import Refs


{-| 文書パスの 1 段。JSON の中の場所(オブジェクトキー / 配列添字)を指す。 -}
type Seg
    = KeySeg String
    | IdxSeg Int


{-| 編集の種類。Set=値の書き込み(無いキーは作る)・Append=配列末尾へ挿入・
Remove=キー/要素の削除・BatchSet=複数 set を 1 回のテキスト当てに畳む
(weights の連動書き戻し等。途中状態の文書を画面に見せないため)。
-}
type Op
    = SetOp
    | AppendOp
    | RemoveOp
    | BatchSetOp


type alias Payload =
    { op : Op
    , path : List Seg

    -- BatchSet では中身の編集列({op,path,value,intField} の配列)を encode 済みで持つ。
    -- path はその時「フィールドの場所」で、洪水を最新 1 件に畳む鍵にだけ使う
    , value : E.Value
    , isInt : Bool
    }


encodeSeg : Seg -> E.Value
encodeSeg seg =
    case seg of
        KeySeg key ->
            E.string key

        IdxSeg i ->
            E.int i


{-| パスの 1 行表記。画面の開閉状態の鍵や、履歴の見出しに使う
(同じ場所を指すパスは必ず同じ文字になる)。
-}
pathKey : List Seg -> String
pathKey path =
    path
        |> List.map
            (\seg ->
                case seg of
                    KeySeg key ->
                        key

                    IdxSeg i ->
                        String.fromInt i
            )
        |> String.join "/"


{-| 参照解決(Refs)のパス語彙からの翻訳。編集のパス型を 2 つ持ち回らないため。 -}
fromRefsSeg : Refs.PathSeg -> Seg
fromRefsSeg seg =
    case seg of
        Refs.Key key ->
            KeySeg key

        Refs.Idx i ->
            IdxSeg i
