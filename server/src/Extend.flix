// Extend — 「ゲームを広げる」のプロンプト。パラメータ調整やアセットの切り替えと違い、
// ゲームの構造 (コードと Doc の形) が変わる仕事なので、Studio は手を動かさず、
// 骨格・規約・完了条件を焼き込んだプロンプトを作って AI へ渡す (Genesis の公式
// プロンプトと同じ流儀)。入口は 3 口: 場面を足す / メカニクスを足す / アセットの種類を足す。
// 骨格はどのジャンルのゲームにも通る言い方だけで書く (具体物の例を焼き込まない —
// 焼き込むと別ジャンルのゲームで嘘の道しるべになる)。
mod Extend {
    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    /// 広げ方 1 口ぶんの定義。id は API の kind、title は画面に出す名前。
    /// steps は【やること】、doneExtras は口ごとの追加の完了条件、hint は
    /// 【あなたの言葉】に添える書き出しの手引き。
    pub type alias Kind = {
        id = String,
        title = String,
        opening = String,
        steps = List[String],
        doneExtras = List[String],
        hint = String
    }

    /// 3 口。並びは画面の並びでもある (場面 → メカニクス → アセットの種類)。
    pub def kinds(): List[Kind] =
           { id = "scene", title = "場面を足す",
             opening = "このゲームに新しい場面を 1 つ足してください。",
             steps =
                    "- 新しい場面 (独立した画面・状態) を 1 つ設計して足す。"
                 :: "- どこから入り、どこへ抜けるか (既存の進行とのつなぎ) を決めて配線する。"
                 :: "- 場面の進行 (始まり・終わり・分かれ道) はコードに、調整したい値や配置は Doc に置く。"
                 :: Nil,
             doneExtras = Nil,
             hint = "(どんな場面か — そこで何が起きて、どう終わるか — をここに書いてください)" }
        :: { id = "mechanic", title = "メカニクスを足す",
             opening = "このゲームに新しいメカニクス (ルール・機能) を 1 つ足してください。",
             steps =
                    "- 新しいルール・メカニクスを 1 つ設計して足す (いつ起きて、何が変わるかを決める)。"
                 :: "- 既存のルールとのぶつかり (順番・優先・例外) を洗い出して整合させる。"
                 :: "- 判定・進行はコードに、調整したい数値 (閾値・確率・収支など) は Doc に置く。"
                 :: Nil,
             doneExtras = Nil,
             hint = "(どんなメカニクスか — 何をすると、何がどう変わるか — をここに書いてください)" }
        :: { id = "material", title = "アセットの種類を足す",
             opening = "このゲームに新しい種類のアセットを 1 つ足してください。",
             steps =
                    "- 新しいアセットの種類を 1 組つくる: Doc (assets/ の JSON) + schema + 読み込みコード。"
                 :: "- 読み込みは fail-open (欠け・壊れは既定値、既定値はコード側)、watchFile で保存即反映にする。"
                 :: "- 新しいアセットをゲームの見た目・進行から実際に使う (置いただけの死にデータにしない)。"
                 :: Nil,
             doneExtras =
                    "- project.json の editor.resources に題 (title) と role を登録し、アトリエに並ぶこと。"
                 :: Nil,
             hint = "(どんな種類のアセットか — 何を表し、ゲームのどこで使うか — をここに書いてください)" }
        :: Nil

    /// GET /prompt/extend?kind=… の応答 (status, body)。ゲームの題名をプロンプトに
    /// 焼き込む (AI が対象を取り違えないように) ため、プロジェクト選択の下で呼ぶ。
    pub def promptJson(dir: String, params: Map[String, String]): (Int32, Json) \ IO =
        respond(Projects.titleOf(dir), dir, params)

    /// promptJson の純粋な本体 (テストはここを叩く)。不明な kind は 400。
    pub def respond(title: String, dir: String, params: Map[String, String]): (Int32, Json) =
        match Map.get("kind", params) {
            case None => (400, failJson("kind を指定してください (scene / mechanic / material)"))
            case Some(id) => match List.findLeft(k -> k#id == id, kinds()) {
                case None => (400, failJson("知らない広げ方です: ${id} (scene / mechanic / material)"))
                case Some(k) => (200, JObject(Map#{
                    "title"  => JString(k#title),
                    "prompt" => JString(extendPrompt(k, title, dir))
                }))
            }
        }

    /// プロンプトの本体。頭で対象を固定し、【やること】【守ること】【完了条件】
    /// 【あなたの言葉】の 4 節で組む。あなたの言葉は末尾の空欄 — ユーザーが
    /// 書き足してから AI へ渡す場所なので、焼き込みの節より後に置く。
    pub def extendPrompt(k: Kind, gameTitle: String, dir: String): String =
        String.unlines(List.flatten(
               (k#opening
                :: "対象ゲーム: ${gameTitle}"
                :: "対象プロジェクト: ${dir}"
                :: Nil)
            :: ("" :: "【やること】" :: k#steps)
            :: rules()
            :: done(k#doneExtras)
            :: ("" :: "【あなたの言葉】" :: k#hint :: Nil)
            :: Nil))

    /// 【守ること】— 3 口とも同じ。構造をいじる仕事の共通規約。
    def rules(): List[String] =
           ""
        :: "【守ること】"
        :: "- エンジンの高水準 API を使う: 実装の前に engine の docs/module-index.md (やりたいこと → モジュールの逆引き) を読み、エンジンにある物を自前で書かない。"
        :: "- 繰り返し調整する値は Doc へ: 1 種類 = 1 Doc + schema、fail-open (欠け・壊れは既定値)。"
        :: "- プレイヤーの目に入る文言は必ず Doc に置く (既定値はコード側)。コード直書きの表示文字列は違反。"
        :: "- 既存の Doc の形を壊さない (既存フィールドの改名・削除・意味変えをしない)。"
        :: "- 仕上げに /critique-code で自己批評し、指摘を潰してから報告する。"
        :: Nil

    /// 【完了条件】。extras は口ごとの追加条件 (material の登録確認など)。
    def done(extras: List[String]): List[String] =
        (""
         :: "【完了条件】"
         :: "- テスト全緑 (足したルール・進行のテストを足す。期待値は Doc から導く)。"
         :: "- render で見た目を確認する (決定的な場面を描き出して目視)。"
         :: Nil)
        ::: extras

    def failJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
