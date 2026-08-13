// Genesis — 「新しいゲームをはじめる」のジャンル一覧と公式プロンプト。
//
// まっさらな画面は作らない: 入口は 8 ジャンル + フリージャンルの 9 枚で、並びは
// 昔の家庭用ゲーム機で人気だった順 (client はこの順のまま表示する)。
// テンプレートの品質は「1 ジャンル = 1 公式プロンプト」で担保する — 骨格(動詞・画面・
// 終わり方、可変値は Doc 指定)と規約・完了条件を焼き込み、ユーザーは
// 「あなたの言葉」(世界・主人公・エッセンス) だけを書き換えて詳細を詰める。
// 土台は常に templates/ のテンプレートの変形 (ゼロから生成しない)。良いテンプレートが
// 生まれたら templates/ へ昇格し、そのジャンルは starter 持ち(複製=ゼロ抽選)になる。
mod Genesis {
    import java.io.{File => JFile}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool}

    /// ジャンル 1 枚ぶんのカード。starter は公式テンプレート (templates/ 相対) があるときだけ非空。
    pub type alias Genre = {
        id = String,
        name = String,
        verb = String,
        includes = String,
        controls = String,
        skeleton = String,
        starter = String
    }

    /// 10 枚のカード。並びは仕様 (client は並べ替えない) — 先頭は blank(まっさらの
    /// 入口)、以降はジャンルの人気順、末尾は free(言葉からの受け皿)。
    /// 説明に実在のゲーム名は使わない (借り物なしで通じる言葉だけ)。
    pub def genres(): List[Genre] =
          { id = "blank",   name = "空のプロジェクト",
            verb = "まっさらから育てる",
            includes = "最小の骨組みだけ(主人公が歩くだけ)",
            controls = "←→↑↓",
            skeleton = "主人公(ドット絵)を ←→↑↓ で動かせるだけの骨組み。Doc(テーマ・ドット絵)の配線と check / test / bake が通る状態から、自分のゲームを育てる",
            starter = "templates/game-starter" }
       :: { id = "action",  name = "アクション",
            verb = "走って、跳んで、壊す",
            includes = "横スクロールジャンプ・ブロック崩し・ステージ攻略",
            controls = "←→ + スペース + B ダッシュ",
            skeleton = "横スクロールの丘・洞窟・塔を走って跳ぶ / B ダッシュで加速し、敵は上から踏んで倒す / 一方通行の床・動く床・消える床・バネ・中間ポイント(触るとハートが 1 つ戻る)。面は Doc の rows(1 文字 = 1 タイル)。終わり: 旗に触れて次の面へ",
            starter = "templates/platformer-starter" }
       :: { id = "rpg",     name = "RPG・アドベンチャー(見下ろし)",
            verb = "歩いて、話して、強くなる",
            includes = "コマンド型RPG・剣と探索・ダンジョン",
            controls = "←→↑↓ + 話す",
            skeleton = "見下ろしマップを ←→↑↓ で歩く / 人に触れると話す(1行ふきだし) / 扉と鍵 / 宝を拾う。終わり: 宝を N 個でクリア(N は Doc)",
            starter = "templates/rpg-starter" }
       :: { id = "sports",  name = "スポーツ・対戦",
            verb = "競って、勝つ",
            includes = "野球・サッカー・1対1の対戦格闘",
            controls = "←→ + ボタン",
            skeleton = "攻めと守りが交代する1対1(野球型): 投げる・打つ・捕る。終わり: 決めた回数で得点を比べる(回数は Doc)",
            starter = "" }
       :: { id = "race",    name = "レース",
            verb = "追い抜いて、一位に",
            includes = "見下ろしレース・ドリフト・接戦の追い抜き",
            controls = "←→ + ブレーキ + ドリフト + ニトロ",
            skeleton = "奥ほど詰まる疑似遠近の見下ろしコースを走る / ←→ で曲がり、押しっぱなしのボタンでドリフト(減速せず深く曲がるが外へ流れる) / 危ない走りでゲージが貯まり満タンで発動できるニトロ(速いが舵は半分) / ライバルは距離差で速さが変わり常に接戦へ戻る(乱数なし。コースは Doc の rows)。終わり: 規定周回を走り切り、最下位なら負け(周回数は Doc)",
            starter = "templates/race-starter" }
       :: { id = "puzzle",  name = "パズル",
            verb = "積んで、そろえて、消す",
            includes = "落ち物パズル・同色そろえ・連鎖",
            controls = "←→ + ↓ + 回転",
            skeleton = "上からピースが落ちる / ←→ で寄せ ↓ で落とし回して積む / 一列そろうと消える(落下速度・得点は Doc)。終わり: 積み上がったら負け",
            starter = "templates/tetris-starter" }
       :: { id = "shooter", name = "シューティング",
            verb = "よけて、撃つ",
            includes = "横スクロール・縦スクロール",
            controls = "←→ + スペース",
            skeleton = "縦スクロール / ←→で動きスペースで撃つ / 敵の波(並びは Doc の rows)。終わり: 全波を全滅",
            starter = "" }
       :: { id = "raise",   name = "育成・シミュレーション",
            verb = "育てて、見守る",
            includes = "生き物や選手の育成・街づくり",
            controls = "選ぶだけ",
            skeleton = "毎日選ぶ → 数値が育つ(伸びは Doc)/ ときどきイベント。終わり: N 日目の発表会(N は Doc)",
            starter = "" }
       :: { id = "novel",   name = "ノベル・推理",
            verb = "読んで、選んで、たどり着く",
            includes = "事件を追う推理もの・分岐するノベル",
            controls = "選ぶだけ",
            skeleton = "文章を読む → 選ぶ → 分岐する / 分岐と結末は Doc の rows。終わり: 結末のどれかへ",
            starter = "templates/novel-starter" }
       :: { id = "free",    name = "フリージャンル",
            verb = "言葉から始める",
            includes = "ここにない、全部",
            controls = "あなたが決める",
            skeleton = "",
            starter = "" }
       :: Nil

    /// GET /genesis/genres の応答。並び順ごと返す。
    pub def genresJson(): Json =
        JObject(Map#{"genres" => JArray(genres() |> List.map(genreJson) |> List.toVector)})

    def genreJson(f: Genre): Json =
        JObject(Map#{
            "id"       => JString(f#id),
            "name"     => JString(f#name),
            "verb"     => JString(f#verb),
            "includes" => JString(f#includes),
            "controls" => JString(f#controls),
            "starter"  => JString(f#starter)
        })

    /// GET /prompt/genesis?genre=…&world=…&hero=…&essence=…(&direction=… は free 用)。
    /// ジャンルは必須。free だけは direction (どんなゲームかの言葉) が必須。
    /// world/hero/essence は空なら例文プレースホルダのまま出す — 依頼文は
    /// ユーザーが書き換える前提の下書きなので、空欄でも 200 で返す。
    pub def promptJson(params: Map[String, String]): (Int32, Json) =
        match Map.get("genre", params) {
            case None => (400, failJson("genre を指定してください"))
            case Some(id) =>
                match List.filter(f -> f#id == id, genres()) |> List.head {
                    case None => (400, failJson("知らないジャンルです: ${id}"))
                    case Some(f) =>
                        if (f#id == "free")
                            match Map.get("direction", params) |> Option.filter(text -> text != "") {
                                case None => (400, failJson("direction を指定してください (どんなゲームかを言葉で)"))
                                case Some(direction) => (200, promptOk(freePrompt(direction)))
                            }
                        else
                            (200, promptOk(genrePrompt(f, params)))
                }
        }

    /// path が登録済みの公式テンプレートか (new-game の複製元の白名簿)。
    pub def isStarter(path: String): Bool =
        path != "" and (genres() |> List.exists(f -> f#starter == path))

    /// GET /genesis/title?genre=… — ジャンルカードの絵。公式テンプレートの
    /// golden/title.png のバイト列。テンプレート無し・絵無し・engine 不在は None
    /// (呼び手が 404 = 絵なしカードに倒す)。
    pub def titleBytes(params: Map[String, String]): Option[Array[Int8, Static]] \ IO =
        let starter = Map.get("genre", params)
            |> Option.flatMap(id -> List.filter(f -> f#id == id, genres()) |> List.head)
            |> Option.map(f -> f#starter)
            |> Option.filter(path -> path != "");
        match starter {
            case None => None
            case Some(path) =>
                let base = "${Scaffold.engineDir()}/${path}/golden";
                let title = new JFile("${base}/title.png");
                let pick = if (title.isFile()) Some(title) else firstGoldenPng(base);
                match pick {
                    case Some(file) => Some(Files.readAllBytes(file.toPath()))
                    case None       => None
                }
        }

    /// golden/ の中の絵。title.png が無い starter でも最初の .png（名前順）を顔にして、
    /// 空絵に倒れないための安全網（新テンプレで title.png を焼き忘れてもカードが絵を持つ）。
    /// 名前取り出し（getName は IO）は先に済ませ、並べ替えは純粋な文字列で行う。
    def firstGoldenPng(dir: String): Option[JFile] \ IO =
        let entries = new JFile(dir).listFiles();
        if (Object.isNull(entries)) None
        else {
            let named = Array.toList(entries)
                |> List.filter(file -> file.isFile())
                |> List.map(file -> (file.getName(), file))
                |> List.filter(pair -> String.endsWith(suffix = ".png", fst(pair)));
            named |> List.sortBy(fst) |> List.map(snd) |> List.head
        }

    def promptOk(text: String): Json =
        JObject(Map#{"prompt" => JString(text)})

    def failJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})

    def wordOr(key: String, fallback: String, params: Map[String, String]): String =
        Map.get(key, params)
            |> Option.filter(text -> text != "")
            |> Option.getWithDefault(fallback)

    /// ジャンルの公式プロンプト。上半分 = 焼き込み(骨格・規約・完了条件)、
    /// 下半分 = あなたの言葉(書き換えて詳細を詰める欄)。
    pub def genrePrompt(f: Genre, params: Map[String, String]): String =
        String.unlines(
            "新しいゲームのテンプレートを作ってください。"
            :: "土台: templates/game-starter を変形(ゼロから書かない)"
            :: "ジャンル: ${f#name}"
            :: "骨格: ${f#skeleton}"
            :: "守ること: AGENTS.md と .claude/skills の規約に従う。ルール・当たり判定・進行はコード、見た目と数値は Doc(material / tuning を仕分け)。プレイヤーの目に入る文言は必ず Doc に置く(既定値はコード側。コード直書きの表示文字列は違反)。テストはルール・進行・収支だけ(期待値は Doc から導く)。"
            :: "エンジン API 優先: 実装の前に engine の docs/module-index.md(やりたいこと → モジュールの逆引き)を引き、エンジンにある物(UI・スクロール・文字折り・当たり判定等)を自前で書かない。"
            :: "完了条件: make check / make test を緑に、golden を作って make bake まで。報告の前に /critique-code で自己批評し、指摘を潰す。gallery の絵を見せて報告する。"
            :: ""
            :: "--- ここから あなたの言葉(書き換えて詳細を) ---"
            :: "世界: ${wordOr("world", "(例: 夜の温泉街)", params)}"
            :: "主人公: ${wordOr("hero", "(例: ちょうちんを持った子ども)", params)}"
            :: "エッセンス: ${wordOr("essence", "(ひとこと — 例: 夜だけの世界)", params)}"
            :: Nil)

    /// フリージャンル: ゼロから書かせない。いちばん近いジャンルのテンプレートを
    /// 土台に選ばせる(土台のテストと golden ごと引き継ぐ)。
    pub def freePrompt(direction: String): String =
        String.unlines(
            "新しいゲームのテンプレートを作ってください。"
            :: "土台: いちばん近いジャンルのテンプレート(templates/)を選んで変形。ゼロから書かない"
            :: "どんなゲーム?: ${direction}"
            :: "決めること: 操作・動かせるもの・終わり方(私の言葉から補って)"
            :: "守ること: AGENTS.md と .claude/skills の規約に従う。ルール・当たり判定・進行はコード、見た目と数値は Doc(material / tuning を仕分け)。プレイヤーの目に入る文言は必ず Doc に置く(既定値はコード側。コード直書きの表示文字列は違反)。テストはルール・進行・収支だけ(期待値は Doc から導く)。"
            :: "エンジン API 優先: 実装の前に engine の docs/module-index.md(やりたいこと → モジュールの逆引き)を引き、エンジンにある物(UI・スクロール・文字折り・当たり判定等)を自前で書かない。"
            :: "完了条件: make check / make test を緑に、土台の golden を引き継いで make bake まで。報告の前に /critique-code で自己批評し、指摘を潰す。gallery の絵を見せて報告する。"
            :: Nil)
}
