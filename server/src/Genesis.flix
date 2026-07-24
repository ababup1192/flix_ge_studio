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

    /// ジャンル 1 枚ぶんの札。starter は公式テンプレート (templates/ 相対) があるときだけ非空。
    pub type alias Family = {
        id = String,
        name = String,
        verb = String,
        includes = String,
        controls = String,
        skeleton = String,
        starter = String
    }

    /// 9 枚の札。並びは人気順 (この順が仕様 — client は並べ替えない)。
    /// 説明に実在のゲーム名は使わない (借り物なしで通じる言葉だけ)。
    pub def families(): List[Family] =
          { id = "action",  name = "アクション",
            verb = "走って、跳んで、壊す",
            includes = "横スクロールジャンプ・ブロック崩し・ステージ攻略",
            controls = "←→ + スペース",
            skeleton = "いちばん小さいアクション「ブロック崩し」から。はね返す・壊す・全部壊すと次の面(面は Doc の rows)",
            starter = "templates/game-starter" }
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
            includes = "見下ろしサーキット・ラジコン風ドリフト",
            controls = "←→(ハンドル)",
            skeleton = "見下ろしコースを ←→ で曲がる / ライバル2台(速さは Doc)。終わり: 3周先にゴール(周回数は Doc)",
            starter = "" }
       :: { id = "puzzle",  name = "パズル",
            verb = "積んで、そろえて、消す",
            includes = "落ち物パズル・同色そろえ・連鎖",
            controls = "←→ + ↓ + 回転",
            skeleton = "上からピースが落ちる / ←→ で寄せ ↓ で落とし回して積む / 一列そろうと消える(落下速度・得点は Doc)。終わり: 積み上がったら負け",
            starter = "templates/tetris-starter" }
       :: { id = "shooter", name = "シューティング",
            verb = "よけて、撃つ",
            includes = "横スクロール・縦スクロール",
            controls = "←→↑↓ + スペース",
            skeleton = "横スクロール / ←→↑↓ + スペースで撃つ / 敵の波(並びは Doc の rows)。終わり: ボスを倒す",
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

    /// GET /genesis/families の応答。並び順ごと返す。
    pub def familiesJson(): Json =
        JObject(Map#{"families" => JArray(families() |> List.map(famJson) |> List.toVector)})

    def famJson(f: Family): Json =
        JObject(Map#{
            "id"       => JString(f#id),
            "name"     => JString(f#name),
            "verb"     => JString(f#verb),
            "includes" => JString(f#includes),
            "controls" => JString(f#controls),
            "starter"  => JString(f#starter)
        })

    /// GET /prompt/genesis?family=…&world=…&hero=…&essence=…(&direction=… は free 用)。
    /// ジャンルは必須。free だけは direction (どんなゲームかの言葉) が必須。
    /// world/hero/essence は空なら例文プレースホルダのまま出す — 依頼文は
    /// ユーザーが書き換える前提の下書きなので、空欄でも 200 で返す。
    pub def promptJson(params: Map[String, String]): (Int32, Json) =
        match Map.get("family", params) {
            case None => (400, failJson("family を指定してください"))
            case Some(id) =>
                match List.filter(f -> f#id == id, families()) |> List.head {
                    case None => (400, failJson("知らないジャンルです: ${id}"))
                    case Some(f) =>
                        if (f#id == "free")
                            match Map.get("direction", params) |> Option.filter(text -> text != "") {
                                case None => (400, failJson("direction を指定してください (どんなゲームかを言葉で)"))
                                case Some(direction) => (200, promptOk(freePrompt(direction)))
                            }
                        else
                            (200, promptOk(familyPrompt(f, params)))
                }
        }

    /// path が登録済みの公式テンプレートか (new-game の複製元の白名簿)。
    pub def isStarter(path: String): Bool =
        path != "" and (families() |> List.exists(f -> f#starter == path))

    /// GET /genesis/title?family=… — ジャンルカードの絵。公式テンプレートの
    /// golden/title.png のバイト列。テンプレート無し・絵無し・engine 不在は None
    /// (呼び手が 404 = 絵なしカードに倒す)。
    pub def titleBytes(params: Map[String, String]): Option[Array[Int8, Static]] \ IO =
        let starter = Map.get("family", params)
            |> Option.flatMap(id -> List.filter(f -> f#id == id, families()) |> List.head)
            |> Option.map(f -> f#starter)
            |> Option.filter(path -> path != "");
        match starter {
            case None => None
            case Some(path) =>
                let file = new JFile("${Scaffold.engineDir()}/${path}/golden/title.png");
                if (file.isFile()) Some(Files.readAllBytes(file.toPath())) else None
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
    pub def familyPrompt(f: Family, params: Map[String, String]): String =
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
