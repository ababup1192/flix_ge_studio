// Journey — 「次のやること」の案内係。プロジェクトのいまの状態 (候補・鮮度・差分) を見て、
// 初めての人でも迷わないよう、次にやると良いことを 1 つだけ提案する。
// 状態集めは IO、どれを提案するかの判断は純粋な関数に分けてある (判断だけテストする)。

mod Journey {
    import java.io.{File => JFile}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JNumber, JBool}

    /// 判断の材料。atelierCandidates = atelier/ の試作 JSON の数、
    /// staleGallery = 素材の方が絵より新しい (焼き直しが要る)、diffCount = golden との差分の数。
    pub type alias Checks = {
        atelierCandidates = Int32,
        staleGallery      = Bool,
        diffCount         = Int32,
        goldenEmpty       = Bool,
        hasStarterDoc     = Bool
    }

    /// 提案 1 件。nav はエディタ画面のどこへ誘導するか。
    pub type alias Suggestion = {
        id     = String,
        title  = String,
        detail = String,
        nav    = String
    }

    /// 材料から提案を 1 つ決める。上から順に見て、最初に当たった物を出す:
    /// 候補あり → 選ぶ / 未読の知らせあり → 見比べ / どれも無し → 設計か試作。
    /// 検査と基準の追随は Changes が裏で自動でやるので、ここでは頼まない —
    /// 提案するのは人の判断が要る事だけ。
    pub def suggest(checks: Checks, unseen: Int32): Suggestion =
        if (checks#atelierCandidates > 0)
            { id     = "pick",
              title  = "候補を比べて選ぼう",
              detail = "アトリエに試作が ${checks#atelierCandidates} 個あります。見比べて、いちばん良い物を選びましょう。",
              nav    = "atelier" }
        else if (unseen > 0)
            // 事故の警報を兼ねる: 自分が触った直後なら「そうだね」で流してよく、
            // 心当たりが無ければ見比べる。基準は既に自動で追随済み。
            { id     = "changed",
              title  = "見た目が変わりました(${unseen}場面)",
              detail = "触った素材のぶんなら、そのままでどうぞ。心当たりがなければ、前と今を見比べてください。",
              nav    = "changes" }
        else if (checks#hasStarterDoc)
            // やる事が何も残っていない産まれたてテンプレート (雛形 Doc がまだ居る):
            // 素材いじりより先に「どんなゲームか」を言葉にしてもらう。
            { id     = "design",
              title  = "あそびを考えよう",
              detail = "見た目の土台はできました。どんなゲームにするかを言葉にして、AIに作らせましょう(アトリエの『つくる』にプロンプトの入口があります)",
              nav    = "atelier" }
        else
            { id     = "create",
              title  = "素材を試作しよう",
              detail = "いまは知らせもありません。新しい素材を作って試してみましょう。",
              nav    = "atelier" }

    /// 素材 (assets/*.json) の方が絵 (gallery/*.png) より新しいか。
    /// 絵が 1 枚も無いのに素材があるときも「焼き直しが要る」。素材が無ければ焼く物も無い。
    pub def isStale(assetMtimes: List[Int64], galleryMtimes: List[Int64]): Bool =
        match List.maximum(assetMtimes) {
            case None           => false
            case Some(newest)   => match List.minimum(galleryMtimes) {
                case None         => true
                case Some(oldest) => newest > oldest
            }
        }

    // ── IO: 材料集め ──────────────────────────────────────

    /// 素材 (assets/*.json) が絵 (gallery/*.png) より新しいか (Changes の自動検査が使う)。
    pub def staleGalleryOf(dir: String): Bool \ IO =
        isStale(jsonMtimes(dir, "assets"), Gallery.galleryMtimes(dir))

    /// GET /journey/state の本文。unseen は Changes の未読の知らせの数。
    pub def stateJson(dir: String, unseen: Int32): Json \ IO =
        let checks = gatherChecks(dir);
        let suggestion = suggest(checks, unseen);
        JObject(Map#{
            "suggestion" => JObject(Map#{
                "id"     => JString(suggestion#id),
                "title"  => JString(suggestion#title),
                "detail" => JString(suggestion#detail),
                "nav"    => JString(suggestion#nav)
            }),
            "checks" => JObject(Map#{
                "atelierCandidates" => JNumber(JsonCodec.intToBd(checks#atelierCandidates)),
                "staleGallery"      => JBool(checks#staleGallery),
                "diffCount"         => JNumber(JsonCodec.intToBd(checks#diffCount)),
                // 生まれたての見本 (sample.kind.json が残っている) か。web は
                // 「テンプレートのまま」前提の問い (インタビューのチップ等) をこれで出し分ける。
                "hasStarterDoc"     => JBool(checks#hasStarterDoc)
            })
        })

    /// ディスクから判断の材料を集める。ディレクトリが無いのは全部「0 個・差分無し」扱い。
    /// atelier/ の数は退避ファイル (prev-N) を除いた「本物の試作」だけ数える。
    def gatherChecks(dir: String): Checks \ IO =
        let candidates = jsonNames(dir, "atelier")
            |> List.filter(name -> not Atelier.isPrevName(name))
            |> List.length;
        let stale = isStale(jsonMtimes(dir, "assets"), Gallery.galleryMtimes(dir));
        let items = Gallery.diffItemsOf(dir);
        let diffs = items |> List.filter(item -> snd(item) != "ok") |> List.length;
        // お手本がまだ空 = 全項目が「golden に無い」。gallery まで空なら items が空で
        // diffs も 0 なので、firstBless ではなく create に落ちる。
        let goldenEmpty = List.forAll(item -> snd(item) == "missingGolden", items);
        // 産まれたてテンプレートの見分け: 雛形 Doc がまだそのまま居るか。
        let hasStarterDoc = new JFile("${dir}/assets/sample.kind.json").isFile();
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs,
          goldenEmpty = goldenEmpty, hasStarterDoc = hasStarterDoc }

    /// dir/subdir 直下の *.json の mtime 一覧。無ければ空。
    def jsonMtimes(dir: String, subdir: String): List[Int64] \ IO =
        jsonFiles(dir, subdir) |> List.map(file -> file.lastModified())

    /// dir/subdir 直下の *.json の名前一覧。無ければ空。
    def jsonNames(dir: String, subdir: String): List[String] \ IO =
        jsonFiles(dir, subdir) |> List.map(file -> file.getName())

    def jsonFiles(dir: String, subdir: String): List[JFile] \ IO =
        let entries = new JFile("${dir}/${subdir}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isFile() and String.endsWith(suffix = ".json", file.getName()))
}
