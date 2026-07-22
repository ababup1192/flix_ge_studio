// Journey — 「つぎの一手」の案内係。プロジェクトのいまの状態 (候補・鮮度・差分) を見て、
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
        diffCount         = Int32
    }

    /// 提案 1 件。nav はエディタ画面のどこへ誘導するか。
    pub type alias Suggestion = {
        id     = String,
        title  = String,
        detail = String,
        nav    = String
    }

    /// 材料から提案を 1 つ決める。上から順に見て、最初に当たった物を出す:
    /// 候補あり → 選ぶ / 素材が新しい → 焼く / 差分あり → 祝福 / どれも無し → 試作。
    pub def suggest(checks: Checks): Suggestion =
        if (checks#atelierCandidates > 0)
            { id     = "pick",
              title  = "候補を比べて選ぼう",
              detail = "アトリエに試作が ${checks#atelierCandidates} 個あります。見比べて、いちばん良い物を選びましょう。",
              nav    = "atelier" }
        else if (checks#staleGallery)
            { id     = "bake",
              title  = "焼いて確かめよう",
              detail = "素材が絵より新しくなっています。焼き直して、いまの見た目を確かめましょう。",
              nav    = "gallery" }
        else if (checks#diffCount > 0)
            { id     = "bless",
              title  = "差分を確認して祝福しよう",
              detail = "お手本と違う絵が ${checks#diffCount} 枚あります。見比べて、良ければ新しいお手本にしましょう。",
              nav    = "gallery" }
        else
            { id     = "create",
              title  = "素材を試作しよう",
              detail = "いまは差分も焼き残しもありません。新しい素材を作って試してみましょう。",
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

    /// GET /journey/state の本文。
    pub def stateJson(dir: String): Json \ IO =
        let checks = gatherChecks(dir);
        let suggestion = suggest(checks);
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
                "diffCount"         => JNumber(JsonCodec.intToBd(checks#diffCount))
            })
        })

    /// ディスクから判断の材料を集める。ディレクトリが無いのは全部「0 個・差分無し」扱い。
    /// atelier/ の数は退避ファイル (prev-N) を除いた「本物の試作」だけ数える。
    def gatherChecks(dir: String): Checks \ IO =
        let candidates = jsonNames(dir, "atelier")
            |> List.filter(name -> not Atelier.isPrevName(name))
            |> List.length;
        let stale = isStale(jsonMtimes(dir, "assets"), Gallery.galleryMtimes(dir));
        let diffs = Gallery.diffItemsOf(dir) |> List.filter(item -> snd(item) != "ok") |> List.length;
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs }

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
