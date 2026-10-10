// NextStep — 「次のやること」の案内係。プロジェクトのいまの状態 (候補・鮮度・差分) を見て、
// 初めての人でも迷わないよう、次にやると良いことを 1 つだけ提案する。
// 状態集めは IO、どれを提案するかの判断は純粋な関数に分けてある (判断だけテストする)。
// HTTP のウィンドウ口は /journey/* のまま (web 側の Journey.elm と対)。名前を揃えるなら両方いっぺんに動かす。

mod NextStep {
    import java.io.{File => JFile}
    import java.nio.file.Files
    import java.lang.Exception
    import java.security.MessageDigest
    import java.util.HexFormat
    import java.util.concurrent.{ConcurrentHashMap => JMap}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JNumber, JBool}

    /// 判断の材料。atelierCandidates = atelier/ の試作 JSON の数、
    /// staleGallery = アセットの方が絵より新しい (作り直しが要る)、diffCount = リファレンス画像との差分の数、
    /// gameRunning = このプロジェクトのゲームが走っているか。
    pub type alias Checks = {
        atelierCandidates = Int32,
        staleGallery      = Bool,
        diffCount         = Int32,
        referenceEmpty       = Bool,
        hasStarterDoc     = Bool,
        gameRunning       = Bool
    }

    /// 提案 1 件。nav はエディタ画面のどこへ誘導するか。
    pub type alias Suggestion = {
        id     = String,
        title  = String,
        detail = String,
        nav    = String
    }

    /// 材料から提案を 1 つ決める。上から順に見て、最初に当たった物を出す:
    /// 候補あり → 選ぶ / 未読の知らせあり → 見比べ /
    /// 生まれたて (雛形 Doc が居る) → 走っていなければ起動、走っていればアレンジ / どれも無し → 試作。
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
              detail = "触ったアセットのぶんなら、そのままでどうぞ。心当たりがなければ、前と今を見比べてください。",
              nav    = "changes" }
        else if (checks#hasStarterDoc and not checks#gameRunning)
            // 生まれたてで、まだ走っていない: 何かをいじる前に、まず遊んで
            // 「動くゲームがもう手元にある」を体で分かってもらう。
            { id     = "launch",
              title  = "ゲームを起動してみましょう",
              detail = "テンプレートのゲームが起動します。まずは遊んで試してみましょう。",
              nav    = "launch" }
        else if (checks#hasStarterDoc)
            // 生まれたてで、もう走っている: 次の一歩は自分の手で変えること。
            { id     = "arrange",
              title  = "ゲームをアレンジしてみましょう",
              detail = "ゲームのパラメータを変えられます。保存した瞬間、走っているゲームに反映されます。",
              nav    = "arrange" }
        else
            { id     = "create",
              title  = "アセットを試作しよう",
              detail = "いまは知らせもありません。新しいアセットを作って試してみましょう。",
              nav    = "atelier" }

    /// アセット (assets/*.json) の方が絵 (gallery/*.png) より新しいか。
    /// 絵が 1 枚も無いのにアセットがあるときも「作り直しが要る」。アセットが無ければ作る物も無い。
    ///
    /// WhyNot: 絵の側の最も古い 1 枚と比べない。gallery/ には描き出しが書き直さない PNG
    /// (別の道具が置いた調べ物のシート等) が混ざり得て、その 1 枚が古いまま居座ると
    /// 判定が永久に真になる。描き出しても真のままなので、描き直しが止まらなくなる。
    pub def isStale(assetMtimes: List[Int64], galleryMtimes: List[Int64]): Bool =
        match (List.maximum(assetMtimes), List.maximum(galleryMtimes)) {
            case (None, _)                 => false
            case (Some(_), None)           => true
            case (Some(newest), Some(drawn)) => newest > drawn
        }

    /// 絵を作り直す必要があるか。fingerprint は今のアセットの中身の指紋 (アセットが無ければ None)、
    /// stamp は「最後に成功した描き出しを起こす直前に測った指紋」。
    ///
    /// WhyNot: 更新時刻で比べない。中身の同じ書き直し (描き出しの前段が同じ値を書き戻す等) でも
    /// 時刻は進むので、描き出しがまた描き出しを呼ぶ輪になる。
    /// WhyNot: stamp があるときに絵の mtime を見ない。絵を誰が書いたかは判定の材料にならず、
    /// 「前の描き出しを起こした後にアセットの中身が変わったか」だけが作り直す理由だから。
    /// WhyNot: 絵が 1 枚も無い場合だけは stamp があっても描き出す — 人が gallery/ を
    /// 消したときに「作り済み」のまま何も知らせず絵が戻らないのは、原因の分からない故障になる。
    pub def needsBake(fingerprint: Option[String], stamp: Option[String],
                      assetMtimes: List[Int64], galleryMtimes: List[Int64]): Bool =
        match fingerprint {
            case None      => false
            case Some(now) =>
                if (List.isEmpty(galleryMtimes)) true
                else match stamp {
                    case Some(baked) => now != baked
                    // 覚えていない (前の描き出しの記録が無い) ときだけ、いちばん新しい絵と時刻で比べる。
                    case None        => isStale(assetMtimes, galleryMtimes)
                }
        }

    /// 描き出しの入力になるアセットか。*.palette.json は Studio が描き出しの前に毎回
    /// 作り直す生成物 (mk/game.mk の palette と同じ物) なので、入力から外す。
    /// 外さないと、描き出しを起こす直前に測った値をこのファイル自身が必ず追い越して、
    /// 次の描き出しの理由になり続ける。
    pub def isSourceAsset(name: String): Bool =
        String.endsWith(suffix = ".json", name)
            and not String.endsWith(suffix = ".palette.json", name)

    // ── IO: 材料集め ──────────────────────────────────────

    /// 絵を作り直す必要があるか (Changes の自動検査が使う)。stamp は Changes が覚えている。
    pub def staleGalleryOf(dir: String, fingerprint: Option[String], stamp: Option[String]): Bool \ IO =
        needsBake(fingerprint, stamp, jsonMtimes(dir, "assets"), Gallery.galleryMtimes(dir))

    /// ファイルごとの SHA-256 を覚えておく器 (キー: "hash|<パス>" = "<mtime>|<size>|<sha>")。
    /// Changes の State をそのまま渡す。
    pub type alias HashCache = JMap[String, String]

    /// いまのアセット (assets/ 直下の描き出しの入力) の中身から作る 1 つの指紋。
    /// 名前順に「名前 + 中身の SHA-256」を並べて、もう一度 SHA-256 をかける。アセットが無ければ None。
    /// 描き出しを起こす直前に測り、成功したら stamp にする値。
    ///
    /// WhyNot: 毎回全部を読み直さない。ポーリングのたびに呼ばれるので、(mtime, size) が
    /// 前と同じファイルは前のハッシュを使い回す。
    pub def computeAssetsFingerprint(cache: HashCache, dir: String): Option[String] \ IO =
        let entries = jsonFiles(dir, "assets")
            |> List.filterMap(file -> computeFileHash(cache, file) |> Option.map(hash -> (file.getName(), hash)))
            |> List.sortBy(fst);
        if (List.isEmpty(entries)) None
        else {
            let listing = entries
                |> List.map(entry -> { let (name, hash) = entry; "${name}\t${hash}\n" })
                |> List.join("");
            Some(sha256Hex(listing.getBytes("UTF-8")))
        }

    /// 1 ファイルの SHA-256。(mtime, size) が覚えている物と同じならそのまま返す。
    /// 一覧を取った後に消えた・読めないファイルは None (指紋に入れない)。
    ///
    /// WhyNot: 読めない例外を上へ投げない。保存の途中や改名の合間に当たり得て、
    /// 投げると検査 (pump) ごと落ちる。消えたファイルは次の目盛りの一覧から外れる。
    def computeFileHash(cache: HashCache, file: JFile): Option[String] \ IO =
        let path = file.getAbsolutePath();
        let mark = "${file.lastModified()}|${file.length()}";
        let cached: String = cache.get("hash|${path}");
        let reused =
            if (Object.isNull(cached)) None
            else if (String.startsWith(prefix = "${mark}|", cached))
                Some(String.dropLeft(String.length(mark) + 1, cached))
            else None;
        match reused {
            case Some(hash) => Some(hash)
            case None       =>
                try {
                    let hash = sha256Hex(Files.readAllBytes(file.toPath()));
                    discard cache.put("hash|${path}", "${mark}|${hash}");
                    Some(hash)
                } catch {
                    case _: Exception => None
                }
        }

    def sha256Hex(bytes: Array[Int8, Static]): String \ IO =
        HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))

    /// GET /journey/state の本文。unseen は Changes の未読の知らせの数。
    pub def stateJson(game: Game.Runtime, dir: String, unseen: Int32): Json \ IO =
        let checks = gatherChecks(game, dir);
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
                "hasStarterDoc"     => JBool(checks#hasStarterDoc),
                "gameRunning"       => JBool(checks#gameRunning)
            })
        })

    /// ディスクから判断の材料を集める。ディレクトリが無いのは全部「0 個・差分無し」扱い。
    /// atelier/ の数は退避ファイル (prev-N) を除いた「本物の試作」だけ数える。
    def gatherChecks(game: Game.Runtime, dir: String): Checks \ IO =
        let candidates = jsonNames(dir, "atelier")
            |> List.filter(name -> not Atelier.isPrevName(name))
            |> List.length;
        let stale = isStale(jsonMtimes(dir, "assets"), Gallery.galleryMtimes(dir));
        let items = Gallery.diffItemsOf(dir);
        let diffs = items |> List.filter(item -> snd(item) != "ok") |> List.length;
        // お手本がまだ空 = 全項目が「reference/ に無い」。gallery まで空なら items が空で
        // diffs も 0 なので、リファレンス画像の更新ではなく create に落ちる。
        let referenceEmpty = List.forAll(item -> snd(item) == "missingReference", items);
        // 産まれたてテンプレートの見分け: 雛形 Doc がまだそのまま居るか。
        let hasStarterDoc = new JFile("${dir}/assets/sample.kind.json").isFile();
        // このプロジェクトのゲームが走っているか (GET /game/status と同じ見方)。
        let gameRunning = Game.isRunning(game, dir);
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs,
          referenceEmpty = referenceEmpty, hasStarterDoc = hasStarterDoc, gameRunning = gameRunning }

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
            |> List.filter(file -> file.isFile() and isSourceAsset(file.getName()))
}
