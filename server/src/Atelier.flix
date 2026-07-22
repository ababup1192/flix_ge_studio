// Atelier — 試作 (atelier/*.json) と本番スロット (assets/*.json) の橋渡し係。
// 候補一覧はスロットごとに束ね、昇格 (promote) はスロットの中身を退避 (prev-N) してから
// 候補の中身をスロットへ書く。走っているゲームは watchFile で保存即反映を拾う。
// 互換判定・prev の名前づけ・経路の門番は純粋な関数に分けてある (そこだけテストする)。

mod Atelier {
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.lang.{String => JavaString}
    import java.math.{BigDecimal => JBigDecimal}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool, JNull}

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// 候補の経路の門番: `atelier/` 直下の安全な 1 語のみ (`..`・絶対経路・深い経路は弾く)。
    pub def isCandidatePath(path: String): Bool =
        isUnder(prefix = "atelier/", path)

    /// スロットの経路の門番: `assets/` 直下の安全な 1 語のみ。
    pub def isSlotPath(path: String): Bool =
        isUnder(prefix = "assets/", path)

    /// prefix 直下の JSON 1 語か。名前 1 語の判定は Gallery と同じ門番を通す。
    def isUnder(prefix: {prefix = String}, path: String): Bool =
        String.startsWith(prefix = prefix#prefix, path)
            and {
                let name = String.dropLeft(String.length(prefix#prefix), path);
                Gallery.isSafeName(name) and String.endsWith(suffix = ".json", name)
            }

    /// 名前から Doc の種類の尻尾 (".sprite.json" 等) を取る。
    /// "villager.a.sprite.json" → Some(".sprite.json")。"x.json" のような 1 語は None。
    pub def kindSuffix(name: String): Option[String] =
        if (not String.endsWith(suffix = ".json", name))
            None
        else {
            let stem = String.dropRight(5, name);
            String.splitOn(substr = ".", stem)
                |> List.last
                |> Option.flatMap(kind -> if (kind == stem or kind == "") None else Some(".${kind}.json"))
        }

    /// 候補がスロットと互換か。スロットに entityId があれば一致が条件、
    /// 無ければ Doc の種類 (".<kind>.json" の尻尾) の一致に落とす。
    pub def isCompatible(slotEntityId: Option[String], slotName: String,
                         candEntityId: Option[String], candName: String): Bool =
        match slotEntityId {
            case Some(entityId) => candEntityId == Some(entityId)
            case None           => match (kindSuffix(slotName), kindSuffix(candName)) {
                case (Some(a), Some(b)) => a == b
                case _                  => false
            }
        }

    /// 退避ファイル ("<base>.prev-N.<kind>.json") の名前か。
    pub def isPrevName(name: String): Bool =
        String.splitOn(substr = ".", name)
            |> List.exists(part ->
                String.startsWith(prefix = "prev-", part)
                    and String.dropLeft(5, part) != ""
                    and String.forAll(Char.isDigit, String.dropLeft(5, part)))

    /// スロット名から n 番目の退避ファイル名を作る:
    /// "prologue.sprite.json" と 1 → "prologue.prev-1.sprite.json"。
    pub def retiredName(slotName: String, n: Int32): String =
        match kindSuffix(slotName) {
            case Some(suffix) =>
                let base = String.dropRight(String.length(suffix), slotName);
                "${base}.prev-${n}${suffix}"
            case None =>
                "${String.dropRight(5, slotName)}.prev-${n}.json"
        }

    /// 退避は 3 世代まで。
    pub def prevKeep(): Int32 = 3

    /// プレビュー PNG の置き場 (プロジェクト相対)。候補 "atelier/<名前>.json" は
    /// "debug/atelier/<名前>.png"、スロット "assets/<名前>.json" は
    /// "debug/atelier/<名前>.current.png"。門番落ちの経路は None。
    pub def previewName(file: String): Option[String] =
        if (isCandidatePath(file))
            Some("debug/atelier/${String.dropRight(5, String.dropLeft(String.length("atelier/"), file))}.png")
        else if (isSlotPath(file))
            Some("debug/atelier/${String.dropRight(5, String.dropLeft(String.length("assets/"), file))}.current.png")
        else
            None

    /// 焼き直しが要るか: PNG が無い (mtime が 0 以下) か、元 JSON より古い (= 未焼成)。
    pub def needsBake(jsonMtime: Int64, pngMtime: Int64): Bool =
        pngMtime <= 0i64 or pngMtime < jsonMtime

    // ── 候補一覧 (GET /atelier/candidates) ────────────────

    /// 1 ファイルぶんの見立て (名前と、中身から読めた entityId / note / mtime)。
    type alias Entry = {name = String, entityId = Option[String], note = Option[String], mtime = Int64}

    /// GET /atelier/candidates の本文。assets/ の各スロットに互換な atelier/ の候補を束ね、
    /// どのスロットとも互換でない候補は loose に理由つきで出す。
    pub def candidatesJson(runner: Runner.State, dir: String): Json \ IO =
        // スキーマは仕様書でありスロットではない。候補の無いスロットも画面の主役を
        // 追いやるだけなので、返すのは「候補が居るスロット」だけに絞る。
        let slots = listEntries(dir, "assets")
            |> List.filter(slot -> not String.endsWith(suffix = ".schema.json", slot#name));
        let candidates = listEntries(dir, "atelier");
        let bundles = slots
            |> List.filterMap(slot -> {
                let matched = candidates
                    |> List.filter(cand -> isCompatible(slot#entityId, slot#name, cand#entityId, cand#name));
                if (List.isEmpty(matched)) None else Some((slot, matched))
            });
        let slotJsons = bundles
            |> List.map(bundle -> {
                let (slot, matched) = bundle;
                JObject(Map#{
                    "slot"                => JString("assets/${slot#name}"),
                    "entityId"            => optString(slot#entityId),
                    "currentPreviewReady" => JBool(previewFresh(dir, "assets/${slot#name}", slot#mtime)),
                    "candidates"          => JArray(matched |> List.map(cand -> candidateJson(dir, cand)) |> List.toVector)
                })
            });
        let loose = candidates
            |> List.filter(cand -> not List.exists(slot ->
                isCompatible(slot#entityId, slot#name, cand#entityId, cand#name), slots))
            |> List.map(cand -> JObject(Map#{
                "file"   => JString("atelier/${cand#name}"),
                "reason" => JString("互換するスロットが見つかりません")
            }));
        let baking = maybeAutoBake(runner, dir, candidates, bundles |> List.map(fst));
        JObject(Map#{
            "slots"  => JArray(List.toVector(slotJsons)),
            "loose"  => JArray(List.toVector(loose)),
            "baking" => JBool(baking)
        })

    /// 未焼成のプレビューがあれば裏で `make atelier-preview` を起こす (自動焼成)。
    /// 焼き係が塞がっていれば黙って見送る — 次のポーリングが拾う。返り値は
    /// 「いま atelier-preview が走っているか」(起こした直後も true)。
    def maybeAutoBake(runner: Runner.State, dir: String, candidates: List[Entry], slots: List[Entry]): Bool \ IO =
        let baking = Runner.runningTarget(runner) == Some("atelier-preview");
        let stale =
            List.exists(cand -> not previewFresh(dir, "atelier/${cand#name}", cand#mtime), candidates)
                or List.exists(slot -> not previewFresh(dir, "assets/${slot#name}", slot#mtime), slots);
        if (stale and not baking) {
            let (status, _) = Runner.launchTarget(runner, dir, "atelier-preview", busy = "焼いている最中です");
            status == 202
        } else
            baking

    /// プレビュー PNG が焼けているか (存在し、元 JSON より新しい)。
    def previewFresh(dir: String, file: String, jsonMtime: Int64): Bool \ IO =
        match previewName(file) {
            case Some(rel) => not needsBake(jsonMtime, new JFile("${dir}/${rel}").lastModified())
            case None      => false
        }

    /// GET /atelier/preview?file=… の中身。門番落ち・PNG 無し・元 JSON より古い (未焼成) は None。
    pub def previewBytes(dir: String, file: String): Option[Array[Int8, Static]] \ IO =
        match previewName(file) {
            case None => None
            case Some(rel) =>
                let png = new JFile("${dir}/${rel}");
                let jsonMtime = new JFile("${dir}/${file}").lastModified();
                if (png.isFile() and not needsBake(jsonMtime, png.lastModified()))
                    Some(Files.readAllBytes(png.toPath()))
                else
                    None
        }

    /// 候補 1 件の JSON。退避ファイルは isPrev:true で区別する (巻き戻し候補として並ぶ)。
    def candidateJson(dir: String, cand: Entry): Json \ IO =
        JObject(Map#{
            "file"         => JString("atelier/${cand#name}"),
            "note"         => optString(cand#note),
            "isPrev"       => JBool(isPrevName(cand#name)),
            "previewReady" => JBool(previewFresh(dir, "atelier/${cand#name}", cand#mtime)),
            "mtime"        => JNumber(JBigDecimal.valueOf(cand#mtime))
        })

    def optString(value: Option[String]): Json =
        value |> Option.map(text -> JString(text)) |> Option.getWithDefault(JNull)

    /// dir/subdir 直下の *.json を見立て込みで集める (名前順)。無い・読めないは空。
    def listEntries(dir: String, subdir: String): List[Entry] \ IO =
        let entries = new JFile("${dir}/${subdir}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isFile() and String.endsWith(suffix = ".json", file.getName()))
            |> List.map(file -> {
                let fields = topFields(readText("${dir}/${subdir}/${file.getName()}"));
                { name     = file.getName(),
                  entityId = fieldString("entityId", fields),
                  note     = fieldString("note", fields),
                  mtime    = file.lastModified() }
            })
            |> List.sortBy(entry -> entry#name)

    /// JSON テキストの最上位オブジェクト (壊れていれば空 = fail-open)。
    def topFields(text: String): Map[String, Json] =
        JsonCompat.parse(text)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.getWithDefault(Map#{})

    def fieldString(key: String, fields: Map[String, Json]): Option[String] =
        Map.get(key, fields) |> Option.flatMap(JsonCodec.expectString)

    // ── 昇格 (POST /atelier/promote) ──────────────────────

    /// 応答 (status, body)。経路の門番 → 実在 → 互換、の順に全部検分してから初めて書く。
    /// 手順: 候補の中身を読む → スロットの中身を prev-1 へ退避 (prev は 3 世代で玉突き) →
    /// 候補の中身をスロットへ書く → 候補ファイルを消す (退避ファイルの昇格 = 巻き戻しのときは
    /// 玉突きが名前を持っていくので消さない)。
    pub def promote(dir: String, bodyText: String): (Int32, Json) \ IO =
        let fields = JsonCompat.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.getWithDefault(Map#{});
        match (fieldString("candidate", fields), fieldString("slot", fields)) {
            case (Some(candidate), Some(slot)) => promoteChecked(dir, candidate, slot)
            case _ => (400, failJson("body は {candidate, slot} の JSON で指定してください"))
        }

    def promoteChecked(dir: String, candidate: String, slot: String): (Int32, Json) \ IO =
        if (not isCandidatePath(candidate))
            (400, failJson("不正な候補の経路です (atelier/ 直下のみ): ${candidate}"))
        else if (not isSlotPath(slot))
            (400, failJson("不正なスロットの経路です (assets/ 直下のみ): ${slot}"))
        else if (not new JFile("${dir}/${candidate}").isFile())
            (400, failJson("候補がありません: ${candidate}"))
        else if (not new JFile("${dir}/${slot}").isFile())
            (400, failJson("スロットがありません: ${slot}"))
        else {
            let slotName = String.dropLeft(String.length("assets/"), slot);
            let candName = String.dropLeft(String.length("atelier/"), candidate);
            let slotEntity = fieldString("entityId", topFields(readText("${dir}/${slot}")));
            let candEntity = fieldString("entityId", topFields(readText("${dir}/${candidate}")));
            if (not isCompatible(slotEntity, slotName, candEntity, candName))
                (400, failJson("候補とスロットの entityId が合いません (互換性がありません)"))
            else
                swap(dir, candidate, slot, slotName, candName)
        }

    /// 検分済みの入れ替え本体。候補の中身は玉突きの前に読んでおく
    /// (巻き戻しでは玉突きが候補ファイル自身を改名するため)。
    def swap(dir: String, candidate: String, slot: String, slotName: String, candName: String): (Int32, Json) \ IO =
        let candBytes = Files.readAllBytes(new JFile("${dir}/${candidate}").toPath());
        discard new JFile("${dir}/atelier").mkdirs();
        rotate(dir, slotName);
        copyFile("${dir}/${slot}", "${dir}/atelier/${retiredName(slotName, 1)}");
        writeBytes("${dir}/${slot}", candBytes);
        if (not isPrevName(candName))
            discard new JFile("${dir}/${candidate}").delete()
        else ();
        (200, JObject(Map#{
            "ok"      => JBool(true),
            "retired" => JString("atelier/${retiredName(slotName, 1)}")
        }))

    /// 退避の玉突き: prev-3 を捨て、prev-2 → prev-3、prev-1 → prev-2 と繰り上げる。
    def rotate(dir: String, slotName: String): Unit \ IO =
        discard new JFile("${dir}/atelier/${retiredName(slotName, prevKeep())}").delete();
        foreach (n <- List.range(1, prevKeep()) |> List.reverse) {
            let src = new JFile("${dir}/atelier/${retiredName(slotName, n)}");
            if (src.isFile())
                discard src.renameTo(new JFile("${dir}/atelier/${retiredName(slotName, n + 1)}"))
            else ()
        }

    // ── ファイルの読み書き (バイトそのまま) ───────────────

    /// バイト列を UTF-8 で読む (無い・読めないは空文字 = fail-open)。
    def readText(path: String): String \ IO =
        let file = new JFile(path);
        if (file.isFile()) new JavaString(Files.readAllBytes(file.toPath()), "UTF-8") else ""

    def copyFile(src: String, dst: String): Unit \ IO =
        writeBytes(dst, Files.readAllBytes(new JFile(src).toPath()))

    def writeBytes(path: String, bytes: Array[Int8, Static]): Unit \ IO =
        let out = new FileOutputStream(path);
        out.write(bytes);
        out.close()

    /// 失敗応答の本文。error を {message} の形にするのは Runner・preview 系と同じ読み方。
    def failJson(message: String): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString(message)})
        })
}
