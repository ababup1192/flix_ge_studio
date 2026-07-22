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

    /// 候補がスロットと互換か。スロットに entityId があれば一致が条件。
    /// 無ければ「種類が同じ+名前がスロットの続き」に落とす — 種類だけだと
    /// summer の候補が autumn 等ぜんぶのテーマに現れてしまう。
    /// 例: スロット "summer.theme.json" ↔ 候補 "summer.a.theme.json"。
    pub def isCompatible(slotEntityId: Option[String], slotName: String,
                         candEntityId: Option[String], candName: String): Bool =
        match slotEntityId {
            case Some(entityId) => candEntityId == Some(entityId)
            case None           => match (kindSuffix(slotName), kindSuffix(candName)) {
                case (Some(a), Some(b)) =>
                    a == b and {
                        let slotBase = String.dropRight(String.length(a), slotName);
                        String.startsWith(prefix = "${slotBase}.", candName)
                    }
                case _ => false
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

    // ── つくる (M4): スロット一覧・プロンプト・自作コピー ──

    /// 案数は 1〜9 に収める (0 や負数・二桁は端に丸める)。
    pub def clampCount(n: Int32): Int32 =
        Int32.max(1, Int32.min(9, n))

    /// 自作コピーの元の名前 (拡張子なしの 1 語)。英数と `_` `-` と途中の `.` だけを許す。
    /// 先頭・末尾の `.` や `..`・区切り文字は経路いじりの芽なので弾く。
    pub def isCopyBaseName(name: String): Bool =
        Gallery.isSafeName(name)
            and not String.startsWith(prefix = ".", name)
            and not String.endsWith(suffix = ".", name)
            and String.forAll(c ->
                (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or Char.isDigit(c) or c == '_' or c == '-' or c == '.', name)

    /// 自作コピーの置き先の名前: スロットの種類の尻尾を受け継ぐ。
    /// "prologue.sprite.json" と "villager.d" → "villager.d.sprite.json"。
    pub def copyTargetName(slotName: String, name: String): String =
        match kindSuffix(slotName) {
            case Some(suffix) => "${name}${suffix}"
            case None         => "${name}.json"
        }

    /// /atelier-draft スキル呼び出しの形の日本語プロンプト (純粋)。長い手順は
    /// ゲーム側 .claude/skills の /atelier-draft に置いてあり、ここは参照するだけ。
    pub def atelierPrompt(projectDir: String, slot: String, entityId: Option[String],
                          schemaPath: String, count: Int32, direction: Option[String]): String =
        let entityText = entityId |> Option.getWithDefault("なし");
        let directionText = direction |> Option.getWithDefault("おまかせ");
        String.unlines(
            "/atelier-draft を実行してください。"
            :: "対象プロジェクト: ${projectDir}"
            :: "スロット: ${slot} (entityId: ${entityText}, スキーマ: ${schemaPath})"
            :: "案数: ${clampCount(count)} / 方向性: ${directionText}"
            :: "候補は atelier/ に置き、make atelier-preview まで済ませてください。assets/ には書かないこと。"
            :: Nil)

    /// /wire-doc スキル呼び出しの形の日本語プロンプト (純粋)。
    pub def wirePrompt(projectDir: String, doc: String): String =
        String.unlines(
            "/wire-doc を実行してください。"
            :: "対象プロジェクト: ${projectDir}"
            :: "対象Doc: ${doc}"
            :: "橋渡しテストは3本まで。make check / make test で確認してください。"
            :: Nil)

    /// 「あそびを作らせる」の日本語プロンプト (純粋)。長い規約はゲーム側の
    /// AGENTS.md と .claude/skills に置いてあり、ここは参照するだけ。
    pub def gamePrompt(projectDir: String, direction: String): String =
        String.unlines(
            "このゲームに遊びを実装してください。"
            :: "対象プロジェクト: ${projectDir}"
            :: "どんな遊びか: ${direction}"
            :: "守ること: AGENTS.md と .claude/skills の規約に従う。ルール・当たり判定・進行はコード、調整する数値や配置は Doc(スキーマは sections 方言、/scaffold で増やせる)。テストはルール・進行・収支だけ(期待値は Doc から導く)。"
            :: "仕上げ: make check / make test を緑にし、Bake.all の場面を遊びが伝わる絵に育て、make bake まで済ませる。素材の候補が要るなら atelier/ に置き atelier-preview まで(assets/ 直書き禁止)。"
            :: Nil)

    /// スキーマの description から一行の手引きを作る (純粋): 最初の「。」までで切り、
    /// 80 文字に収める。「。」無しはそのまま (切りようがない)。
    pub def hintOf(description: String): String =
        let sentence = match String.splitOn(substr = "。", description) {
            case head :: _ :: _ => head + "。"
            case _              => description
        };
        String.takeLeft(80, sentence)

    /// スロットの手引き: スキーマ JSON のトップの description の最初の文。
    /// スキーマ無し・読めない・description 欠けは "" (fail-open)。
    def slotHint(dir: String, slotPath: String, decl: ResourceDecl.Decl): String \ IO =
        let schemaPath = decl#schemaPath
            |> Option.getWithDefault(ResourceDecl.siblingSchemaPath(slotPath));
        fieldString("description", topFields(readText("${dir}/${schemaPath}")))
            |> Option.map(hintOf)
            |> Option.getWithDefault("")

    /// project.json の editor.resources 宣言を読む (壊れ・欠けは空 = fail-open)。
    def resourceDecls(dir: String): List[ResourceDecl.Decl] \ IO =
        JsonCompat.parse(readText("${dir}/project.json"))
            |> Option.flatMap(json -> ResourceDecl.fromProjectJson(json) |> Result.toOption)
            |> Option.map(decls -> decls#resources)
            |> Option.getWithDefault(Nil)

    /// GET /atelier/slots の本文。assets/ 直下の JSON のうち、素材 (role=material) の
    /// 宣言に当たるものだけを並べる。*.schema.json は仕様書なので除く。
    pub def slotsJson(dir: String): Json \ IO =
        let decls = resourceDecls(dir);
        let slots = listEntries(dir, "assets")
            |> List.filter(entry -> not String.endsWith(suffix = ".schema.json", entry#name))
            |> List.filterMap(entry -> {
                let path = "assets/${entry#name}";
                ResourceDecl.materialDeclFor(decls, path)
                    |> Option.map(decl -> JObject(Map#{
                        "file"     => JString(path),
                        "entityId" => optString(entry#entityId),
                        "kind"     => JString(kindOf(entry#name, decl)),
                        "title"    => JString(decl#title |> Option.getWithDefault(decl#id)),
                        "hint"     => JString(slotHint(dir, path, decl))
                    }))
            });
        JObject(Map#{"slots" => JArray(List.toVector(slots))})

    /// スロットの種類名: 名前の尻尾 (".sprite.json" → "sprite")。尻尾が無ければ宣言 id で代用。
    def kindOf(name: String, decl: ResourceDecl.Decl): String =
        match kindSuffix(name) {
            case Some(suffix) => String.dropRight(5, String.dropLeft(1, suffix))
            case None         => decl#id
        }

    /// GET /prompt/atelier?slot=…&count=…&direction=… の応答 (status, body)。
    /// スロットは経路の門番と素材宣言の両方に当たって初めて通す。
    pub def promptAtelierJson(dir: String, params: Map[String, String]): (Int32, Json) \ IO =
        match Map.get("slot", params) {
            case None => (400, failJson("slot を指定してください"))
            case Some(slot) =>
                if (not isSlotPath(slot))
                    (400, failJson("不正なスロットの経路です (assets/ 直下のみ): ${slot}"))
                else match ResourceDecl.materialDeclFor(resourceDecls(dir), slot) {
                    case None => (400, failJson("素材 (role=material) のスロットではありません: ${slot}"))
                    case Some(decl) =>
                        let entityId = fieldString("entityId", topFields(readText("${dir}/${slot}")));
                        let schemaPath = decl#schemaPath
                            |> Option.getWithDefault(ResourceDecl.siblingSchemaPath(slot));
                        let count = Map.get("count", params)
                            |> Option.flatMap(Int32.fromString)
                            |> Option.getWithDefault(3);
                        let direction = Map.get("direction", params)
                            |> Option.filter(text -> text != "");
                        (200, JObject(Map#{
                            "prompt" => JString(atelierPrompt(dir, slot, entityId, schemaPath, count, direction))
                        }))
                }
        }

    /// GET /prompt/wire?doc=… の応答 (status, body)。doc は assets/ 直下の JSON のみ。
    pub def promptWireJson(dir: String, params: Map[String, String]): (Int32, Json) =
        match Map.get("doc", params) {
            case Some(doc) if isSlotPath(doc) =>
                (200, JObject(Map#{"prompt" => JString(wirePrompt(dir, doc))}))
            case Some(doc) => (400, failJson("不正な Doc の経路です (assets/ 直下のみ): ${doc}"))
            case None      => (400, failJson("doc を指定してください"))
        }

    /// GET /prompt/game?direction=… の応答 (status, body)。どんな遊びかは必須
    /// (空のままではプロンプトにならないので 400 で聞き返す)。
    pub def promptGameJson(dir: String, params: Map[String, String]): (Int32, Json) =
        match Map.get("direction", params) |> Option.filter(text -> text != "") {
            case Some(direction) =>
                (200, JObject(Map#{"prompt" => JString(gamePrompt(dir, direction))}))
            case None => (400, failJson("direction を指定してください (どんな遊びかを言葉で)"))
        }

    /// POST /atelier/copy (body: {slot, name}) — スロットの写しを atelier/ に置く自作の出発点。
    /// note を「自作 (元: スロット)」に差し替え、entityId ほか中身はそのまま受け継ぐ。
    pub def copy(dir: String, bodyText: String): (Int32, Json) \ IO =
        let fields = JsonCompat.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.getWithDefault(Map#{});
        match (fieldString("slot", fields), fieldString("name", fields)) {
            case (Some(slot), Some(name)) => copyChecked(dir, slot, name)
            case _ => (400, failJson("body は {slot, name} の JSON で指定してください"))
        }

    def copyChecked(dir: String, slot: String, name: String): (Int32, Json) \ IO =
        if (not isSlotPath(slot))
            (400, failJson("不正なスロットの経路です (assets/ 直下のみ): ${slot}"))
        else if (not isCopyBaseName(name))
            (400, failJson("不正な名前です (英数・_ - と途中の . のみ): ${name}"))
        else if (not new JFile("${dir}/${slot}").isFile())
            (400, failJson("スロットがありません: ${slot}"))
        else {
            let slotName = String.dropLeft(String.length("assets/"), slot);
            let target = "atelier/${copyTargetName(slotName, name)}";
            if (new JFile("${dir}/${target}").isFile())
                (409, failJson("すでにあります: ${target}"))
            else match JsonCompat.parse(readText("${dir}/${slot}")) |> Option.flatMap(JsonCodec.expectObject) {
                case None => (400, failJson("スロットの JSON が読めません: ${slot}"))
                case Some(obj) => {
                    let stamped = Map.insert("note", JString("自作 (元: ${slot})"), obj);
                    discard new JFile("${dir}/atelier").mkdirs();
                    writeBytes("${dir}/${target}",
                        (Util.Json.toPrettyString(2, JObject(stamped)) + "\n").getBytes("UTF-8"));
                    (200, JObject(Map#{"ok" => JBool(true), "file" => JString(target)}))
                }
            }
        }

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
