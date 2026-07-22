// Runner — Studio から「焼く (make)」と「祝福 (gallery→golden の個別コピー)」を安全に行う係。
// 走らせられる make 対象は白名前簿の 3 つだけ。同時に走るのは 1 本 (重ね焼きは 409 で断る)。
// ログは最新の 1 回ぶんを尻尾 500 行まで覚え、GET /runner/log で配る。
// 対象の検分・名前の門番・ログの刈り込みは純粋な関数に分けてある (そこだけテストする)。

mod Runner {
    import java.lang.ProcessBuilder
    import java.util.concurrent.Executors
    import java.util.concurrent.Callable
    import java.lang.Exception
    import java.util.ArrayList
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.io.InputStreamReader
    import java.io.BufferedReader
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool, JNull}

    /// 最新の 1 回の様子を覚える器 (キー: running / target / exitCode / runId / log)。
    // WhyNot: Ref を使わない — Ref は region の寿命に縛られ、リクエストと焼き係スレッドを
    // またいで生きる可変の入れ物にならないので、Projects と同じく Java の Map を借りる。
    pub type alias State = JMap[String, String]

    /// 器を作る (何も走っていない状態)。
    pub def make(): State \ IO = new JMap()

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// 走らせてよい make 対象の白名前簿。ここに無い対象は 400 で断る。
    pub def isAllowedTarget(target: String): Bool =
        Set.memberOf(target, Set#{"bake", "gallery-prologue", "gallery-sounds", "atelier-preview"})

    /// 祝福してよい名前か: 経路をまたがない 1 語 (Gallery と同じ門番) で .png のみ。
    pub def isBlessName(name: String): Bool =
        Gallery.isSafeName(name) and String.endsWith(suffix = ".png", name)

    /// ログの尻尾 cap 行だけ残す (頭から溢れたぶんを捨てる)。
    pub def keepTail(cap: Int32, lines: List[String]): List[String] =
        List.drop(List.length(lines) - cap, lines)

    /// ログで覚える行数の上限。
    def logCap(): Int32 = 500

    // ── 焼く (POST /gallery/bake) ─────────────────────────

    /// 応答 (status, body)。対象を検分し、走っていなければ焼き係スレッドを起こして 202 を返す。
    pub def startBake(state: State, dir: String, bodyText: String): (Int32, Json) \ IO =
        let targetOpt = JsonCompat.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> Map.get("target", obj))
            |> Option.flatMap(JsonCodec.expectString);
        match targetOpt {
            case None => (400, failJson("body must be a JSON object {target}"))
            case Some(target) =>
                if (not isAllowedTarget(target))
                    (400, failJson("unknown target: ${target}"))
                else
                    launchTarget(state, dir, target, busy = "焼いている最中です")
        }

    /// 走行権を取れたら起動する。putIfAbsent が入れ物ごと排他になるので、
    /// 同時に来た 2 本のうち 1 本だけが通る。Game (make debug) も別の器でこれを使う。
    pub def launchTarget(state: State, dir: String, target: String, busy: {busy = String}): (Int32, Json) \ IO =
        let prev: String = state.putIfAbsent("running", "1");
        if (not Object.isNull(prev))
            (409, failJson(busy#busy))
        else {
            let runId = nextRunId(state);
            discard state.put("target", target);
            discard state.remove("exitCode");
            discard state.put("log", "");
            launch(state, dir, target);
            (202, JObject(Map#{
                "ok"    => JBool(true),
                "runId" => JNumber(JsonCodec.intToBd(runId))
            }))
        }

    /// いま走っている make 対象 (走っていなければ None)。Atelier が「焼いている最中か」を
    /// 応答へ載せるのと、二重焼きの回避に使う。
    pub def runningTarget(state: State): Option[String] \ IO =
        let runningVal: String = state.get("running");
        let targetVal: String = state.get("target");
        if (Object.isNull(runningVal) or Object.isNull(targetVal)) None else Some(targetVal)

    /// 通し番号を 1 進めて返す (器が新品なら 1 から)。
    def nextRunId(state: State): Int32 \ IO =
        let prev: String = state.get("runId");
        let runId = if (Object.isNull(prev)) 1
                    else (Int32.fromString(prev) |> Option.getWithDefault(0)) + 1;
        discard state.put("runId", "${runId}");
        runId

    /// make target を別スレッドで走らせる。
    // WhyNot: Flix の region + spawn を使わない — region は出口で合流を待つので、
    // 202 を先に返して裏で焼き続けるという形にならない。また Runnable でなく Callable なのは
    // `run` が Flix の予約語で匿名クラスの手続き名に書けないため。
    def launch(state: State, dir: String, target: String): Unit \ IO =
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                try {
                    runMake(state, dir, target)
                } catch {
                    case e: Exception => {
                        appendLine(state, "[runner] ${e.toString()}");
                        finish(state, -1)
                    }
                };
                ""
            }
        });
        executor.shutdown()

    /// プロジェクトの dir で `make target` を走らせ、標準出力+標準エラーを 1 行ずつログへ積む。
    /// この Mac の DEVELOPER_DIR/SDKROOT は nix パスが刺さっていて子プロセスを壊すので外す
    /// (Editor.runCommand と同じ理由)。
    def runMake(state: State, dir: String, target: String): Unit \ IO =
        let args = new ArrayList();
        discard args.add("make");
        discard args.add(target);
        let pb = new ProcessBuilder(args);
        discard pb.directory(new JFile(dir));
        discard pb.redirectErrorStream(true);
        let env = pb.environment();
        discard env.remove("DEVELOPER_DIR");
        discard env.remove("SDKROOT");
        let proc = pb.start();
        let reader = new BufferedReader(new InputStreamReader(proc.getInputStream(), "UTF-8"));
        pump(state, reader);
        finish(state, proc.waitFor())

    /// 子プロセスの出力を終端まで 1 行ずつログへ流し込む。
    def pump(state: State, reader: BufferedReader): Unit \ IO =
        let line = reader.readLine();
        if (Object.isNull(line)) ()
        else {
            appendLine(state, line);
            pump(state, reader)
        }

    /// ログへ 1 行足し、上限を超えたぶんを頭から捨てる。書き手は焼き係スレッド 1 本だけなので、
    /// 読み手 (GET /runner/log) は put 単位の一貫したスナップショットを見る。
    def appendLine(state: State, line: String): Unit \ IO =
        let current: String = state.get("log");
        let text = if (Object.isNull(current) or current == "") line else "${current}\n${line}";
        let trimmed = keepTail(logCap(), String.lines(text)) |> List.join("\n");
        discard state.put("log", trimmed)

    /// 終了を記録して走行権を返す。
    def finish(state: State, exitCode: Int32): Unit \ IO =
        discard state.put("exitCode", "${exitCode}");
        discard state.remove("running")

    // ── ログ (GET /runner/log) ────────────────────────────

    /// 最新の 1 回の様子。まだ 1 回も走っていなければ全部 null と空の一覧。
    pub def logJson(state: State): Json \ IO =
        let runningVal: String = state.get("running");
        let targetVal: String = state.get("target");
        let exitVal: String = state.get("exitCode");
        let logVal: String = state.get("log");
        let lines = if (Object.isNull(logVal) or logVal == "") Nil else String.lines(logVal);
        JObject(Map#{
            "running"  => JBool(not Object.isNull(runningVal)),
            "target"   => if (Object.isNull(targetVal)) JNull else JString(targetVal),
            "exitCode" =>
                if (Object.isNull(exitVal)) JNull
                else Int32.fromString(exitVal)
                    |> Option.map(code -> JNumber(JsonCodec.intToBd(code)))
                    |> Option.getWithDefault(JNull),
            "lines"    => JArray(lines |> List.map(line -> JString(line)) |> List.toVector)
        })

    // ── 祝福 (POST /gallery/bless) ────────────────────────

    /// 応答 (status, body)。confirm:true が無い・名前が空・門番落ち・gallery/ に無い、は 400。
    /// 全部の検分に通ってからコピーする (途中まで祝福して止まる事故を避ける)。
    pub def bless(dir: String, bodyText: String): (Int32, Json) \ IO =
        match JsonCompat.parse(bodyText) |> Option.flatMap(JsonCodec.expectObject) {
            case None => (400, failJson("body must be a JSON object {names, confirm}"))
            case Some(obj) =>
                let confirmed = Map.get("confirm", obj)
                    |> Option.flatMap(JsonCodec.expectBool)
                    |> Option.getWithDefault(false);
                let namesOpt = Map.get("names", obj)
                    |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectString));
                if (not confirmed)
                    (400, failJson("confirm:true を付けてください (祝福は golden を書き換えます)"))
                else match namesOpt {
                    case None       => (400, failJson("names は文字列の配列で指定してください"))
                    case Some(Nil)  => (400, failJson("names が空です"))
                    case Some(names) => blessValidated(dir, names)
                }
        }

    /// 名前を全部検分してからコピーする。
    def blessValidated(dir: String, names: List[String]): (Int32, Json) \ IO =
        match List.find(name -> not isBlessName(name), names) {
            case Some(bad) => (400, failJson("不正な名前です: ${bad}"))
            case None => match List.find(name -> not new JFile("${dir}/gallery/${name}").isFile(), names) {
                case Some(missing) => (400, failJson("gallery/ にありません: ${missing}"))
                case None => {
                    discard new JFile("${dir}/golden").mkdirs();
                    foreach (name <- names) copyFile("${dir}/gallery/${name}", "${dir}/golden/${name}");
                    (200, JObject(Map#{
                        "ok"      => JBool(true),
                        "blessed" => JArray(names |> List.map(name -> JString(name)) |> List.toVector)
                    }))
                }
            }
        }

    /// バイトそのままのコピー (bench のバイト比較がそのまま ok になる形)。
    def copyFile(src: String, dst: String): Unit \ IO =
        let bytes = Files.readAllBytes(new JFile(src).toPath());
        let out = new FileOutputStream(dst);
        out.write(bytes);
        out.close()

    /// 失敗応答の本文。error を {message} の形にするのは noProjectJson・preview 系と同じ読み方。
    def failJson(message: String): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString(message)})
        })
}
