// Runner — Studio から make を安全に走らせる係。
// 走らせられる make 対象は白名前簿の中だけ。同時に走るのは 1 本 (重ねては 409 で断る)。
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
    import java.lang.{String => JavaString}
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

    /// 「焼く」の白名前簿 (この順で配る)。atelier-preview は試作の窓口なのでここには入れない。
    pub def bakeTargets(): List[String] =
        "bake" :: "gallery-prologue" :: "gallery-sounds" :: Nil

    /// Makefile の本文に実在する焼き対象だけを選ぶ (純粋)。行頭が「<対象>:」の行が
    /// あれば実在とみなす (白名前簿の中しか見ないので安い走査で足りる)。
    /// 本文が空 (Makefile が無い・読めない) は "bake" だけに倒す (fail-open)。
    pub def targetsFromMakefileText(text: String): List[String] =
        if (text == "")
            "bake" :: Nil
        else {
            let lines = String.lines(text);
            bakeTargets() |> List.filter(target ->
                List.exists(line -> String.startsWith(prefix = "${target}:", line), lines))
        }

    /// ログの尻尾 cap 行だけ残す (頭から溢れたぶんを捨てる)。
    pub def keepTail(cap: Int32, lines: List[String]): List[String] =
        List.drop(List.length(lines) - cap, lines)

    /// ログで覚える行数の上限。
    def logCap(): Int32 = 500

    /// 走行権を取れたら起動する。putIfAbsent が入れ物ごと排他になるので、
    /// 同時に来た 2 本のうち 1 本だけが通る。Game (make debug) も別の器でこれを使う。
    pub def launchTarget(state: State, dir: String, target: String, busy: {busy = String}): (Int32, Json) \ IO =
        launchMake(state, dir, target, Nil, busy)

    /// launchTarget の一般形: `make target ARG=…` のように追加引数を渡せる (Scaffold が使う)。
    pub def launchMake(state: State, dir: String, target: String, extraArgs: List[String], busy: {busy = String}): (Int32, Json) \ IO =
        let prev: String = state.putIfAbsent("running", "1");
        if (not Object.isNull(prev))
            (409, failJson(busy#busy))
        else {
            let runId = nextRunId(state);
            discard state.put("target", target);
            discard state.remove("exitCode");
            discard state.put("log", "");
            launch(state, dir, target, extraArgs);
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
    def launch(state: State, dir: String, target: String, extraArgs: List[String]): Unit \ IO =
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                try {
                    runMake(state, dir, target, extraArgs)
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
    def runMake(state: State, dir: String, target: String, extraArgs: List[String]): Unit \ IO =
        let args = new ArrayList();
        discard args.add("make");
        discard args.add(target);
        foreach (extra <- extraArgs) discard args.add(extra);
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

    /// 失敗応答の本文。error を {message} の形にするのは noProjectJson・preview 系と同じ読み方。
    def failJson(message: String): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString(message)})
        })
}
