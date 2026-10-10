// Runner — Studio から make を安全に走らせる係。
// 走らせられる make 対象は許可リストの中だけ。同時に走るのは 1 本 (重ねては 409 で断る)。
// ログは最新の 1 回ぶんを尻尾 500 行まで覚え、GET /runner/log で配る。
// 焼き (bake) だけは make の代わりに「走行中のゲームの命令口 (POST /bake)」へ頼む道もある
// (launchRemoteBake)。走行権・ログ・終了記録は make と同じ器なので、外から見れば同じ 1 回の bake。
// 対象の検分・名前の門番・ログの刈り込み・命令口の応答の解釈は純粋な関数に分けてある
// (そこだけテストする)。

mod Runner {
    import java.lang.Process
    import java.lang.ProcessBuilder
    import java.lang.System
    import java.util.concurrent.Executors
    import java.util.concurrent.Callable
    import java.lang.Exception
    import java.lang.Throwable
    import java.util.ArrayList
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.util.concurrent.TimeUnit
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.lang.{String => JavaString}
    import java.io.InputStreamReader
    import java.io.BufferedReader
    import java.net.InetSocketAddress
    import java.net.Socket
    import java.net.SocketTimeoutException
    import java.lang.Thread
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool, JNull}

    /// 最新の 1 回の様子を覚える器 (キー: running / target / exitCode / runId / log)。
    // WhyNot: Ref を使わない — Ref は region の寿命に縛られ、リクエストと焼き係スレッドを
    // またいで生きる可変の入れ物にならないので、Projects と同じく Java の Map を借りる。
    pub type alias State = JMap[String, String]

    /// コンテナを作る (何も走っていない状態)。
    pub def make(): State \ IO = new JMap()

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// 走らせてよい make 対象の許可リスト。ここに無い対象は 400 で断る。
    pub def isAllowedTarget(target: String): Bool =
        Set.memberOf(target, Set#{"render", "gallery-prologue", "gallery-sounds", "atelier-preview"})

    /// 「焼く」の許可リスト (この順で配る)。atelier-preview は試作のウィンドウ口なのでここには入れない。
    pub def bakeTargets(): List[String] =
        "render" :: "gallery-prologue" :: "gallery-sounds" :: Nil

    /// Makefile の本文に実在する焼き対象だけを選ぶ (純粋)。行頭が「<対象>:」の行が
    /// あれば実在とみなす (許可リストの中しか見ないので安い走査で足りる)。
    /// 本文が空 (Makefile が無い・読めない) は "render" だけにする (fail-open)。
    pub def targetsFromMakefileText(text: String): List[String] =
        if (text == "")
            "render" :: Nil
        else {
            let lines = String.lines(text);
            bakeTargets() |> List.filter(target ->
                List.exists(line -> String.startsWith(prefix = "${target}:", line), lines))
        }

    /// ログの尻尾 cap 行だけ残す (頭から溢れたぶんを捨てる)。
    pub def keepTail(cap: Int32, lines: List[String]): List[String] =
        List.drop(List.length(lines) - cap, lines)

    /// 生の HTTP 応答テキストから本文を取り出す (最初の空行の後ろ全部)。
    /// 空行が無い (途中で切れた・HTTP でない) は空文字 = 「焼けていない」に倒す。
    pub def bodyOfHttpText(raw: String): String =
        String.indexOfLeft(substr = "\r\n\r\n", raw)
            |> Option.map(pos -> String.dropLeft(pos + 4, raw))
            |> Option.getWithDefault("")

    /// ゲームの命令口の /bake 応答本文の 1 行目の解釈: "ok baked=N ms=…" だけが成功。
    /// "error: …" (bake 未対応・ループ無応答) や空・壊れた応答は失敗 =
    /// 従来の make bake へのフォールバック。
    pub def remoteBakeOk(body: String): Bool =
        String.lines(body) |> List.head
            |> Option.exists(line -> String.startsWith(prefix = "ok ", line))

    /// 走行中のゲームへの描き出し依頼がどう終わったか。
    pub enum RemoteBakeOutcome with Eq, ToString {
        /// "ok rendered=…" が返った。
        case Done
        /// 命令口に繋がらない (接続拒否・接続の時間切れ・途中で切れた)。ゲームは描いていない。
        case Unreachable
        /// ゲームが描き出しを持っていない ("error: render not supported")。
        case Unsupported
        /// 時間切れ (読み取りの時間切れ・ループ無応答の 503)。ゲームはまだ描いている途中。
        case Busy
        /// それ以外の応答。
        case Failed
    }

    /// 命令口へ 1 発送った結果の生の形。分類は classifyRemoteBake の仕事。
    pub enum GameReply {
        /// 応答が全部読めた (生の HTTP 応答テキスト)。
        case Answered(String)
        /// 繋ぐ段で失敗した (例外の文面)。
        case ConnectFailed(String)
        /// 繋がった後、読み取りが時間切れになった。
        case ReadTimedOut
        /// 繋がった後、読み取りが時間切れ以外で失敗した (例外の文面)。
        case ReadFailed(String)
    }

    /// 依頼の結果を分ける。make render へ倒してよいのは、ゲームが描いていないと分かる時だけ —
    /// Busy で倒すと、ゲームの中の描き出しと make の描き出しが同時に走る。
    pub def classifyRemoteBake(reply: GameReply): RemoteBakeOutcome =
        match reply {
            case GameReply.ConnectFailed(_) => RemoteBakeOutcome.Unreachable
            case GameReply.ReadFailed(_)    => RemoteBakeOutcome.Unreachable
            case GameReply.ReadTimedOut     => RemoteBakeOutcome.Busy
            case GameReply.Answered(raw)    =>
                let body = bodyOfHttpText(raw);
                if (String.startsWith(prefix = "HTTP/1.1 503", raw)
                    or String.startsWith(prefix = "error: game loop did not answer", body)) RemoteBakeOutcome.Busy
                else if (remoteBakeOk(body)) RemoteBakeOutcome.Done
                else if (String.startsWith(prefix = "error: render not supported", body)) RemoteBakeOutcome.Unsupported
                else RemoteBakeOutcome.Failed
        }

    /// 時間切れの後、ゲームの描き出しの終わりを待つ上限 (ミリ秒)。
    pub def remoteBakeWaitLimitMs(): Int64 = 5i64 * 60i64 * 1000i64

    /// make の対象名から、make を使わずに直接走らせられる仕事を引く。
    /// 引けない対象 (gallery-prologue などゲーム固有の焼き) は None = 従来の make に任せる。
    pub def taskOfTarget(target: String): Option[EngineTasks.Task] =
        match target {
            case "debug"           => Some(EngineTasks.Task.Debug)
            case "render"          => Some(EngineTasks.Task.Render)
            case "atelier-preview" => Some(EngineTasks.Task.AtelierPreview)
            case _                 => None
        }

    /// ログで覚える行数の上限。
    def logCap(): Int32 = 500

    /// 走行権を取れたら対象を起動する。タスク表に載っている対象は make を使わず
    /// java を直接呼び、載っていない対象は従来の make に倒す (runTarget)。
    pub def launchTarget(state: State, dir: String, target: String, busy: {busy = String}): (Int32, Json) \ IO =
        launchTargetEnv(state, dir, target, Nil, busy)

    /// launchTarget の環境変数つきの形 (Game が DEBUG_HTTP_PORT を渡すのに使う)。
    pub def launchTargetEnv(state: State, dir: String, target: String, envVars: List[(String, String)], busy: {busy = String}): (Int32, Json) \ IO =
        launchTargetEnvHook(state, dir, target, envVars, _ -> checked_ecast(()), busy)

    /// launchTargetEnv の子プロセス受け取りつきの形: 起動した Process を呼び手へ渡す
    /// (Game が実機再生の生死と停止を自分で握るのに使う)。make に倒れた場合は渡らない。
    pub def launchTargetEnvHook(state: State, dir: String, target: String, envVars: List[(String, String)], onStart: Process -> Unit \ IO, busy: {busy = String}): (Int32, Json) \ IO =
        launchJob(state, target, () -> runTarget(state, dir, target, envVars, onStart), busy)

    /// launchTarget の一般形: `make target ARG=…` のように追加引数を渡せる (Scaffold が使う)。
    pub def launchMake(state: State, dir: String, target: String, extraArgs: List[String], busy: {busy = String}): (Int32, Json) \ IO =
        launchJob(state, target, () -> runMake(state, dir, target, extraArgs, Nil), busy)

    /// 焼きを走行中のゲームの命令口 (POST 127.0.0.1:port/bake) へ頼む。走行権・ログ・
    /// 終了記録は make の bake と同じ器 = 外から見れば同じ 1 回の "bake" (baking 表示や
    /// 焼き上がり後の自動追随はそのまま働く)。頼めなかったとき (ゲーム停止・error 応答・
    /// 接続失敗・無応答) は同じスレッドでその場で従来の `make bake` に切り替え、
    /// 覚えていたポートは forget で忘れてもらう。
    /// ゲームの生死の見立て (alive) と覚え書きの消去 (forget) を関数で受け取るのは、
    /// make の器としての Runner を Game へ依存させないため。
    pub def launchRemoteBake(state: State, dir: String, port: Int32, alive: {alive = Unit -> Bool \ IO}, forget: {forget = Unit -> Unit \ IO}, busy: {busy = String}): (Int32, Json) \ IO =
        launchJob(state, "render", () -> runRemoteBake(state, dir, port, alive, forget), busy)

    /// 走行権を取れたら任意の仕事を裏で走らせ、返ってきた終了コードを記録する
    /// (NewGame が使う)。取れなければ 409。
    pub def launchWork(state: State, target: String, work: Unit -> Int32 \ IO, busy: {busy = String}): (Int32, Json) \ IO =
        launchJob(state, target, () -> finish(state, work()), busy)

    /// ログへ 1 行足す (他の係が同じコンテナへ進み具合を書けるように)。
    pub def logLine(state: State, line: String): Unit \ IO = appendLine(state, line)

    /// タスク表の 1 仕事をその場で走らせ、出力をログへ流して終了コードを返す
    /// (走行権はいじらない — launchWork の仕事の中から使う)。flix.jar が無ければ -1。
    pub def runTaskLogged(state: State, dir: String, task: EngineTasks.Task): Int32 \ IO =
        match EngineTasks.exec() {
            case Some(exec) => runProcess(state, dir, exec, task, Nil, _ -> checked_ecast(()))
            case None => {
                appendLine(state, "[runner] engine の bin/flix.jar が見つかりません (${EngineHome.dir()})");
                -1
            }
        }

    /// 走行権を取れたら仕事を別スレッドで起こして 202、取れなければ 409。
    /// putIfAbsent が入れ物ごと排他になるので、同時に来た 2 本のうち 1 本だけが通る。
    def launchJob(state: State, target: String, work: Unit -> Unit \ IO, busy: {busy = String}): (Int32, Json) \ IO =
        let prev: String = state.putIfAbsent("running", "1");
        if (not Object.isNull(prev))
            (409, failJson(busy#busy))
        else {
            let runId = nextRunId(state);
            discard state.put("target", target);
            discard state.remove("exitCode");
            discard state.put("log", "");
            launch(state, work);
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

    /// 終わった最後の 1 本 (走行中は None)。Changes が「自分が起こした描き出しが
    /// 成功して終わったか」を後から確かめるのに使う。
    ///
    /// WhyNot: 終了の瞬間に呼び戻す形にしない。終わりを記録するのは仕事の側のスレッドで、
    /// 判定を持つのは HTTP スレッド。呼び戻しを通すと、走らせ方 3 通り (java 直呼び・
    /// 走行中のゲームへの依頼・make) それぞれの終了点を全部つなぎ直すことになる。
    pub def lastRun(state: State): Option[{target = String, runId = Int32, exitCode = Int32}] \ IO =
        let runningVal: String = state.get("running");
        let targetVal: String = state.get("target");
        let runIdVal: String = state.get("runId");
        let exitVal: String = state.get("exitCode");
        if (not Object.isNull(runningVal) or Object.isNull(targetVal)
            or Object.isNull(runIdVal) or Object.isNull(exitVal))
            None
        else
            match (Int32.fromString(runIdVal), Int32.fromString(exitVal)) {
                case (Some(runId), Some(exitCode)) =>
                    Some({ target = targetVal, runId = runId, exitCode = exitCode })
                case _ => None
            }

    /// いま記録されている通し番号 (まだ 1 本も走っていなければ None)。
    pub def currentRunId(state: State): Option[Int32] \ IO =
        let value: String = state.get("runId");
        if (Object.isNull(value)) None else Int32.fromString(value)

    /// 通し番号を 1 進めて返す (コンテナが新品なら 1 から)。
    def nextRunId(state: State): Int32 \ IO =
        let prev: String = state.get("runId");
        let runId = if (Object.isNull(prev)) 1
                    else (Int32.fromString(prev) |> Option.getWithDefault(0)) + 1;
        discard state.put("runId", "${runId}");
        runId

    /// 仕事 (make の実行やリモート焼き) を別スレッドで走らせる。想定外の失敗は
    /// ログに残して終了記録 (-1) — 走行権を握ったまま黙り込まない。
    // WhyNot: Flix の region + spawn を使わない — region は出口で合流を待つので、
    // 202 を先に返して裏で焼き続けるという形にならない。また Runnable でなく Callable なのは
    // `run` が Flix の予約語で匿名クラスの手続き名に書けないため。
    def launch(state: State, work: Unit -> Unit \ IO): Unit \ IO =
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                try {
                    work()
                } catch {
                    // WhyNot: Exception でなく Throwable で受ける — Flix の bug! は Error で
                    // 飛ぶので、Exception だけだと走行権を握ったまま黙り込み、以後ずっと
                    // 409 (走っている最中です) から戻れなくなる。
                    case e: Throwable => {
                        appendLine(state, "[runner] ${e.toString()}");
                        finish(state, -1)
                    }
                };
                ""
            }
        });
        executor.shutdown()

    /// 対象を走らせる道を選ぶ: タスク表に載っているか、ゲームが project.json の
    /// studioTasks で入口を宣言していて、engine の flix.jar もあるなら java 直呼び。
    /// どちらも無ければ macOS は従来の make に倒し、make を持たない Windows は
    /// 走らせられない旨をログに残して終える (黙って 202 のまま何も起きない、を防ぐ)。
    def runTarget(state: State, dir: String, target: String, envVars: List[(String, String)], onStart: Process -> Unit \ IO): Unit \ IO =
        let task = match taskOfTarget(target) {
            case Some(known) => Some(known)
            case None        => EngineTasks.declaredEntry(dir, target)
                                    |> Option.map(entry -> EngineTasks.Task.EntryPoint(entry))
        };
        match (task, EngineTasks.exec()) {
            case (Some(found), Some(exec)) => runTask(state, dir, exec, found, envVars, onStart)
            case _ =>
                if (EngineTasks.isWindows()) {
                    appendLine(state, "[runner] この環境では ${target} を走らせる手がありません (engine の bin/flix.jar が無いか、project.json の studioTasks に ${target} の宣言がありません)");
                    finish(state, -1)
                } else
                    runMake(state, dir, target, Nil, envVars)
        }

    /// タスク表で 1 本走らせて終了を記録する。焼き (Bake) は、パレットを持つゲームなら
    /// 先にパレットを書き出し、書けなければ焼きを中止する (make の `bake: palette` と
    /// 同じ順・同じ厳しさ)。持たないゲームでは前段を丸ごと飛ばす — 必ず失敗する
    /// 1 コンパイルを毎回の焼きに足さない。
    def runTask(state: State, dir: String, exec: EngineTasks.Exec, task: EngineTasks.Task, envVars: List[(String, String)], onStart: Process -> Unit \ IO): Unit \ IO =
        if (task == EngineTasks.Task.Render and EngineTasks.hasPalette(dir)) {
            let paletteExit = runProcess(state, dir, exec, EngineTasks.Task.Palette, Nil, _ -> checked_ecast(()));
            if (paletteExit != 0) {
                appendLine(state, "[runner] パレットが書けなかったので描き出しを中止します");
                finish(state, paletteExit)
            } else
                finish(state, runProcess(state, dir, exec, task, envVars, onStart))
        } else
            finish(state, runProcess(state, dir, exec, task, envVars, onStart))

    /// タスクを 1 プロセス走らせ、出力を 1 行ずつログへ流し、終了コードを返す
    /// (走行権や終了記録はいじらない — 呼び手が束ねる)。
    def runProcess(state: State, dir: String, exec: EngineTasks.Exec, task: EngineTasks.Task, envVars: List[(String, String)], onStart: Process -> Unit \ IO): Int32 \ IO =
        let argList = EngineTasks.argv(exec, EngineTasks.isMac(), task);
        // 起動コマンドはログにも残す — 遠くの機械 (特に Windows) で「どの java が
        // どの旗で走ったか」を、ログを見るだけで答え合わせできるように。
        appendLine(state, "[runner] 起動: ${String.intercalate(" ", argList)}");
        let args = new ArrayList();
        foreach (arg <- argList) discard args.add(arg);
        let pb = new ProcessBuilder(args);
        discard pb.directory(new JFile(dir));
        discard pb.redirectErrorStream(true);
        let env = pb.environment();
        discard env.remove("DEVELOPER_DIR");
        discard env.remove("SDKROOT");
        foreach (envVar <- EngineTasks.envOf(task) ::: envVars) {
            let (name, value) = envVar;
            discard env.put(name, value)
        };
        let proc = pb.start();
        onStart(proc);
        if (EngineTasks.isHeadless(task)) watchdog(state, proc) else ();
        let reader = new BufferedReader(new InputStreamReader(proc.getInputStream(), "UTF-8"));
        pump(state, reader);
        proc.waitFor()

    /// ウィンドウなしの仕事の見張り番: 「出力が 10 分止まったら」打ち切る — 固まったコンパイルが
    /// 走行権を握ったまま黙り込むのを防ぐ。合計時間では見ない — 遅い機械の初回コンパイルや
    /// 大きな焼きは 10 分を超えても、喋り続けている限り正当な仕事だから。
    /// 実機再生 (ウィンドウあり) には付けない — 遊んでいる時間はいくら長くてもよい。
    def watchdog(state: State, proc: Process): Unit \ IO =
        stampOutput(state);
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                watchLoop(state, proc);
                ""
            }
        });
        executor.shutdown()

    /// 30 秒おきに様子を見て、最後の出力から 10 分過ぎていたら打ち切る。
    def watchLoop(state: State, proc: Process): Unit \ IO =
        if (proc.waitFor(30i64, TimeUnit.SECONDS)) ()
        else {
            let silent = System.currentTimeMillis() - lastOutputAt(state);
            if (silent > 10i64 * 60i64 * 1000i64) {
                appendLine(state, "[runner] 出力が 10 分止まったままなので打ち切りました");
                discard proc.destroyForcibly()
            } else
                watchLoop(state, proc)
        }

    /// 最後に出力を見た時刻を書く/読む (見張り番の材料)。記録が無ければ「いま」扱い。
    def stampOutput(state: State): Unit \ IO =
        discard state.put("lastLineAt", "${System.currentTimeMillis()}")

    def lastOutputAt(state: State): Int64 \ IO =
        let value: String = state.get("lastLineAt");
        if (Object.isNull(value)) System.currentTimeMillis()
        else Int64.fromString(value) |> Option.getWithDefault(System.currentTimeMillis())

    /// プロジェクトの dir で `make target` を走らせ、標準出力+標準エラーを 1 行ずつログへ積む。
    /// この Mac の DEVELOPER_DIR/SDKROOT は nix パスが刺さっていて子プロセスを壊すので外す
    /// (Editor.runCommand と同じ理由)。engine の場所は ENGINE で教える —
    /// ゲームの Makefile の `ENGINE ?=` はこちらが優先される。
    def runMake(state: State, dir: String, target: String, extraArgs: List[String], envVars: List[(String, String)]): Unit \ IO =
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
        match EngineHome.forGames() {
            case Some(engine) => discard env.put("ENGINE", engine)
            // 見当たらないなら、ゲームの Makefile が持つ既定に賭けるしかない。
            // 黙って賭けると、転んだ時にログが「make の中で何かが起きた」としか言わない。
            case None         =>
                appendLine(state, "[runner] engine が見当たりません (${EngineHome.dir()}) — ゲームの Makefile の既定で試します")
        };
        foreach (envVar <- envVars) {
            let (name, value) = envVar;
            discard env.put(name, value)
        };
        let proc = pb.start();
        let reader = new BufferedReader(new InputStreamReader(proc.getInputStream(), "UTF-8"));
        pump(state, reader);
        finish(state, proc.waitFor())

    // ── リモート焼き (走行中のゲームに頼む) ────────────────

    /// 走行中のゲームに焼きを頼み、ゲームが描いていないと分かったら同じスレッドで `make render` へ切り替える。
    /// 成功の応答 ("ok rendered=…" と [rendered] のパス列) はそのままログへ積む —
    /// GET /runner/log で make のときと同じように読める。
    def runRemoteBake(state: State, dir: String, port: Int32, alive: {alive = Unit -> Bool \ IO}, forget: {forget = Unit -> Unit \ IO}): Unit \ IO =
        if (not (alive#alive)()) {
            (forget#forget)();
            appendLine(state, "[runner] ゲームが走っていないので make render で描き出します");
            runTarget(state, dir, "render", Nil, _ -> checked_ecast(()))
        } else {
            appendLine(state, "[runner] 走行中のゲームに描き出しを頼みます (POST 127.0.0.1:${port}/render)");
            let reply = requestGame(port, "POST /render");
            match classifyRemoteBake(reply) {
                case RemoteBakeOutcome.Done => finishRemoteBake(state, reply)
                case RemoteBakeOutcome.Busy => {
                    appendLine(state, "[runner] ゲームが描き出し中なので待ちます");
                    awaitRemoteBake(state, dir, port, forget,
                        System.currentTimeMillis() + remoteBakeWaitLimitMs())
                }
                case _ => fallBackToMake(state, dir, forget, reasonOf(reply))
            }
        }

    /// 時間切れの後、ゲームの描き出しが終わるのを待つ。
    ///
    /// WhyNot: 終わりを知るのに POST /render を送り直さない — 描き出しがもう 1 回走る。
    /// 命令口は届いた順に答えを 1 つずつ返すので、時間切れで受け取り損ねた描き出しの答えは
    /// 次に送った依頼 (中身を変えない GET /help) の応答として届く。
    /// WhyNot: 待った後は命令口を忘れる。受け取り損ねた答えの分だけ、以後の応答が 1 つずつ
    /// ずれたままになり、次の依頼が前の答えを受け取ってしまう。
    def awaitRemoteBake(state: State, dir: String, port: Int32, forget: {forget = Unit -> Unit \ IO}, deadline: Int64): Unit \ IO =
        if (System.currentTimeMillis() > deadline) {
            (forget#forget)();
            appendLine(state, "[runner] ${remoteBakeWaitLimitMs() / 60000i64} 分待ってもゲームの描き出しが終わらないので、失敗として終えます");
            finish(state, -1)
        } else {
            let reply = requestGame(port, "GET /help");
            match classifyRemoteBake(reply) {
                case RemoteBakeOutcome.Done => {
                    (forget#forget)();
                    finishRemoteBake(state, reply)
                }
                case RemoteBakeOutcome.Busy => {
                    Thread.sleep(1000i64);
                    awaitRemoteBake(state, dir, port, forget, deadline)
                }
                // 描いている途中でゲームが落ちた: もう二重にはならないので make で描き直す。
                case RemoteBakeOutcome.Unreachable => fallBackToMake(state, dir, forget, reasonOf(reply))
                case _ => {
                    (forget#forget)();
                    appendLine(state, "[runner] ゲームの描き出しの結果を受け取れませんでした: ${reasonOf(reply)}");
                    finish(state, -1)
                }
            }
        }

    def finishRemoteBake(state: State, reply: GameReply): Unit \ IO =
        match reply {
            case GameReply.Answered(raw) =>
                foreach (line <- String.lines(bodyOfHttpText(raw))) appendLine(state, line)
            case _ => ()
        };
        finish(state, 0)

    def fallBackToMake(state: State, dir: String, forget: {forget = Unit -> Unit \ IO}, reason: String): Unit \ IO =
        (forget#forget)();
        appendLine(state, "[runner] ゲームが描き出せなかったので make render で描き出します: ${reason}");
        runTarget(state, dir, "render", Nil, _ -> checked_ecast(()))

    /// ログの理由書き。応答なら本文の 1 行目、例外ならその文面。
    def reasonOf(reply: GameReply): String =
        match reply {
            case GameReply.Answered(raw)        => headLineOf(bodyOfHttpText(raw))
            case GameReply.ConnectFailed(cause) => cause
            case GameReply.ReadTimedOut         => "読み取りの時間切れ"
            case GameReply.ReadFailed(cause)    => cause
        }

    /// 本文の 1 行目 (ログの理由書きに使う)。空なら「(空の応答)」。
    def headLineOf(body: String): String =
        String.lines(body) |> List.head |> Option.getWithDefault("(空の応答)")

    /// ゲームの命令口へ 1 発 ("POST /render" のように メソッドとパス)。生の HTTP 応答テキストを返す。
    /// 接続 2 秒・読み取り 15 秒で見切る (命令口自身がループ無応答を 10 秒で 503 にするので、
    /// それより長く待ってから諦める)。素の Socket なのは、依存もキャストも増やさず
    /// 「1 行書いて全部読む」だけで足りるため (応答は Content-Length 付き + Connection: close)。
    /// 繋ぐ段と読む段を分けて返すのは、接続の時間切れ (描いていない) と読み取りの時間切れ
    /// (描いている途中) が同じ SocketTimeoutException で来るため。
    def requestGame(port: Int32, request: String): GameReply \ IO =
        let socket = new Socket();
        let connected =
            try {
                socket.connect(new InetSocketAddress("127.0.0.1", port), 2000);
                None
            } catch {
                case e: Exception => Some(GameReply.ConnectFailed(e.toString()))
            };
        let reply = match connected {
            case Some(failed) => failed
            case None         =>
                try {
                    socket.setSoTimeout(15000);
                    let text = "${request} HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
                    socket.getOutputStream().write(text.getBytes("UTF-8"));
                    socket.getOutputStream().flush();
                    let bytes = socket.getInputStream().readAllBytes();
                    GameReply.Answered(new JavaString(bytes, "UTF-8"))
                } catch {
                    case _: SocketTimeoutException => GameReply.ReadTimedOut
                    case e: Exception              => GameReply.ReadFailed(e.toString())
                }
        };
        try { socket.close() } catch { case _: Exception => () };
        reply

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
        stampOutput(state);
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
