// Headless — 会話のプレビューに使う「窓なしのゲーム」の世話係（プロジェクトごとに 1 本）。
//
// 窓なしのゲームはゲーム自身のコード（engine の App.Headless・ゲームの E2e.serveHeadless）で、
// engine の bin/headless-run が jar を作って流す。Studio は自分で描かず、ここへ「この場面を 1 枚」と頼む
// （本物の立ち絵・表情・窓・後ろの場面が出る）。起動中のゲームは動かさない（動かすのは「この行へ飛ばす」だけ）。
//
// 1 本の移り変わり（Launch。旗を重ねずにこの 1 つで持つ）:
//
//     Idle ──頼み──▶ Booting ──ポートが答えた──▶ Running ──放っておかれて終わった──▶ Idle
//                      │  └─ 3 分たっても答えない: Stalled を答える（理由とログの末尾。待ち続ける）
//                      └─ ready にならずに終わった ──▶ Crashed(続けて落ちた回数, 理由, ログの末尾)
//     Crashed(1) ──次の頼み──▶ Booting（1 回だけ黙って起こし直す）
//     Crashed(2 以上) ──▶ Failed を答えたまま待つ（「もう一度起こす」= restart で 1 本だけ起こす）
//
// 決まり:
//   - 起こすのは頼まれた時だけ。立ち上げ中は待たせず「起こしています」を返す。Booting の中は 2 段（BootStage）:
//     jar を作っている（src が変わった時だけ。1〜2 分）と、起こしている（JVM の立ち上げ・最初の 1 枚。数秒）。
//     どちらかは debug/headless.log と jar を作る鍵（build/headless/.lock）から毎回決め直す（Launch には写さない）
//   - ゲーム（窓あり）が起動の途中なら起こさない（コンパイルと JVM のメモリの山を重ねない。Game.isStarting）
//   - 続けて落ちる時は起こし直さない（頼まれるたびに JVM とコンパイルを量産しない）。数えた回数は ready で 0 に戻る
//   - 放っておかれたら自分で終わる（HEADLESS_IDLE_SECS）。メモリの上限（以前の -Xmx1g）は付けない —
//     HEADLESS_JAVA_OPTS は jar の指紋に入るので、共通 jar を作る時・プレイと同じ組にそろえる（SharedJar.jarEnv）
//     そうして消えた時（Running から終わった時）は次の頼みで黙って立て直す
//   - 窓なしのゲームは実機でない扱い（セーブ・表示中の Doc の名乗り・遊んだ記録を書かない。engine 側の決まり）
// ログは <ゲーム>/debug/headless.log。何をするかは純粋な decide / decideRestart / afterExit が決め、
// IO（ポートに繋がるか・ログを読む・プロセスを起こす）はこのモジュールの下の方で読んで渡す。
mod Headless {
    import java.io.{File => JFile}
    import java.lang.Exception
    import java.lang.Process
    import java.lang.ProcessHandle
    import java.lang.{String => JavaString}
    import java.lang.System
    import java.net.{ServerSocket, Socket, InetSocketAddress}
    import java.nio.file.Files
    import java.util.ArrayList
    import java.util.Iterator
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.util.concurrent.Executors
    import java.util.concurrent.Callable
    import java.util.concurrent.locks.ReentrantLock
    import java.lang.ProcessBuilder

    // ── 状態と答えの型 ────────────────────────────────────

    /// 窓なしのゲーム 1 本の今（プロジェクトごとに 1 つ）。
    pub enum Launch with Eq, ToString {
        /// 起こしていない（まだ頼まれていない・放っておかれて自分から終わった）
        case Idle
        /// 起こして ready を待っている（ポート, 起こした時刻のミリ秒, それまで続けて落ちた回数）
        case Booting(Int32, Int64, Int32)
        /// ready になった（ポートが答えた）
        case Running(Int32)
        /// ready にならずに終わった（続けて落ちた回数, 理由, debug/headless.log の末尾）
        case Crashed(Int32, String, List[String])
    }

    /// 世話係の器。
    ///   launches … プロジェクトのパス → Launch
    ///   procs    … プロジェクトのパス → 起こしたプロセス（長く起きない時に止めて起こし直すため）
    ///   lock     … Launch の読み書きを 1 本ずつにする鍵（起こすのを 1 本にする門番）
    pub type alias State = {launches = JMap[String, Launch], procs = JMap[String, Process], lock = ReentrantLock}

    pub def make(): State \ IO = {launches = new JMap(), procs = new JMap(), lock = new ReentrantLock()}

    /// ゲームに置く入口の名前（engine の App.Headless の冒頭の書き方）。
    pub def entry(): String = "E2e.serveHeadless"

    /// 起こしている 1 本が今どの段か（engine の bin/headless-run の出す行で分かる）。
    ///   BuildingJar  … jar を作っている（src・lib が変わった時だけ。1〜2 分）
    ///   StartingGame … jar はある。JVM を立ち上げて最初の 1 枚を待っている（数秒）
    pub enum BootStage with Eq, ToString {
        case BuildingJar
        case StartingGame
    }

    /// 頼みの答え。
    ///   Ready(port)          … 頼める
    ///   Booting(stage, 秒, 落ちた回数) … 起こしている 1 本を待っている（段と起こしてからの経過秒。少し後にもう一度頼む）
    ///   Starting(why)        … それ以外で待っている（ゲームが起動の途中・答えない。少し後にもう一度頼む）
    ///   Stalled(why, log)    … 起こしてから長く ready にならない（待ち続けるが、理由とログの末尾と起こし直す口を見せる）
    ///   Failed(why, log)     … ready にならずに落ちた（続けては起こさない。画面は理由と「もう一度起こす」を出す）
    ///   Unsupported(why)     … このゲーム・この OS では起こせない
    pub enum Readiness with Eq, ToString {
        case Ready(Int32)
        case Booting(BootStage, Int64, Int32)
        case Starting(String)
        case Stalled(String, List[String])
        case Failed(String, List[String])
        case Unsupported(String)
    }

    /// 頼みのたびに IO で読んで決まりへ渡す、外の様子。
    ///   now          … 今の時刻（ミリ秒）
    ///   answering    … 今の 1 本のポートが繋がるか
    ///   gameStarting … ゲーム（窓あり）が起動の途中か
    ///   logTail      … debug/headless.log の末尾
    ///   stage        … 起こしている 1 本の段（bootStageOf で決めた物）
    pub type alias Seen = {now = Int64, answering = Bool, gameStarting = Bool, logTail = List[String], stage = BootStage}

    /// 頼みをどうするか。
    pub enum Decision with Eq, ToString {
        /// 状態はそのままで答える
        case Answer(Readiness)
        /// ready になった印を付けて Ready を答える（ポート）
        case MarkRunning(Int32)
        /// 新しく 1 本起こす（それまで続けて落ちた回数を持ち越す）
        case Start(Int32)
    }

    // ── 純粋な決まり ──────────────────────────────────────

    /// 続けて何回落ちたら黙って起こし直すのをやめるか（1 回目は 1 度だけ起こし直す）。
    pub def maxCrashesInRow(): Int32 = 2

    /// 起こしてから何ミリ秒 ready にならなければ「長く起きない」とするか（初回の jar 作りは 1〜2 分）。
    pub def stallMillis(): Int64 = 180000i64

    /// 理由に添えるログの行数。
    pub def tailLines(): Int32 = 12

    /// ふつうの頼みをどうするか。
    pub def decide(launch: Launch, seen: Seen): Decision = match launch {
        case Launch.Running(port) =>
            if (seen#answering) Decision.Answer(Readiness.Ready(port))
            else Decision.Answer(Readiness.Starting("窓なしのゲームが答えません（終わる途中なら、次の頼みで起こし直します）"))
        case Launch.Booting(port, startedAt, failures) =>
            if (seen#answering) Decision.MarkRunning(port)
            else if (seen#now - startedAt >= stallMillis())
                Decision.Answer(Readiness.Stalled(stalledMessage(seen#now - startedAt), seen#logTail))
            else Decision.Answer(Readiness.Booting(seen#stage, (seen#now - startedAt) / 1000i64, failures))
        case Launch.Idle => startUnlessGameStarting(0, seen)
        case Launch.Crashed(failures, why, tail) =>
            if (failures >= maxCrashesInRow()) Decision.Answer(Readiness.Failed(why, tail))
            else startUnlessGameStarting(failures, seen)
    }

    /// 「もう一度起こす」の頼み。落ちた・長く起きない時だけ 1 本起こし直す（それ以外はふつうの頼みと同じ）。
    /// 落ちた回数は持ち越す — 押して起こした 1 本がまた落ちたら、すぐ理由に戻る（押すたびに 1 本だけ）。
    pub def decideRestart(launch: Launch, seen: Seen): Decision = match launch {
        case Launch.Crashed(failures, _, _) => startUnlessGameStarting(failures, seen)
        case Launch.Booting(_, startedAt, failures) if not seen#answering and seen#now - startedAt >= stallMillis() =>
            startUnlessGameStarting(failures, seen)
        case _ => decide(launch, seen)
    }

    /// 起こしたプロセスが終わった後の状態。終わったのが今の 1 本（同じポート）でなければ変えない
    /// （起こし直した後に、止めた古い 1 本の終わりが届いても混ぜない）。
    pub def afterExit(exit: {port = Int32, why = String, logTail = List[String]}, launch: Launch): Launch = match launch {
        case Launch.Booting(port, _, failures) if port == exit#port => Launch.Crashed(failures + 1, exit#why, exit#logTail)
        case Launch.Running(port) if port == exit#port => Launch.Idle
        case other => other
    }

    /// 終了コードから落ちた理由。
    pub def exitReason(code: Int32): String =
        "窓なしのゲームが ready にならずに終わりました（終了コード ${code}）"

    /// bin/headless-run が jar を作る（またはほかの make が作り終えるのを待つ・jar なしでコンパイルする）時に出す行の印。
    pub def buildMarks(): List[String] =
        "[headless-run] jar を作る" :: "が jar を作っているので待つ" :: "[headless-run] flix run で流す" :: Nil

    /// 段を決める。log … debug/headless.log の全行・buildLocked … jar を作る鍵（build/headless/.lock）があるか。
    /// jar を作ると言った後でも、鍵が外れていれば作り終えて JVM を起こしている（flix run は鍵なしでコンパイルし続ける）。
    pub def bootStageOf(log: List[String], buildLocked: Bool): BootStage =
        let said = mark -> List.exists(line -> String.contains(substr = mark, line), log);
        if (said("[headless-run] flix run で流す")) BootStage.BuildingJar
        else if (buildLocked and List.exists(said, buildMarks())) BootStage.BuildingJar
        else BootStage.StartingGame

    /// 起こしている間の一言（段・起こしてからの経過秒。前に落ちていれば、それも言う）。
    pub def bootingMessage(stage: BootStage, elapsedSecs: Int64, failures: Int32): String =
        let again = if (failures > 0) "落ちたので、もう一度" else "";
        let secs = Int64.toString(elapsedSecs);
        match stage {
            case BootStage.BuildingJar  => "窓なしのゲームの jar を作っています（src が変わったので 1〜2 分。${again}起こして ${secs} 秒）"
            case BootStage.StartingGame => "窓なしのゲームを${again}起こしています（数秒。${secs} 秒）"
        }

    /// 長く起きない時の一言（起こしてからの経過ミリ秒）。
    pub def stalledMessage(elapsed: Int64): String =
        "窓なしのゲームが ${Int64.toString(elapsed / 60000i64)} 分たっても ready になりません（下は debug/headless.log の末尾。「もう一度起こす」で止めて起こし直せます）"

    def startUnlessGameStarting(failures: Int32, seen: Seen): Decision =
        if (seen#gameStarting) Decision.Answer(Readiness.Starting("ゲームが起動の途中なので、窓なしのゲームは起動が済んでから起こします"))
        else Decision.Start(failures)

    /// 今の 1 本のポート（起こしていなければ無し）。
    pub def portOf(launch: Launch): Option[Int32] = match launch {
        case Launch.Booting(port, _, _) => Some(port)
        case Launch.Running(port)       => Some(port)
        case _                          => None
    }

    // ── 入口 ──────────────────────────────────────────────

    /// ゲームが入口（pub def serveHeadless）を持っているか。src の .flix を探す。
    pub def hasEntry(dir: String): Bool \ IO =
        flixFilesUnder(new JFile("${dir}/src"))
            |> List.exists(file -> String.contains(substr = "def serveHeadless()", readText(file)))

    def flixFilesUnder(dir: JFile): List[JFile] \ IO =
        let entries = dir.listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries) |> List.flatMap(f ->
            if (f.isDirectory()) flixFilesUnder(f)
            else if (String.endsWith(suffix = ".flix", f.getName())) f :: Nil
            else Nil)

    def readText(file: JFile): String \ IO =
        try { new JavaString(Files.readAllBytes(file.toPath()), "UTF-8") } catch { case _: Exception => "" }

    /// 頼める状態にする。頼めればポート、まだなら裏で起こし始めて理由を返す。落ちていれば理由。
    pub def ensure(state: State, game: Game.Runtime, dir: String): Readiness \ IO =
        answer(state, game, dir, decide)

    /// 「もう一度起こす」。落ちた・長く起きない 1 本を起こし直す（止めてから 1 本だけ）。
    pub def restart(state: State, game: Game.Runtime, dir: String): Readiness \ IO =
        answer(state, game, dir, decideRestart)

    def answer(state: State, game: Game.Runtime, dir: String, rule: (Launch, Seen) -> Decision): Readiness \ IO =
        if (EngineTasks.isWindows())
            Readiness.Unsupported("Windows ではまだ窓なしのゲームを起こせません（bin/headless-run が bash のため）")
        else if (not hasEntry(dir))
            Readiness.Unsupported("このゲームには窓なしの入口がありません（src に pub def serveHeadless() を置く。engine の App.Headless の冒頭）")
        else {
            let gameStarting = Game.isStarting(game, dir);
            withLock(state, () -> {
                let launch = launchOf(state, dir);
                let seen = {
                    now = System.currentTimeMillis(),
                    answering = portOf(launch) |> Option.exists(answers),
                    gameStarting = gameStarting,
                    logTail = logTail(dir, tailLines()),
                    stage = bootStageOf(logLines(dir), new JFile("${dir}/build/headless/.lock").exists())
                };
                match rule(launch, seen) {
                    case Decision.Answer(readiness) => readiness
                    case Decision.MarkRunning(port) =>
                        discard (state#launches).put(dir, Launch.Running(port));
                        Readiness.Ready(port)
                    case Decision.Start(failures) =>
                        // 長く起きない 1 本を起こし直す時は、先に止める（JVM を 2 本並べない）
                        stopProcess(state, dir);
                        let port = freePort();
                        discard (state#launches).put(dir, Launch.Booting(port, seen#now, failures));
                        startInBackground(state, dir, port);
                        Readiness.Booting(BootStage.StartingGame, 0i64, failures)
                }
            })
        }

    /// 鍵を取って body を流す。body の中の思わぬ例外でも鍵は必ず返す（返さないと以後の頼みが全部止まる）。
    def withLock(state: State, body: Unit -> Readiness \ IO): Readiness \ IO =
        let lock = state#lock;
        lock.lock();
        let result = try { body() } catch {
            case e: Exception => Readiness.Failed("Studio が窓なしのゲームを世話できませんでした: ${e.getMessage()}", Nil)
        };
        lock.unlock();
        result

    def launchOf(state: State, dir: String): Launch \ IO =
        let value: Launch = (state#launches).get(dir);
        if (Object.isNull(value)) Launch.Idle else value

    /// 繋がるか（何も頼まずに閉じる）。
    def answers(port: Int32): Bool \ IO =
        try {
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 300);
            socket.close();
            true
        } catch {
            case _: Exception => false
        }

    /// 裏で起こして、終わったら状態に残す（ready にならずに終わったら Crashed）。
    def startInBackground(state: State, dir: String, port: Int32): Unit \ IO =
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                let why = launch(state, dir, port);
                discard withLock(state, () -> {
                    let exit = {port = port, why = why, logTail = logTail(dir, tailLines())};
                    discard (state#launches).put(dir, afterExit(exit, launchOf(state, dir)));
                    Readiness.Starting("")
                });
                "done"
            }
        });
        executor.shutdown()

    /// 起こして、終わるまで待つ。終わった理由（終了コード・起こせなかった例外）を返す。
    def launch(state: State, dir: String, port: Int32): String \ IO =
        try {
            let args = new ArrayList();
            discard args.add("bash");
            discard args.add("${EngineHome.dir()}/bin/headless-run");
            discard args.add(entry());
            let pb = new ProcessBuilder(args);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            discard new JFile("${dir}/debug").mkdirs();
            discard pb.redirectOutput(new JFile("${dir}/debug/headless.log"));
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            discard env.put("HEADLESS_PORT", "${port}");
            discard env.put("HEADLESS_IDLE_SECS", "600");
            // JAVA と HEADLESS_JAVA_OPTS は共通 jar を作る時・プレイと同じ組（SharedJar.jarEnv）。
            // 違う組で呼ぶと engine の jar の指紋が変わり、プレビューのたびに jar を作り直す
            foreach (pair <- SharedJar.jarEnv(EngineTasks.javaExe())) {
                let (name, value) = pair;
                discard env.put(name, value)
            };
            let proc = pb.start();
            discard (state#procs).put(dir, proc);
            let code = proc.waitFor();
            discard (state#procs).remove(dir, proc);
            exitReason(code)
        } catch {
            case e: Exception => "窓なしのゲームを起こせませんでした: ${e.getMessage()}"
        }

    /// 起こしたプロセスを子ども（jar を作っているコンパイラ・java）ごと止める。居なければ何もしない。
    def stopProcess(state: State, dir: String): Unit \ IO =
        let proc: Process = (state#procs).get(dir);
        if (Object.isNull(proc)) ()
        else try {
            stopAll(proc.descendants().iterator());
            discard proc.destroyForcibly();
            discard (state#procs).remove(dir, proc)
        } catch {
            case _: Exception => ()
        }

    def stopAll(handles: Iterator[ProcessHandle]): Unit \ IO =
        if (not handles.hasNext()) ()
        else {
            let handle = handles.next();
            discard handle.destroyForcibly();
            stopAll(handles)
        }

    /// ログの末尾（起こせなかった時の理由に添える）。空の行は数えない。
    pub def logTail(dir: String, count: Int32): List[String] \ IO =
        let lines = logLines(dir);
        List.drop(List.length(lines) - count, lines)

    /// debug/headless.log の全行（色の印を外し、空の行は数えない。起こすたびに書き直される）。
    def logLines(dir: String): List[String] \ IO =
        String.lines(readText(new JFile("${dir}/debug/headless.log")))
            |> List.map(withoutColorCodes)
            |> List.filter(line -> String.trim(line) != "")

    /// 端末の色の印（ESC [ … m。flix の出力に混ざる）を外す — 画面にはそのまま字で出るので。
    pub def withoutColorCodes(line: String): String =
        line.replaceAll("\u001b\\[[0-9;]*m", "")

    def freePort(): Int32 \ IO =
        let socket = new ServerSocket(0);
        let port = socket.getLocalPort();
        socket.close();
        port
}
