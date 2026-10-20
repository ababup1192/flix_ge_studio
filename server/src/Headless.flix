// Headless — 会話のプレビューに使う「窓なしのゲーム」の世話係（プロジェクトごとに 1 本）。
//
// 窓なしのゲームはゲーム自身のコード（engine の App.Headless・ゲームの E2e.serveHeadless）で、
// engine の bin/headless-run が jar を作って流す。Studio は自分で描かず、ここへ「この場面を 1 枚」と頼む
// （本物の立ち絵・表情・窓・後ろの場面が出る）。起動中のゲームは動かさない（動かすのは「この行へ飛ばす」だけ）。
//
// 1 本の移り変わり（Launch。旗を重ねずにこの 1 つで持つ）:
//
//     Idle ──頼み──▶ Booting ──ポートが答えた──▶ Running ──放っておかれて終わった──▶ Idle
//                      │  └─ 起こす段で 3 分たっても答えない: Stalled を答える（理由とログの末尾。待ち続ける）
//                      └─ ready にならずに終わった ──▶ Crashed(続けて落ちた回数, 理由, ログの末尾)
//     Crashed(1) ──次の頼み──▶ Booting（1 回だけ黙って起こし直す）
//     Crashed(2 以上) ──▶ Failed を答えたまま待つ（「もう一度起こす」= restart で 1 本だけ起こす）
//
// 決まり:
//   - 起こすのは頼まれた時だけ。立ち上げ中は待たせず「起こしています」を返す。Booting の中は 2 段（BootPhase）:
//     jar を作っている（Building）と、起こしている（Starting。JVM の立ち上げ・最初の 1 枚）。
//     jar を作る鍵（build/headless/.lock/pid）に生きている作り手が居る間は Building。作り手の pid が替わったら
//     経過秒を数え直して「2 回目」と数える。かかる時間の目安は決め打ちせず、engine が残す前回の秒数があればそれを言う
//   - 「長く起きない」（3 分）は起こしている段だけで数える。作っている段は作り手が生きている間待つ（上限は 20 分）
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
        /// 起こして ready を待っている（ポート, それまで続けて落ちた回数, 今の段）
        case Booting(Int32, Int32, BootPhase)
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

    /// 起こしている 1 本の段（Launch.Booting が持つ。頼みのたびに observe で進める）。時刻はミリ秒。
    pub enum BootPhase with Eq, ToString {
        /// JVM を起こして最初の 1 枚を待っている（この段に入った時刻, それまでに作り手を見た回数）
        case Starting(Int64, Int32)
        /// jar を作っている・作り終えるのを待っている（作り手の pid, この作り手を初めて見た時刻, 何回目の作り手か）
        case Building(Int32, Int64, Int32)
    }

    /// 画面に見せる段。
    ///   BuildingJar(何回目, 前回の秒数) … jar を作っている（前回の秒数は engine が残した物。無ければ目安を言わない）
    ///   StartingGame                    … jar はある。JVM を立ち上げて最初の 1 枚を待っている
    pub enum BootStage with Eq, ToString {
        case BuildingJar(Int32, Option[Int64])
        case StartingGame
    }

    /// 頼みの答え。
    ///   Ready(port)          … 頼める
    ///   Booting(stage, 秒, 落ちた回数) … 起こしている 1 本を待っている（段とその段に入ってからの経過秒。少し後にもう一度頼む）
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
    ///   builder      … jar を作っている生きた作り手の pid（鍵 build/headless/.lock/pid。jar を使わず flix run で
    ///                  コンパイルしている時は 0。誰も作っていなければ無し。builderOf で決めた物）
    ///   lastBuild    … engine が残した前回 jar を作るのにかかった秒数（lastBuildFile。無ければ無し）
    pub type alias Seen = {now = Int64, answering = Bool, gameStarting = Bool, logTail = List[String],
                           builder = Option[Int32], lastBuild = Option[Int64]}

    /// 頼みをどうするか。
    pub enum Decision with Eq, ToString {
        /// 状態はそのままで答える
        case Answer(Readiness)
        /// ready になった印を付けて Ready を答える（ポート）
        case MarkRunning(Int32)
        /// 状態を進めて答える（起こしている段が移った・作り手が替わった）
        case Observe(Launch, Readiness)
        /// 新しく 1 本起こす（それまで続けて落ちた回数を持ち越す）
        case Start(Int32)
    }

    // ── 純粋な決まり ──────────────────────────────────────

    /// 続けて何回落ちたら黙って起こし直すのをやめるか（1 回目は 1 度だけ起こし直す）。
    pub def maxCrashesInRow(): Int32 = 2

    /// 起こす段に入ってから何ミリ秒 ready にならなければ「長く起きない」とするか（jar を作っている間は数えない）。
    pub def stallMillis(): Int64 = 180000i64

    /// jar を作っている段の見切り（作り手が生きている間は待つが、これを過ぎたら「長く起きない」を見せる）。
    /// engine の headless-run も、ほかの作り手を 15 分待って諦める。
    pub def buildStallMillis(): Int64 = 1200000i64

    /// 理由に添えるログの行数。
    pub def tailLines(): Int32 = 12

    /// ふつうの頼みをどうするか。
    pub def decide(launch: Launch, seen: Seen): Decision = match launch {
        case Launch.Running(port) =>
            if (seen#answering) Decision.Answer(Readiness.Ready(port))
            else Decision.Answer(Readiness.Starting("窓なしのゲームが答えません（終わる途中なら、次の頼みで起こし直します）"))
        case Launch.Booting(port, failures, phase) =>
            if (seen#answering) Decision.MarkRunning(port)
            else {
                let next = observe(seen#now, seen#builder, phase);
                Decision.Observe(Launch.Booting(port, failures, next), readinessOf(seen, failures, next))
            }
        case Launch.Idle => startUnlessGameStarting(0, seen)
        case Launch.Crashed(failures, why, tail) =>
            if (failures >= maxCrashesInRow()) Decision.Answer(Readiness.Failed(why, tail))
            else startUnlessGameStarting(failures, seen)
    }

    /// 「もう一度起こす」の頼み。落ちた・長く起きない時だけ 1 本起こし直す（それ以外はふつうの頼みと同じ）。
    /// 落ちた回数は持ち越す — 押して起こした 1 本がまた落ちたら、すぐ理由に戻る（押すたびに 1 本だけ）。
    pub def decideRestart(launch: Launch, seen: Seen): Decision = match launch {
        case Launch.Crashed(failures, _, _) => startUnlessGameStarting(failures, seen)
        case Launch.Booting(_, failures, phase) if not seen#answering and isStalled(seen#now, observe(seen#now, seen#builder, phase)) =>
            startUnlessGameStarting(failures, seen)
        case _ => decide(launch, seen)
    }

    /// 起こしたプロセスが終わった後の状態。終わったのが今の 1 本（同じポート）でなければ変えない
    /// （起こし直した後に、止めた古い 1 本の終わりが届いても混ぜない）。
    pub def afterExit(exit: {port = Int32, why = String, logTail = List[String]}, launch: Launch): Launch = match launch {
        case Launch.Booting(port, failures, _) if port == exit#port => Launch.Crashed(failures + 1, exit#why, exit#logTail)
        case Launch.Running(port) if port == exit#port => Launch.Idle
        case other => other
    }

    /// 終了コードから落ちた理由。
    pub def exitReason(code: Int32): String =
        "窓なしのゲームが ready にならずに終わりました（終了コード ${code}）"

    /// 段を進める（builder … 今の作り手の pid。無ければ誰も作っていない）。
    /// 作り手が現れたら Building・替わったら経過を数え直して回数を 1 足す・居なくなったら Starting（そこから数える）。
    pub def observe(now: Int64, builder: Option[Int32], phase: BootPhase): BootPhase = match (phase, builder) {
        case (BootPhase.Building(pid, since, round), Some(current)) =>
            if (pid == current) BootPhase.Building(pid, since, round) else BootPhase.Building(current, now, round + 1)
        case (BootPhase.Building(_, _, round), None) => BootPhase.Starting(now, round)
        case (BootPhase.Starting(_, done), Some(pid)) => BootPhase.Building(pid, now, done + 1)
        case (BootPhase.Starting(since, done), None) => BootPhase.Starting(since, done)
    }

    /// 長く起きないか（起こす段は stallMillis・作っている段は buildStallMillis）。
    pub def isStalled(now: Int64, phase: BootPhase): Bool = match phase {
        case BootPhase.Starting(since, _)    => now - since >= stallMillis()
        case BootPhase.Building(_, since, _) => now - since >= buildStallMillis()
    }

    /// 段から答え（経過はその段に入ってからの秒）。
    pub def readinessOf(seen: Seen, failures: Int32, phase: BootPhase): Readiness =
        if (isStalled(seen#now, phase)) Readiness.Stalled(stalledMessage(phase, seen#now), seen#logTail)
        else match phase {
            case BootPhase.Starting(since, _) =>
                Readiness.Booting(BootStage.StartingGame, (seen#now - since) / 1000i64, failures)
            case BootPhase.Building(_, since, round) =>
                Readiness.Booting(BootStage.BuildingJar(round, seen#lastBuild), (seen#now - since) / 1000i64, failures)
        }

    /// jar を使わずに flix run でコンパイルしている時に headless-run が出す行の印（鍵を取らない）。
    pub def flixRunMark(): String = "[headless-run] flix run で流す"

    /// 作り手を決める（IO で読んだ物を渡す）。lockOwner … 鍵の pid のうち生きている物・log … debug/headless.log の全行。
    /// 鍵の作り手が居ればそれ。jar を使わず flix run でコンパイルしているなら 0（この 1 本が自分でコンパイルしている）。
    pub def builderOf(lockOwner: Option[Int32], log: List[String]): Option[Int32] = match lockOwner {
        case Some(pid) => Some(pid)
        case None =>
            if (List.exists(line -> String.contains(substr = flixRunMark(), line), log)) Some(0) else None
    }

    /// engine が残す、前回 jar を作るのにかかった秒数のファイル（ゲームの根から。engine 0.52.1 の名前は仮）。
    pub def lastBuildFile(): String = "build/headless/last-build-seconds"

    /// 起こしている間の一言（段・その段に入ってからの経過秒。前に落ちていれば、それも言う）。
    /// かかる時間の目安は決め打ちしない（前回の秒数が分かる時だけ言う）。
    pub def bootingMessage(stage: BootStage, elapsedSecs: Int64, failures: Int32): String =
        let again = if (failures > 0) "落ちたので、もう一度" else "";
        let secs = Int64.toString(elapsedSecs);
        match stage {
            case BootStage.BuildingJar(round, last) =>
                let what = if (round <= 1) "jar を作っています" else "jar を作り直しています（${Int32.toString(round)} 回目）";
                let took = match last {
                    case Some(n) => "前回は ${Int64.toString(n)} 秒。いま ${secs} 秒"
                    case None    => "${secs} 秒"
                };
                "窓なしのゲームの ${what}（${again}${took}）"
            case BootStage.StartingGame => "窓なしのゲームを${again}起こしています（${secs} 秒）"
        }

    /// 長く起きない時の一言（段とその段に入ってからの経過）。
    pub def stalledMessage(phase: BootPhase, now: Int64): String = match phase {
        case BootPhase.Starting(since, _) =>
            "窓なしのゲームが ${Int64.toString((now - since) / 60000i64)} 分たっても ready になりません（下は debug/headless.log の末尾。「もう一度起こす」で止めて起こし直せます）"
        case BootPhase.Building(_, since, _) =>
            "jar を作り始めて ${Int64.toString((now - since) / 60000i64)} 分たっても終わりません（下は debug/headless.log の末尾。「もう一度起こす」で止めて起こし直せます）"
    }

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
                    builder = builderOf(lockOwner(dir), logLines(dir)),
                    lastBuild = lastBuildSeconds(dir)
                };
                match rule(launch, seen) {
                    case Decision.Answer(readiness) => readiness
                    case Decision.Observe(next, readiness) =>
                        discard (state#launches).put(dir, next);
                        readiness
                    case Decision.MarkRunning(port) =>
                        discard (state#launches).put(dir, Launch.Running(port));
                        Readiness.Ready(port)
                    case Decision.Start(failures) =>
                        // 長く起きない 1 本を起こし直す時は、先に止める（JVM を 2 本並べない）
                        stopProcess(state, dir);
                        let port = freePort();
                        discard (state#launches).put(dir, Launch.Booting(port, failures, BootPhase.Starting(seen#now, 0)));
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

    /// jar を作る鍵の作り手のうち、生きている pid（鍵が無い・読めない・作り手が居なければ無し）。
    def lockOwner(dir: String): Option[Int32] \ IO =
        Int32.fromString(String.trim(readText(new JFile("${dir}/build/headless/.lock/pid"))))
            |> Option.filter(pid -> ProcessHandle.of(Int32.toInt64(pid)).isPresent())

    /// engine が残した前回 jar を作るのにかかった秒数（無い・読めなければ無し）。
    pub def lastBuildSeconds(dir: String): Option[Int64] \ IO =
        Int64.fromString(String.trim(readText(new JFile("${dir}/${lastBuildFile()}"))))

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
