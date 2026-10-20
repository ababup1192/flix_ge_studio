// SharedJar — ゲームの共通 jar（engine の bin/headless-run が build/headless/headless.jar に作る 1 本）を、
// Studio が裏で先に作っておく係（プロジェクトごとに 1 つ）。
//
// 作った jar は、プレイ（窓あり）・描き出し（SceneRender.all など）・会話のプレビュー（窓なしのゲーム）が
// 使い回す。押した時に jar が古ければ headless-run が作り終えるのを待つだけ（鍵 build/headless/.lock で 1 本に並ぶ）。
//
// 1 本の移り変わり（Build。旗を重ねずにこの 1 つで持つ）:
//
//     Idle ──開いた・様子見──▶ Building ──済んだ──▶ Ready / Failed ──60 秒たって様子見・保存──▶ Building
//                               └─ 作っている間に保存 ──▶ BuildingThenAgain ──済んだ──▶ Building（もう 1 回）
//     engine の headless-run に作るだけの口が無い（古い版）・Windows ──▶ Unsupported（今までどおり flix run）
//
// engine の口（engine-gaps c397c425 の bin/headless-run）:
//   headless-run --build-only          … jar が古ければ作るだけ（新しければすぐ終わる）。終了 0 = jar がある・3 = 作れなかった
//   headless-run --window <入口>       … 窓ありで流す（入口 main はファイル直下の def main。macOS の -XstartOnFirstThread は自分で付ける）
//   headless-run <入口>                … 窓なしで流す（今までの形。常駐が「GL が要る」124 と答えた時は自分で窓ありに流し直す）
// jar の指紋には JAVA と HEADLESS_JAVA_OPTS の字も入るので、Studio から呼ぶ時は必ず jarEnv の同じ組を渡す。
// 口があるかは headless-run の本文にその字があるかで見る（古い版に知らない旗を渡すと、
// 旗を入口の名前と読んで flix run のコンパイルを始めてしまうため、渡す前に確かめる）。
//
// 何をするかは純粋な onLook / onSave / afterBuild / routeOf が決め、IO（プロセスを起こす・本文を読む）は
// このモジュールの下の方で読んで渡す。作った時の出力は debug/jar-build.log。
mod SharedJar {
    use fge::JsonCodec

    import java.io.{File => JFile}
    import java.lang.Exception
    import java.lang.{String => JavaString}
    import java.lang.ProcessBuilder
    import java.lang.System
    import java.nio.file.Files
    import java.util.ArrayList
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.util.concurrent.Executors
    import java.util.concurrent.Callable
    import java.util.concurrent.locks.ReentrantLock

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber}

    // ── 状態の型 ──────────────────────────────────────────

    /// 共通 jar の今（プロジェクトごとに 1 つ）。時刻はミリ秒。
    pub enum Build with Eq, ToString {
        /// まだ確かめていない
        case Idle
        /// 作っている（起こした時刻）
        case Building(Int64)
        /// 作っている間に保存が来た（起こした時刻。済んだらもう 1 回作る）
        case BuildingThenAgain(Int64)
        /// jar がある（確かめた時刻）
        case Ready(Int64)
        /// 作れなかった（確かめた時刻, debug/jar-build.log の末尾）
        case Failed(Int64, List[String])
        /// engine の headless-run に作るだけの口が無い・Windows（今までどおり flix run で流す）
        case Unsupported
    }

    /// 世話係の器（プロジェクトのパス → Build と、読み書きを 1 本ずつにする鍵）。
    pub type alias State = {builds = JMap[String, Build], lock = ReentrantLock}

    pub def make(): State \ IO = {builds = new JMap(), lock = new ReentrantLock()}

    // ── 純粋な決まり ──────────────────────────────────────

    /// 済んだ jar をもう一度確かめるまでの間（Studio の外で src を直した時に拾う）。
    pub def recheckMillis(): Int64 = 60000i64

    /// 理由に添えるログの行数。
    pub def tailLines(): Int32 = 15

    /// 開いた時・様子見（GET /jar/status・旅路）の決まり。Bool は「いま 1 本起こす」。
    pub def onLook(now: Int64, build: Build): (Build, Bool) = match build {
        case Build.Idle => (Build.Building(now), true)
        case Build.Ready(at) if now - at >= recheckMillis() => (Build.Building(now), true)
        case Build.Failed(at, _) if now - at >= recheckMillis() => (Build.Building(now), true)
        case other => (other, false)
    }

    /// jar に効くファイルを保存した後の決まり。作っている最中なら、済んだ後にもう 1 回作る印だけ付ける。
    pub def onSave(now: Int64, build: Build): (Build, Bool) = match build {
        case Build.Building(at)          => (Build.BuildingThenAgain(at), false)
        case Build.BuildingThenAgain(at) => (Build.BuildingThenAgain(at), false)
        case Build.Unsupported           => (Build.Unsupported, false)
        case _                           => (Build.Building(now), true)
    }

    /// 1 本作り終えた後の決まり（終了コード・ログの末尾）。作っている間に保存が来ていれば、もう 1 本。
    pub def afterBuild(now: Int64, exit: {code = Int32, tail = List[String]}, build: Build): (Build, Bool) = match build {
        case Build.BuildingThenAgain(_) => (Build.Building(now), true)
        case Build.Building(_) =>
            if (exit#code == 0) (Build.Ready(now), false) else (Build.Failed(now, exit#tail), false)
        case other => (other, false)
    }

    /// 保存したファイルが jar の中身（engine の指紋: src・flix.toml・lib）に効くか。
    /// assets/ や .yarn は jar に入らない（走る時にディスクから読む）ので作り直さない。
    pub def affectsJar(relPath: String): Bool =
        String.startsWith(prefix = "src/", relPath) or String.startsWith(prefix = "lib/", relPath)
            or relPath == "flix.toml" or relPath == "packages.lock"

    /// 作るだけの口（仮の名前）。
    pub def buildOnlyFlag(): String = "--build-only"

    /// 窓ありで流す口（仮の名前）。
    pub def windowFlag(): String = "--window"

    /// headless-run の本文から、作るだけの口があるか。
    pub def canBuildOnly(script: String): Bool = String.contains(substr = buildOnlyFlag(), script)

    /// その仕事を jar で流す時の headless-run への引数（bash と headless-run の後ろ）。
    /// jar で流せない仕事（型検査・テスト）・口の無い版・Windows は None（今までどおり flix run）。
    pub def routeOf(script: Option[String], isWindows: Bool, task: EngineTasks.Task): Option[List[String]] =
        match script {
            case Some(text) if not isWindows and canBuildOnly(text) => match task {
                case EngineTasks.Task.Debug =>
                    if (String.contains(substr = windowFlag(), text)) Some(windowFlag() :: "main" :: Nil) else None
                case EngineTasks.Task.Check            => None
                case EngineTasks.Task.Test             => None
                case EngineTasks.Task.Palette          => Some("Palette.write" :: Nil)
                case EngineTasks.Task.Render           => Some("SceneRender.all" :: Nil)
                case EngineTasks.Task.AtelierPreview   => Some("SceneRender.atelier" :: Nil)
                case EngineTasks.Task.EntryPoint(name) => Some(name :: Nil)
            }
            case _ => None
        }

    /// headless-run へ渡す環境変数。Studio から呼ぶ 3 つ（裏で作る --build-only・プレイと描き出し・
    /// 会話のプレビューの窓なしのゲーム）は必ずこの同じ組を渡す。
    // WhyNot: 仕事ごとに HEADLESS_JAVA_OPTS を変えない（会話のプレビューだけ -Xmx1g・窓ありだけ
    // -XstartOnFirstThread など）。engine の jar の指紋（bin/fge resident manifest）は src・lib に加えて
    // JAVA と HEADLESS_JAVA_OPTS の字も混ぜるので、旗が違う 2 本が交互に来ると、そのたびに jar を作り直す
    // （1〜2 分）。窓ありの旗は headless-run --window が自分で付ける。
    pub def jarEnv(javaExe: String): List[(String, String)] =
        ("JAVA", javaExe) :: ("HEADLESS_JAVA_OPTS", "") :: Nil

    /// 画面に出す様子（state: idle / building / ready / failed / unsupported・elapsed は作り始めてからの秒）。
    pub def statusJson(now: Int64, build: Build): Json =
        let (name, elapsed, log) = match build {
            case Build.Idle                  => ("idle", 0i64, Nil)
            case Build.Building(at)          => ("building", (now - at) / 1000i64, Nil)
            case Build.BuildingThenAgain(at) => ("building", (now - at) / 1000i64, Nil)
            case Build.Ready(_)              => ("ready", 0i64, Nil)
            case Build.Failed(_, tail)       => ("failed", 0i64, tail)
            case Build.Unsupported           => ("unsupported", 0i64, Nil)
        };
        JObject(Map#{
            "state"   => JString(name),
            "elapsed" => JNumber(JsonCodec.intToBd(Int64.clampToInt32(min = 0, max = Int32.maxValue(), elapsed))),
            "log"     => JArray(log |> List.map(JString) |> List.toVector)
        })

    // ── 入口（IO）──────────────────────────────────────────

    /// 様子に、engine が残した前回 jar を作るのにかかった秒数を足す（分かる時だけ。画面が「前回は N 秒」と言う）。
    pub def withLastBuild(last: Option[Int64], json: Json): Json = match (last, json) {
        case (Some(n), JObject(fields)) =>
            JObject(Map.insert("lastBuildSeconds", JNumber(JsonCodec.intToBd(Int64.clampToInt32(min = 0, max = Int32.maxValue(), n))), fields))
        case _ => json
    }

    /// 開いた時・様子見。要れば裏で 1 本作り始めて、今の様子を返す。
    pub def look(state: State, dir: String): Json \ IO =
        step(state, dir, onLook) |> withLastBuild(Headless.lastBuildSeconds(dir))

    /// jar に効くファイルを保存した後に呼ぶ（効かないパスなら何もしない）。
    pub def saved(state: State, dir: String, relPath: String): Unit \ IO =
        if (affectsJar(relPath)) discard step(state, dir, onSave) else ()

    /// その仕事を jar で流す時の完全な引数列（bash から）。流せなければ None。
    pub def argvFor(task: EngineTasks.Task): Option[List[String]] \ IO =
        routeOf(scriptText(), EngineTasks.isWindows(), task)
            |> Option.map(rest -> "bash" :: headlessRun() :: rest)

    def step(state: State, dir: String, rule: (Int64, Build) -> (Build, Bool)): Json \ IO =
        let now = System.currentTimeMillis();
        let supported = not EngineTasks.isWindows() and (scriptText() |> Option.exists(canBuildOnly));
        withLock(state, () -> {
            let current = if (supported) buildOf(state, dir) else Build.Unsupported;
            let (next, start) = rule(now, current);
            discard (state#builds).put(dir, next);
            if (start) startInBackground(state, dir) else ();
            statusJson(now, next)
        })

    def withLock(state: State, body: Unit -> Json \ IO): Json \ IO =
        let lock = state#lock;
        lock.lock();
        let result = try { body() } catch {
            case e: Exception => statusJson(0i64, Build.Failed(0i64, "Studio が共通 jar を世話できませんでした: ${e.getMessage()}" :: Nil))
        };
        lock.unlock();
        result

    def buildOf(state: State, dir: String): Build \ IO =
        let value: Build = (state#builds).get(dir);
        if (Object.isNull(value)) Build.Idle else value

    def headlessRun(): String \ IO = "${EngineHome.dir()}/bin/headless-run"

    def scriptText(): Option[String] \ IO =
        try {
            let file = new JFile(headlessRun());
            if (file.isFile()) Some(new JavaString(Files.readAllBytes(file.toPath()), "UTF-8")) else None
        } catch {
            case _: Exception => None
        }

    /// 裏で 1 本作り、済んだら afterBuild で次を決める（もう 1 本なら続けて起こす）。
    def startInBackground(state: State, dir: String): Unit \ IO =
        let executor = Executors.newSingleThreadExecutor();
        discard executor.submit(new Callable[String] {
            def call(_this: Callable[String]): String \ IO = {
                let code = buildOnce(dir);
                let exit = {code = code, tail = logTail(dir)};
                discard withLock(state, () -> {
                    let now = System.currentTimeMillis();
                    let (next, again) = afterBuild(now, exit, buildOf(state, dir));
                    discard (state#builds).put(dir, next);
                    if (again) startInBackground(state, dir) else ();
                    statusJson(now, next)
                });
                "done"
            }
        });
        executor.shutdown()

    /// headless-run --build-only を 1 回流して終了コードを返す（起こせなければ -1）。
    def buildOnce(dir: String): Int32 \ IO =
        try {
            let args = new ArrayList();
            discard args.add("bash");
            discard args.add(headlessRun());
            discard args.add(buildOnlyFlag());
            let pb = new ProcessBuilder(args);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            discard new JFile("${dir}/debug").mkdirs();
            discard pb.redirectOutput(new JFile("${dir}/debug/jar-build.log"));
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            foreach (pair <- jarEnv(EngineTasks.javaExe())) {
                let (name, value) = pair;
                discard env.put(name, value)
            };
            pb.start().waitFor()
        } catch {
            case _: Exception => -1
        }

    def logTail(dir: String): List[String] \ IO =
        let text = try { new JavaString(Files.readAllBytes(new JFile("${dir}/debug/jar-build.log").toPath()), "UTF-8") } catch {
            case _: Exception => ""
        };
        let lines = String.lines(text) |> List.map(Headless.withoutColorCodes) |> List.filter(line -> String.trim(line) != "");
        List.drop(List.length(lines) - tailLines(), lines)
}
