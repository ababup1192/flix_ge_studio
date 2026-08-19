// EngineUpdate — 新しい engine を迎えに行く係。落とす・照合する・展開するの中身は
// engine の道具 (fge fetch-engine) が持っていて、ここはそれを走らせて結果を読み、
// 置き場の指し先を進める。切り替わった後は、開いているゲームを新しい engine へ
// 載せ替える (fge upgrade-game) ところまで続ける。

mod EngineUpdate {
    import java.lang.ProcessBuilder
    import java.lang.System
    import java.lang.Throwable
    import java.io.InputStreamReader
    import java.io.BufferedReader
    import java.util.ArrayList

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool, JNull}

    // ── 純粋なルール ──────────────────────────────────────

    /// 走らせる道具の場所。今使っている engine の物を使う。
    // WhyNot: 新しい側の道具を使わない — まだ落としていないので手元に無い。
    // WhyNot: bin/fge (sh のラッパ) でなく中身を直に呼ぶ — Windows には sh が無い。
    pub def toolPath(engineDir: String, onWindows: Bool): String =
        if (onWindows) "${engineDir}/bin/fge-go.exe" else "${engineDir}/bin/fge-go"

    /// 新しいバージョンが出ているかを見に行く呼び方。
    pub def checkArgs(tool: String, current: String): List[String] =
        tool :: "fetch-engine" :: "--check" :: "--current" :: current :: "--json" :: Nil

    /// 落として組み立てる呼び方。持ち越す元として今の置き場を渡す。
    pub def installArgs(r: {tool = String, version = String, versionsDir = String, carryFrom = String}): List[String] =
        r#tool :: "fetch-engine" :: "--version" :: r#version
            :: "--into" :: r#versionsDir :: "--carry-from" :: r#carryFrom :: "--json" :: Nil

    /// 開いているゲームを新しい engine へ載せ替える呼び方。
    /// engine 側にしか無い置き場 (fpkg の在り処・lib の中の名前) をここで組む。
    // WhyNot: engineDir に実パスを渡さない — 道具はこれを ENGINE=… としてゲームの
    // make へ流し、ゲームの Makefile は `FLIX := $(ENGINE)/bin/flix` をクォート無しで
    // 展開するので、名前に空白のあるホームでは道が千切れる。呼ぶ側は別名を渡す。
    pub def upgradeArgs(r: {tool = String, engineDir = String, game = String, version = String}): List[String] =
        r#tool :: "upgrade-game" :: "--root" :: r#engineDir
            :: "--game" :: r#game :: "--version" :: r#version
            :: "--fpkg" :: "${r#engineDir}/engine_full/artifact/engine_full.fpkg"
            :: "--toml" :: "${r#engineDir}/engine_full/flix.toml"
            :: "--dest" :: "lib/github/ababup1192/flix_game_engine/${r#version}"
            :: "--fpkg-name" :: "flix_game_engine-${r#version}.fpkg"
            :: "--toml-name" :: "flix_game_engine-${r#version}.toml"
            :: "--json" :: Nil

    /// 差し替えを実行できる環境か。
    // WhyNot: Windows で押させない — 公開されている zip は macOS 向けだけで、
    // 落とし切ってから中身の検査で必ず落ちる (帯の案内だけにする)。
    // WhyNot: 置き場から走っていないとき (engine のリポで開発中) も押させない —
    // 持ち越す元がリポ・置き先が置き場というちぐはぐな組み合わせになる。
    pub def canUpdate(r: {onWindows = Bool, fromStore = Bool}): Bool =
        not r#onWindows and r#fromStore

    /// 道具の出力から最後の JSON の行を採る。進み具合の行が前に並ぶため。
    pub def resultOf(lines: List[String]): Option[Map[String, Json]] =
        lines
            |> List.filterMap(line -> DocJson.parse(line) |> Option.flatMap(JsonCodec.expectObject))
            |> List.last

    /// 帯に「新しいのが出ています」を出してよいか。
    // WhyNot: 今のバージョンが読めないときに出さない — 道具は current が空だと
    // 無条件に available を返すので、既に最新でも帯が出て、押せば必ず失敗する。
    pub def shouldOffer(r: {current = String, available = Bool}): Bool =
        r#current != "" and r#available

    /// 差し替えが済んだときのバージョン。済んでいなければ None。
    pub def installedVersion(fields: Map[String, Json]): Option[String] =
        if (boolAt("installed", fields) == Some(true)) stringAt("version", fields) else None

    /// 指し先を進めてよいか。終了コードが 0 で、道具が済んだと言ったときだけ。
    // WhyNot: 終了コードだけで決めない — 途中で諦めても 0 で終わる形があり得るので、
    // 道具が置いたと言った物と両方がそろって初めて進める。
    pub def nextPointer(code: Int32, lines: List[String]): Option[String] =
        if (code != 0) None
        else resultOf(lines) |> Option.flatMap(installedVersion)

    /// 載せ替えの結果を人の言葉にする。終了コードは 0 上げ切った / 3 追随待ち / それ以外は失敗。
    pub def upgradeNote(code: Int32): String =
        match code {
            case 0 => "[update] 開いているゲームも新しい engine で緑になりました"
            case 3 => "[update] 開いているゲームは追随が要ります (直下の UPDATE_PLAN.md)"
            case _ => "[update] 開いているゲームの載せ替えは通りませんでした (元に戻してあります)"
        }

    // ── 走らせる ──────────────────────────────────────────

    /// GET /engine/update/check — 新しいバージョンが出ているか。
    pub def checkJson(): Json \ IO =
        let engine = EngineHome.dir();
        let current = EngineStore.versionOf(engine) |> Option.getWithDefault("");
        let updatable = canUpdate({onWindows = isWindows(), fromStore = isFromStore(engine)});
        let tool = toolPath(engine, isWindows());
        let (code, lines) = runCapture(checkArgs(tool, current), _ -> checked_ecast(()));
        match (code, resultOf(lines)) {
            case (0, Some(fields)) => {
                let available = shouldOffer({current = current,
                                             available = boolAt("available", fields) == Some(true)});
                JObject(Map#{
                    "ok"        => JBool(true),
                    "current"   => JString(current),
                    "updatable" => JBool(updatable),
                    "available" => JBool(available),
                    "version"   => if (available)
                                       stringAt("version", fields) |> Option.map(JString) |> Option.getWithDefault(JNull)
                                   else JNull
                })
            }
            // WhyNot: 見に行けないことを赤い失敗にしない — 網の向こうの都合で
            // エディタが使えなくなるのは割に合わない。出さないだけにする。
            case _ => JObject(Map#{
                "ok"        => JBool(true),
                "current"   => JString(current),
                "updatable" => JBool(updatable),
                "available" => JBool(false),
                "version"   => JNull
            })
        }

    /// 落として組み立て、済んだら指し先を進め、開いているゲームを載せ替える。
    /// 返すのは終了コード。
    pub def install(log: String -> Unit \ IO, version: String, game: Option[String]): Int32 \ IO =
        let home = javaHome();
        let engine = EngineHome.dir();
        let tool = toolPath(engine, isWindows());
        let versions = EngineStore.versionsDir(home);
        log("[update] engine ${version} を迎えに行きます");
        log("[update] いま走っているゲームは古い engine のまま動き続けます");
        let (code, lines) = runCapture(
            installArgs({tool = tool, version = version, versionsDir = versions, carryFrom = engine}), log);
        // WhyNot: 常駐の型検査を止めるのを落とし終えてからにする — 落とすのに数分かかる間に
        // 保存のフックが古い engine で起こし直すので、先に止めても意味が無い。指し先を
        // 動かす直前に止めれば、古い flix.jar を掴んだまま新しい規約で判定する形を断てる。
        log("[update] 常駐している型検査を止めます");
        discard runCapture(tool :: "checkd" :: "--stop-all" :: Nil, log);
        match nextPointer(code, lines) {
            case None => {
                log("[update] engine を差し替えられませんでした");
                if (code == 0) 1 else code
            }
            case Some(placed) =>
                // WhyNot: 指し先を進めるのを組み立ての後に置く — 途中で転んだときに
                // 実体の無いバージョンを指したまま次の起動を迎えないため。
                if (not EngineStore.writeCurrent(home, placed)) {
                    log("[update] 指し先を書き替えられませんでした (engine は前のままです)");
                    1
                } else {
                    EngineHome.relinkAlias();
                    log("[update] engine ${placed} に切り替えました");
                    upgradeGame(log, placed, game);
                    0
                }
        }

    /// 開いているゲームを新しい engine へ載せ替える。開いていなければ何もしない。
    // WhyNot: 載せ替えの失敗で差し替えを失敗にしない — engine の切り替えは既に済んで
    // いて、ゲーム側は道具が元へ戻す。ここで赤にすると、済んだ物まで失敗に見える。
    def upgradeGame(log: String -> Unit \ IO, version: String, game: Option[String]): Unit \ IO =
        match game {
            case None => log("[update] 開いているゲームはありません")
            case Some(dir) => {
                let engine = EngineHome.dir();
                // 道具を呼ぶのは実パス (シェルを通さないので空白があってもよい)。
                // ゲームの make へ流れる方 (--root) だけ別名を使う。
                let forGames = EngineHome.forGames() |> Option.getWithDefault(engine);
                log("[update] 開いているゲームを engine ${version} へ載せ替えます");
                let args = upgradeArgs({tool = toolPath(engine, isWindows()), engineDir = forGames,
                                        game = dir, version = version});
                let (code, _) = runCapture(args, log);
                log(upgradeNote(code))
            }
        }

    /// POST /engine/update — 走行権を取って差し替えを裏で始める。
    /// body は {version} の JSON。押すのは人 (バージョン上げは絵と挙動まで変わる)。
    pub def launch(state: Runner.State, game: Option[String], bodyText: String): (Int32, Json) \ IO =
        let engine = EngineHome.dir();
        if (not canUpdate({onWindows = isWindows(), fromStore = isFromStore(engine)}))
            (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("この環境では engine を差し替えられません (Studio ごと入れ直してください)")
            }))
        else match requestedVersion(bodyText) {
            case None => (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("body は {\"version\": \"0.32.0\"} の JSON で指定してください")
            }))
            case Some(version) =>
                Runner.launchWork(state, "engine-update",
                    () -> install(line -> Runner.logLine(state, line), version, game),
                    busy = "engine を差し替えている最中です")
        }

    /// body から差し替え先のバージョンを読む。置けない字は受けない。
    pub def requestedVersion(bodyText: String): Option[String] =
        DocJson.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(fields -> stringAt("version", fields))
            |> Option.filter(EngineStore.isVersionName)

    def isFromStore(engine: String): Bool \ IO =
        EngineHome.isStorePath(javaHome(), engine)

    def runCapture(args: List[String], onLine: String -> Unit \ IO): (Int32, List[String]) \ IO =
        try {
            let list = new ArrayList();
            foreach (arg <- args) discard list.add(arg);
            let pb = new ProcessBuilder(list);
            discard pb.redirectErrorStream(true);
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            let proc = pb.start();
            let reader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
            let lines = readLines(reader, onLine, Nil);
            (proc.waitFor(), lines)
        } catch {
            case e: Throwable => {
                onLine("[update] ${e.toString()}");
                (-1, Nil)
            }
        }

    // WhyNot: 全行を溜めない — 読みたいのは道具が最後に出す JSON の 1 行だけで、
    // 進み具合の行は log へ流した時点で役目が済んでいる。
    def readLines(reader: BufferedReader, onLine: String -> Unit \ IO, acc: List[String]): List[String] \ IO =
        let line = reader.readLine();
        if (Object.isNull(line)) List.reverse(acc)
        else {
            onLine(line);
            let kept = if (String.startsWith(prefix = "{", String.trim(line))) line :: acc else acc;
            readLines(reader, onLine, kept)
        }

    def javaHome(): String \ IO = System.getProperty("user.home")

    def isWindows(): Bool \ IO =
        String.startsWith(prefix = "windows", String.toLowerCase(System.getProperty("os.name")))

    def stringAt(key: String, fields: Map[String, Json]): Option[String] =
        Map.get(key, fields) |> Option.flatMap(JsonCodec.expectString)

    def boolAt(key: String, fields: Map[String, Json]): Option[Bool] =
        Map.get(key, fields) |> Option.flatMap(JsonCodec.expectBool)
}
