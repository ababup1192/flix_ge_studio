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

    /// 押させない理由。押せるなら空文字。
    // WhyNot: 「押せない」を黙って帯ごと消さない — 人からは壊れているのと区別が付かず、
    // 次に何をすればよいか (Studio ごと入れ直す) が誰にも伝わらない。
    // WhyNot: Windows で押させない — 公開されている zip は macOS 向けだけで、
    // 落とし切ってから中身の検査で必ず落ちる (帯の案内だけにする)。
    // WhyNot: 置き場から走っていないとき (engine のリポで開発中) も押させない —
    // 持ち越す元がリポ・置き先が置き場というちぐはぐな組み合わせになる。
    pub def blockReason(r: {onWindows = Bool, fromStore = Bool, hasTool = Bool}): String =
        if (r#onWindows)
            "Windows では engine を差し替えられません (Studio ごと入れ直してください)"
        else if (not r#fromStore)
            "engine のリポから走っているので、ここからは差し替えません"
        else if (not r#hasTool)
            "この engine は自分で上げられません (Studio ごと入れ直してください)"
        else ""

    /// 押せるはずの環境なのに道具が無い状態か。ここだけは、上げる先が分からなくても
    /// 画面に理由を出す (押せる環境と見分けが付かないまま何も出ないのを避ける)。
    pub def isStuck(r: {onWindows = Bool, fromStore = Bool, hasTool = Bool}): Bool =
        not r#onWindows and r#fromStore and not r#hasTool

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

    /// ゲームの flix.toml から、そのゲームが使う engine のバージョンを読む。
    // WhyNot: TOML として読まない — 要るのは依存 1 つの version だけ。
    // WhyNot: 行ごとに見ない — 依存は次のように複数行へ割って書けるので、
    // 行で切ると読めない (engine の道具の方は割れていても読める)。
    //     "github:ababup1192/flix_game_engine" = {
    //         version = "0.32.0", security = "unrestricted" }
    pub def gameVersionOf(tomlText: String): Option[String] =
        let body = stripComments(tomlText);
        String.indexOfLeft(substr = engineDependencyKey(), body)
            |> Option.map(i -> String.drop(i + String.length(engineDependencyKey()), body))
            |> Option.flatMap(inlineTable)
            |> Option.flatMap(versionIn)
            |> Option.filter(EngineStore.isVersionName)

    /// 依存の名前。owner まで含めて見る (フォークを本物と取り違えないため)。
    def engineDependencyKey(): String = "\"github:ababup1192/flix_game_engine\""

    /// `= { … }` の中身。次の閉じ括弧までを採る。
    def inlineTable(rest: String): Option[String] =
        String.indexOfLeft(substr = "{", rest)
            |> Option.flatMap(open -> {
                let inner = String.drop(open + 1, rest);
                String.indexOfLeft(substr = "}", inner) |> Option.map(close -> String.take(close, inner))
            })

    def versionIn(table: String): Option[String] =
        String.indexOfLeft(substr = "version", table)
            |> Option.flatMap(i -> firstQuoted(String.drop(i, table)))

    /// コメントの行を落とす (書き換え前の依存がコメントで残っていることがある)。
    def stripComments(tomlText: String): String =
        String.lines(tomlText)
            |> List.filter(line -> not String.startsWith(prefix = "#", String.trim(line)))
            |> String.unlines

    def firstQuoted(text: String): Option[String] =
        String.indexOfLeft(substr = "\"", text)
            |> Option.flatMap(i -> {
                let rest = String.drop(i + 1, text);
                String.indexOfLeft(substr = "\"", rest) |> Option.map(j -> String.take(j, rest))
            })

    /// ゲームが engine に追いついていないか。載せ替えのボタンを出す条件。
    // WhyNot: 「違う」ではなく「古い」で見る — ゲームの方が新しいときに押させると、
    // engine を落とすのでなくゲームを引き戻すことになり、押した人の意図と逆になる。
    pub def gameBehind(r: {game = Option[String], engine = String}): Bool =
        match r#game {
            case None    => false
            case Some(v) => EngineStore.isNewerVersion(v, r#engine)
        }

    /// 載せ替えの結果を人の言葉にする。終了コードは 0 上げ切った / 3 追随待ち / それ以外は失敗。
    pub def upgradeNote(code: Int32): String =
        match code {
            case 0 => "[update] 開いているゲームも新しい engine で緑になりました"
            case 3 => "[update] 開いているゲームは追随が要ります (直下の UPDATE_PLAN.md)"
            case _ => "[update] 開いているゲームの載せ替えは通りませんでした (元に戻してあります)"
        }

    // ── 走らせる ──────────────────────────────────────────

    /// その仕事ができる道具を選ぶ。今使っている engine → .app 同梱 engine の順。
    // WhyNot: 今の engine の道具に決め打ちしない — fetch-engine も upgrade-game も
    // 0.32.0 で入ったので、それより古い engine を指していると、押せるのに必ず失敗する。
    // 同梱の方は Studio を入れ直すたびに新しくなるので、逃げ道として使える。
    pub def pickTool(verb: String): Option[String] \ IO =
        let onWindows = isWindows();
        (EngineHome.dir() :: (EngineStore.bundledDir() |> Option.toList))
            |> List.map(dir -> toolPath(dir, onWindows))
            |> List.distinct
            |> List.find(tool -> hasVerb(tool, verb))

    def hasVerb(tool: String, verb: String): Bool \ IO =
        let (code, lines) = runLines(tool :: "--list" :: Nil);
        code == 0 and List.exists(line -> String.trim(line) == verb, lines)

    /// GET /engine/update/check — 新しいバージョンが出ているか。
    pub def checkJson(): Json \ IO =
        let engine = EngineHome.dir();
        let current = EngineStore.versionOf(engine) |> Option.getWithDefault("");
        let onWindows = isWindows();
        let fromStore = isFromStore(engine);
        // WhyNot: 先に環境を見てから道具を探す — Windows と engine のリポでは
        // どのみち押させないので、探すために起こすプロセスがそのまま無駄になる。
        let chosen = if (onWindows or not fromStore) None else pickTool("fetch-engine");
        let hasTool = not Option.isEmpty(chosen);
        let reason = blockReason({onWindows = onWindows, fromStore = fromStore, hasTool = hasTool});
        let updatable = reason == "";
        let stuck = isStuck({onWindows = onWindows, fromStore = fromStore, hasTool = hasTool});
        let tool = chosen |> Option.getWithDefault(toolPath(engine, onWindows));
        let (code, lines) = runCapture(checkArgs(tool, current), _ -> checked_ecast(()));
        match (code, resultOf(lines)) {
            case (0, Some(fields)) => {
                let available = shouldOffer({current = current,
                                             available = boolAt("available", fields) == Some(true)});
                JObject(Map#{
                    "ok"        => JBool(true),
                    "current"   => JString(current),
                    "updatable" => JBool(updatable),
                    "reason"    => JString(reason),
                    "stuck"     => JBool(stuck),
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
                "reason"    => JString(reason),
                "stuck"     => JBool(stuck),
                "available" => JBool(false),
                "version"   => JNull
            })
        }

    /// 落として組み立て、済んだら指し先を進め、開いているゲームを載せ替える。
    /// 返すのは終了コード。
    pub def install(log: String -> Unit \ IO, tool: String, version: String, game: Option[String]): Int32 \ IO =
        let home = javaHome();
        let engine = EngineHome.dir();
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
                    discard upgradeGame(log, tool, placed, game);
                    0
                }
        }

    /// GET /engine/game/check — 開いているゲームが engine に追いついているか。
    // WhyNot: 道具の有無まで見てから behind を立てる — upgrade-game は 0.32.0 で
    // 入ったので、それより古い engine ではボタンを出しても押した先で必ず失敗する。
    pub def gameCheckJson(game: Option[String]): Json \ IO =
        let engine = EngineStore.versionOf(EngineHome.dir()) |> Option.getWithDefault("");
        let onGame = gameVersion(game);
        let behind = gameBehind({game = onGame, engine = engine})
            and not Option.isEmpty(pickTool("upgrade-game"));
        JObject(Map#{
            "ok"     => JBool(true),
            "engine" => JString(engine),
            "game"   => onGame |> Option.map(JString) |> Option.getWithDefault(JNull),
            "behind" => JBool(behind)
        })

    /// 開いているゲームが使っている engine のバージョン。
    def gameVersion(game: Option[String]): Option[String] \ IO =
        game
            |> Option.flatMap(dir -> EngineStore.loadTextIfFile("${dir}/flix.toml"))
            |> Option.flatMap(gameVersionOf)

    /// POST /engine/upgrade-game — engine はそのままで、開いているゲームだけを追いつかせる。
    // WhyNot: engine の差し替えと同じ走行権を使う — どちらもゲームの lib を書き替えるので、
    // 同時に走らせると片方の書き込みがもう片方の途中の姿を踏む。
    pub def launchGameUpgrade(state: Runner.State, game: Option[String]): (Int32, Json) \ IO =
        match (game, EngineStore.versionOf(EngineHome.dir()), pickTool("upgrade-game")) {
            case (Some(dir), Some(version), Some(tool)) =>
                Runner.launchWork(state, "engine-update",
                    () -> upgradeGame(line -> Runner.logLine(state, line), tool, version, Some(dir)),
                    busy = "engine を差し替えている最中です")
            case (None, _, _) => (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("ゲームを開いてから押してください")
            }))
            case (_, None, _) => (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("いま使っている engine のバージョンが読めません")
            }))
            case (_, _, None) => (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("この engine はゲームを載せ替えられません (Studio ごと入れ直してください)")
            }))
        }

    /// 開いているゲームを新しい engine へ載せ替える。開いていなければ何もしない。
    /// 返すのは終了コード (追随が要るだけの 3 は、済んだ物として 0 で返す)。
    // WhyNot: 載せ替えの失敗で差し替えを失敗にしない — engine の切り替えは既に済んで
    // いて、ゲーム側は道具が元へ戻す。ここで赤にすると、済んだ物まで失敗に見える。
    def upgradeGame(log: String -> Unit \ IO, tool: String, version: String,
                    game: Option[String]): Int32 \ IO =
        match game {
            case None => { log("[update] 開いているゲームはありません"); 0 }
            case Some(dir) => {
                let engine = EngineHome.dir();
                // 道具を呼ぶのは実パス (シェルを通さないので空白があってもよい)。
                // ゲームの make へ流れる方 (--root) だけ別名を使う。
                let forGames = EngineHome.forGames() |> Option.getWithDefault(engine);
                log("[update] 開いているゲームを engine ${version} へ載せ替えます");
                let args = upgradeArgs({tool = tool, engineDir = forGames,
                                        game = dir, version = version});
                let (code, _) = runCapture(args, log);
                log(upgradeNote(code));
                if (code == 3) 0 else code
            }
        }

    /// POST /engine/update — 走行権を取って差し替えを裏で始める。
    /// body は {version} の JSON。押すのは人 (バージョン上げは絵と挙動まで変わる)。
    pub def launch(state: Runner.State, game: Option[String], bodyText: String): (Int32, Json) \ IO =
        let engine = EngineHome.dir();
        let onWindows = isWindows();
        let fromStore = isFromStore(engine);
        let chosen = if (onWindows or not fromStore) None else pickTool("fetch-engine");
        let reason = blockReason({onWindows = onWindows, fromStore = fromStore,
                                  hasTool = not Option.isEmpty(chosen)});
        if (reason != "")
            (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString(reason)
            }))
        else match requestedVersion(bodyText) {
            case None => (400, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JString("body は {\"version\": \"0.32.0\"} の JSON で指定してください")
            }))
            case Some(version) => {
                let tool = chosen |> Option.getWithDefault(toolPath(engine, onWindows));
                Runner.launchWork(state, "engine-update",
                    () -> install(line -> Runner.logLine(state, line), tool, version, game),
                    busy = "engine を差し替えている最中です")
            }
        }

    /// body から差し替え先のバージョンを読む。置けない字は受けない。
    pub def requestedVersion(bodyText: String): Option[String] =
        DocJson.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(fields -> stringAt("version", fields))
            |> Option.filter(EngineStore.isVersionName)

    def isFromStore(engine: String): Bool \ IO =
        EngineHome.isStorePath(javaHome(), engine)

    /// 出力を全部そのまま採る (道具が何を持っているかの一覧を読むため)。
    def runLines(args: List[String]): (Int32, List[String]) \ IO =
        runWith(args, _ -> checked_ecast(()), _ -> true)

    def runCapture(args: List[String], onLine: String -> Unit \ IO): (Int32, List[String]) \ IO =
        // WhyNot: 全行を溜めない — 読みたいのは道具が最後に出す JSON の 1 行だけで、
        // 進み具合の行は log へ流した時点で役目が済んでいる。
        runWith(args, onLine, line -> String.startsWith(prefix = "{", String.trim(line)))

    def runWith(args: List[String], onLine: String -> Unit \ IO,
                keep: String -> Bool): (Int32, List[String]) \ IO =
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
            let lines = readLines(reader, onLine, keep, Nil);
            (proc.waitFor(), lines)
        } catch {
            case e: Throwable => {
                onLine("[update] ${e.toString()}");
                (-1, Nil)
            }
        }

    def readLines(reader: BufferedReader, onLine: String -> Unit \ IO,
                  keep: String -> Bool, acc: List[String]): List[String] \ IO =
        let line = reader.readLine();
        if (Object.isNull(line)) List.reverse(acc)
        else {
            onLine(line);
            let kept = if (keep(line)) line :: acc else acc;
            readLines(reader, onLine, keep, kept)
        }

    def javaHome(): String \ IO = System.getProperty("user.home")

    def isWindows(): Bool \ IO =
        String.startsWith(prefix = "windows", String.toLowerCase(System.getProperty("os.name")))

    def stringAt(key: String, fields: Map[String, Json]): Option[String] =
        Map.get(key, fields) |> Option.flatMap(JsonCodec.expectString)

    def boolAt(key: String, fields: Map[String, Json]): Option[Bool] =
        Map.get(key, fields) |> Option.flatMap(JsonCodec.expectBool)
}
