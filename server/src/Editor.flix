// Editor — ui.json / hitbox.json エディタの常駐 HTTP サーバ本体。
// ブラウザのエディタ画面 (別オリジン) から fetch で叩くため、許可したオリジンにだけ
// CORS ヘッダを付けて応える。編集対象のプロジェクトは固定ではなく、起動後に
// POST /project で切り替えられる (未選択のまま起動もできる)。
// RemoteDebug と違いキュー (Bridge) は使わない: こちらにはゲームループが無く、リクエストの
// 相手 (ファイル) 以外に共有する状態も無いので、ハンドラの中でその場で読んで返せば足りる。
// executor を与えない (null) ので処理は 1 スレッド直列 = 同時リクエストも順番に並ぶ。

mod Editor {
    import java.lang.System
    import java.lang.Thread
    import java.lang.Exception
    import java.lang.ProcessBuilder
    import java.util.ArrayList
    import java.io.{File => JFile}
    import java.nio.file.Files
    import java.math.{BigDecimal => JBigDecimal}
    import java.net.InetSocketAddress
    import com.sun.net.httpserver.HttpServer
    import com.sun.net.httpserver.HttpHandler
    import com.sun.net.httpserver.HttpExchange
    import java.lang.{String => JavaString}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool, JNull}

    /// /health の version に載せる自分の版 (flix.toml の version と揃える)。
    def version(): String = "0.1.0"

    /// 環境変数 EDITOR_DIR (起動時に選んでおく編集対象)。未設定・空なら None (未選択で起動)。
    /// project.json の無いパス (展開されなかった ~ や打ち間違い) は、そのまま開くと全 API が
    /// 「読めない」で固まって見えるため、未選択に落としてピッカーに任せる。
    pub def dirFromEnv(): Option[String] \ IO =
        let value = System.getenv("EDITOR_DIR");
        if (Object.isNull(value) or value == "")
            None
        else if (not Projects.hasProjectJson(value)) {
            println("[studio] EDITOR_DIR にプロジェクトが見つかりません (project.json 無し): ${value} — 未選択で起動します");
            None
        } else
            Some(value)

    /// 環境変数 EDITOR_PORT。未設定・数値でなければ既定の 8787。
    pub def portFromEnv(): Int32 \ IO =
        let value = System.getenv("EDITOR_PORT");
        if (Object.isNull(value)) 8787 else Int32.fromString(value) |> Option.getWithDefault(8787)

    /// 環境変数 EDITOR_WEB (ビルド済みエディタ画面のディレクトリ)。未設定なら既定の
    /// 置き場所 (このリポの隣の flix_ge_editor/dist。cwd は editor_server/ なので 2 つ上がる)。
    /// ディレクトリとして存在しなければ None = 静的配信は無効 (API サーバとしては従来どおり動く)。
    pub def webRootFromEnv(): Option[String] \ IO =
        let value = System.getenv("EDITOR_WEB");
        let candidate = if (Object.isNull(value) or value == "") "../../flix_ge_editor/dist" else value;
        let dirFile = new JFile(candidate);
        if (dirFile.isDirectory()) Some(dirFile.getCanonicalPath()) else None

    /// 127.0.0.1:port で HTTP サーバを立て、戻らずに待ち続ける。
    /// initialDir は起動時の選択 (無ければ未選択で始め、POST /project を待つ)。
    /// caches はここで 1 回だけ作り、全リクエストで共有する (フォント atlas 等の焼き直し回避)。
    pub def serve(initialDir: Option[String], port: Int32): Unit \ IO =
        let selection = Projects.makeSelection(initialDir);
        let caches = Caches.make();
        let runner = Runner.make();
        let game = Game.make();
        // 新しいゲームづくり (make new-game) 用の走行権。焼き・ゲーム起動と同時でも走れる。
        let scaffold = Runner.make();
        // 検査と基準の自動追随の記録 (見た目が変わった知らせ)。
        let changes = Changes.make();
        let webRoot = webRootFromEnv();
        let server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0);
        discard server.createContext("/", new HttpHandler {
            def handle(_this: HttpHandler, exchange: HttpExchange): Unit \ IO =
                // 想定外の失敗でもサーバは落とさず、500 の JSON で理由を返して次のリクエストを受け続ける。
                try {
                    dispatch(selection, caches, runner, game, scaffold, changes, webRoot, exchange)
                } catch {
                    case e: Exception => {
                        let message = e.toString();
                        sendJson(500, errorJson("internal error: ${message}"), exchange)
                    }
                }
        });
        server.start();
        match initialDir {
            case Some(dir) => println("[studio] serving ${dir} on http://127.0.0.1:${port}")
            case None      => println("[studio] no project selected — GET /projects で候補を見て POST /project で選ぶ (http://127.0.0.1:${port})")
        };
        match webRoot {
            case Some(_) => println("[studio] web UI: http://127.0.0.1:${port}/")
            case None    => println("[studio] web UI: 無効(EDITOR_WEB が見つかりません)")
        };
        waitForever()

    /// main が返ると Flix ランタイムごと終了し得るので、ここで眠り続けてサーバを生かす。
    def waitForever(): Unit \ IO =
        Thread.sleep(60000i64);
        waitForever()

    // ── ルーティング ──────────────────────────────────────

    /// メソッドとパスで各エンドポイントへ振り分ける。API のパスが先に並び、
    /// どれにも当たらない GET はビルド済みエディタ画面の静的ファイルとして探す。
    def dispatch(selection: Projects.Selection, caches: Caches.Caches, runner: Runner.State, game: Runner.State, scaffold: Runner.State, changes: Changes.State, webRoot: Option[String], exchange: HttpExchange): Unit \ IO =
        let methodName = exchange.getRequestMethod();
        let uri = exchange.getRequestURI();
        let path = uri.getPath();
        let rawQuery = uri.getQuery();
        let params = queryParams(if (Object.isNull(rawQuery)) "" else rawQuery);
        match (methodName, path) {
            case ("OPTIONS", _)        => sendPreflight(exchange)
            case ("GET", "/projects")  => handleProjects(selection, exchange)
            case ("POST", "/project")  => handleSelectProject(selection, readBody(exchange), exchange)
            case ("GET", "/running-games") => handleRunningGames(exchange)
            // 「新しいゲームをはじめる」の家族一覧と公式プロンプト。
            // プロジェクト未選択でも呼べる (まっさらからの入口なので)。
            case ("GET", "/genesis/families") => sendJson(200, Genesis.familiesJson(), exchange)
            case ("GET", "/prompt/genesis") => {
                let (status, json) = Genesis.promptJson(params);
                sendJson(status, json, exchange)
            }
            case ("GET", "/genesis/title") => match Genesis.titleBytes(params) {
                case Some(bytes) => sendBytes("image/png", bytes, exchange)
                case None        => sendJson(404, errorJson("title not found"), exchange)
            }
            // 新しいゲームづくりはプロジェクト未選択でも呼べる (まっさらからの入口なので)。
            case ("POST", "/projects/new") => {
                let (status, json) = Scaffold.startNew(scaffold, readBody(exchange));
                sendJson(status, json, exchange)
            }
            case ("GET", "/projects/new/log") => sendJson(200, Runner.logJson(scaffold), exchange)
            case ("GET", "/health")    => withProject(selection, exchange, dir -> handleHealth(dir, exchange))
            case ("GET", "/files")     => withProject(selection, exchange, dir -> handleFiles(dir, exchange))
            case ("GET", "/changes")   => withProject(selection, exchange, dir -> handleChanges(dir, exchange))
            case ("GET", "/resources") => withProject(selection, exchange, dir -> handleResources(dir, exchange))
            case ("GET", "/file")      => withProject(selection, exchange, dir -> handleReadFile(dir, params, exchange))
            case ("PUT", "/file")      => withProject(selection, exchange, dir -> handleWriteFile(dir, changes, runner, readBody(exchange), exchange))
            case ("POST", "/preview/ui") => withProject(selection, exchange, dir -> {
                let (status, json) = PreviewUi.respond(dir, caches, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("POST", "/preview/hitbox") => withProject(selection, exchange, dir -> {
                let (status, json) = PreviewHitbox.respond(dir, caches, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("POST", "/preview/fx") => withProject(selection, exchange, dir -> {
                let (status, json) = PreviewFx.respond(dir, caches, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("POST", "/preview/items") => withProject(selection, exchange, dir -> {
                let (status, json) = PreviewItems.respond(dir, caches, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("GET", "/gallery/list") => withProject(selection, exchange, dir ->
                sendJson(200, Gallery.listJson(dir), exchange))
            case ("GET", "/gallery/image") => withProject(selection, exchange, dir ->
                handleGalleryImage(dir, params, exchange))
            case ("GET", "/gallery/sound") => withProject(selection, exchange, dir ->
                handleGallerySound(dir, params, exchange))
            // 旅路を見るたびに検査が 1 目盛り進む (自動焼き・自動追随は Changes の仕事)。
            case ("GET", "/journey/state") => withProject(selection, exchange, dir -> {
                discard Changes.pump(changes, runner, dir);
                sendJson(200, Journey.stateJson(dir, Changes.unseenCount(changes, dir)), exchange)
            })
            case ("GET", "/journey/changes") => withProject(selection, exchange, dir ->
                sendJson(200, Changes.pump(changes, runner, dir), exchange))
            case ("POST", "/journey/changes/seen") => withProject(selection, exchange, dir -> {
                let (status, json) = Changes.markSeen(changes, dir);
                sendJson(status, json, exchange)
            })
            case ("GET", "/runner/log") => sendJson(200, Runner.logJson(runner), exchange)
            case ("GET", "/atelier/candidates") => withProject(selection, exchange, dir ->
                sendJson(200, Atelier.candidatesJson(runner, dir), exchange))
            case ("GET", "/atelier/preview") => withProject(selection, exchange, dir ->
                handleAtelierPreview(dir, params, exchange))
            case ("POST", "/atelier/promote") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.promote(dir, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("GET", "/atelier/archive") => withProject(selection, exchange, dir ->
                sendJson(200, Atelier.archiveJson(dir), exchange))
            case ("POST", "/atelier/archive") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.archive(dir, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("POST", "/atelier/restore") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.restore(dir, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("GET", "/atelier/slots") => withProject(selection, exchange, dir ->
                sendJson(200, Atelier.slotsJson(dir), exchange))
            case ("GET", "/prompt/atelier") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.promptAtelierJson(dir, params);
                sendJson(status, json, exchange)
            })
            case ("GET", "/prompt/wire") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.promptWireJson(dir, params);
                sendJson(status, json, exchange)
            })
            case ("GET", "/prompt/extend") => withProject(selection, exchange, dir -> {
                let (status, json) = Extend.promptJson(dir, params);
                sendJson(status, json, exchange)
            })
            case ("POST", "/atelier/copy") => withProject(selection, exchange, dir -> {
                let (status, json) = Atelier.copy(dir, readBody(exchange));
                sendJson(status, json, exchange)
            })
            case ("GET", "/game/status") => withProject(selection, exchange, dir ->
                sendJson(200, Game.statusJson(dir), exchange))
            case ("POST", "/game/start") => withProject(selection, exchange, dir -> {
                let (status, json) = Game.start(game, dir);
                sendJson(status, json, exchange)
            })
            case ("GET", "/game/log") => sendJson(200, Runner.logJson(game), exchange)
            case ("GET", _)            => handleStatic(webRoot, path, exchange)
            case ("HEAD", _)           => handleStatic(webRoot, path, exchange)
            case _                     => sendJson(404, errorJson("unknown endpoint: ${methodName} ${path}"), exchange)
        }

    /// 選択中プロジェクトが要るエンドポイントの門番。未選択は 200 の ok:false で理由を返す
    /// (エディタ画面はこれを見てプロジェクト選択へ誘導する)。
    def withProject(selection: Projects.Selection, exchange: HttpExchange, respond: String -> Unit \ IO): Unit \ IO =
        match Projects.currentDir(selection) {
            case None      => sendJson(200, noProjectJson(), exchange)
            case Some(dir) => respond(dir)
        }

    /// プロジェクト未選択のときの応答本文。error を {message} の形にするのは、
    /// preview 系の失敗応答と同じ読み方でエディタ画面が扱えるようにするため。
    def noProjectJson(): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString("no project selected")})
        })

    // ── プロジェクト切替 ──────────────────────────────────

    /// GET /projects — 切替候補の一覧。current は選択中 (未選択は null)、recent は最近開いた順、
    /// found は探索起点 (EDITOR_SCAN_ROOTS、既定 $HOME/Desktop) の深さ 2 までで見つけた
    /// project.json 持ちディレクトリ。
    def handleProjects(selection: Projects.Selection, exchange: HttpExchange): Unit \ IO =
        let current = Projects.currentDir(selection);
        // 消えた・移動したプロジェクトは候補に出さない。減っていたら履歴ファイルも掃除する
        let stored = Projects.loadRecent();
        let alive = Projects.pruneRecent(dir -> Projects.hasProjectJson(dir), stored);
        if (List.length(alive) != List.length(stored)) Projects.saveRecent(alive) else ();
        let recent = alive |> List.map(dir -> projectEntryJson(dir));
        let found = Projects.findProjects(Projects.scanRootsFromEnv()) |> List.map(dir -> projectEntryJson(dir));
        sendJson(200, JObject(Map#{
            "current" => current |> Option.map(dir -> JString(dir)) |> Option.getWithDefault(JNull),
            "recent"  => JArray(List.toVector(recent)),
            "found"   => JArray(List.toVector(found))
        }), exchange)

    /// 候補 1 件 {dir, title}。title は project.json の題名 (読めなければディレクトリ名)。
    def projectEntryJson(dir: String): Json \ IO =
        JObject(Map#{"dir" => JString(dir), "title" => JString(Projects.titleOf(dir))})

    /// POST /project (body: {dir}) — 編集対象プロジェクトの切替。存在しない・project.json が
    /// 無い/読めない dir は 400 で断り、選択は変えない。成功したら履歴の先頭へ記録する。
    def handleSelectProject(selection: Projects.Selection, bodyText: String, exchange: HttpExchange): Unit \ IO =
        let dirOpt = JsonCompat.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> Map.get("dir", obj))
            |> Option.flatMap(JsonCodec.expectString);
        match dirOpt {
            case None      => sendJson(400, errorJson("body must be a JSON object {dir}"), exchange)
            case Some(dir) =>
                if (not new JFile(dir).isDirectory())
                    sendJson(400, errorJson("プロジェクトが見つかりません(削除または移動されています): ${dir}"), exchange)
                else if (not Projects.hasProjectJson(dir))
                    sendJson(400, errorJson("この場所は開けません(project.json がありません): ${dir}"), exchange)
                else
                    // 履歴・current が同じプロジェクトの別表記 (末尾スラッシュ等) で
                    // 二重登録されないよう、正規化した絶対パスに揃えてから選択する。
                    selectValidated(new JFile(dir).getCanonicalPath(), selection, exchange)
        }

    /// 存在確認済みの dir へ切り替える。project.json が壊れていて読めないときは 400 で断る
    /// (title も design も返せない切替は、エディタ画面がその後の全 API で困るだけ)。
    def selectValidated(dir: String, selection: Projects.Selection, exchange: HttpExchange): Unit \ IO =
        match (run { ProjectLoader.loadProject(dir) } with Fs.FileRead.runWithIO) {
            case Err(message)      => sendJson(400, errorJson("project.json: ${message}"), exchange)
            case Ok(loadedProject) =>
                Projects.switchTo(dir, selection);
                Projects.saveRecent(Projects.pushRecent(dir, Projects.loadRecent()));
                let config = loadedProject#config;
                sendJson(200, JObject(Map#{
                    "ok"     => JBool(true),
                    "dir"    => JString(dir),
                    "title"  => JString(config#title),
                    "design" => JObject(Map#{
                        "w" => JNumber(JsonCodec.intToBd(config#designWidth)),
                        "h" => JNumber(JsonCodec.intToBd(config#designHeight))
                    })
                }), exchange)
        }

    // ── 走っているゲームの検知 ────────────────────────────

    /// GET /running-games — いま走っているゲームプロセスを {pid, cwd} の列で返す。
    /// ブラウザでも .app でも同じく動くよう、検知はサーバ側 (外部コマンド) で行う。
    /// 目印は LWJGL/GLFW が要る起動フラグ -XstartOnFirstThread で、かつ headless でない
    /// (このサーバ自身は headless なので除かれる)。
    // WhyNot: 失敗を 500 にしない — 検知はバッジ表示という補助機能で、外部コマンドが
    // 落ちてもエディタ本体は動くべきなので、取れなければ空の一覧で静かに返す (fail-open)。
    def handleRunningGames(exchange: HttpExchange): Unit \ IO =
        let games = try {
            runCommand("ps" :: "-Ao" :: "pid=,command=" :: Nil)
                |> String.lines
                |> List.filterMap(parseGameLine)
                |> List.map(pid -> JObject(Map#{
                    "pid" => JNumber(JsonCodec.intToBd(pid)),
                    "cwd" => JString(cwdOf(pid))
                }))
        } catch {
            case _: Exception => Nil
        };
        sendJson(200, JObject(Map#{"games" => JArray(List.toVector(games))}), exchange)

    /// ps の 1 行 "  1234 /usr/bin/java …" を見て、ゲームなら pid を返す。
    /// 先頭の数字が pid、残りがコマンドライン。目印フラグを持ち headless でない行だけ拾う。
    def parseGameLine(line: String): Option[Int32] =
        let trimmed = String.trim(line);
        match String.indexOfLeft(substr = " ", trimmed) {
            case None      => None
            case Some(pos) =>
                let pidText = String.slice(start = 0, end = pos, trimmed);
                let cmdline = String.slice(start = pos, end = String.length(trimmed), trimmed);
                let isGame = String.contains(substr = "-XstartOnFirstThread", cmdline)
                    and not String.contains(substr = "-Djava.awt.headless=true", cmdline);
                if (isGame) Int32.fromString(pidText) else None
        }

    /// pid の作業ディレクトリ (cwd)。lsof の -Fn 出力は "n<path>" 行なので、その接頭辞を剥がす。
    /// 取れなければ空文字 (どのプロジェクトかは分からないが、走っている事実は伝わる)。
    def cwdOf(pid: Int32): String \ IO =
        try {
            runCommand("lsof" :: "-a" :: "-d" :: "cwd" :: "-p" :: "${pid}" :: "-Fn" :: Nil)
                |> String.lines
                |> List.filterMap(line ->
                    if (String.startsWith(prefix = "n", line)) Some(String.dropLeft(1, line)) else None)
                |> List.head
                |> Option.getWithDefault("")
        } catch {
            case _: Exception => ""
        }

    /// 外部コマンドを 1 回走らせ、標準出力を UTF-8 文字列で返す。
    /// この Mac の DEVELOPER_DIR/SDKROOT は nix パスが刺さっていて子プロセスを壊すので外す。
    def runCommand(args: List[String]): String \ IO =
        let list = new ArrayList();
        foreach (arg <- args) discard list.add(arg);
        let pb = new ProcessBuilder(list);
        let env = pb.environment();
        discard env.remove("DEVELOPER_DIR");
        discard env.remove("SDKROOT");
        let proc = pb.start();
        let bytes = proc.getInputStream().readAllBytes();
        discard proc.waitFor();
        new JavaString(bytes, "UTF-8")

    // ── ファイル・プレビュー系 ────────────────────────────

    /// GET /health — 対象プロジェクトの題名と設計解像度を返す。エディタ画面の接続確認用。
    /// project.json が読めなくてもサーバは起動継続する (ok: false で理由だけ返す)。
    def handleHealth(dir: String, exchange: HttpExchange): Unit \ IO =
        let loaded = run { ProjectLoader.loadProject(dir) } with Fs.FileRead.runWithIO;
        match loaded {
            case Err(message) => sendJson(200, errorJson(message), exchange)
            case Ok(loadedProject) =>
                let config = loadedProject#config;
                sendJson(200, JObject(Map#{
                    "ok"      => JBool(true),
                    "dir"     => JString(dir),
                    "title"   => JString(config#title),
                    "design"  => JObject(Map#{
                        "w" => JNumber(JsonCodec.intToBd(config#designWidth)),
                        "h" => JNumber(JsonCodec.intToBd(config#designHeight))
                    }),
                    "version" => JString(version())
                }), exchange)
        }

    /// GET /files — 編集対象ファイルの一覧 (ui / hitbox / fx / palette 別) を返す。
    /// ui はメタ情報付き {path, composable, paletteFile?} の列 (hitbox は従来どおり文字列の列)。
    /// palette は ui 側の "paletteFile" 参照から逆引きした実在ファイルの列 —
    /// 名前パターンで拾うより「実際に参照されている物」を出す方が確実。
    def handleFiles(dir: String, exchange: HttpExchange): Unit \ IO =
        let index = FileIndex.listFiles(dir);
        let uiMetas = index#ui |> List.map(relPath -> (relPath, uiFileMeta(dir, relPath)));
        let paletteList = uiMetas
            |> List.filterMap(entry -> snd(entry)#paletteFile)
            |> List.distinct
            |> List.sort
            |> List.filter(path -> Result.isOk(FileIndex.readInside(dir, path)));
        let uiEntries = uiMetas |> List.map(entry -> {
            let (relPath, meta) = entry;
            let base = Map#{"path" => JString(relPath), "composable" => JBool(meta#composable)};
            JObject(match meta#paletteFile {
                case None      => base
                case Some(ref) => Map.insert("paletteFile", JString(ref), base)
            })
        });
        sendJson(200, JObject(Map#{
            "root"    => JString(dir),
            "ui"      => JArray(List.toVector(uiEntries)),
            "hitbox"  => JsonCodec.encodeList(p -> JString(p), index#hitbox),
            "fx"      => JsonCodec.encodeList(p -> JString(p), index#fx),
            "palette" => JsonCodec.encodeList(p -> JString(p), paletteList)
        }), exchange)

    /// GET /resources — project.json の "editor" 宣言と一致ファイルを配る。
    /// サーバは宣言を解釈しない(plugin は文字列のまま素通し) — どれを扱えるかの選別は
    /// エディタ側の仕事にすることで、種別が増えてもサーバ改修を要らなくする。
    def handleResources(dir: String, exchange: HttpExchange): Unit \ IO =
        match FileIndex.readInside(dir, "project.json") {
            case Err(message) => sendJson(400, errorJson("project.json: ${message}"), exchange)
            case Ok(text) => match JsonCompat.parse(text) {
                case None => sendJson(400, errorJson("project.json が JSON として読めません"), exchange)
                case Some(json) => match ResourceDecl.fromProjectJson(json) {
                    case Err(message) => sendJson(400, errorJson(message), exchange)
                    case Ok(decls) => {
                        let all = FileIndex.listAllJson(dir);
                        let entries = decls#resources |> List.map(decl -> resourceJson(dir, all, decl));
                        let dashboards = decls#dashboards |> List.map(dashboardJson);
                        let warnings = List.append(
                            declKeyWarnings(json),
                            decls#resources |> List.flatMap(decl -> declWarnings(dir, all, decl)));
                        sendJson(200, JObject(Map#{
                            "root"       => JString(dir),
                            "resources"  => JArray(List.toVector(entries)),
                            "dashboards" => JArray(List.toVector(dashboards)),
                            "warnings"   => JArray(warnings |> List.map(w -> JString(w)) |> List.toVector)
                        }), exchange)
                    }
                }
            }
        }

    /// 宣言 1 件ぶんの応答。files は pattern に一致した実ファイルで、各ファイルに
    /// スキーマパス(宣言 or 隣の *.schema.json)を実在確認つきで添える — エディタが
    /// 「フォームが出るか」を開く前に知れるように。
    def resourceJson(dir: String, allJsonPaths: List[String], decl: ResourceDecl.Decl): Json \ IO =
        let files = ResourceDecl.matchFiles(decl#pattern, allJsonPaths) |> List.map(path -> {
            let schemaPath = match decl#schemaPath {
                case Some(s) => s
                case None    => ResourceDecl.siblingSchemaPath(path)
            };
            let base = Map#{"path" => JString(path)};
            JObject(if (Result.isOk(FileIndex.readInside(dir, schemaPath)))
                        Map.insert("schema", JString(schemaPath), base)
                    else base)
        });
        let base = Map#{
            "id"      => JString(decl#id),
            "pattern" => JString(decl#pattern),
            "files"   => JArray(List.toVector(files))
        };
        let withTitle = match decl#title {
            case Some(t) => Map.insert("title", JString(t), base)
            case None    => base
        };
        JObject(match decl#plugin {
            case Some(p) => Map.insert("plugin", JString(p), withTitle)
            case None    => withTitle
        })

    /// editor.resources[] の各宣言に知らないキーが無いかを見る。タイポ(例: schemaPath)は
    /// エラーにならず「schema 未指定」として黙って sibling 規約へ倒れ、フォームが出ない理由が
    /// 誰にも分からなくなる — ここで名指しすることでだけ気づける。
    def declKeyWarnings(projectJson: Json): List[String] =
        let known = Set#{"id", "pattern", "title", "schema", "plugin", "role"};
        let decls = JsonCodec.expectObject(projectJson)
            |> Option.flatMap(obj -> Map.get("editor", obj))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> Map.get("resources", obj))
            |> Option.map(rs -> match rs {
                case JArray(items) => Vector.toList(items)
                case _             => Nil
            })
            |> Option.getWithDefault(Nil);
        decls |> List.flatMap(d -> match JsonCodec.expectObject(d) {
            case None      => Nil
            case Some(obj) =>
                let declId = Map.get("id", obj) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("?");
                Map.keysOf(obj) |> Set.toList
                    |> List.filter(k -> not Set.memberOf(k, known))
                    |> List.map(k -> "宣言『${declId}』に未知のキー『${k}』があります(使えるのは id / pattern / title / schema / plugin / role)")
        })

    /// 宣言と実ファイルの健康診断。応答は 200 のまま warnings で「見える化」だけする —
    /// 読み込みは fail-open で既定へ倒れるので、壊れていてもゲームは動く。ここで気づけないと
    /// 「直したのに反映されない(実は JSON が壊れて既定に倒れている)」で人が迷子になる。
    def declWarnings(dir: String, allJsonPaths: List[String], decl: ResourceDecl.Decl): List[String] \ IO =
        let files = ResourceDecl.matchFiles(decl#pattern, allJsonPaths);
        let noMatch =
            if (List.isEmpty(files)) "宣言『${decl#id}』の pattern『${decl#pattern}』に一致するファイルがありません" :: Nil
            else Nil;
        let schemaMissing = match decl#schemaPath {
            case Some(s) =>
                if (Result.isOk(FileIndex.readInside(dir, s))) Nil
                else "宣言『${decl#id}』の schema『${s}』が見つかりません" :: Nil
            case None => Nil
        };
        let fileWarns = files |> List.flatMap(path -> match FileIndex.readInside(dir, path) {
            case Err(_)   => "『${path}』が読めません" :: Nil
            case Ok(text) => match JsonCompat.parse(text) {
                case None       => "『${path}』が JSON として読めません(読み込みは既定値へ倒れます)" :: Nil
                case Some(json) => match JsonCodec.expectObject(json) {
                    case None      => "『${path}』のトップが JSON オブジェクトではありません" :: Nil
                    case Some(obj) =>
                        if (Map.memberOf("version", obj)) Nil
                        else "『${path}』に version がありません(Doc 規約)" :: Nil
                }
            }
        });
        noMatch ::: schemaMissing ::: fileWarns

    /// ダッシュボード宣言 1 件ぶんの応答(素通し)。
    def dashboardJson(decl: ResourceDecl.DashboardDecl): Json =
        let base = Map#{
            "id"     => JString(decl#id),
            "plugin" => JString(decl#plugin),
            "uses"   => JsonCodec.encodeList(u -> JString(u), decl#uses)
        };
        JObject(match decl#title {
            case Some(t) => Map.insert("title", JString(t), base)
            case None    => base
        })

    /// ui ファイル 1 件ぶんのメタ情報。composable = poly の rotation/pivot を含まない
    /// (含む文書は合成プレビューが断るので、選ばせる前にここで伝える)。
    /// 読めない・JSON として壊れているファイルは composable=true に倒す — 回転が理由では
    /// ないので、実際に合成を試みたときの本来のエラーで伝えるのが正しい。
    def uiFileMeta(dir: String, relPath: String): {composable = Bool, paletteFile = Option[String]} \ IO =
        match FileIndex.readInside(dir, relPath) {
            case Err(_)   => { composable = true, paletteFile = None }
            case Ok(text) => match JsonCompat.parse(text) {
                case None       => { composable = true, paletteFile = None }
                case Some(json) => { composable = not PreviewUi.containsRotation(json),
                                     paletteFile = PreviewUi.paletteRefOf(json) }
            }
        }

    /// GET /changes — 監視対象 (/files と同じ集合 + *.preview.json + project.json) の
    /// mtime 一覧と集計 token。エディタ画面はこれを定期的に見て、ディスク上の外部変更へ
    /// 追従する (token 変化 = 何かが変わった、個別 mtime = どれが変わった)。
    // WhyNot: SSE で push しない — このサーバは executor 無しの 1 スレッド直列で、
    // 常駐接続が 1 本あるとスレッドを占有して全リクエストが詰まるため。
    def handleChanges(dir: String, exchange: HttpExchange): Unit \ IO =
        let all = FileIndex.listAllJson(dir);
        let palettes = all
            |> List.filter(path -> String.endsWith(suffix = ".ui.json", path))
            |> List.filterMap(path -> uiFileMeta(dir, path)#paletteFile);
        let entries = FileIndex.mtimesOf(dir, FileIndex.changeTargets(palettes, all));
        sendJson(200, JObject(Map#{
            "token" => JString(FileIndex.changeToken(entries)),
            "files" => JObject(entries |> List.map(entry -> (fst(entry), JNumber(mtimeToBd(snd(entry))))) |> List.toMap)
        }), exchange)

    /// mtime (Int64 ミリ秒) を JSON 数値へ。JS の Number で安全な範囲 (2^53) に収まる。
    def mtimeToBd(value: Int64): BigDecimal =
        JBigDecimal.valueOf(value)

    /// GET /file?path=... — dir 相対パスのファイル内容を返す。無い・プロジェクト外は 404。
    def handleReadFile(dir: String, params: Map[String, String], exchange: HttpExchange): Unit \ IO =
        match Map.get("path", params) {
            case None => sendJson(400, errorJson("missing query parameter: path"), exchange)
            case Some(relPath) => match FileIndex.readInside(dir, relPath) {
                case Err(message) => sendJson(404, errorJson(message), exchange)
                case Ok(content)  => sendJson(200, JObject(Map#{
                    "path"    => JString(relPath),
                    "content" => JString(content),
                    "mtime"   => JNumber(mtimeToBd(mtimeOf(dir, relPath)))
                }), exchange)
            }
        }

    /// ファイル 1 つの mtime (ミリ秒)。取れなければ 0 — If-Match の比較で必ず不一致になり、
    /// 「消えたファイルへの上書き」も衝突として扱われる。
    def mtimeOf(dir: String, relPath: String): Int64 \ IO =
        FileIndex.mtimesOf(dir, relPath :: Nil) |> List.head |> Option.map(snd) |> Option.getWithDefault(0i64)

    /// PUT /file (body: {path, content, ifMtime?}) — dir 相対パスへ書き込む。
    /// プロジェクト外・親ディレクトリ不在は拒否する。
    /// ifMtime (GET /file が返した mtime) 付きなら、ディスクの現 mtime と違うとき 409 を返す —
    /// 別の窓・別プロセスが先に保存した内容を黙って上書きしない(最後の書き手勝ちの事故防止)。
    /// 省略時は従来どおり無条件で書く(後方互換)。
    /// 見た目に効き得る素材の保存は、成功した瞬間に検査 (Changes.pump) を蹴る —
    /// 次のポーリング (約8秒) を待たずに焼きが始まる。焼き自体は裏で走るので、
    /// この応答が焼きの完了を待つことはない。
    def handleWriteFile(dir: String, changes: Changes.State, runner: Runner.State, bodyText: String, exchange: HttpExchange): Unit \ IO =
        let parsed = JsonCompat.parse(bodyText) |> Option.flatMap(JsonCodec.expectObject);
        let fields = parsed
            |> Option.flatMap(obj -> forM (
                relPath <- Map.get("path", obj) |> Option.flatMap(JsonCodec.expectString);
                content <- Map.get("content", obj) |> Option.flatMap(JsonCodec.expectString)
            ) yield (relPath, content));
        let ifMtime = parsed
            |> Option.flatMap(obj -> Map.get("ifMtime", obj))
            |> Option.flatMap(JsonCodec.expectFloat);
        match fields {
            case None => sendJson(400, errorJson("body must be a JSON object {path, content}"), exchange)
            case Some((relPath, content)) =>
                let current = mtimeOf(dir, relPath);
                let conflict = match ifMtime {
                    case None           => false
                    case Some(expected) => Int64.toFloat64(current) != expected
                };
                if (conflict)
                    sendJson(409, JObject(Map#{
                        "ok"           => JBool(false),
                        "error"        => JString("conflict: ディスク上のファイルが編集開始後に変わっています"),
                        "currentMtime" => JNumber(mtimeToBd(current))
                    }), exchange)
                else match FileIndex.writeInside(str = content, dir, relPath) {
                    case Err(message) => sendJson(400, errorJson(message), exchange)
                    case Ok(_)        => {
                        // fail-open: 検査の起動失敗は保存の失敗ではない — 応答は保存成功のまま返す。
                        if (Changes.kicksPumpOnSave(relPath))
                            try {
                                discard Changes.pump(changes, runner, dir)
                            } catch {
                                case e: Exception =>
                                    println("[studio] 保存後の検査の起動に失敗 (保存は成功): ${e.toString()}")
                            }
                        else ();
                        sendJson(200, JObject(Map#{
                            "ok"    => JBool(true),
                            "mtime" => JNumber(mtimeToBd(mtimeOf(dir, relPath)))
                        }), exchange)
                    }
                }
        }

    // ── ギャラリー (読むだけの絵と音) ─────────────────────

    /// GET /gallery/image?dir=…&name=… — PNG のバイト列。不正な指定・無いファイルは 404。
    def handleGalleryImage(dir: String, params: Map[String, String], exchange: HttpExchange): Unit \ IO =
        let found = match (Map.get("dir", params), Map.get("name", params)) {
            case (Some(subdir), Some(name)) => Gallery.imageBytes(dir, subdir, name)
            case _                          => None
        };
        match found {
            case Some(bytes) => sendBytes("image/png", bytes, exchange)
            case None        => sendJson(404, errorJson("image not found"), exchange)
        }

    /// GET /atelier/preview?file=… — 候補/スロットのプレビュー PNG のバイト列。
    /// 不正な指定・PNG 無し・元 JSON より古い (未焼成) はどれも 404。
    def handleAtelierPreview(dir: String, params: Map[String, String], exchange: HttpExchange): Unit \ IO =
        let found = match Map.get("file", params) {
            case Some(file) => Atelier.previewBytes(dir, file)
            case None       => None
        };
        match found {
            case Some(bytes) => sendBytes("image/png", bytes, exchange)
            case None        => sendJson(404, errorJson("preview not found"), exchange)
        }

    /// GET /gallery/sound?name=… — debug/sounds/ の WAV のバイト列。無ければ 404。
    def handleGallerySound(dir: String, params: Map[String, String], exchange: HttpExchange): Unit \ IO =
        let found = match Map.get("name", params) {
            case Some(name) => Gallery.soundBytes(dir, name)
            case None       => None
        };
        match found {
            case Some(bytes) => sendBytes("audio/wav", bytes, exchange)
            case None        => sendJson(404, errorJson("sound not found"), exchange)
        }

    // ── 静的配信 (ビルド済みエディタ画面) ─────────────────

    /// API に無い GET パスを EDITOR_WEB 配下のファイルで答える。
    /// 配信無効・門番で弾いた・実ファイルが無い、はどれも同じ 404 —
    /// 区別して返すとプロジェクト外のファイルの有無が外から探れてしまう。
    def handleStatic(webRoot: Option[String], path: String, exchange: HttpExchange): Unit \ IO =
        let target = forM (
            root    <- webRoot;
            relPath <- staticRelPath(path)
        ) yield (root, relPath);
        let served = target |> Option.flatMap(pair -> {
            let (root, relPath) = pair;
            FileIndex.resolveInside(root, relPath)
                |> Option.filter(fullPath -> new JFile(fullPath).isFile())
                |> Option.map(fullPath -> (relPath, fullPath))
        });
        match served {
            case Some((relPath, fullPath)) =>
                // HTML はキャッシュさせない — 古い index.html がハッシュ違いの旧 JS を
                // 指し続け、「リロードしても更新されない」罠になるため。ハッシュ付き
                // assets/ は中身が変われば名前も変わるので長期キャッシュしてよい。
                let headers = exchange.getResponseHeaders();
                if (String.endsWith(suffix = ".html", relPath))
                    headers.set("Cache-Control", "no-cache")
                else if (String.startsWith(prefix = "assets/", relPath))
                    headers.set("Cache-Control", "public, max-age=31536000, immutable")
                else
                    ();
                sendBytes(contentTypeOf(relPath), Files.readAllBytes(new JFile(fullPath).toPath()), exchange)
            case None =>
                sendJson(404, errorJson("not found: ${path}"), exchange)
        }

    /// URL パス → 配信ルート相対のファイルパス。"/" は index.html を指す。
    /// 空・`..` 入り・先頭を外しても "/" で始まる ("//host/…" 等) は None。
    /// ここは文字列だけの純粋な門番で、シンボリックリンク越えの最終確認は
    /// FileIndex.resolveInside (正規化して配下チェック) が受け持つ。
    pub def staticRelPath(path: String): Option[String] =
        let relPath =
            if (path == "/") "index.html"
            else if (String.startsWith(prefix = "/", path)) String.dropLeft(1, path)
            else path;
        let hasDotDot = String.splitOn(substr = "/", relPath) |> List.exists(segment -> segment == "..");
        if (relPath == "" or hasDotDot or String.startsWith(prefix = "/", relPath))
            None
        else
            Some(relPath)

    /// 拡張子から Content-Type を決める。エディタ画面の dist に現れる種類だけ知っていて、
    /// それ以外は中身を解釈させない octet-stream に倒す。
    pub def contentTypeOf(path: String): String =
        if      (String.endsWith(suffix = ".html", path))  "text/html; charset=utf-8"
        else if (String.endsWith(suffix = ".js", path))    "text/javascript; charset=utf-8"
        else if (String.endsWith(suffix = ".css", path))   "text/css; charset=utf-8"
        else if (String.endsWith(suffix = ".svg", path))   "image/svg+xml"
        else if (String.endsWith(suffix = ".png", path))   "image/png"
        else if (String.endsWith(suffix = ".woff2", path)) "font/woff2"
        else if (String.endsWith(suffix = ".json", path))  "application/json; charset=utf-8"
        else "application/octet-stream"

    /// バイト列そのままの 200 応答。sendJson (文字列経由) と別口なのは、
    /// 画像やフォントを文字列に通すとバイトが壊れるため。
    /// HEAD にはヘッダだけ返す (HttpServer は HEAD の本文書き込みを許さない)。
    def sendBytes(contentType: String, bytes: Array[Int8, Static], exchange: HttpExchange): Unit \ IO =
        applyCors(exchange);
        let headers = exchange.getResponseHeaders();
        headers.set("Content-Type", contentType);
        if (exchange.getRequestMethod() == "HEAD") {
            exchange.sendResponseHeaders(200, -1i64);
            exchange.close()
        } else {
            exchange.sendResponseHeaders(200, Int32.toInt64(Array.length(bytes)));
            let out = exchange.getResponseBody();
            out.write(bytes);
            out.close()
        }

    // ── CORS ──────────────────────────────────────────────

    /// 既定で許すエディタ画面のオリジン (Vite の開発サーバ)。
    def defaultAllowedOrigins(): List[String] =
        "http://localhost:5173" :: "http://127.0.0.1:5173" :: Nil

    /// 許可オリジンの一覧。EDITOR_ALLOW_ORIGINS (カンマ区切り) で追加できる。
    def allowedOrigins(): List[String] \ IO =
        let value = System.getenv("EDITOR_ALLOW_ORIGINS");
        defaultAllowedOrigins() ::: (if (Object.isNull(value)) Nil else splitOrigins(value))

    /// カンマ区切りの許可オリジン指定を分解する (前後の空白と空要素は捨てる)。
    pub def splitOrigins(text: String): List[String] =
        String.splitOn(substr = ",", text)
            |> List.map(origin -> String.trim(origin))
            |> List.filter(origin -> origin != "")

    /// エコーしてよい Origin を決める。許可リストに無い・Origin ヘッダ自体が無い (curl 等) は
    /// None = CORS ヘッダを付けない (ブラウザ以外のクライアントはヘッダ無しでそのまま通る)。
    // WhyNot: ワイルドカード "*" を返さない — 任意の Web サイトがブラウザ経由で
    // 127.0.0.1 の PUT /file を叩ける穴になるため。
    pub def corsOriginFor(origin: Option[String], allowList: List[String]): Option[String] =
        origin |> Option.flatMap(o -> if (List.memberOf(o, allowList)) Some(o) else None)

    /// リクエストの Origin ヘッダ。無ければ None。
    def requestOrigin(exchange: HttpExchange): Option[String] \ IO =
        let value = exchange.getRequestHeaders().getFirst("Origin");
        if (Object.isNull(value)) None else Some(value)

    /// 許可した Origin にだけ Access-Control-Allow-Origin をエコーする。
    /// Vary: Origin は、途中のキャッシュが別オリジン宛の応答を使い回さないための印。
    def applyCors(exchange: HttpExchange): Unit \ IO =
        match corsOriginFor(requestOrigin(exchange), allowedOrigins()) {
            case None         => ()
            case Some(origin) =>
                let headers = exchange.getResponseHeaders();
                headers.set("Access-Control-Allow-Origin", origin);
                headers.set("Vary", "Origin")
        }

    // ── HTTP の細部 ───────────────────────────────────────

    /// リクエストボディを UTF-8 文字列で読む。
    def readBody(exchange: HttpExchange): String \ IO =
        let requestBody = exchange.getRequestBody();
        new JavaString(requestBody.readAllBytes(), "UTF-8")

    /// "a=1&b=2" を Map にする。URL エンコードは扱わない
    /// (対象ファイルのパスは英数と ./_- だけの想定で、崩れたキーは単に不一致で 404 に落ちる)。
    def queryParams(text: String): Map[String, String] =
        if (text == "") Map.empty()
        else String.splitOn(substr = "&", text)
             |> List.filterMap(pair -> match String.splitOn(substr = "=", pair) {
                 case key :: value :: Nil => Some((key, value))
                 case _                   => None
             })
             |> List.toMap

    /// OPTIONS 全パス — ブラウザの事前確認 (CORS preflight) に 204 で答える。
    def sendPreflight(exchange: HttpExchange): Unit \ IO =
        applyCors(exchange);
        let headers = exchange.getResponseHeaders();
        headers.set("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS");
        headers.set("Access-Control-Allow-Headers", "Content-Type");
        headers.set("Access-Control-Max-Age", "86400");
        exchange.sendResponseHeaders(204, -1i64);
        exchange.close()

    /// JSON 応答を返す。許可したオリジンのエディタ画面から読めるよう CORS ヘッダを付ける。
    def sendJson(status: Int32, json: Json, exchange: HttpExchange): Unit \ IO =
        applyCors(exchange);
        let headers = exchange.getResponseHeaders();
        headers.set("Content-Type", "application/json; charset=utf-8");
        let bytes = (Util.Json.toPrettyString(2, json) + "\n").getBytes("UTF-8");
        exchange.sendResponseHeaders(status, Int32.toInt64(Array.length(bytes)));
        let out = exchange.getResponseBody();
        out.write(bytes);
        out.close()

    /// 失敗応答の本文 {ok: false, error}。
    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
