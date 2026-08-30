// Plugins — 拡張プラグイン (studio-plugins/<id>/ の自己完結 HTML) の一覧と配信。
//
// 一覧 (GET /plugins): project.json の editor.plugins[] 宣言 (PluginDecl が読む) に
// index.html の実在確認を重ね、無い物は warnings に載せて一覧から外す (fail-open —
// 壊れた宣言でも他のプラグインと Studio 本体は動き続ける)。
//
// 配信 (GET /plugin-ui/<id>/<path...>): プロジェクト配下の studio-plugins/ を
// 同一オリジンで配る。門番は uiRelPath (文字列の検査) + FileIndex.resolveInside
// (正規化して配下チェック — シンボリックリンク越えも塞ぐ) の二段。
// キャッシュ制御 (全ファイル no-cache) と応答の書き出しは Editor が受け持つ。
mod Plugins {
    import java.io.{File => JFile}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool}

    /// プラグインの HTML 置き場 (プロジェクト相対)。
    def pluginsDir(): String = "studio-plugins"

    // ── 一覧 (GET /plugins) ───────────────────────────────

    /// GET /plugins の応答 (status, json)。
    /// 宣言があっても index.html が無ければ一覧から外し warnings で知らせる。
    /// project.json が読めない・宣言の形が違うのは /resources と同じ 400。
    pub def listJson(dir: String): (Int32, Json) \ IO =
        match FileIndex.readInside(dir, "project.json") {
            case Err(message) => (400, errorJson("project.json: ${message}"))
            case Ok(text) => match DocJson.parse(text) {
                case None => (400, errorJson("project.json が JSON として読めません"))
                case Some(json) => match PluginDecl.fromProjectJson(json) {
                    case Err(message) => (400, errorJson(message))
                    case Ok(decls) => {
                        let (present, missing) = List.partition(decl -> hasIndexHtml(dir, decl#id), decls);
                        let warnings = List.append(
                            PluginDecl.keyWarnings(json),
                            missing |> List.map(decl ->
                                "プラグイン『${decl#id}』の ${pluginsDir()}/${decl#id}/index.html が見つかりません(一覧から外しました)"));
                        (200, JObject(Map#{
                            "ok"       => JBool(true),
                            "plugins"  => JArray(present |> List.map(pluginJson) |> List.toVector),
                            "warnings" => JArray(warnings |> List.map(w -> JString(w)) |> List.toVector)
                        }))
                    }
                }
            }
        }

    /// 宣言 1 件ぶんの応答。title は無ければ id で埋める (画面側の代用を待たない)。
    /// performUrl / performCmd は宣言がある時だけ載せる (resources の素通しと同じ流儀)。
    def pluginJson(decl: PluginDecl.Decl): Json =
        let base = Map#{
            "id"    => JString(decl#id),
            "title" => JString(decl#title |> Option.getWithDefault(decl#id))
        };
        let withUrl = match decl#performUrl {
            case Some(u) => Map.insert("performUrl", JString(u), base)
            case None    => base
        };
        JObject(match decl#performCmd {
            case Some(c) => Map.insert("performCmd", JString(c), withUrl)
            case None    => withUrl
        })

    /// studio-plugins/<id>/index.html が実在するか。id は宣言時に検査済みだが、
    /// 念のためここでも resolveInside の門番を通す。
    def hasIndexHtml(dir: String, id: String): Bool \ IO =
        FileIndex.resolveInside(dir, "${pluginsDir()}/${id}/index.html")
            |> Option.exists(fullPath -> new JFile(fullPath).isFile())

    // ── 配信 (GET /plugin-ui/…) ───────────────────────────

    /// URL パスをプロジェクト相対のファイルパスへ解いてバイト列で読む。
    /// 返るのは (相対パス = Content-Type の材料, 中身)。門番で弾いた・実ファイルが
    /// 無い、はどちらも None (= Editor が 404 にする)。
    pub def serveFile(dir: String, urlPath: String): Option[(String, Array[Int8, Static])] \ IO =
        uiRelPath(urlPath)
            |> Option.flatMap(relPath ->
                FileIndex.resolveInside(dir, relPath)
                    |> Option.filter(fullPath -> new JFile(fullPath).isFile())
                    |> Option.map(fullPath ->
                        (relPath, Files.readAllBytes(new JFile(fullPath).toPath()))))

    /// "/plugin-ui/<id>" (末尾スラッシュ無し) か。ここへ index.html を直接返すと、
    /// 中の相対参照 (./app.js 等) が /plugin-ui/ を基準に解けて壊れるので、
    /// Editor はこの形を "<id>/" へ 301 リダイレクトする。
    pub def isRootWithoutSlash(urlPath: String): Bool =
        String.startsWith(prefix = "/plugin-ui/", urlPath)
            and PluginDecl.isSafeId(String.dropLeft(String.length("/plugin-ui/"), urlPath))

    /// "/plugin-ui/<id>/<path...>" → "studio-plugins/<id>/<path...>" の純粋な門番。
    /// id は isSafeId (英数字と `-` `_` だけ) で断り、パス側も `..`・先頭 `/` を断る
    /// (staticRelPath と同じ検査。シンボリックリンク越えは resolveInside が受け持つ)。
    /// 末尾スラッシュは index.html を指す。スラッシュ無しの "/plugin-ui/<id>" は
    /// ここでは配らず None (Editor が isRootWithoutSlash で 301 にする)。
    pub def uiRelPath(urlPath: String): Option[String] =
        if (not String.startsWith(prefix = "/plugin-ui/", urlPath))
            None
        else {
            let rest = String.dropLeft(String.length("/plugin-ui/"), urlPath);
            match String.indexOf(substr = "/", rest) {
                case None     => None
                case Some(at) => {
                    let id = String.slice(start = 0, end = at, rest);
                    let sub = String.dropLeft(at + 1, rest);
                    let filePath =
                        if (sub == "" or String.endsWith(suffix = "/", sub)) sub + "index.html"
                        else sub;
                    let normalized = String.replace(src = "\\", dst = "/", filePath);
                    let hasDotDot = String.splitOn(substr = "/", normalized)
                        |> List.exists(segment -> segment == "..");
                    if (not PluginDecl.isSafeId(id) or hasDotDot or String.startsWith(prefix = "/", normalized))
                        None
                    else
                        Some("${pluginsDir()}/${id}/${filePath}")
                }
            }
        }

    /// 失敗応答の本文 {ok: false, error} (Editor の物と同形)。
    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
