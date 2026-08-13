// NewGame — engine の `make new-game` を使わずに、新しいゲームを server 自身の手で産む係。
// テンプレートの複製 → 名前・題名・大きさの書き込み → 依存 (engine_full) の種付け →
// AGENTS の配布 → 産声の確認 (check → test → 焼き)、を make や perl 抜きでやる —
// Windows にはどちらも無いため。進み具合は Scaffold の器 (GET /projects/new/log) に流す。
// 文字の書き換えは純粋な関数に分けてある (そこだけテストする)。

mod NewGame {
    import java.lang.Exception
    import java.lang.ProcessBuilder
    import java.lang.System
    import java.lang.{String => JavaString}
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.nio.file.Files
    import java.util.ArrayList

    use Util.Json.Json

    /// 産むのに必要な材料一式。Scaffold が検分を済ませてから渡してくる。
    pub type alias Request = {
        engine   = String,
        gameDir  = String,
        name     = String,
        title    = String,
        w        = Int32,
        h        = Int32,
        template = String
    }

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// テンプレート本文の置き換え。__NAME__ などのプレースホルダを実際の値に差し替える。
    /// __WW__/__WH__ はウィンドウの大きさ (設計解像度の 2 倍)。
    /// engine の現在地は Makefile に書かない — git に入れない local.mk へ書く
    /// (localMkText)。Makefile はマシンをまたいで共有できる形のまま。
    pub def applyTokens(request: Request, text: String): String =
        text
            |> String.replace(src = "__NAME__", dst = request#name)
            |> String.replace(src = "__TITLE__", dst = request#title)
            |> String.replace(src = "__WW__", dst = "${request#w * 2}")
            |> String.replace(src = "__WH__", dst = "${request#h * 2}")
            |> String.replace(src = "__W__", dst = "${request#w}")
            |> String.replace(src = "__H__", dst = "${request#h}")
            // 古い engine のテンプレ (ENGINE ?= __ENGINE__) との互換のためだけに残すプレースホルダ。
            // いまのテンプレの Makefile は engine の場所を持たない (local.mk が教える)。
            |> String.replace(src = "__ENGINE__", dst = request#engine)

    /// local.mk の本文。このマシンでの engine の場所と、(教わっていれば) java の場所。
    /// Windows のパスは \ を / に直して書く — make の wildcard や sh のクォートは
    /// バックスラッシュと相性が悪く、java は / のパスをそのまま受け付けるため。
    /// ?= なのは、Studio が環境変数 ENGINE で渡してくる場所 (EngineHome) を勝たせるため —
    /// 優先順は コマンドライン > 環境変数 > local.mk > Makefile の既定。
    pub def localMkText(engine: String, java: Option[String]): String =
        let line = path -> String.replace(src = "\\", dst = "/", path);
        "# このマシンの実行環境 (git には入れない — .gitignore 済み)。Studio が生成。\n"
            + "ENGINE ?= ${line(engine)}\n"
            + (java |> Option.map(exe -> "JAVA ?= ${line(exe)}\n") |> Option.getWithDefault(""))

    /// プレースホルダの書き換え対象か。テンプレの文字ファイルだけ (絵や音はそのまま写す)。
    pub def isTokenTarget(fileName: String): Bool =
        String.endsWith(suffix = ".flix", fileName)
            or String.endsWith(suffix = ".json", fileName)
            or String.endsWith(suffix = ".toml", fileName)
            or String.endsWith(suffix = ".md", fileName)
            or fileName == "Makefile"

    /// flix.toml の本文から version = "…" の中身を取り出す。無ければ None。
    pub def versionFromToml(text: String): Option[String] =
        String.lines(text)
            |> List.filterMap(line -> {
                let trimmed = String.trim(line);
                if (String.startsWith(prefix = "version", trimmed))
                    String.splitOn(substr = "\"", trimmed) |> List.drop(1) |> List.head
                else None
            })
            |> List.head

    /// engine の Makefile 本文から VERSION := の値を取り出す。無ければ None。
    /// 規則は engine の bin/status.py (read_engine_version) と同じ —
    /// 行頭の VERSION、空白を挟んで :=、次の 1 語 (空白の手前まで)。
    pub def versionFromMakefile(text: String): Option[String] =
        String.lines(text)
            |> List.filterMap(line ->
                if (String.startsWith(prefix = "VERSION", line)) {
                    let afterName = String.dropLeft(String.length("VERSION"), line) |> String.trimLeft;
                    if (String.startsWith(prefix = ":=", afterName)) {
                        let value = String.dropLeft(2, afterName)
                            |> String.trimLeft
                            |> String.takeWhile(c -> not Char.isWhitespace(c));
                        if (value == "") None else Some(value)
                    } else None
                } else None)
            |> List.head

    /// SKILL.md の frontmatter から description: の中身を取り出す。
    /// description: で始まる最初の行の残りを trim し、先頭・末尾の " を 1 個ずつ外す。
    /// 無ければ "" (engine の bin/sync-agents.py の skill_description と同じ規則)。
    pub def skillDescriptionOf(text: String): String =
        String.lines(text)
            |> List.filterMap(line ->
                if (String.startsWith(prefix = "description:", line))
                    Some(String.dropLeft(String.length("description:"), line) |> String.trim)
                else None)
            |> List.head
            |> Option.map(stripOuterQuotes)
            |> Option.getWithDefault("")

    /// 先頭と末尾の " を 1 個ずつだけ外す (中の " や全部を消すのは規則違い)。
    def stripOuterQuotes(text: String): String =
        let noHead = if (String.startsWith(prefix = "\"", text)) String.dropLeft(1, text) else text;
        if (String.endsWith(suffix = "\"", noHead)) String.dropRight(1, noHead) else noHead

    /// 「配られているスキル一覧」の節 (見出し + 空行 + 1 スキル 1 行)。
    /// skills は (名前, description) を名前昇順で渡す。末尾に改行は付けない
    /// (つなぎは agentsText の仕事)。
    pub def skillsSection(skills: List[(String, String)]): String =
        String.intercalate("\n",
            "## 配られているスキル一覧（sync-agents が自動生成。手で編集しない）"
                :: ""
                :: List.map(entry -> "- `.claude/skills/${fst(entry)}/SKILL.md` — ${snd(entry)}", skills))

    /// 末尾の改行をぜんぶ落とす (engine 側 Python の rstrip("\n") と同じ)。
    def dropTrailingNewlines(text: String): String =
        if (String.endsWith(suffix = "\n", text)) dropTrailingNewlines(String.dropRight(1, text))
        else text

    /// AGENTS.md の本文を組み立てる。engine の bin/sync-agents.py とバイト一致させる:
    /// 刻印行 → 空行 → 共通部 → 空行 → スキル一覧の節 → (あれば 空行 + ゲーム固有部)。
    /// 終端は改行 1 個。
    pub def agentsText(engineVersion: String, core: String, skills: List[(String, String)], localPart: Option[String]): String =
        "<!-- generated by flix_game_engine agents-pack (engine v${engineVersion}). 編集しないで。共通部は engine の agents-pack/AGENTS.core.md、ゲーム固有部は AGENTS.local.md を編集して再 sync -->\n\n"
            + dropTrailingNewlines(core) + "\n\n"
            + skillsSection(skills)
            + (localPart |> Option.map(text -> "\n\n" + dropTrailingNewlines(text)) |> Option.getWithDefault(""))
            + "\n"

    /// manifest の読み取り結果。3 節 (copy / copyIfAbsent / copyDirs) の (src, dst) 一覧。
    pub type alias ManifestPlan = {
        copyList         = List[(String, String)],
        copyIfAbsentList = List[(String, String)],
        copyDirsList     = List[(String, String)]
    }

    /// manifest の 1 節を (src, dst) の一覧に解く。節が無い・形が違えば None。
    pub def manifestEntries(key: String, top: Map[String, Json]): Option[List[(String, String)]] =
        Map.get(key, top)
            |> Option.flatMap(JsonCodec.decodeList(element ->
                JsonCodec.expectObject(element) |> Option.flatMap(obj ->
                    Map.get("src", obj) |> Option.flatMap(JsonCodec.expectString) |> Option.flatMap(src ->
                        Map.get("dst", obj) |> Option.flatMap(JsonCodec.expectString)
                            |> Option.map(dst -> (src, dst))))))

    /// manifest.json の本文を配布計画に解く。3 節そろって初めて Some —
    /// 1 節でも欠けたら全体 None にして、呼び手を旧来の配布へ倒す (半端に配らない)。
    pub def manifestPlan(text: String): Option[ManifestPlan] =
        DocJson.parse(text)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(top ->
                manifestEntries("copy", top) |> Option.flatMap(copyEntries ->
                    manifestEntries("copyIfAbsent", top) |> Option.flatMap(absentEntries ->
                        manifestEntries("copyDirs", top) |> Option.map(dirEntries ->
                            {copyList = copyEntries,
                             copyIfAbsentList = absentEntries,
                             copyDirsList = dirEntries}))))

    // ── 産む (Runner.launchWork の仕事として走る) ─────────

    /// 新しいゲームを 1 つ産む。終了コード 0 = 産まれた。途中の失敗はログに残して非 0。
    /// 想定外の例外は呼び手 (Runner.launch) が受けて -1 で記録する。
    pub def create(state: Runner.State, request: Request): Int32 \ IO =
        let templateDir = "${request#engine}/templates/${request#template}";
        let fpkg = "${request#engine}/engine_full/artifact/engine_full.fpkg";
        if (not new JFile(templateDir).isDirectory()) {
            Runner.logLine(state, "[new-game] テンプレートがありません: ${templateDir}");
            1
        } else if (not new JFile(fpkg).isFile()) {
            Runner.logLine(state, "[new-game] ${fpkg} がありません — engine の同梱が壊れています");
            1
        } else {
            Runner.logLine(state, "[new-game] ${request#gameDir} を作成します (NAME=${request#name} TITLE=${request#title} design=${request#w}x${request#h} TEMPLATE=${request#template})");
            copyTree(skipTopLevel = Set#{"lib"}, templateDir, request#gameDir);
            foreach (sub <- "gallery" :: "reference" :: "debug" :: "atelier" :: Nil)
                discard new JFile("${request#gameDir}/${sub}").mkdirs();
            renameAssets(request);
            rewriteTokens(request);
            writeLocalMk(request);
            seedEngineFull(state, request);
            seedLibDirs(state, request);
            gitInit(state, request#gameDir);
            syncAgents(state, request);
            firstCry(state, request)
        }

    /// 産声の確認: check → test → (パレットを持つなら パレット →) 焼き。
    /// どこかで転んだらそこで止める (make new-game の check→test→bake と同じ厳しさ)。
    def firstCry(state: Runner.State, request: Request): Int32 \ IO =
        Runner.logLine(state, "[new-game] 産声の確認: check → test → bake");
        let checkExit = Runner.runTaskLogged(state, request#gameDir, EngineTasks.Task.Check);
        if (checkExit != 0) checkExit
        else {
            let testExit = Runner.runTaskLogged(state, request#gameDir, EngineTasks.Task.Test);
            if (testExit != 0) testExit
            else {
                let paletteExit =
                    if (EngineTasks.hasPalette(request#gameDir))
                        Runner.runTaskLogged(state, request#gameDir, EngineTasks.Task.Palette)
                    else 0;
                if (paletteExit != 0) paletteExit
                else {
                    let bakeExit = Runner.runTaskLogged(state, request#gameDir, EngineTasks.Task.Bake);
                    if (bakeExit == 0)
                        Runner.logLine(state, "🎉 新しいゲームが産まれました: ${request#gameDir}")
                    else ();
                    bakeExit
                }
            }
        }

    // ── 各工程 ────────────────────────────────────────────

    /// assets/__NAME__.* を assets/<name>.* に改名する。
    def renameAssets(request: Request): Unit \ IO =
        let assets = new JFile("${request#gameDir}/assets");
        let entries = assets.listFiles();
        if (Object.isNull(entries)) ()
        else Array.toList(entries) |> List.forEach(file -> {
            let fileName = file.getName();
            if (String.startsWith(prefix = "__NAME__.", fileName)) {
                let rest = String.dropLeft(String.length("__NAME__."), fileName);
                discard file.renameTo(new JFile("${request#gameDir}/assets/${request#name}.${rest}"))
            } else ()
        })

    /// ゲーム内の文字ファイル全部にプレースホルダの書き換えをかける。
    def rewriteTokens(request: Request): Unit \ IO =
        walkFiles(new JFile(request#gameDir))
            |> List.forEach(file ->
                if (isTokenTarget(file.getName()))
                    writeText(file.getPath(), applyTokens(request, readText(file.getPath())))
                else ())

    /// このマシンでの engine / java の場所を local.mk へ書く (Makefile はマシン共有のまま)。
    /// java は app が同梱 JRE を EDITOR_JAVA で教えてきたときだけ書く (手元開発では PATH の java)。
    def writeLocalMk(request: Request): Unit \ IO =
        let javaValue = System.getenv("EDITOR_JAVA");
        let java = if (Object.isNull(javaValue) or String.trim(javaValue) == "") None else Some(javaValue);
        writeText("${request#gameDir}/local.mk", localMkText(request#engine, java))

    /// engine_full の fpkg と flix.toml を、ゲームの lib/github/… にバージョン名で置く
    /// (初回ビルドでのダウンロードを不要にする)。バージョンが読めなければ飛ばす —
    /// その場合は初回ビルドが自分でダウンロードする (fail-open)。
    def seedEngineFull(state: Runner.State, request: Request): Unit \ IO =
        match versionFromToml(readText("${request#engine}/engine_full/flix.toml")) {
            case None =>
                Runner.logLine(state, "[new-game] engine_full のバージョンが読めません — lib の種付けを飛ばします (初回ビルドでダウンロードされます)")
            case Some(version) => {
                let seedDir = "${request#gameDir}/lib/github/ababup1192/flix_game_engine/${version}";
                discard new JFile(seedDir).mkdirs();
                copyFile("${request#engine}/engine_full/artifact/engine_full.fpkg", "${seedDir}/flix_game_engine-${version}.fpkg");
                copyFile("${request#engine}/engine_full/flix.toml", "${seedDir}/flix_game_engine-${version}.toml")
            }
        }

    /// engine の lib/cache・lib/external があれば写す (Maven 依存の初回ダウンロード回避)。
    /// 種が無いときに黙って続けないのは、そのときだけ初回ビルドが Maven へ出て、
    /// 回線が細い所や proxy の内側では何分も無言で止まって見えるため。
    def seedLibDirs(state: Runner.State, request: Request): Unit \ IO =
        foreach (sub <- "cache" :: "external" :: Nil) {
            let src = "${request#engine}/lib/${sub}";
            if (new JFile(src).isDirectory())
                copyTree(skipTopLevel = Set#{}, src, "${request#gameDir}/lib/${sub}")
            else ()
        };
        if (new JFile("${request#engine}/lib/cache").isDirectory())
            Runner.logLine(state, "[new-game] lib/ を種付けしました (初回ビルドは Maven に出ずに済みます)")
        else
            Runner.logLine(state, "[new-game] Maven の種が同梱されていません — 初回ビルドで lwjgl を取り寄せます (回線によっては数分かかります)")

    /// git init を試す。git が無い環境 (Windows のポータブル配布など) では
    /// 1 行残して続行 — 産むのに git は必須ではない。init が通ったら pre-commit 関所の
    /// 配線 (core.hooksPath) も追試する — clone ごとの make hooks を産まれた瞬間の 1 回で済ませる。
    def gitInit(state: Runner.State, gameDir: String): Unit \ IO =
        let ok = try {
            runGit(gameDir, "init" :: "-q" :: Nil)
        } catch {
            case _: Exception => false
        };
        if (not ok) Runner.logLine(state, "[new-game] git が見つからないので git init は飛ばしました (無くても遊べます)")
        else {
            let wired = try {
                runGit(gameDir, "config" :: "core.hooksPath" :: "bin/githooks" :: Nil)
            } catch {
                case _: Exception => false
            };
            if (not wired) Runner.logLine(state, "[new-game] pre-commit 関所の配線に失敗しました (あとで make hooks でどうぞ)") else ()
        }

    /// git をゲームのフォルダで走らせる (終了 0 = 成功)。git 不在の例外は呼び手が受ける。
    def runGit(gameDir: String, args: List[String]): Bool \ IO =
        let argList = new ArrayList();
        discard argList.add("git");
        List.forEach(arg -> { discard argList.add(arg); () }, args);
        let pb = new ProcessBuilder(argList);
        discard pb.directory(new JFile(gameDir));
        pb.start().waitFor() == 0

    /// AGENTS の配布一式。失敗してもゲーム自体は使えるので、1 行残して続行する。
    def syncAgents(state: Runner.State, request: Request): Unit \ IO =
        try {
            distribute(request#engine, request#gameDir, message -> Runner.logLine(state, message))
        } catch {
            case _: Exception =>
                Runner.logLine(state, "[new-game] AGENTS の配布に失敗しました (ゲーム自体は使えます)")
        }

    /// CLI 入口 (--sync-agents <engine> <game>)。HTTP を立てずに配布だけして終わる。
    /// Makefile の check-agents-parity が engine の bin/sync-agents.py との一致検査に使う。
    /// 失敗は 1 行 + 非 0 終了 — 黙って 0 で帰ると、呼び手のシェルが壊れた配布を
    /// 「成功」と誤読する (server 内の配布と違い、ここにはログの器が無い)。
    pub def distributeCli(engine: String, gameDir: String): Unit \ IO =
        try {
            if (not new JFile(gameDir).isDirectory()) {
                println("error: ゲームのフォルダが見つかりません: ${gameDir}");
                System.exit(1)
            } else
                distribute(engine, gameDir, message -> println(message))
        } catch {
            case e: Exception => {
                let reason = if (Object.isNull(e.getMessage())) e.toString() else e.getMessage();
                println("error: AGENTS の配布に失敗しました (engine の場所は正しいですか): ${reason}");
                System.exit(1)
            }
        }

    /// AGENTS の配布本体 (engine の bin/sync-agents.py と同じ結果になる形):
    /// AGENTS.md + CLAUDE.md の生成と、agents-pack/manifest.json の
    /// copy / copyIfAbsent / copyDirs の実行。manifest が無い・読めない古い同梱 engine では
    /// 旧来のハードコード一覧へ倒す (fail-open — 悪化はさせない)。
    pub def distribute(engine: String, gameDir: String, log: String -> Unit \ IO): Unit \ IO =
        writeAgentsDocs(engine, gameDir);
        let manifestPath = "${engine}/agents-pack/manifest.json";
        let plan = if (new JFile(manifestPath).isFile()) manifestPlan(readText(manifestPath)) else None;
        match plan {
            case Some(entries) => runPlan(engine, gameDir, entries)
            case None => {
                log("[new-game] agents-pack/manifest.json が読めないので旧来の一覧で配ります (fail-open)");
                distributeLegacy(engine, gameDir)
            }
        }

    /// AGENTS.md (刻印 + 共通部 + スキル一覧 + ゲーム固有部) と CLAUDE.md を書く。
    def writeAgentsDocs(engine: String, gameDir: String): Unit \ IO =
        let core = readText("${engine}/agents-pack/AGENTS.core.md");
        let localPath = "${gameDir}/AGENTS.local.md";
        let localPart = if (new JFile(localPath).isFile()) Some(readText(localPath)) else None;
        writeText("${gameDir}/AGENTS.md", agentsText(engineVersionOf(engine), core, skillCatalog(engine), localPart));
        writeText("${gameDir}/CLAUDE.md", "@AGENTS.md\n")

    /// バージョン刻印の出どころ: engine の Makefile の VERSION := が正。Makefile を持たない
    /// 同梱 engine では engine_full/flix.toml (bump が同じバージョンへそろえる) に倒す。
    /// engine 自身の flix.toml を見ないのは、あれが 0.1.0 のまま動かないラベルだから。
    def engineVersionOf(engine: String): String \ IO =
        match (readTextIfFile("${engine}/Makefile") |> Option.flatMap(versionFromMakefile)) {
            case Some(version) => version
            case None =>
                readTextIfFile("${engine}/engine_full/flix.toml")
                    |> Option.flatMap(versionFromToml)
                    |> Option.getWithDefault("?")
        }

    /// agents-pack/skills の (名前, description) 一覧を名前昇順で。
    def skillCatalog(engine: String): List[(String, String)] \ IO =
        let entries = new JFile("${engine}/agents-pack/skills").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(entry -> entry.isDirectory())
            |> List.map(dir -> dir.getName())
            |> List.sort
            |> List.map(name -> (
                name,
                readTextIfFile("${engine}/agents-pack/skills/${name}/SKILL.md")
                    |> Option.map(skillDescriptionOf)
                    |> Option.getWithDefault("")))

    /// manifest の配布計画を実行する。順序は engine 側と同じ copyDirs → copy → copyIfAbsent。
    def runPlan(engine: String, gameDir: String, entries: ManifestPlan): Unit \ IO =
        List.forEach(entry -> copyDirEntry("${engine}/${fst(entry)}", "${gameDir}/${snd(entry)}"), entries#copyDirsList);
        List.forEach(entry -> copyEntry("${engine}/${fst(entry)}", "${gameDir}/${snd(entry)}"), entries#copyList);
        List.forEach(entry -> {
            let dst = "${gameDir}/${snd(entry)}";
            if (new JFile(dst).exists()) () else copyEntry("${engine}/${fst(entry)}", dst)
        }, entries#copyIfAbsentList)

    /// 1 ファイルを親フォルダごと写す。src が無ければ黙って飛ばす — 同梱 engine が
    /// 痩せていても配布全体を失敗にしない (足りない分は engine 側の make sync-agents で配り直せる)。
    def copyEntry(src: String, dst: String): Unit \ IO =
        if (not new JFile(src).isFile()) ()
        else {
            let parent = new JFile(dst).getParentFile();
            if (Object.isNull(parent)) () else { discard parent.mkdirs() };
            copyFile(src, dst)
        }

    /// フォルダを再帰で重ね書きする (消し込みはしない)。src が無ければ黙って飛ばす。
    def copyDirEntry(src: String, dst: String): Unit \ IO =
        if (not new JFile(src).isDirectory()) ()
        else copyTree(skipTopLevel = Set#{}, src, dst)

    /// manifest を持たない古い同梱 engine への配布 (従来のハードコード一覧)。
    def distributeLegacy(engine: String, gameDir: String): Unit \ IO =
        copyDirEntry("${engine}/agents-pack/skills", "${gameDir}/.claude/skills");
        copyDirEntry("${engine}/agents-pack/rules", "${gameDir}/.claude/rules");
        discard new JFile("${gameDir}/bin").mkdirs();
        foreach (lint <- "lint-view.py" :: "lint-palette.py" :: "lint-sprite.py" :: "lint-anim.py" :: Nil)
            copyEntry("${engine}/bin/${lint}", "${gameDir}/bin/${lint}")

    def readTextIfFile(path: String): Option[String] \ IO =
        if (new JFile(path).isFile()) Some(readText(path)) else None

    // ── ファイルの下回り ──────────────────────────────────

    /// src の中身を dst へ丸ごと写す (dst は無ければ作る)。skipTopLevel は最上位でだけ
    /// 飛ばす名前 (テンプレの lib はビルド成果物なので写さない)。
    def copyTree(skipTopLevel: {skipTopLevel = Set[String]}, src: String, dst: String): Unit \ IO =
        discard new JFile(dst).mkdirs();
        let entries = new JFile(src).listFiles();
        if (Object.isNull(entries)) ()
        else Array.toList(entries) |> List.forEach(entry -> {
            if (Set.memberOf(entry.getName(), skipTopLevel#skipTopLevel)) ()
            else if (entry.isDirectory())
                copyTree(skipTopLevel = Set#{}, entry.getPath(), "${dst}/${entry.getName()}")
            else copyFile(entry.getPath(), "${dst}/${entry.getName()}")
        })

    /// dir 配下の全ファイル (再帰)。
    def walkFiles(dir: JFile): List[JFile] \ IO =
        let entries = dir.listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries) |> List.flatMap(entry ->
            if (entry.isDirectory()) walkFiles(entry) else entry :: Nil)

    def readText(path: String): String \ IO =
        new JavaString(Files.readAllBytes(new JFile(path).toPath()), "UTF-8")

    def writeText(path: String, text: String): Unit \ IO =
        let out = new FileOutputStream(path);
        out.write(text.getBytes("UTF-8"));
        out.close()

    /// 実行ビットも写すのは、checkd / explain-error / githooks/pre-commit を
    /// 配った先でそのまま実行できる形に保つため (絵や JSON には付かないので無害)。
    def copyFile(src: String, dst: String): Unit \ IO =
        let out = new FileOutputStream(dst);
        out.write(Files.readAllBytes(new JFile(src).toPath()));
        out.close();
        if (new JFile(src).canExecute()) { discard new JFile(dst).setExecutable(true) } else ()
}
