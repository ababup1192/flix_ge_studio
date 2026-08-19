// EngineStore — engine の実体を .app の外 (~/.flix_ge_studio/engines/<バージョン>/) に置き、
// いま使うバージョンを engine-current.txt の 1 行で指す係。
// 起動のたびに同梱 engine をここへ 1 回だけ写し (種まき)、以後は写した方を使う。

mod EngineStore {
    import java.lang.System
    import java.lang.Exception
    import java.lang.Throwable
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.lang.{String => JavaString}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool, JNull}

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// Studio が自分の持ち物を置く場所。
    pub def baseDir(userHome: String): String = "${userHome}/.flix_ge_studio"

    /// バージョンごとの engine を並べる場所。
    pub def versionsDir(userHome: String): String = "${baseDir(userHome)}/engines"

    /// バージョン 1 つぶんの engine の場所。
    pub def versionDir(userHome: String, version: String): String =
        "${versionsDir(userHome)}/${version}"

    /// いま使うバージョンを 1 行で書いておくファイル。
    // WhyNot: 指し先を symlink にしない — Windows の symlink は管理者権限か開発者モードが
    // 要る。テキスト 1 行なら両 OS で同じに書けて、テンプファイル → rename で差し替えられる。
    pub def currentFile(userHome: String): String = "${baseDir(userHome)}/engine-current.txt"

    /// engine-current.txt の中身の読み方。中身のある最初の行だけを見る。
    // WhyNot: 使えない行を飛ばして次を採らない — 壊れたファイルの 2 行目が偶然通ると、
    // 人が読んだ「今のバージョン」と実際に使う物が食い違う。
    pub def currentVersion(text: String): Option[String] =
        String.lines(text)
            |> List.map(String.trim)
            |> List.filter(line -> line != "")
            |> List.head
            |> Option.filter(isVersionName)

    /// フォルダ名として置いてよいバージョンの字か。数字で始まり、英数と . _ - だけ。
    // WhyNot: 禁じる字を並べるのでなく通す字を並べる — ".." や "/" で置き場の外を指される
    // 道だけでなく、空白・制御文字・Windows の予約名 (CON/NUL/COM1) のように
    // 「作れないのでこの先ずっと写しに失敗する」名前もまとめて落ちるため。
    pub def isVersionName(name: String): Bool =
        let chars = String.toList(name);
        match chars {
            case Nil     => false
            case c :: _  => Char.isDigit(c)
                                and String.length(name) <= 64
                                and List.forAll(isNameChar, chars)
        }

    def isNameChar(c: Char): Bool =
        Char.isLetter(c) or Char.isDigit(c) or c == '.' or c == '_' or c == '-'

    /// バージョンの前後。数字の列として比べる (字の順で 0.9.0 > 0.31.0 と読み違えない)。
    // WhyNot: 読めない字を「新しい」に倒さない — 0.32.0-rc1 のような字は数に割れないので
    // false になり、試作を人の意図なく本番へ進めることがない。
    pub def isNewerVersion(current: String, candidate: String): Bool =
        match (numbersOf(current), numbersOf(candidate)) {
            case (Some(a), Some(b)) => laterThan(a, b)
            case _                  => false
        }

    def numbersOf(version: String): Option[List[Int32]] =
        let parts = String.splitOn(substr = ".", version) |> List.map(Int32.fromString);
        if (List.isEmpty(parts) or List.exists(Option.isEmpty, parts)) None
        else Some(List.filterMap(x -> x, parts))

    def laterThan(current: List[Int32], candidate: List[Int32]): Bool =
        match (current, candidate) {
            case (Nil, Nil)           => false
            case (Nil, y :: ys)       => y > 0 or laterThan(Nil, ys)
            case (_ :: _, Nil)        => false
            case (x :: xs, y :: ys)   => if (y != x) y > x else laterThan(xs, ys)
        }

    /// 種まきが要るか。同梱として教わっていて、バージョンが読めて、まだ置き場に無いときだけ。
    // WhyNot: 手元の engine リポで開発しているとき (ランチャーが EDITOR_ENGINE を渡さない)
    // は写さない — ソースと生成物で数 GB あるうえ、写した方を使うと engine リポでの編集が
    // その場で効かなくなる。
    pub def needsSeeding(r: {bundled = Option[String], version = Option[String], hasVersionDir = Bool}): Bool =
        not Option.isEmpty(r#bundled) and not Option.isEmpty(r#version) and not r#hasVersionDir

    /// engine-current.txt を書き直すか。
    // WhyNot: 同梱バージョンで毎回上書きしない — self-update で新しいバージョンへ進めた
    // 指し先を、次に開いた拍に .app の同梱バージョンへ引き戻してしまうため。
    // WhyNot: それでも同梱の方が新しいときは進める — Windows は self-update を持たず
    // 「アプリごと入れ直し」が唯一の更新手段なので、ここで止めると永久に上がらない。
    pub def shouldWriteCurrent(r: {current = Option[String], currentDirExists = Bool, bundledIsNewer = Bool}): Bool =
        match r#current {
            case None    => true
            case Some(_) => not r#currentDirExists or r#bundledIsNewer
        }

    // ── 置き場の読み書き ──────────────────────────────────

    /// ランチャーが教えてくる同梱 engine の場所 (固めた配布物のときだけ来る)。
    pub def bundledDir(): Option[String] \ IO =
        let value = System.getenv("EDITOR_ENGINE");
        if (Object.isNull(value) or String.trim(value) == "") None else Some(value)

    /// engine の Makefile から VERSION := を読む。読めなければ None。
    // WhyNot: 読んだままの字を通さない — フォルダ名にも engine-current.txt にも使うので、
    // ここを素通りさせると置き場の外へ書きに行けるうえ、読み返しで必ず弾かれて
    // 指し先が永久に立たない (毎回書いては読めない) 形になる。
    pub def versionOf(engineDir: String): Option[String] \ IO =
        loadTextIfFile("${engineDir}/Makefile")
            |> Option.flatMap(NewGame.versionFromMakefile)
            |> Option.filter(isVersionName)

    /// いま使う engine の置き場。指し先が読めない・実体が揃っていないなら None。
    pub def currentDir(): Option[String] \ IO =
        let home = System.getProperty("user.home");
        loadTextIfFile(currentFile(home))
            |> Option.flatMap(currentVersion)
            |> Option.map(version -> versionDir(home, version))
            |> Option.filter(hasEngine)

    /// GET /engine/version — いま使っている engine のバージョンと置き場。
    pub def versionJson(): Json \ IO =
        let home = EngineHome.dir();
        JObject(Map#{
            "ok"      => JBool(true),
            "dir"     => JString(home),
            "version" => versionOf(home) |> Option.map(JString) |> Option.getWithDefault(JNull),
            "bundled" => bundledDir() |> Option.flatMap(versionOf) |> Option.map(JString) |> Option.getWithDefault(JNull)
        })

    /// 同梱 engine を engines/<バージョン>/ へ写し、指し先を書く。
    /// 済んでいる・同梱でないなら何もしない (何度呼んでも同じ)。
    pub def seed(): Unit \ IO =
        let home = System.getProperty("user.home");
        let bundled = bundledDir();
        let version = bundled |> Option.flatMap(versionOf);
        let target = version |> Option.map(v -> versionDir(home, v));
        let hasVersionDir = target |> Option.exists(hasEngine);
        let seeding = needsSeeding({bundled = bundled, version = version, hasVersionDir = hasVersionDir});
        match (bundled, version, target) {
            case (Some(source), Some(found), Some(dest)) => {
                if (seeding) copyEngine(home, source, dest, found) else ();
                writeCurrentIfStale(home, found);
                // WhyNot: 別名 (~/.flix_ge_studio/engine) をここで張り直すのは、指し先が
                // 変わる値になったため。Studio の画面からゲームを触るまで張り替えないと、
                // ターミナルや AI エージェントの make が古いバージョンを掴み続ける。
                EngineHome.relinkAlias()
            }
            case _ => ()
        }

    /// engine が 1 式そろっている場所か。ランチャーと同じ見立て (コンパイラまで在って初めて認める)。
    def hasEngine(path: String): Bool \ IO =
        new JFile("${path}/bin/flix.jar").isFile()

    /// 隣の .partial-… へ写し切ってから名前を付ける。
    // WhyNot: 目当ての名前へ直に写さない — 途中で落ちると半端な中身が「揃っている」と
    // 見えてしまい、次の起動が写し直さないため。
    // WhyNot: 途中の名前を固定にしない — Studio が 2 つ同時に上がると、片方が写している
    // 最中に他方がそこを消し、中身の混ざったフォルダが「揃っている」として確定してしまう。
    // WhyNot: 例外でなく Throwable で受ける — Flix の bug! は Error で飛ぶので、
    // Exception だけだと起動直後のサーバごと道連れになる。
    def copyEngine(home: String, source: String, dest: String, version: String): Unit \ IO =
        let staging = "${dest}.partial-${Int64.toString(System.nanoTime())}";
        try {
            discard new JFile(versionsDir(home)).mkdirs();
            clearHalfDone(dest);
            println("[studio] engine ${version} を ${dest} へ写しています…");
            copyTree(source, staging);
            if (new JFile(staging).renameTo(new JFile(dest)))
                println("[studio] engine ${version} を置きました: ${dest}")
            else {
                removeAllUnder(new JFile(staging));
                println("[studio] engine ${version} を置けませんでした (${dest})")
            }
        } catch {
            case e: Throwable => {
                removeAllUnder(new JFile(staging));
                println("[studio] engine を写せませんでした: ${e.toString()}")
            }
        }

    /// 半端な engines/<バージョン>/ を先にどける。
    // WhyNot: 残したまま rename しない — Java の renameTo は中身のあるフォルダを相手に
    // すると必ず false を返すので、毎回 60MB 写して毎回失敗する形で詰まる。
    def clearHalfDone(dest: String): Unit \ IO =
        let file = new JFile(dest);
        if (file.exists() and not hasEngine(dest)) removeAllUnder(file) else ()

    /// 指し先が無い・指した先の実体が無いときだけ書く。
    def writeCurrentIfStale(home: String, version: String): Unit \ IO =
        let current = loadTextIfFile(currentFile(home)) |> Option.flatMap(currentVersion);
        let exists = current |> Option.exists(v -> hasEngine(versionDir(home, v)));
        let newer = current |> Option.exists(v -> isNewerVersion(v, version));
        if (shouldWriteCurrent({current = current, currentDirExists = exists, bundledIsNewer = newer}))
            discard writeCurrent(home, version)
        else ()

    /// 隣に書いてから改名で差し替え、読み返して確かめる。書けたら true。
    // WhyNot: 先に消してから改名しない — 間で電源が落ちると指し先そのものが消え、
    // 次の起動は「指し先が無い」として同梱バージョンへ黙って戻る。改名が通らない
    // 場合 (Windows は行き先があると失敗する) だけ、消してからもう一度やる。
    // WhyNot: 書きっぱなしにしない — 呼ぶ側が「切り替えました」と言い切るので、
    // 実際に読み返せた時だけ true を返す。
    pub def writeCurrent(home: String, version: String): Bool \ IO =
        try {
            discard new JFile(baseDir(home)).mkdirs();
            let path = currentFile(home);
            let temp = "${path}.tmp";
            writeText(temp, "${version}\n");
            if (not new JFile(temp).renameTo(new JFile(path))) {
                discard new JFile(path).delete();
                discard new JFile(temp).renameTo(new JFile(path))
            } else ();
            loadTextIfFile(path) |> Option.flatMap(currentVersion) == Some(version)
        } catch {
            case _: Exception => false
        }

    // ── ファイルの下回り ──────────────────────────────────

    // WhyNot: リンクを追わない — 祖先を指すフォルダのリンクが 1 つでもあると際限なく
    // 潜ってディスクを埋める。ファイルのリンクも実体の複製になり、写す量が読めなくなる。
    def copyTree(src: String, dst: String): Unit \ IO =
        discard new JFile(dst).mkdirs();
        let entries = new JFile(src).listFiles();
        if (Object.isNull(entries)) ()
        else Array.toList(entries) |> List.forEach(entry ->
            if (Files.isSymbolicLink(entry.toPath())) ()
            else if (entry.isDirectory()) copyTree(entry.getPath(), "${dst}/${entry.getName()}")
            else copyFile(entry.getPath(), "${dst}/${entry.getName()}"))

    // WhyNot: 実行ビットを落とさない — bin/flix と bin/fge-go が走らなくなり、
    // ゲームのビルドも新しいゲームづくりも一切通らなくなる。
    def copyFile(src: String, dst: String): Unit \ IO =
        let out = new FileOutputStream(dst);
        out.write(Files.readAllBytes(new JFile(src).toPath()));
        out.close();
        // WhyNot: setExecutable(true) の 1 引数の形を使わない — 持ち主だけの実行ビット
        // (744) になり、元の 755 と食い違う。第 2 引数 false で全員に付ける。
        if (new JFile(src).canExecute()) { discard new JFile(dst).setExecutable(true, false) } else ()

    def removeAllUnder(file: JFile): Unit \ IO =
        if (file.isDirectory()) {
            let entries = file.listFiles();
            if (Object.isNull(entries)) () else Array.toList(entries) |> List.forEach(removeAllUnder);
            discard file.delete()
        } else
            discard file.delete()

    /// ファイルの中身を UTF-8 で読む。ファイルでなければ None。
    pub def loadTextIfFile(path: String): Option[String] \ IO =
        try {
            if (new JFile(path).isFile())
                Some(new JavaString(Files.readAllBytes(new JFile(path).toPath()), "UTF-8"))
            else None
        } catch {
            case _: Exception => None
        }

    def writeText(path: String, text: String): Unit \ IO =
        let out = new FileOutputStream(path);
        out.write(text.getBytes("UTF-8"));
        out.close()
}
