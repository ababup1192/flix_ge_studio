// FileIndex — 編集対象プロジェクト配下のファイル一覧と、安全な読み書きの窓口。
// パスは常に「プロジェクトディレクトリ (dir) からの相対」で受け渡す。
// ブラウザから来るパスをそのまま信じるとプロジェクト外を読み書きできてしまうので、
// 読み書きは必ず resolveInside の門番を通す。

mod FileIndex {
    import java.io.{File => JFile}

    /// 走査から除外するディレクトリ名。生成物・依存・履歴はエディタの編集対象にしない。
    /// 走査から外す置き場。編集対象でない生成物・写し(debug は焼き係と実機が
    /// 書き続ける — 混ぜると /changes の token が絶えず変わり「差分なし」が二度と
    /// 成立しない)。隠しディレクトリは walkJson 側で名前の頭の "." で一律に外す。
    pub def excludedDirs(): Set[String] = Set#{"build", "lib", "gallery", "node_modules", "debug", "reference"}

    /// ディレクトリの中身 1 件 (名前と、ディレクトリかどうか)。
    pub type alias DirEntry = {name = String, isDir = Bool}

    /// dir 配下の編集対象ファイルを列挙する。*.ui.json は ui、hitbox.json は hitbox、
    /// *.fx.json は fx。返るパスは dir 相対。
    pub def listFiles(dir: String): {ui = List[String], hitbox = List[String], fx = List[String]} \ IO =
        let all = listAllJson(dir);
        { ui     = all |> List.filter(path -> String.endsWith(suffix = ".ui.json", path)),
          hitbox = all |> List.filter(path -> baseName(path) == "hitbox.json"),
          fx     = all |> List.filter(path -> String.endsWith(suffix = ".fx.json", path)) }

    /// dir 配下の .json ファイル全部 (prune 走査・dir 相対・ソート済み)。
    pub def listAllJson(dir: String): List[String] \ IO =
        walkJson(relDir -> childrenOf(dir, relDir), "") |> List.sort

    /// relDir ("" = プロジェクト直下) から下りながら .json ファイルの相対パスを集める。
    /// 除外ディレクトリには最初から降りない。
    // WhyNot: 「全部集めてから除外パスを捨てる」フィルタにしない — build/ は数十万ファイルに
    // 膨れることがあり、歩くだけで一覧の応答が数十秒かかるため。
    pub def walkJson(listChildren: String -> List[DirEntry] \ ef, relDir: String): List[String] \ ef =
        listChildren(relDir) |> List.flatMap(entry -> {
            let path = if (relDir == "") entry#name else "${relDir}/${entry#name}";
            if (entry#isDir) {
                if (Set.memberOf(entry#name, excludedDirs())
                        or String.startsWith(prefix = ".", entry#name)) Nil
                else walkJson(listChildren, path)
            }
            else if (String.endsWith(suffix = ".json", entry#name)) path :: Nil
            else Nil
        })

    /// dir/relDir の中身を列挙する。読めないディレクトリは空扱い。
    def childrenOf(dir: String, relDir: String): List[DirEntry] \ IO =
        let absPath = if (relDir == "") dir else "${dir}/${relDir}";
        let entries = new JFile(absPath).listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.map(file -> ({name = file.getName(), isDir = file.isDirectory()}: DirEntry))

    /// パスの最後の要素 (ファイル名)。
    def baseName(path: String): String =
        String.splitOn(substr = "/", path) |> List.last |> Option.getWithDefault(path)

    // ── ディスク変更の見張り (/changes) ───────────────────

    /// (パス, mtime) の一覧全体から決まる集計値。ファイルの増減でも、どれか 1 つの
    /// mtime 変化でも必ず別の値になる。エディタ画面はこれの変化だけ見れば
    /// 「何かが変わった」と分かる (どれが変わったかは個別の mtime で判定する)。
    pub def changeToken(entries: List[(String, Int64)]): String =
        let mix = entries |> List.foldLeft((acc, entry) -> {
            let (path, mtime) = entry;
            acc * 1000003i64 + Int32.toInt64(Hash.hash(path)) + mtime
        }, 0i64);
        "${List.length(entries)}-${mix}"

    /// dir 相対パスたちの mtime (ミリ秒)。走査と読みの合間に消えたファイルは
    /// (mtime 0 で報告せず) 一覧から落とす。
    pub def mtimesOf(dir: String, paths: List[String]): List[(String, Int64)] \ IO =
        paths |> List.filterMap(path -> {
            let mtime = new JFile("${dir}/${path}").lastModified();
            if (mtime == 0i64) None else Some((path, mtime))
        })

    // ── 読み書き ──────────────────────────────────────────

    /// リクエストで来た相対パスを dir 配下の絶対パスへ解決する。
    /// 絶対パス・`..` 入り・正規化すると dir の外へ出るものは None。
    /// ブラウザから来る値でプロジェクト外を読み書きさせないための門番。
    /// 区切りは "\" も "/" と同じに扱う — Windows の JFile は "\" も道の区切りと読むので、
    /// "/" だけ見ていると "\.." のすり抜けと「正規化の結果が \ 区切りで返って照合が必ず外れ、
    /// 全ファイルがプロジェクト外扱いになる」の両方が起きる。
    pub def resolveInside(dir: String, relPath: String): Option[String] \ IO =
        let normalized = String.replace(src = "\\", dst = "/", relPath);
        let hasDotDot = String.splitOn(substr = "/", normalized) |> List.exists(segment -> segment == "..");
        if (String.startsWith(prefix = "/", normalized) or hasDotDot)
            None
        else {
            let rootCanonical = new JFile(dir).getCanonicalPath();
            let target = new JFile("${dir}/${relPath}").getCanonicalPath();
            if (String.startsWith(prefix = rootCanonical + JFile.separator, target)) Some(target) else None
        }

    /// dir 相対パスのファイルを読む。プロジェクト外・読めないは Err(理由)。
    pub def readInside(dir: String, relPath: String): Result[String, String] \ IO =
        match resolveInside(dir, relPath) {
            case None => Err("path is outside the project: ${relPath}")
            case Some(fullPath) =>
                let readResult = run { Fs.ReadFile.read(fullPath) }
                    with Fs.ReadFile.runWithFileRead
                    with Fs.FileRead.runWithIO;
                readResult |> Result.mapErr(ioError -> "read failed: ${ioError}")
        }

    /// 書き込み時に親ディレクトリを Studio が作ってよい相対パスか。下書き置き場
    /// (draft/ 配下) だけ真。
    // WhyNot: 全パスで親を作らない — 置き場所の打ち間違いが静かにディレクトリを
    // 増やすのを防ぐ既定は保ち、ゲームの動きに関わらない draft/ だけ例外にする。
    pub def allowsParentCreation(relPath: String): Bool =
        String.replace(src = "\\", dst = "/", relPath)
            |> String.splitOn(substr = "/")
            |> List.head
            |> Option.map(segment -> segment == "draft")
            |> Option.getWithDefault(false)

    /// dir 相対パスへ文字列を書き込む。プロジェクト外・親ディレクトリ不在は Err(理由)。
    /// draft/ 配下だけは親ディレクトリが無ければ作ってから書く。
    pub def writeInside(str: {str = String}, dir: String, relPath: String): Result[String, Unit] \ IO =
        match resolveInside(dir, relPath) {
            case None => Err("path is outside the project: ${relPath}")
            case Some(fullPath) =>
                let parent = new JFile(fullPath).getParentFile();
                if (allowsParentCreation(relPath) and not Object.isNull(parent))
                    discard parent.mkdirs()
                else ();
                // 新しいディレクトリは作らない: 置き場所の打ち間違いが静かにディレクトリを増やすのを防ぐ。
                if (Object.isNull(parent) or not parent.isDirectory())
                    Err("parent directory does not exist: ${relPath}")
                else {
                    let writeResult = run { Fs.WriteFile.write(str, fullPath) }
                        with Fs.WriteFile.runWithFileWrite
                        with Fs.FileWrite.runWithIO;
                    writeResult
                        |> Result.mapErr(ioError -> "write failed: ${ioError}")
                        |> Result.map(_ -> ())
                }
        }
}
