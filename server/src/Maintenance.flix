// Maintenance — こまったときの掃除。ビルドの詰まり (lib/cache に残った *.lock) と
// フォントの焼き置き (全ゲーム共有のキャッシュ) を、画面のボタンから捨てられるようにする。
// 遊んでいるときの重さを直す係ではない — 「始まらない・固まる・字が乱れる」への手当て。

mod Maintenance {
    import java.io.{File => JFile}
    import java.lang.System

    use Util.Json.Json
    use Util.Json.Json.{JObject, JBool, JNumber}

    /// ビルドの詰まりの掃除: プロジェクトの lib/cache 配下に残った *.lock を消す。
    /// flix の中断 (Ctrl-C・強制終了) は Maven cache の lock を残し、次のビルドが
    /// 黙って待ち続ける。lock 以外には触らない — cache 本体を消すと再取得が重いため。
    pub def cleanLocks(dir: String): (Int32, Json) \ IO =
        let removed = removeLocksUnder(new JFile("${dir}/lib/cache"));
        (200, JObject(Map#{"ok" => JBool(true), "removed" => JNumber(JsonCodec.intToBd(removed))}))

    /// フォントの焼き置き (全ゲーム共有) を捨てる。壊れた焼き置きで字が乱れたときの手。
    /// 次の起動は焼き直しになるぶん遅い — だから自動では消さず、ボタンにだけ置く。
    pub def clearFontCache(): (Int32, Json) \ IO =
        let custom = System.getenv("FLIX_GE_CACHE_DIR");
        let home = System.getProperty("user.home");
        let target = fontCacheDir(if (Object.isNull(custom)) None else Some(custom), {home = home});
        let removed = removeAllUnder(new JFile(target));
        (200, JObject(Map#{"ok" => JBool(true), "removed" => JNumber(JsonCodec.intToBd(removed))}))

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// 焼き置きの場所。ゲーム側 (render_gl の FontCache) と同じ決まりに揃える:
    /// FLIX_GE_CACHE_DIR があればそこ、無ければ ~/.cache/flix_game_engine/font。
    /// 空白だけの指定は「無し」と同じに扱う (空の環境変数で家の外を消さないため)。
    pub def fontCacheDir(custom: Option[String], home: {home = String}): String =
        let fallback = "${home#home}/.cache/flix_game_engine/font";
        match custom {
            case Some(path) => if (String.trim(path) == "") fallback else path
            case None       => fallback
        }

    /// 消してよいファイルの見分け: lock だけ。
    pub def isLock(name: String): Bool =
        String.endsWith(suffix = ".lock", name)

    // ── ファイル歩き ──────────────────────────────────────

    /// file 配下の *.lock を消し、消した数を返す。無い場所を渡されたら 0 (掃除済みと同じ)。
    def removeLocksUnder(file: JFile): Int32 \ IO =
        if (file.isDirectory())
            childrenOf(file) |> List.foldLeft((acc, child) -> acc + removeLocksUnder(child), 0)
        else if (isLock(file.getName()) and file.delete())
            1
        else
            0

    /// file 配下を丸ごと消し、消したファイル数を返す (入れ物のディレクトリは数えない)。
    def removeAllUnder(file: JFile): Int32 \ IO =
        if (file.isDirectory()) {
            let count = childrenOf(file) |> List.foldLeft((acc, child) -> acc + removeAllUnder(child), 0);
            discard file.delete();
            count
        } else if (file.delete())
            1
        else
            0

    /// ディレクトリの中身。読めない (権限・消えた) は空 = そこで歩きが止まるだけ。
    def childrenOf(file: JFile): List[JFile] \ IO =
        let entries = file.listFiles();
        if (Object.isNull(entries)) Nil else Array.toList(entries)
}
