// TestEngineStore — 置き場の道の組み立てと、指し先の読み方・種まきが要るかの規則だけを
// 固定する。実際の写し (60MB) と rename はファイルを触るので通し確認に任せる。

mod TestEngineStore {

    /// 道の組み立て: ホームの下に .flix_ge_studio/engines/<バージョン>/ が並ぶ。
    @Test
    def testPaths(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/home/me/.flix_ge_studio",
                        "/home/me/.flix_ge_studio/engines",
                        "/home/me/.flix_ge_studio/engines/0.31.0",
                        "/home/me/.flix_ge_studio/engine-current.txt"),
            (EngineStore.baseDir("/home/me"),
             EngineStore.versionsDir("/home/me"),
             EngineStore.versionDir("/home/me", "0.31.0"),
             EngineStore.currentFile("/home/me")))

    /// 指し先の読み方: 最初の中身のある行。空・空白だけ・末尾の改行は None にしない。
    @Test
    def testCurrentVersion(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("0.31.0"), Some("0.32.0"), None, None),
            (EngineStore.currentVersion("0.31.0\n"),
             EngineStore.currentVersion("  0.32.0  \n0.31.0\n"),
             EngineStore.currentVersion(""),
             EngineStore.currentVersion("   \n\n")))

    /// 壊れた指し先で置き場の外を指させない。
    @Test
    def testCurrentVersionRejectsEscape(): Unit \ Assert =
        Assert.assertEq(
            expected = (None, None, None),
            (EngineStore.currentVersion("..\n"),
             EngineStore.currentVersion("../../etc\n"),
             EngineStore.currentVersion("/absolute\n")))

    /// フォルダ名として置いてよい字。数字で始まり、英数と . _ - だけ。
    @Test
    def testIsVersionName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false, false),
            (EngineStore.isVersionName("0.31.0"),
             EngineStore.isVersionName("0.32.0-rc1"),
             EngineStore.isVersionName(""),
             EngineStore.isVersionName(".."),
             EngineStore.isVersionName("a/b"),
             EngineStore.isVersionName("C:")))

    /// 数字で始まらない名前は落とす (空白入り・隠しフォルダ・長すぎる名前も)。
    @Test
    def testIsVersionNameRejectsUnusableNames(): Unit \ Assert =
        Assert.assertEq(
            expected = (false, false, false, false),
            (EngineStore.isVersionName("0.31 0"),
             EngineStore.isVersionName(".git"),
             EngineStore.isVersionName("CON"),
             EngineStore.isVersionName(String.repeat(65, "0"))))

    /// バージョンの前後は数字の列として比べる (字の順で 0.9.0 > 0.31.0 と読み違えない)。
    @Test
    def testIsNewerVersion(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, true),
            (EngineStore.isNewerVersion("0.9.0", "0.31.0"),
             EngineStore.isNewerVersion("0.31.0", "0.9.0"),
             EngineStore.isNewerVersion("0.31.0", "0.31.0"),
             EngineStore.isNewerVersion("0.31", "0.31.1")))

    /// 数に割れない字は「新しい」に倒さない (試作を人の意図なく本番へ進めない)。
    @Test
    def testIsNewerVersionIgnoresUnreadable(): Unit \ Assert =
        Assert.assertEq(
            expected = (false, false),
            (EngineStore.isNewerVersion("0.31.0", "0.32.0-rc1"),
             EngineStore.isNewerVersion("nightly", "0.32.0")))

    /// 種まきが要るのは、同梱として教わっていて、バージョンが読めて、まだ置き場に無いときだけ。
    @Test
    def testNeedsSeeding(): Unit \ Assert =
        Assert.assertEq(
            expected = true,
            EngineStore.needsSeeding({bundled = Some("/app/engine"), version = Some("0.31.0"), hasVersionDir = false}))

    /// 2 回目以降は写さない (置き場に既にある)。
    @Test
    def testNeedsSeedingIdempotent(): Unit \ Assert =
        Assert.assertEq(
            expected = false,
            EngineStore.needsSeeding({bundled = Some("/app/engine"), version = Some("0.31.0"), hasVersionDir = true}))

    /// 手元の engine リポで開発中 (同梱として教わっていない) は写さない。
    @Test
    def testNeedsSeedingSkipsDev(): Unit \ Assert =
        Assert.assertEq(
            expected = (false, false),
            (EngineStore.needsSeeding({bundled = None, version = Some("0.31.0"), hasVersionDir = false}),
             EngineStore.needsSeeding({bundled = Some("/app/engine"), version = None, hasVersionDir = false})))

    /// 指し先を書くのは、まだ無いときと、指した先の実体が無いときだけ。
    /// 更新で進めた指し先 (0.32.0) は、同梱が古いなら引き戻さない。
    @Test
    def testShouldWriteCurrent(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false),
            (EngineStore.shouldWriteCurrent({current = None, currentDirExists = false, bundledIsNewer = false}),
             EngineStore.shouldWriteCurrent({current = Some("0.30.0"), currentDirExists = false, bundledIsNewer = false}),
             EngineStore.shouldWriteCurrent({current = Some("0.32.0"), currentDirExists = true, bundledIsNewer = false})))

    /// アプリを新しく入れ直したとき (同梱の方が新しい) は指し先を進める。
    /// Windows は更新の手段がこれしかないので、ここで止めると永久に上がらない。
    @Test
    def testShouldWriteCurrentAdvancesForNewerBundle(): Unit \ Assert =
        Assert.assertEq(
            expected = true,
            EngineStore.shouldWriteCurrent({current = Some("0.31.0"), currentDirExists = true, bundledIsNewer = true}))
}
