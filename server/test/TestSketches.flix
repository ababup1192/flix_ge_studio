// TestSketches — ラフ塗りのバージョン番号・安全な名前判定を固定する。
// 見るのは純粋なルールだけ (versionOf / nextVersion / isSafeName)。
// 一覧を返す HTTP の口・ディレクトリの読み取りは IO なので見ない。

mod TestSketches {

    // ── versionOf ────────────────────────────────────────

    /// "vN.json" の N をバージョン番号として読み取る。
    @Test
    def testVersionOfReadsNumber(): Unit \ Assert =
        Assert.assertEq(expected = Some(3), Sketches.versionOf("v3.json"))

    /// 2 桁以上の番号も読み取る。
    @Test
    def testVersionOfReadsMultiDigit(): Unit \ Assert =
        Assert.assertEq(expected = Some(10), Sketches.versionOf("v10.json"))

    /// 0 はバージョン番号として認めない。
    @Test
    def testVersionOfRejectsZero(): Unit \ Assert =
        Assert.assertEq(expected = None, Sketches.versionOf("v0.json"))

    /// "v" で始まらない名前は None。
    @Test
    def testVersionOfRejectsOtherName(): Unit \ Assert =
        Assert.assertEq(expected = None, Sketches.versionOf("sketch.json"))

    /// 数字が 1 つも無ければ None。
    @Test
    def testVersionOfRejectsNoDigits(): Unit \ Assert =
        Assert.assertEq(expected = None, Sketches.versionOf("v.json"))

    /// 数字以外の文字が混じっていれば None。
    @Test
    def testVersionOfRejectsNonDigit(): Unit \ Assert =
        Assert.assertEq(expected = None, Sketches.versionOf("vx.json"))

    // ── nextVersion ──────────────────────────────────────

    /// まだ 1 つも無ければ 1。
    @Test
    def testNextVersionStartsAtOne(): Unit \ Assert =
        Assert.assertEq(expected = 1, Sketches.nextVersion(Nil))

    /// 連番の次は最大値 + 1。
    @Test
    def testNextVersionAfterConsecutive(): Unit \ Assert =
        Assert.assertEq(expected = 4, Sketches.nextVersion(1 :: 2 :: 3 :: Nil))

    /// 抜け番があっても最大値の次を返す (埋めない)。
    @Test
    def testNextVersionSkipsGap(): Unit \ Assert =
        Assert.assertEq(expected = 4, Sketches.nextVersion(1 :: 3 :: Nil))

    /// 降順で渡っても結果は変わらない。
    @Test
    def testNextVersionOrderIndependent(): Unit \ Assert =
        Assert.assertEq(expected = 4, Sketches.nextVersion(3 :: 1 :: Nil))

    // ── isSafeName ───────────────────────────────────────

    /// 普通の名前は通す。
    @Test
    def testIsSafeNameAcceptsPlainName(): Unit \ Assert =
        Assert.assertEq(expected = true, Sketches.isSafeName("my_sketch"))

    /// 空・カレント・上へ移動・区切り入りは弾く。
    @Test
    def testIsSafeNameRejectsUnsafe(): Unit \ Assert =
        Assert.assertEq(
            expected = (false, false, false, false),
            (Sketches.isSafeName(""),
             Sketches.isSafeName(".."),
             Sketches.isSafeName("a/b"),
             Sketches.isSafeName("a\\b")))
}
