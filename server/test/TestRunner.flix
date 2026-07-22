// TestRunner — 焼き係のルールだけ固定する: 走らせてよい対象・ログの刈り込み。
// プロセス起動や HTTP は curl での通し確認に任せ、ここは判定だけを見る。

mod TestRunner {

    /// make 対象は白名前簿の 4 つだけ。似た名前や空・任意コマンドは通さない。
    @Test
    def testIsAllowedTarget(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, true, true, false, false, false),
            (Runner.isAllowedTarget("bake"),
             Runner.isAllowedTarget("gallery-prologue"),
             Runner.isAllowedTarget("gallery-sounds"),
             Runner.isAllowedTarget("atelier-preview"),
             Runner.isAllowedTarget("golden"),
             Runner.isAllowedTarget("bake; rm -rf /"),
             Runner.isAllowedTarget("")))

    /// Makefile に実在する焼き対象だけ配る: 行頭の「対象:」だけが実在の印
    /// (字下げ・コメント・似た名前は数えない)。空の本文 (読めない) は bake だけに倒す。
    @Test
    def testTargetsFromMakefileText(): Unit \ Assert =
        Assert.assertEq(
            expected = (Runner.bakeTargets(), "bake" :: "gallery-sounds" :: Nil, "bake" :: Nil, "bake" :: Nil),
            (Runner.targetsFromMakefileText("bake: all\ngallery-prologue:\ngallery-sounds: bake\n"),
             Runner.targetsFromMakefileText("bake:\n\t@echo hi\n# gallery-prologue:\n\tgallery-prologue: x\ngallery-sounds:\n"),
             Runner.targetsFromMakefileText("bake:\nrebake: bake\nbakery:\n"),
             Runner.targetsFromMakefileText("")))

    /// ログは尻尾 cap 行だけ残す。cap 以内なら丸ごと残る。
    @Test
    def testKeepTail(): Unit \ Assert =
        Assert.assertEq(
            expected = ("b" :: "c" :: Nil, "a" :: Nil, Nil),
            (Runner.keepTail(2, "a" :: "b" :: "c" :: Nil),
             Runner.keepTail(5, "a" :: Nil),
             Runner.keepTail(3, (Nil: List[String]))))
}
