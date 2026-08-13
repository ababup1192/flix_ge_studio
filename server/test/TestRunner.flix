// TestRunner — 焼き係のルールだけ固定する: 走らせてよい対象・ログの刈り込み。
// プロセス起動や HTTP は curl での通し確認に任せ、ここは判定だけを見る。

mod TestRunner {

    /// make 対象は許可リストの 4 つだけ。似た名前や空・任意コマンドは通さない。
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

    /// ゲームの命令口の /bake 応答の 1 行目の解釈: "ok baked=…" だけが成功。
    /// "error: …" (未対応・ループ無応答) や空は失敗 = make bake へのフォールバック。
    @Test
    def testRemoteBakeOk(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false),
            (Runner.remoteBakeOk("ok baked=3 ms=250\n[baked]\ngallery/title.png"),
             Runner.remoteBakeOk("error: bake not supported (App.onBakeRequest 未登録)"),
             Runner.remoteBakeOk("error: game loop did not answer within 10s"),
             Runner.remoteBakeOk("")))

    /// 生の HTTP 応答から本文を取り出す: 最初の空行 (CRLF 2 連) の後ろ全部。
    /// 空行が無い壊れた応答は空文字 = 「焼けていない」に倒す。
    @Test
    def testBodyOfHttpText(): Unit \ Assert =
        Assert.assertEq(
            expected = ("ok baked=1 ms=200\n[baked]\ngallery/title.png", ""),
            (Runner.bodyOfHttpText("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nok baked=1 ms=200\n[baked]\ngallery/title.png"),
             Runner.bodyOfHttpText("HTTP/1.1 200 OK\r\nContent-Type: text/plain")))
}
