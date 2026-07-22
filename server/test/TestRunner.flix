// TestRunner — 焼き係のルールだけ固定する: 走らせてよい対象・祝福してよい名前・ログの刈り込み。
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

    /// 祝福の名前は「安全な 1 語」かつ .png のみ。経路またぎ・別拡張子は弾く。
    @Test
    def testIsBlessName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false),
            (Runner.isBlessName("villagers.png"),
             Runner.isBlessName("../villagers.png"),
             Runner.isBlessName("villagers.wav"),
             Runner.isBlessName("")))

    /// ログは尻尾 cap 行だけ残す。cap 以内なら丸ごと残る。
    @Test
    def testKeepTail(): Unit \ Assert =
        Assert.assertEq(
            expected = ("b" :: "c" :: Nil, "a" :: Nil, Nil),
            (Runner.keepTail(2, "a" :: "b" :: "c" :: Nil),
             Runner.keepTail(5, "a" :: Nil),
             Runner.keepTail(3, (Nil: List[String]))))
}
