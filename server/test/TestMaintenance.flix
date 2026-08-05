// TestMaintenance — 掃除の純粋なルールだけ固定する: 焼き置きの場所の決まりと、
// 消してよいファイルの見分け。実際のファイル削除は通し確認に任せる。

mod TestMaintenance {

    /// 場所: 指定があればそこ。空白だけ・無しは ~/.cache の既定へ倒す。
    @Test
    def testFontCacheDir(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/custom/font", "/home/u/.cache/flix_game_engine/font", "/home/u/.cache/flix_game_engine/font"),
            ( Maintenance.fontCacheDir(Some("/custom/font"), home = "/home/u"),
              Maintenance.fontCacheDir(Some("   "), home = "/home/u"),
              Maintenance.fontCacheDir(None, home = "/home/u") ))

    /// 見分け: 消すのは .lock だけ。名前の途中の lock や拡張子なしは消さない。
    @Test
    def testIsLock(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            ( Maintenance.isLock("resolve.lock"),
              Maintenance.isLock("library.jar"),
              Maintenance.isLock("lockfile") ))
}
