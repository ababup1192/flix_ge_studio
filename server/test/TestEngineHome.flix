// TestEngineHome — engine の置き場を決める規則だけを固定する。
// 実在するかの確認 (bin/flix があるか) はファイルを触るので通し確認に任せる。

mod TestEngineHome {

    /// 教わった場所があればそれを使い、未設定・空・空白だけなら手元の既定へ倒す。
    @Test
    def testResolve(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/Applications/Flix GE Studio.app/Contents/Resources/engine",
                        "/home/me/Desktop/flix_game_engine",
                        "/home/me/Desktop/flix_game_engine",
                        "/home/me/Desktop/flix_game_engine"),
            (EngineHome.resolve(None, Some("/Applications/Flix GE Studio.app/Contents/Resources/engine"), "/home/me"),
             EngineHome.resolve(None, None, "/home/me"),
             EngineHome.resolve(None, Some(""), "/home/me"),
             EngineHome.resolve(None, Some("   "), "/home/me")))

    /// 置き場 (engines/<バージョン>/) が決まっていれば、同梱先より先にそれを使う。
    @Test
    def testResolvePrefersStore(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/home/me/.flix_ge_studio/engines/0.32.0",
                        "/home/me/.flix_ge_studio/engines/0.32.0"),
            (EngineHome.resolve(Some("/home/me/.flix_ge_studio/engines/0.32.0"),
                                Some("/Applications/Flix GE Studio.app/Contents/Resources/engine"),
                                "/home/me"),
             EngineHome.resolve(Some("/home/me/.flix_ge_studio/engines/0.32.0"), None, "/home/me")))

    /// 置き場の中かどうか (中ならゲームへ渡すのは別名の方)。
    @Test
    def testIsStorePath(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (EngineHome.isStorePath("/home/me", "/home/me/.flix_ge_studio/engines/0.31.0"),
             EngineHome.isStorePath("/home/me", "/home/me/Desktop/flix_game_engine"),
             EngineHome.isStorePath("/home/me", "/Applications/Flix GE Studio.app/Contents/Resources/engine")))

    /// 空白を含む道はそのまま渡せない (ゲームの Makefile が千切る)。
    @Test
    def testNeedsAlias(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (EngineHome.needsAlias("/Applications/Flix GE Studio.app/Contents/Resources/engine"),
             EngineHome.needsAlias("/Users/me/Desktop/flix_game_engine"),
             EngineHome.needsAlias("/Users/me/.flix_ge_studio/engine")))
}
