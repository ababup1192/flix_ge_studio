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
            (EngineHome.resolve(Some("/Applications/Flix GE Studio.app/Contents/Resources/engine"), "/home/me"),
             EngineHome.resolve(None, "/home/me"),
             EngineHome.resolve(Some(""), "/home/me"),
             EngineHome.resolve(Some("   "), "/home/me")))

    /// 空白を含む道はそのまま渡せない (ゲームの Makefile が千切る)。
    @Test
    def testNeedsAlias(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (EngineHome.needsAlias("/Applications/Flix GE Studio.app/Contents/Resources/engine"),
             EngineHome.needsAlias("/Users/me/Desktop/flix_game_engine"),
             EngineHome.needsAlias("/Users/me/.flix_ge_studio/engine")))
}
