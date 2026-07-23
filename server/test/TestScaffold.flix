// TestScaffold — まっさらの入口のルールだけ固定する: 名前の門番。
// プロセス起動やファイル書きは curl での通し確認に任せ、ここは純粋な判定だけを見る。

mod TestScaffold {

    /// 名前は ^[a-z][a-z0-9_]*$。大文字・数字始まり・記号・空・経路いじりは通さない。
    @Test
    def testIsName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false, false, false),
            (Scaffold.isName("breakout2"),
             Scaffold.isName("my_game"),
             Scaffold.isName("Breakout"),
             Scaffold.isName("2games"),
             Scaffold.isName(""),
             Scaffold.isName("a-b"),
             Scaffold.isName("../x")))
}
