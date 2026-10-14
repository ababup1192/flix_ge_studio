// TestGame — ゲームの命令口 (dir → DEBUG_HTTP_PORT) の覚え書きの読み方だけ固定する。
// プロセスの生死判定や実際の起動は通し確認に任せ、ここは純粋な判定だけを見る。

mod TestGame {

    /// 覚え書きの値の解釈: 数値の記録だけがポート。無記録 (コンテナの get が null = None) と
    /// 数値でない記録は None = 命令口は知らない (従来の make bake へ倒れる)。
    @Test
    def testPortFromRecord(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some(8123), None, None),
            (Game.portFromRecord(Some("8123")),
             Game.portFromRecord(None),
             Game.portFromRecord(Some("こわれた記録"))))

    /// 起動の途中の判定: 走っていなければ途中ではない。走っていて命令口がまだ答えない間と、
    /// 答え始めてから warmupMillis に届くまでは途中 (自動の描き出しを始めない)。
    @Test
    def testIsStartingBy(): Unit \ Assert =
        let warm = Game.warmupMillis();
        Assert.assertEq(
            expected = (false, true, true, false),
            (Game.isStartingBy({running = false, readyAge = None}),
             Game.isStartingBy({running = true, readyAge = None}),
             Game.isStartingBy({running = true, readyAge = Some(warm - 1i64)}),
             Game.isStartingBy({running = true, readyAge = Some(warm)})))
}
