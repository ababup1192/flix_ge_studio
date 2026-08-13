// TestChanges — 基準の自動追随のルールを固定する: 退避の名前づけ・バージョン番号・記録の読み書き。
// 実ファイルのコピーと自動焼きの起動は通し確認に任せ、ここは純粋な判定だけを見る。

mod TestChanges {

    /// 退避の名前: 拡張子の手前に vN。場面名からバージョン番号を読み戻せる。
    /// 別の場面の退避や、v が数字でない名前は読まない。
    @Test
    def testVersionName(): Unit \ Assert =
        Assert.assertEq(
            expected = ("title.v3.png", Some(3), None, None),
            (Changes.versionName("title.png", 3),
             Changes.verIfVersionOf("title.v3.png", "title.png"),
             Changes.verIfVersionOf("over.v3.png", "title.png"),
             Changes.verIfVersionOf("title.vx.png", "title.png")))

    /// 次のバージョン番号: その場面の退避の最大 + 1。無ければ 1。歯抜けでも最大基準。
    @Test
    def testNextVer(): Unit \ Assert =
        Assert.assertEq(
            expected = (1, 4),
            (Changes.nextVerFrom(Nil, "title.png"),
             Changes.nextVerFrom("title.v1.png" :: "title.v3.png" :: "over.v9.png" :: Nil,
                 "title.png")))

    /// 保存が検査を蹴る対象か: assets/ の JSON だけ。焼きの成果物 (gallery/ や reference/) や
    /// JSON でない物は蹴らない — 保存→焼き→保存…の堂々巡りを判定の段階で断つ。
    @Test
    def testKicksPumpOnSave(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false),
            (Changes.kicksPumpOnSave("assets/sample.kind.json"),
             Changes.kicksPumpOnSave("gallery/title.png"),
             Changes.kicksPumpOnSave("reference/title.png"),
             Changes.kicksPumpOnSave("assets/title.png")))

    /// 既読の判定: 無記録 (器の get が null = None) は「未読なし」に倒して落ちない。
    /// "false" だけが未読で、それ以外の記録は既読。
    @Test
    def testSeenFromRecord(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, true),
            (Changes.seenFromRecord(None),
             Changes.seenFromRecord(Some("false")),
             Changes.seenFromRecord(Some("true"))))

    /// 記録の読み書き: 追記で後ろに積まれ、同じ場面は最新の 1 件に畳まれる。
    /// 壊れた断片は黙って捨てる (fail-open)。
    @Test
    def testEntries(): Unit \ Assert =
        let text = Changes.appendEntry(Changes.appendEntry("", "title.png", 1), "over.png", 2);
        Assert.assertEq(
            expected = (("title.png", 1) :: ("over.png", 2) :: Nil,
                        ("over.png", 2) :: ("title.png", 3) :: Nil,
                        Nil),
            (Changes.entriesOf(text),
             Changes.entriesOf(Changes.appendEntry(text, "title.png", 3)),
             Changes.entriesOf("こわれた断片;まだこわれてる")))
}
