// TestGallery — 配ってよい名前の門番と、golden との見比べ (差分の種類分け) のルールを固定する。
// ファイルの実読みや HTTP は curl での通し確認に任せ、ここは判定だけを見る。

mod TestGallery {

    /// 名前 1 語だけ通す: 区切り (/ \) や `..` 入り・空は弾く。
    @Test
    def testIsSafeName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false, false),
            (Gallery.isSafeName("spring.png"),
             Gallery.isSafeName("a/b.png"),
             Gallery.isSafeName("..\\x.png"),
             Gallery.isSafeName("../x.png"),
             Gallery.isSafeName("")))

    /// 差分の種類分け: 両方あり + 一致 = ok、不一致 = diff、
    /// gallery だけ = missingGolden、golden だけ = missingGallery。
    @Test
    def testDiffStatus(): Unit \ Assert =
        Assert.assertEq(
            expected = ("ok", "diff", "missingGolden", "missingGallery"),
            (Gallery.diffStatus(true, true, true),
             Gallery.diffStatus(true, true, false),
             Gallery.diffStatus(true, false, false),
             Gallery.diffStatus(false, true, false)))

    /// 一覧は両側の名前の和集合 (名前順・重複なし)。一致判定は両方に在る名前だけに使う。
    @Test
    def testDiffItemsUnion(): Unit \ Assert =
        Assert.assertEq(
            expected = ("autumn.png", "missingGallery") :: ("spring.png", "ok")
                    :: ("summer.png", "diff") :: ("winter.png", "missingGolden") :: Nil,
            Gallery.diffItems(
                "spring.png" :: "summer.png" :: "winter.png" :: Nil,
                "spring.png" :: "summer.png" :: "autumn.png" :: Nil,
                name -> name == "spring.png"))
}
