// TestGallery — 配ってよい名前の門番と、リファレンス画像との見比べ (差分の種類分け) のルールを固定する。
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
    /// gallery だけ = missingReference、reference だけ = missingGallery。
    @Test
    def testDiffStatus(): Unit \ Assert =
        Assert.assertEq(
            expected = ("ok", "diff", "missingReference", "missingGallery"),
            (Gallery.diffStatus(true, true, true),
             Gallery.diffStatus(true, true, false),
             Gallery.diffStatus(true, false, false),
             Gallery.diffStatus(false, true, false)))

    /// 一覧は両側の名前の和集合 (名前順・重複なし)。一致判定は両方に在る名前だけに使う。
    @Test
    def testDiffItemsUnion(): Unit \ Assert =
        Assert.assertEq(
            expected = ("autumn.png", "missingGallery") :: ("spring.png", "ok")
                    :: ("summer.png", "diff") :: ("winter.png", "missingReference") :: Nil,
            Gallery.diffItems(
                "spring.png" :: "summer.png" :: "winter.png" :: Nil,
                "spring.png" :: "summer.png" :: "autumn.png" :: Nil,
                name -> name == "spring.png",
                _ -> false))

    /// SHA256SUMS.txt の読み方 (engine の reference-update.sh が書き、checkReference が読む形):
    /// "<ハッシュ>  <名前>" を名前 → ハッシュに。2 つに割れない行は飛ばす。空の本文は空
    /// (= 一覧の無いゲームは承認済みが無く、今までどおり)。
    @Test
    def testParseReferenceSums(): Unit \ Assert =
        Assert.assertEq(
            expected = (Map#{"title.png" => "ab12", "s1 clear.png" => "cd34"}, Map#{}),
            (Gallery.parseReferenceSums("ab12  title.png\ncd34  s1 clear.png\nbroken\n\n"),
             Gallery.parseReferenceSums("")))

    /// 手元の基準と違っても、SHA256SUMS.txt と同じ絵は approved (承認済み)。基準が無い絵も同じ。
    @Test
    def testDiffItemsApproved(): Unit \ Assert =
        Assert.assertEq(
            expected = ("spring.png", "approved") :: ("summer.png", "diff")
                    :: ("winter.png", "approved") :: Nil,
            Gallery.diffItems(
                "spring.png" :: "summer.png" :: "winter.png" :: Nil,
                "spring.png" :: "summer.png" :: Nil,
                _ -> false,
                name -> name != "summer.png"))

    /// 基準とバイトで同じ絵は、SHA256SUMS.txt と違っても ok のまま (今までどおり)。
    @Test
    def testDiffItemsSameBytesWins(): Unit \ Assert =
        Assert.assertEq(
            expected = ("spring.png", "ok") :: Nil,
            Gallery.diffItems("spring.png" :: Nil, "spring.png" :: Nil, _ -> true, _ -> false))
}
