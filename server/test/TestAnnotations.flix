// TestAnnotations — 注釈チケットの README の読み書きルールを固定する。
// engine が書く README の形 (自動タイトル + 「## コメント」の空欄) を前提に、
// タイトルの取り出し・一言の読み書き・チケット名の門番だけを見る (IO は見ない)。

mod TestAnnotations {

    /// engine の readmeText と同じ形の README (コメント欄は空欄の印のまま)。
    def sampleReadme(): String =
        String.unlines(
            "# 注釈: my_game — frame 3412（2026-08-12 14:03:05）"
            :: ""
            :: "![highlighted](highlighted.png)"
            :: ""
            :: "## 囲った場所"
            :: ""
            :: "- フレーム: 3412"
            :: ""
            :: "## コメント"
            :: ""
            :: "（ここに修正指示を書いてください）"
            :: Nil)

    // ── タイトルとコメントの読み取り ──────────────────────

    /// タイトルは 1 行目から "# " を剥がした物。空欄の印しか無いコメントは未記入 = "" と読む。
    @Test
    def testReadFreshTicket(): Unit \ Assert =
        Assert.assertEq(
            expected = ("注釈: my_game — frame 3412（2026-08-12 14:03:05）", ""),
            (Annotations.titleOf(sampleReadme()), Annotations.commentOf(sampleReadme())))

    /// 書いた一言は commentOf でそのまま読み戻せる (書く → 読むの往復)。
    @Test
    def testCommentRoundtrip(): Unit \ Assert =
        let written = Annotations.withComment(comment = "敵がジャンプ台の上に湧いて理不尽", sampleReadme());
        Assert.assertEq(
            expected = "敵がジャンプ台の上に湧いて理不尽",
            Annotations.commentOf(written))

    /// 一言を書いてもコメント欄より前 (自動タイトル・囲った場所) は変えない。
    @Test
    def testWithCommentKeepsHead(): Unit \ Assert =
        let written = Annotations.withComment(comment = "一言", sampleReadme());
        Assert.assertEq(
            expected = (Annotations.titleOf(sampleReadme()), true),
            (Annotations.titleOf(written), String.contains(substr = "## 囲った場所", written)))

    /// 二度目の書き込みは前の一言を置き換える (継ぎ足しで増えない)。
    @Test
    def testWithCommentReplaces(): Unit \ Assert =
        let once = Annotations.withComment(comment = "最初の一言", sampleReadme());
        let twice = Annotations.withComment(comment = "直した一言", once);
        Assert.assertEq(
            expected = ("直した一言", false),
            (Annotations.commentOf(twice), String.contains(substr = "最初の一言", twice)))

    /// コメント節の後に別の節が続く README でも、一言は見出しの直後に入り
    /// (読み戻せる)、後続の節はそのまま残る。
    @Test
    def testWithCommentKeepsTrailingSection(): Unit \ Assert =
        let readme = String.unlines(
            "# 注釈: x — frame 1（日時）"
            :: "## コメント"
            :: ""
            :: "（ここに修正指示を書いてください）"
            :: ""
            :: "## 追記"
            :: ""
            :: "残したい行"
            :: Nil);
        let once = Annotations.withComment(comment = "最初の一言", readme);
        let twice = Annotations.withComment(comment = "直した一言", once);
        Assert.assertEq(
            expected = ("直した一言", true, false),
            (Annotations.commentOf(twice),
             String.contains(substr = "残したい行", twice),
             String.contains(substr = "最初の一言", twice)))

    /// コメント節が無い README (手で消した等) でも、末尾に節を足して書ける。
    @Test
    def testWithCommentWithoutSection(): Unit \ Assert =
        let bare = String.unlines("# 注釈: x — frame 1（日時）" :: "" :: "本文だけ" :: Nil);
        Assert.assertEq(
            expected = "後から足した一言",
            Annotations.commentOf(Annotations.withComment(comment = "後から足した一言", bare)))

    // ── チケット名の門番 ──────────────────────────────────

    /// 普通のチケット名 (日時_題名_フレーム) は通し、空・上への移動・区切り入り・
    /// archive (移動先の入れ物) は弾く。
    @Test
    def testIsSafeId(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false, false, false),
            (Annotations.isSafeId("20260812-140305_my_game_f3412"),
             Annotations.isSafeId(""),
             Annotations.isSafeId(".."),
             Annotations.isSafeId("a/b"),
             Annotations.isSafeId("a\\b"),
             Annotations.isSafeId("archive")))

    // ── ラフと見比べて切るチケット ────────────────────────

    /// 置き場の名前は「日時 + 見比べた絵 + 印」。頭が日時なので降順がそのまま新しい順。
    @Test
    def testTicketDirName(): Unit \ Assert =
        Assert.assertEq(
            expected = "20260812-140305_title_png_sketch",
            Annotations.ticketDirName({stamp = "20260812-140305", scene = "title.png"}))

    /// 絵を選ばずに切ったチケットでも名前は空にならない。
    @Test
    def testTicketDirNameWithoutScene(): Unit \ Assert =
        Assert.assertEq(
            expected = "20260812-140305_rough_sketch",
            Annotations.ticketDirName({stamp = "20260812-140305", scene = ""}))

    /// 印は末尾だけを見る。engine のチケット (末尾が _f<フレーム>) は遊んで気づいた物のまま。
    @Test
    def testIsSketchId(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (Annotations.isSketchId("20260812-140305_title_png_sketch"),
             Annotations.isSketchId("20260812-140305_my_game_f3412")))

    /// 書いた README は engine の物と同じ読み方ができる (見出し・一言が読み戻せる)。
    @Test
    def testSketchReadmeRoundtrip(): Unit \ Assert =
        let readme = Annotations.sketchReadme({
            title = "ラフと見比べ: title.png ↔ title v3",
            stamp = "2026-08-12 14:03:05",
            body = "## 見比べた物\n\n- 生成された絵: gallery/title.png",
            comment = "空が明るすぎる"
        });
        Assert.assertEq(
            expected = ("ラフと見比べ: title.png ↔ title v3（2026-08-12 14:03:05）", "空が明るすぎる"),
            (Annotations.titleOf(readme), Annotations.commentOf(readme)))

    /// 一言が空でも engine と同じ空欄の印を置く (後から Studio の入力欄で書ける)。
    @Test
    def testSketchReadmeEmptyComment(): Unit \ Assert =
        let readme = Annotations.sketchReadme({
            title = "ラフと見比べ", stamp = "2026-08-12 14:03:05", body = "本文", comment = "  "
        });
        Assert.assertEq(expected = "", Annotations.commentOf(readme))

    /// 本文 (見比べた物の場所) はコメント欄に飲み込まれず残る。
    @Test
    def testSketchReadmeKeepsBody(): Unit \ Assert =
        let readme = Annotations.sketchReadme({
            title = "ラフと見比べ", stamp = "2026-08-12 14:03:05",
            body = "- ラフ: draft/sketch/title/v3.json", comment = "一言"
        });
        Assert.assertEq(
            expected = true,
            String.contains(substr = "draft/sketch/title/v3.json", readme))
}
