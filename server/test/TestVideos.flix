// TestVideos — 動画のタブの口の決まり（配ってよいパスの門番・Range の読み・道具の答えの読み）を固定する。
// 撮れたかの判定は engine の bin/fge videos status の仕事（そちらの Go のテストが見る）。

mod TestVideos {
    use Util.Json.Json.{JObject, JBool}

    /// 置き場の下の mp4・PNG・txt だけを通す。外・.. 入り・ほかの拡張子は弾く。
    @Test
    def testIsServable(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, true, false, false, false),
            (Videos.isServable("debug/video/entry.mp4"),
             Videos.isServable("debug/video/entry/f0012.png"),
             Videos.isServable("debug/video/entry/sfx.txt"),
             Videos.isServable("debug/video/../../secret.mp4"),
             Videos.isServable("gallery/title.png"),
             Videos.isServable("debug/video/entry.video.json")))

    /// bytes=a-b は両端を含む範囲・終わりの無い形はファイルの終わりまで・終わりはファイルの中へ詰める。
    @Test
    def testRangeOfPart(): Unit \ Assert =
        Assert.assertEq(
            expected = (Videos.Range.Part(0i64, 1i64), Videos.Range.Part(5i64, 9i64), Videos.Range.Part(8i64, 9i64)),
            (Videos.rangeOf(Some("bytes=0-1"), 10i64),
             Videos.rangeOf(Some("bytes=5-"), 10i64),
             Videos.rangeOf(Some("bytes=8-99"), 10i64)))

    /// bytes=-N は末尾の N バイト。
    @Test
    def testRangeOfSuffix(): Unit \ Assert =
        Assert.assertEq(expected = Videos.Range.Part(7i64, 9i64), Videos.rangeOf(Some("bytes=-3"), 10i64))

    /// ヘッダ無し・読めない形は全部（200）・ファイルの外は 416。
    @Test
    def testRangeOfWholeAndUnsatisfiable(): Unit \ Assert =
        Assert.assertEq(
            expected = (Videos.Range.Whole, Videos.Range.Whole, Videos.Range.Unsatisfiable),
            (Videos.rangeOf(None, 10i64),
             Videos.rangeOf(Some("items=0-1"), 10i64),
             Videos.rangeOf(Some("bytes=10-"), 10i64)))

    /// 道具の出力は最後の JSON の行を読み、使えた印と置き場の有無を足す。
    @Test
    def testAnswerOfTakesLastJsonLine(): Unit \ Assert =
        let answer = Videos.answerOf("進み具合" :: "{\"dir\":\"debug/video\",\"videos\":[]}" :: Nil, false);
        let flags = answer |> Option.flatMap(json -> match json {
            case JObject(obj) => Some((Map.get("available", obj), Map.get("here", obj)))
            case _            => None
        });
        Assert.assertEq(expected = Some((Some(JBool(true)), Some(JBool(false)))), flags)

    /// JSON の行が無ければ読めない（画面は理由を出す）。
    @Test
    def testAnswerOfWithoutJson(): Unit \ Assert =
        Assert.assertEq(expected = true, Option.isEmpty(Videos.answerOf("知らないサブコマンド: videos" :: Nil, true)))

    /// 命令口の答えの 1 行目が ok なら流れた・FAIL はその行が理由。
    @Test
    def testRerunOutcome(): Unit \ Assert =
        Assert.assertEq(
            expected = (Ok(()), Err("FAIL e2e/a.flow:3: 待ちが max に当たった")),
            (Videos.rerunOutcome("ok frames=120 repeat=1\n[places]\n"),
             Videos.rerunOutcome("FAIL e2e/a.flow:3: 待ちが max に当たった\n    until x")))

    /// 一覧から名前の動画の flow を引く（flow の無い動画・知らない名前は空）。
    @Test
    def testFlowOf(): Unit \ Assert =
        let listing = Util.Json.Json.JObject(Map#{"videos" => Util.Json.Json.JArray(Vector#{
            Util.Json.Json.JObject(Map#{"name" => Util.Json.Json.JString("a"), "flow" => Util.Json.Json.JString("e2e/a.flow")}),
            Util.Json.Json.JObject(Map#{"name" => Util.Json.Json.JString("b")})})});
        Assert.assertEq(expected = ("e2e/a.flow", "", ""),
            (Videos.flowOf("a", listing), Videos.flowOf("b", listing), Videos.flowOf("c", listing)))

    /// 道具を試す順: Studio の engine を先に、置き場の engine は新しい版から（同じ物は 2 回試さない）。
    @Test
    def testToolCandidatesNewestFirst(): Unit \ Assert =
        Assert.assertEq(expected = List#{"/e/0.45.0", "/e/0.49.0", "/e/0.45.10", "/e/0.45.9"},
            Videos.toolCandidates("/e/0.45.0", List#{"/e/0.45.9", "/e/0.49.0", "/e/0.45.0", "/e/0.45.10"}))
}
