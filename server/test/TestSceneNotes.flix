// TestSceneNotes — 場面の説明 Doc (assets/*.scenes.json) とラベルの橋渡しを固定する。
// 期待値は SceneNotes.defaultNote() から導く (文言リテラルを貼らない)。
// ディスク読み (load) は curl での通し確認に任せ、ここは中身の解釈だけを見る。

mod TestSceneNotes {

    /// 書き損じた JSON はラベルゼロへ落ちる (fail-open)。
    @Test
    def testBrokenJsonFallsBackToEmpty(): Unit \ Assert =
        Assert.assertEq(
            expected = 0,
            Map.size(SceneNotes.fromText("{ rows: [")))

    /// name で引くと、その行の題が出る。
    @Test
    def testTitleByName(): Unit \ Assert =
        let doc = "{\"version\":1,\"rows\":[{\"name\":\"s3_clear\",\"title\":\"ライン消し\"}]}";
        Assert.assertEq(
            expected = Some("ライン消し"),
            SceneNotes.fromText(doc) |> Map.get("s3_clear") |> Option.map(note -> note#title))

    /// rows の長さに追随する (name を持たない行はラベルにならない)。
    @Test
    def testFollowsRowsLength(): Unit \ Assert =
        let doc = "{\"version\":1,\"rows\":[{\"name\":\"a\"},{\"name\":\"b\"},{\"title\":\"名無し\"}]}";
        Assert.assertEq(
            expected = 2,
            Map.size(SceneNotes.fromText(doc)))
}
