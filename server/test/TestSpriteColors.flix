// TestSpriteColors — ドット絵 legend の実色解決 (POST /sprite/colors) の純粋な芯を
// 固定する。ファイル走査・HTTP の封筒は薄い IO 殻なので、ここでは表づくりの
// ルール (解決の可否と勝ち順) だけを見る。
mod TestSpriteColors {
    use Util.Json.Json

    /// 文字列をトップレベルの Map[String, Json] に解く (テスト入力用)。
    def topOf(text: String): Map[String, Json] =
        match (DocJson.parse(text) |> Option.flatMap(JsonCodec.expectObject)) {
            case Some(m) => m
            case None    => bug!("test json does not parse: ${text}")
        }

    /// legend の値の拾い方: 文字列だけ・重複は 1 つ。legend 欠けは空 (fail-open)。
    @Test
    def testLegendValuesOf(): Unit \ Assert =
        let doc = DocJson.parse("{\"legend\": {\"a\": \"accent\", \"b\": \"accent\", \"x\": \"#102030\", \"n\": 3}}")
            |> Option.getWithDefault(Json.JNull);
        let noLegend = DocJson.parse("{\"sprites\": {}}") |> Option.getWithDefault(Json.JNull);
        Assert.assertEq(
            expected = (List.sort("accent" :: "#102030" :: Nil), (Nil: List[String])),
            (SpriteColors.legendValuesOf(doc) |> List.sort, SpriteColors.legendValuesOf(noLegend)))

    /// 解決の 3 形: 色表記はそのまま (小文字へ正規化)、意味色キーはテーマ引き
    /// ("@名前" も裸の "名前" も同じ)、どこにも無いキーは表に載らない。
    /// テーマの色でないフィールド (note・数値) は色票にならない。
    @Test
    def testTableOfResolvesForms(): Unit \ Assert =
        let theme = topOf("{\"note\": \"words\", \"rainCount\": 7, \"accent\": \"#FFC95E\", \"ink\": {\"rgb\": [1, 0, 0]}}");
        Assert.assertEq(
            expected = Map#{"#A0B1C2" => "#a0b1c2", "accent" => "#ffc95e", "@ink" => "#ff0000"},
            SpriteColors.tableOf(theme :: Nil, Map.empty(),
                "#A0B1C2" :: "accent" :: "@ink" :: "accentLight" :: "note" :: Nil))

    /// テーマは色を "colors": {...} の入れ子に持つ形もある (村の prologue.theme.json 等)。
    /// トップレベルのフラット色 (engine テンプレ) と入れ子の色 (村) の両方が legend の解決に使える。
    @Test
    def testTableOfNestedColors(): Unit \ Assert =
        let theme = topOf("{\"note\": \"words\", \"colors\": {\"skin\": \"#EEC39A\", \"body\": \"#5B6EE1\", \"hair\": \"#2E241E\"}}");
        Assert.assertEq(
            expected = Map#{"skin" => "#eec39a", "body" => "#5b6ee1", "hair" => "#2e241e"},
            SpriteColors.tableOf(theme :: Nil, Map.empty(),
                "skin" :: "body" :: "hair" :: Nil))

    /// catalog のテーマ (色を {"hex": "#rrggbb"} の包みで持つ形・fe_rogue や谷の色票) も
    /// legend の解決に使える。色でない catalog (文言の {"text": ...}) は色票にならない。
    @Test
    def testTableOfCatalogTheme(): Unit \ Assert =
        let theme = topOf("{\"colors\": {\"plain\": {\"hex\": \"#6ABE30\"}, \"wall\": {\"hex\": \"#595652\"}}, " +
            "\"labels\": {\"attack\": {\"text\": \"こうげき\"}}}");
        Assert.assertEq(
            expected = Map#{"plain" => "#6abe30", "wall" => "#595652"},
            SpriteColors.tableOf(theme :: Nil, Map.empty(),
                "plain" :: "wall" :: "attack" :: Nil))

    /// 解決できなかった legend の値は名前のまま返す (画面が印を出せるように)。
    /// 色表記そのものは表に載るので、ここには残らない。
    @Test
    def testUnresolvedOf(): Unit \ Assert =
        let legendValues = "plain" :: "#a0b1c2" :: "body" :: "trim" :: Nil;
        let colors = SpriteColors.tableOf(topOf("{\"plain\": \"#6ABE30\"}") :: Nil, Map.empty(), legendValues);
        Assert.assertEq(
            expected = "body" :: "trim" :: Nil,
            SpriteColors.unresolvedOf(colors, legendValues))

    /// 勝ち順: 先のテーマ < 後のテーマ (同名テーマを最後に回す並び) < doc 自身の palette。
    @Test
    def testTableOfPrecedence(): Unit \ Assert =
        let early = topOf("{\"accent\": \"#111111\", \"wall\": \"#222222\", \"bg\": \"#333333\"}");
        let late  = topOf("{\"accent\": \"#444444\", \"wall\": \"#555555\"}");
        Assert.assertEq(
            expected = Map#{"accent" => "#999999", "wall" => "#555555", "bg" => "#333333"},
            SpriteColors.tableOf(early :: late :: Nil, Map#{"accent" => "#999999"},
                "accent" :: "wall" :: "bg" :: Nil))
}
