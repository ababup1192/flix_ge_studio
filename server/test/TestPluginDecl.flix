// TestPluginDecl — project.json の editor.plugins 宣言の読み取りと、
// 配信パスの門番 (Plugins.uiRelPath) を固定する。どちらもファイル IO の無い純粋層。
// レコードは assertEq で直接比べられないので、フィールドを並べたタプルに射影してから比べる
// (TestResourceDecl と同じ流儀)。

mod TestPluginDecl {
    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString}

    /// Decl をタプル (id, title, performUrl, performCmd) に射影する。
    def declView(decl: PluginDecl.Decl): (String, Option[String], Option[String], Option[String]) =
        (decl#id, decl#title, decl#performUrl, decl#performCmd)

    /// editor.plugins をフル指定 + 最小指定 (id だけ) の 2 件でデコードする。
    @Test
    def testFromProjectJsonFull(): Unit \ Assert =
        let projectJson = JObject(Map#{
            "editor" => JObject(Map#{
                "plugins" => JArray(Vector#{
                    JObject(Map#{
                        "id"         => JString("battle-sim"),
                        "title"      => JString("戦闘シミュレータ"),
                        "performUrl" => JString("http://127.0.0.1:8790/sim"),
                        "performCmd" => JString("make sim-server")
                    }),
                    JObject(Map#{"id" => JString("deck-lab")})
                })
            })
        });
        Assert.assertEq(
            expected = Ok(
                   ("battle-sim", Some("戦闘シミュレータ"),
                    Some("http://127.0.0.1:8790/sim"), Some("make sim-server"))
                :: ("deck-lab", (None: Option[String]), (None: Option[String]), (None: Option[String]))
                :: Nil),
            PluginDecl.fromProjectJson(projectJson) |> Result.map(List.map(declView)))

    /// "plugins" が無い project.json は Err ではなく Ok(空) — 宣言の無いゲームも従来どおり。
    @Test
    def testFromProjectJsonNoPlugins(): Unit \ Assert =
        Assert.assertEq(
            expected = Ok(Nil) |> Result.map(List.map(declView)),
            PluginDecl.fromProjectJson(JObject(Map#{"title" => JString("shooter")}))
                |> Result.map(List.map(declView)))

    /// 危ない id (パス区切り入り) は、どの項目かが分かる日本語 1 行の Err。
    @Test
    def testFromProjectJsonUnsafeId(): Unit \ Assert =
        let projectJson = JObject(Map#{
            "editor" => JObject(Map#{
                "plugins" => JArray(Vector#{
                    JObject(Map#{"id" => JString("../evil")})
                })
            })
        });
        Assert.assertEq(
            expected = Err("editor.plugins[0] の id『../evil』は使えません(英数字と `-` `_` だけ・空は不可)"),
            PluginDecl.fromProjectJson(projectJson) |> Result.map(List.map(declView)))

    /// 配信パスの門番: 通る 2 形 (末尾スラッシュ / ふつうのファイル) と
    /// 弾く 3 形 (末尾スラッシュ無し = 301 の担当 / `..` 入り / id 無し)。
    @Test
    def testUiRelPath(): Unit \ Assert =
        Assert.assertEq(
            expected = ((None: Option[String]),
                        Some("studio-plugins/battle-sim/index.html"),
                        Some("studio-plugins/battle-sim/js/app.js"),
                        (None: Option[String]),
                        (None: Option[String])),
            (Plugins.uiRelPath("/plugin-ui/battle-sim"),
             Plugins.uiRelPath("/plugin-ui/battle-sim/"),
             Plugins.uiRelPath("/plugin-ui/battle-sim/js/app.js"),
             Plugins.uiRelPath("/plugin-ui/battle-sim/../secret.json"),
             Plugins.uiRelPath("/plugin-ui/")))

    /// 末尾スラッシュ無しの "/plugin-ui/<id>" だけが 301 の対象。
    @Test
    def testIsRootWithoutSlash(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (Plugins.isRootWithoutSlash("/plugin-ui/battle-sim"),
             Plugins.isRootWithoutSlash("/plugin-ui/battle-sim/"),
             Plugins.isRootWithoutSlash("/plugin-ui/")))

    /// id の許可リスト: 英数字と `-` `_` だけ。URL を壊す文字 (`#` `?` `%` 空白) は弾く。
    @Test
    def testIsSafeId(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false, false),
            (PluginDecl.isSafeId("battle-sim_2"),
             PluginDecl.isSafeId(""),
             PluginDecl.isSafeId("a#b"),
             PluginDecl.isSafeId("a b"),
             PluginDecl.isSafeId("a?b=1")))
}
