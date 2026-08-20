// Changes — 検査と基準の自動追随。ユーザーに儀式は求めない:
// 変わったら知らせる・見たければ前と今を見比べる・戻すのはアセットのバージョン(アーカイブ)。
//
// pump が 1 目盛り進める (journey/state と GET /changes が呼ぶ):
//   1. アセットが絵より新しい (stale) → 走行権が空いていれば焼きを裏で起こす。
//      このプロジェクトのゲームが走っていて命令口 (DEBUG_HTTP_PORT) を覚えていれば
//      走行中のゲームに頼み (速い)、それ以外は従来の make bake
//   2. 焼きたてでリファレンス画像と違う絵がある → 旧いリファレンス画像を reference/archive/ の vN へ退避し、
//      新しい絵を基準にする (自動追随)。変わった場面を記録し、未読の知らせにする
//   3. 初めての場面 (基準が無い) は黙って基準になる (知らせない)
//
// 記録はサーバの生存中だけ (メモリ)。事故の警報が目的で、レジストリではない —
// 「触っていないのに知らせが来た」を成立させるのが仕事。
mod Changes {
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.lang.System
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool, JNumber}

    /// dir ごとの記録 (キー: "<dir>|list" = "name,ver;name,ver…"、"<dir>|seen"、
    /// "<dir>|stamp" = 最後に成功した描き出しを起こす直前のアセットの新しさ、
    /// "<dir>|failedStamp" = 失敗したときの同じ値 (これを超えるまで起こし直さない)、
    /// "<dir>|pending" = 起こした描き出しの通し番号、"<dir>|pendingStamp" = その控え、
    /// "<dir>|renderAt" = 最後に描き出しを起こした時刻)。
    /// Runner と同じ理由で Java の Map を借りる (リクエストをまたいで生きる可変の器)。
    pub type alias State = JMap[String, String]

    pub def make(): State \ IO = new JMap()

    /// 1 目盛り進めて、いまの様子を返す。冪等 — 何度呼んでも安全。
    pub def pump(state: State, runner: Runner.State, game: Game.Runtime, dir: String): Json \ IO =
        observeBake(state, runner, dir);
        let items = Gallery.diffItemsOf(dir);
        let diffs = items |> List.filter(item -> snd(item) == "diff") |> List.map(fst);
        let firsts = items |> List.filter(item -> snd(item) == "missingReference") |> List.map(fst);
        let running = Runner.runningTarget(runner) == Some("render");
        let stale = NextStep.staleGalleryOf(dir, stampOf(state, dir));
        let baking =
            if (stale and not running and not isBlocked(state, dir)) {
                // 起こす直前に測る。描き出している最中に入った保存はこの値より新しいので、
                // 次の目盛りでそのまま拾える。
                let candidate = NextStep.assetsMaxMtime(dir);
                let startedAt = System.currentTimeMillis();
                let (status, _) = launchBake(runner, game, dir);
                if (status == 202) {
                    discard state.put("${dir}|renderAt", "${startedAt}");
                    match (Runner.currentRunId(runner), candidate) {
                        case (Some(runId), Some(mtime)) =>
                            discard state.put("${dir}|pending", "${runId}");
                            discard state.put("${dir}|pendingStamp", "${mtime}")
                        case _ => ()
                    }
                } else ();
                status == 202
            } else
                running;
        // 描き出した直後の時だけ追随する (stale の間の差分は古い絵との比較なので信用しない)
        if (not stale and not baking and (not List.isEmpty(diffs) or not List.isEmpty(firsts))) {
            discard new JFile("${dir}/reference").mkdirs();
            discard new JFile("${dir}/reference/archive").mkdirs();
            foreach (name <- ownNames(state, dir, firsts))
                copyFile("${dir}/gallery/${name}", "${dir}/reference/${name}");
            foreach (name <- diffs) {
                let ver = nextVer(dir, name);
                copyFile("${dir}/reference/${name}", "${dir}/reference/archive/${versionName(name, ver)}");
                copyFile("${dir}/gallery/${name}", "${dir}/reference/${name}");
                record(state, dir, name, ver)
            }
        } else ();
        statusJson(state, dir, baking)

    /// 焼きを裏で起こす。ゲームの命令口を覚えていれば走行中のゲームに頼む
    /// (生死の確認と POST は焼き係スレッドの中 — PUT /file の応答経路から呼ばれても
    /// ここでは待たない)。覚えていなければ従来の make bake。頼めなかったときの
    /// make bake への切り替えと、古い覚え書きの掃除は Runner.launchRemoteBake の中。
    def launchBake(runner: Runner.State, game: Game.Runtime, dir: String): (Int32, Json) \ IO =
        match Game.portOf(game#ports, dir) {
            case Some(port) =>
                Runner.launchRemoteBake(runner, dir, port,
                    alive = () -> Game.isRunning(game, dir),
                    forget = () -> Game.forgetPort(game#ports, dir),
                    busy = "ほかの作業が走っています")
            case None =>
                Runner.launchTarget(runner, dir, "render", busy = "ほかの作業が走っています")
        }

    // ── 描き出しの結果を後から観測する ─────────────────────

    /// 自分が起こした描き出しが終わっていれば、起こす直前に測っておいた値 (pendingStamp) を
    /// 成功なら stamp、失敗なら failedStamp へ移す。
    ///
    /// WhyNot: 通し番号が合わないときに結果を決めつけない。Runner が覚えているのは最後の
    /// 1 本だけなので、自分の描き出しを観測する前にアトリエのプレビュー等が始まると
    /// 永久にすれ違う。番号が自分より進んでいたら「結果は分からない」として控えだけ落とし、
    /// 次の目盛りで判定をやり直す。
    def observeBake(state: State, runner: Runner.State, dir: String): Unit \ IO =
        match (pendingOf(state, dir), Runner.lastRun(runner)) {
            case (Some(pending), Some(last)) =>
                if (last#runId == pending) {
                    let candidate = pendingStampOf(state, dir);
                    clearPending(state, dir);
                    if (last#exitCode == 0) {
                        discard state.remove("${dir}|failedStamp");
                        putStamp(state, "${dir}|stamp", candidate)
                    } else
                        // 失敗したまま起こし続けると、直らない限り繰り返しになる。
                        // アセットがこの値を追い越す (＝誰かが直した) まで自動では起こさない。
                        putStamp(state, "${dir}|failedStamp", candidate)
                } else if (last#runId > pending)
                    clearPending(state, dir)
                else ()
            case _ => ()
        }

    def clearPending(state: State, dir: String): Unit \ IO =
        discard state.remove("${dir}|pending");
        discard state.remove("${dir}|pendingStamp")

    def putStamp(state: State, key: String, value: Option[Int64]): Unit \ IO =
        match value {
            case Some(mtime) => discard state.put(key, "${mtime}")
            case None        => ()
        }

    def stampOf(state: State, dir: String): Option[Int64] \ IO =
        int64At(state, "${dir}|stamp")

    def pendingStampOf(state: State, dir: String): Option[Int64] \ IO =
        int64At(state, "${dir}|pendingStamp")

    def int64At(state: State, key: String): Option[Int64] \ IO =
        let value = state.get(key);
        if (Object.isNull(value)) None else Int64.fromString(value)

    def pendingOf(state: State, dir: String): Option[Int32] \ IO =
        let value = state.get("${dir}|pending");
        if (Object.isNull(value)) None else Int32.fromString(value)

    /// 前の描き出しが失敗した値のまま、アセットが動いていないか。
    def isBlocked(state: State, dir: String): Bool \ IO =
        match (int64At(state, "${dir}|failedStamp"), NextStep.assetsMaxMtime(dir)) {
            case (Some(failed), Some(now)) => now <= failed
            case (Some(_), None)           => true
            case _                         => false
        }

    /// 追随してよい絵の名前 — 最後に起こした描き出しが書いた物だけ。
    ///
    /// WhyNot: gallery/ の PNG を名前だけで基準にしない。gallery/ には別の道具が置いた
    /// 調べ物のシートが混ざり得て、それを基準に取り込むと、以後その道具が動くたびに
    /// 「見た目が変わった」と知らせることになる。
    def ownNames(state: State, dir: String, names: List[String]): List[String] \ IO =
        match int64At(state, "${dir}|renderAt") {
            case None     => Nil
            case Some(at) =>
                let written = Gallery.galleryNamesSince(dir, at);
                names |> List.filter(name -> List.memberOf(name, written))
        }

    /// POST /changes/seen — 知らせを既読にする。
    pub def markSeen(state: State, dir: String): (Int32, Json) \ IO =
        discard state.put("${dir}|seen", "true");
        (200, JObject(Map#{"ok" => JBool(true)}))

    /// 未読の知らせの数 (journey の提案に使う)。
    pub def unseenCount(state: State, dir: String): Int32 \ IO =
        if (seenOf(state, dir)) 0 else List.length(entriesOf(listOf(state, dir)))

    // ── 記録 (メモリ) ──────────────────────────────────────

    def record(state: State, dir: String, name: String, ver: Int32): Unit \ IO =
        discard state.put("${dir}|list", appendEntry(listOf(state, dir), name, ver));
        discard state.put("${dir}|seen", "false")

    def listOf(state: State, dir: String): String \ IO =
        let value = state.get("${dir}|list");
        if (Object.isNull(value)) "" else value

    def seenOf(state: State, dir: String): Bool \ IO =
        let value = state.get("${dir}|seen");
        seenFromRecord(if (Object.isNull(value)) None else Some(value))

    def statusJson(state: State, dir: String, baking: Bool): Json \ IO =
        let entries = entriesOf(listOf(state, dir));
        JObject(Map#{
            "baking" => JBool(baking),
            "seen"   => JBool(seenOf(state, dir)),
            "changes" => JArray(entries |> List.map(entry -> {
                let (name, ver) = entry;
                JObject(Map#{
                    "name"   => JString(name),
                    "ver"    => JNumber(JsonCodec.intToBd(ver)),
                    "before" => JString("reference/archive/${versionName(name, ver)}"),
                    "after"  => JString("reference/${name}")
                })
            }) |> List.toVector)
        })

    // ── 純粋なルール (テスト対象) ──────────────────────────

    /// 既読記録の値から既読かを判定。無記録 (JMap.get の null 由来の None) は
    /// 「未読の知らせ無し」= 既読に倒す — null をそのまま String 比較へ流すと
    /// NPE で応答ごと落ちるので、判定へ入る前にここで受け止める。
    pub def seenFromRecord(value: Option[String]): Bool =
        match value {
            case None    => true
            case Some(v) => v != "false"
        }

    /// この相対パスへの保存が検査 (pump) をその場で蹴る対象か —
    /// 見た目に効き得るアセット (assets/ の JSON) だけ。stale 判定が見るのも assets/ の
    /// JSON なので、それに合わせる。gallery/ や reference/ や debug/ への書き込みでは
    /// 蹴らない — 焼きの成果物の保存がまた焼きを呼ぶ、という堂々巡りをここで断つ。
    /// 対象を広めに拾っても pump は冪等 (stale と走行権で守られる) なので安全。
    pub def kicksPumpOnSave(relPath: String): Bool =
        String.startsWith(prefix = "assets/", relPath)
            and String.endsWith(suffix = ".json", relPath)

    /// リファレンス画像の退避先の名前: "title.png" と 3 → "title.v3.png"。
    pub def versionName(name: String, n: Int32): String =
        if (String.endsWith(suffix = ".png", name))
            "${String.dropRight(4, name)}.v${n}.png"
        else
            "${name}.v${n}"

    /// name が scene の退避バージョンなら、そのバージョン番号。
    pub def verIfVersionOf(name: String, scene: String): Option[Int32] =
        let base = if (String.endsWith(suffix = ".png", scene)) String.dropRight(4, scene) else scene;
        let prefixText = "${base}.v";
        if (String.startsWith(prefix = prefixText, name) and String.endsWith(suffix = ".png", name)) {
            let middle = String.dropRight(4, String.dropLeft(String.length(prefixText), name));
            if (middle != "" and String.forAll(Char.isDigit, middle))
                Int32.fromString(middle)
            else None
        } else None

    /// 次のバージョン番号 (この場面の退避の最大 + 1、無ければ 1)。
    pub def nextVerFrom(names: List[String], scene: String): Int32 =
        (names |> List.filterMap(name -> verIfVersionOf(name, scene))
               |> List.foldLeft(Int32.max, 0)) + 1

    /// 記録の読み書き ("name,ver;name,ver…")。名前は Gallery の門番 (英数._-) を
    /// 通った物だけなので、区切り文字と衝突しない。
    pub def entriesOf(text: String): List[(String, Int32)] =
        if (text == "") Nil
        else String.splitOn(substr = ";", text)
            |> List.filterMap(part -> match String.splitOn(substr = ",", part) {
                case name :: ver :: Nil =>
                    Int32.fromString(ver) |> Option.map(n -> (name, n))
                case _ => None
            })

    pub def appendEntry(text: String, name: String, ver: Int32): String =
        let kept = entriesOf(text) |> List.filter(entry -> fst(entry) != name);
        (kept ::: (name, ver) :: Nil)
            |> List.map(entry -> { let (n, v) = entry; "${n},${v}" })
            |> List.join(";")

    // ── ファイル ──────────────────────────────────────────

    def nextVer(dir: String, scene: String): Int32 \ IO =
        let entries = new JFile("${dir}/reference/archive").listFiles();
        let names = if (Object.isNull(entries)) Nil
            else Array.toList(entries)
                |> List.filter(file -> file.isFile())
                |> List.map(file -> file.getName());
        nextVerFrom(names, scene)

    /// バイトそのままのコピー (bench のバイト比較がそのまま ok になる形)。
    def copyFile(src: String, dst: String): Unit \ IO =
        let bytes = Files.readAllBytes(new JFile(src).toPath());
        let out = new FileOutputStream(dst);
        out.write(bytes);
        out.close()
}
