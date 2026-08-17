# NOTES

## 次やること（2026-08-17・engine 0.28.1 追随）

0.28.0 追随と 0.28.1 追随が別々の枝で走ってぶつかった。**0.28.1 が 0.28.0 を含むので
0.28.1 側を採ってマージ**（`server/lib/.../0.28.0/` は 0.27.0 と一緒に落とす）。

1. **`make swap-engine` をやり直す** — `/Applications` の同梱 engine は **0.28.0 のまま**。
   手元の `local.mk` がそこを指しているので、入れ直さないとゲームが古い engine で動く
2. **人の目で確かめる**（Studio を Cmd+Q → 開き直し）。新規ゲーム作成で 0.28.1 の
   fpkg が種から解けるかを見る
3. engine リポ側は 0.28.1 を公開済み（`ShaderGen` の文字列の組み立てを 1 回にした回。
   `pub` のシグネチャも出力バイトも変わっていないので、Studio 側のコードは直していない）

## 次やること（2026-08-16・ギャラリー画面。未コミット）

1. **人の目で確かめる**（`/Applications` へ swap-jar / swap-web / swap-engine 済み。
   Studio を Cmd+Q → 開き直すだけ）。上部ナビの「ギャラリー」タブ、ホームの
   「全場面を見る」からも入れる。ラベルの無いゲームで名前だけ並ぶ事も見る
2. **コミット**（server / web / engine templates の 3 箇所。この環境は git commit が
   拒否されるので人の手が要る）。engine リポ側 = `templates/*/assets/*.scenes.json` +
   `scenes.schema.json` + `project.json` + `SceneRender.flix` のコメント
3. 残り = ギャラリーの並べ替え・絞り込み（今は名前順のみ）と、`debug/` と音を
   見せるかの判断（今回は `gallery/` だけに絞った）

## 済んだ物（2026-08-16・ギャラリー画面と場面の説明 Doc）

- **場面の説明 Doc を新設**: `assets/*.scenes.json`（`version` + `rows[{name,title,desc,tags}]`）。
  描き出す場面を決めるのは今までどおり `SceneRender.shotNames()` で、この Doc はラベルを添えるだけ。
  engine の 6 template 全部に実データつきで配り、`project.json` の `editor.resources` にも宣言
- **server**: `SceneNotes.flix`（Doc を読むだけ・fail-open）+ `Gallery.scenesJson` +
  `GET /gallery/scenes`（絵 + ラベル + リファレンス差分を 1 応答で）。`/gallery/list` は
  ミニプレイヤーとラフ比較が食べているので形を変えていない
- **web**: `GalleryView.elm` を新設し、上部ナビに「ギャラリー」タブ。旧「全場面」モーダル
  （`viewScenesModal` / `type Scenes` / `ScenesOpened`）は撤去し、ホームの入口はタブへ送るだけ
- 確認: server 154 tests / elm-test 688 / vitest 19 / parity 一致。
  rpg-starter を実際に開いて `/gallery/scenes` を curl し、ラベルあり・ラベルなしの両方を見た

## 次やること（2026-08-13 昼・言葉づかいの直し。未コミット）

1. **`NewGame.elm` の `family` → `genre` が未着手**（74 箇所）。server とサーバの口は
   `genre` へ改名済みで、`web/src/js/realApi.ts` が**新旧どちらの名前でも動くように
   両受け**にしてある（`payload.genre ?? payload.family`・port のタグも
   `case "genesisGenres": case "genesisFamilies":`）。**Elm を付け替えたら、
   その両受けと `?? payload.family` を消す**
   - UI に出ている「家族」も「ジャンル」へ（人からの「家族のカード -> 家族？」の指摘）
2. **engine 側の `golden/` → `reference/` 改名の追随は完了**（ディスク上のパスに続いて
   識別子と HTTP の口も改名済み。下の「済んだ物」を見る）。
   残るのは `docs/glossary.md` の見比べのパス（engine 側は `reference/archive/<scene>.vN.png`）
3. **反映は未実施**。server も web も変えたので `make swap-jar` と `make swap-web` の両方が要る

## 済んだ物（2026-08-13 昼・言葉づかい）

- 独自語を業界の言葉へ（ボタン / カード / ラベル / バージョン）。
  engine 側では `AGENTS.md` の言葉づかいの節そのものを書き換えた（単語は業界の言葉・
  説明は平易に、の 2 段構え）+ 名前の付け方の節を新設 + `bin/lint-jargon.py` で
  コミット時に止める仕組みを入れた
- `family` → `genre`（server・realApi.ts・テスト・docs）
- `golden` → `reference` の識別子と HTTP の口（約 210 箇所）。
  `/reference/status` `/reference/update`・`server/src/Reference.flix`(`mod Reference`)・
  `web/src/ReferenceView.elm`・`Api.ReferenceItem` / `referenceMtime` / `referenceStatus`・
  `reference-*` クラス・`.studio/reference-diff.json`・server 内部の `missingReference`。
  UI 文言は「リファレンス画像を更新」に統一。
  **触っていない物**: `PixelEditor.goldenHue`（黄金角）と `TestRunner` の否定例の文字列 `"golden"`
- `bless` → `update`。`POST /reference/update`・`Reference.update`・
  Elm の `ReferenceUpdated` / `referenceUpdate` / `onUpdate`・CSS の `reference-update`。
  日本語も「更新」へ

## （旧）ラフ 3 点は実装済み（2026-08-13 朝）

1. **人の目で確かめる**（`/Applications` へ反映済み。Studio を Cmd+Q → 開き直すだけ）。
   見る所は下の「済んだ物」の 3 節それぞれの末尾に書いた手順
2. **コミット**（この環境は git add / commit が拒否されるので人の手が要る）。
   server も web も変えたので、コミット後の再反映は要らない（もう入っている）
3. 残り = ラフのフレーム（1 枚のラフに複数コマ）と、engine 側の由来の覚え書き（要相談）。
   **再現度(fidelity)のつまみの処遇**も未決のまま（消さずに残して相談）

## 済んだ物（2026-08-13 未明・レイヤーとラフ比較の第 2 段）

### ラフに奥・主役・手前の 3 層（web/src/SketchPad.elm だけ）

- 道具列の下に層のボタン。選んだ層だけ塗り、**他の層は薄く透けて見える**
- 保存は `rows` = 3 層を畳んだ 1 枚（今までと同じ形）+ `layers` は主役以外にも
  塗ったときだけ足す。**層を知らない読み手が今までどおり絵を出せる**のが狙い
- 依頼文も同じ考え方（主役だけなら出力は従来と 1 バイトも変わらない）
- 層の無い古いラフは「主役 1 枚」として読む。テストで固定済み
- 畳んだ絵が要る所は `SketchPad.flatRows`（比較の窓はこれを使う）

### ラフ比較の第 2 段（マスを指してひとこと → やること一覧へ）

- `POST /annotations/create` を新設（server/src/Annotations.flix + Editor.flix にルート 1 行）。
  `debug/annotations/<日時>_<絵>_sketch/` に README.md + screenshot.png を置く。
  **engine 側は触っていない**
- 印は**置き場の名前の末尾 `_sketch`**（engine のチケットは `_f<フレーム>` で終わるので衝突しない）。
  判定は純関数 `isSketchId` 1 本で、サーバは中身を読まない
- README の文面は Studio が組み、サーバは日時を足して置くだけ
- 一覧（Tickets.elm）の各行に「ラフ比較」/「遊んで」の印。依頼文は印で手がかりを出し分ける

### 確かめ方（Studio を開き直してから）

1. 右上「ラフと見比べ」→ 絵とラフを 1 枚ずつ選ぶ → 「重ねる」で透過を動かす
2. ラフのマスを 1 つ押す（赤枠 + 「左から N・上から M マス目」）→ ひとことを書いて
   「やること一覧へ並べる」
3. ホームの「🎫 注釈チケット」に「ラフ比較」の印つきで増える
4. アトリエのラフ描きで層ボタンを切り替え、奥に塗って主役へ戻すと薄く透けること

## （旧）ラフ比較の窓 = 第 1 段（2026-08-12 深夜）

人と決めたこと: **重ねる相手は人が選ぶ**（生成された絵に由来が書かれていないので、
自動の紐づけは後回し。engine 側の相談はしない）／**第一弾は重ねて見るだけ**
（マスを指してひとこと書く・一覧に並べるのは次の段）。

### 調べ済み（次の人が調べ直さなくていい）

- **サーバの口は全部そろっている。新設は要らない**
  - ラフの一覧 = `GET /sketch/list` → `{sketches:[{name, versions:[3,2,1]}]}`
  - ラフの中身 = `GET /file?path=draft/sketch/<名前>/v<番号>.json`（汎用の口で読める。
    `FileIndex.readInside` はプロジェクト内なら通る）
  - 生成された絵の一覧 = `GET /gallery/list`（gallery/ reference/ debug/ の PNG）、
    画像 = `GET /gallery/image?dir=gallery&name=…`
- **ラフを読み戻す純関数は既にある** — `SketchPad.decode : String -> Maybe Model`（encode と対）
- **重ねの流儀は `ReferenceView.elm` にある** — `Mode = SideBySide | Overlay | Diff`、
  透過は `opacity : Float` を `HA.style "opacity"` で上の img に掛けるだけ（ReferenceView.elm:360-369）。
  `view` が URL を作る関数を引数で受け、口の組み方を呼び側に残す形も踏襲する
- **ラフの絵はマスの div の並び** — `SketchPad.viewGrid` / `viewCell`（2011-2074 行）。
  色は `legend` の `fill`、空きマスは `emptyChar = '.'` で `bg-black/20`。
  ただし viewCell は `Msg`（塗る操作）を持つので**そのままは使えない**

### 作る物

1. **web/src/SketchCompare.elm**（新設）— ReferenceView と同じ「窓」の形
   （`Model` / `Handlers msg` / `init` / `open` / `close` / `view`）。
   - 選ぶ物は 2 つ: 生成された絵（gallery の名前）と ラフ（名前 + バージョン）
   - 見比べ方は 2 つ: 並べる・重ねる（透過スライダ）。「違いを塗る」は入れない
     （ラフはマスの塗り絵で、画素を比べても意味が無い）
   - ラフを描くのは読み取り専用の小さな関数を**この中に持つ**
     （`viewCell` は Msg 付きなので流用せず、`Model.rows` + `legend` から div を並べる。
     重ねるので大きさは絵に合わせて伸縮させる = `width:100%` + `aspect-ratio`）
2. **web/src/Main.elm** — 窓の開け閉めと、選ばれたラフの `GET /file` 取得を配線
   （`sketchList` と同じ足回りが 957-959 行あたりにある）
3. **テスト** — `web/tests/SketchCompareTest.elm`（純関数だけ: 選び直しの残り方・
   一覧が変わったときの倒れ方。ReferenceView.withStatus と同じ考え方）

反映は web だけなので `make swap-web`。Studio は Cmd+Q → 開き直し。

## 済んだ物（2026-08-12 夜・ラフのバージョン。コミット済み 9e71edc）

1. **人の目で 1 往復**: Studio を開き直して（swap-jar / swap-web は済み）アトリエでラフを保存 →
   `draft/sketch/<名前>/v1.json` → 描き足して保存 → v2 が増えることと、依頼文の「原本: 」行が
   保存したバージョンを指すことを確かめる。
2. **コミット**（この環境は git add / commit が拒否されるので人の手が要る）。
3. **engine 側 1 点は未着手**（生成された絵の隣に「どのラフのどのバージョンから・いつ・置き場」の
   覚え書きを吐く）。flix_game_engine 側の変更なので着手前に相談する。

### 済んだ物（2026-08-12 夜・ラフのバージョン）

- **server/src/Sketches.flix**（新設）+ Editor.flix にルート 1 行 —
  `GET /sketch/list` → `{ok, sketches:[{name, versions:[3,2,1]}]}`。純関数は
  `versionOf` / `nextVersion` / `isSafeName`。テスト = server/test/TestSketches.flix。
  実物のサーバでも応答を確認済み（vN.json 以外は数えない・置き場が無ければ空の配列）
- **SketchPad.elm** — 保存先が `draft/sketch/<名前>/v<番号>.json`。番号は一覧の最大 +1。
  `SaveState` が「今どこへ書いたか」を運び、**絵と名前を触るまで同じ場所を指し続ける**
  （「原本: 」行が実在するファイルを指すため）。一覧は名前ごとに丸ごと持つ（名前を
  打ち替えて別のラフを上書きしないため）
- **Main.elm / realApi.ts** — `sketchList` を annotationsList と同じ足回りで配線
  （起動時・プロジェクト切替・ホームを開いた足・ラフ保存の直後に取り直し）。口を持たない
  古いサーバは fail-open。**NewGame には流さない**（まだ無いプロジェクトなので必ず v1 から）
- server 144/0・elm 614/0・vitest 19/0。docs/plan-sketch-roundtrip.md に節を追記

設計の元データ（第 3 版・人と合意済み）:
https://claude.ai/code/artifact/c592edd2-0190-4e1b-b29a-a61498942db5

## 調べて分かっている前提（次の人が調べ直さなくていい）

- SketchPad.elm(1962 行) は **書きっぱなし** — ラフを読み戻す口が今は無い。だから版番号は
  サーバの一覧から取るしかない（1 を先に作る理由）。保存は汎用の putFile(OutSave)。
- `FileIndex.writeInside` は **draft/ 配下だけ親ディレクトリを作る**ので、
  `draft/sketch/<name>/v3.json` は新しい口を足さずに書ける（FileIndex.flix:112 allowsParentCreation）。
- 一覧に並べるのは既にある「注釈チケット」の列（Tickets.elm + Annotations.flix）。
  **印だけ分けて混ぜる**のが第 3 版の決め事で、ラフ専用の一覧は作らない。
- docs/plan-sketch-roundtrip.md は「グリッド往復は見送り」と書いてあるが、
  **第 3 版はその見送りを覆した物ではない** — 機械の差分判定は入れないまま、
  版と由来の配管だけを足す。あの文書は書き足しが要る。
- 反映は server を触ったら `make swap-jar`、web を触ったら `make swap-web`。

## 人に相談してから動かす所

- **engine 側 1 点**（生成された絵の隣に「どのラフのどの版から・いつ・置き場」の覚え書きを吐く）は
  flix_game_engine 側の変更。着手前に相談する（AGENTS.md の決まり）。
- **再現度(fidelity)のつまみ**は第 3 版のメモでは「消えた」ことになっているが、
  あれは機械判定をやめた事とは別で、AI への注文の強さを決める実用がある。**消さずに残して相談**。

## 済んだ物

- 施策1「作る」ボタン一気通貫: docs/plan-one-click-genesis.md(未着手)
- 施策2 は「注釈チケットの窓」に絞って実装済み(2026-08-12)。反映は swap-jar と swap-web
