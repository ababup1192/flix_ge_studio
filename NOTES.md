# NOTES

## 次やること（2026-08-12 深夜・ラフ比較の窓を作る）

人と決めたこと: **重ねる相手は人が選ぶ**（生成された絵に由来が書かれていないので、
自動の紐づけは後回し。engine 側の相談はしない）／**第一弾は重ねて見るだけ**
（マスを指してひとこと書く・一覧に並べるのは次の段）。

### 調べ済み（次の人が調べ直さなくていい）

- **サーバの口は全部そろっている。新設は要らない**
  - ラフの一覧 = `GET /sketch/list` → `{sketches:[{name, versions:[3,2,1]}]}`
  - ラフの中身 = `GET /file?path=draft/sketch/<名前>/v<番号>.json`（汎用の口で読める。
    `FileIndex.readInside` はプロジェクト内なら通る）
  - 生成された絵の一覧 = `GET /gallery/list`（gallery/ golden/ debug/ の PNG）、
    画像 = `GET /gallery/image?dir=gallery&name=…`
- **ラフを読み戻す純関数は既にある** — `SketchPad.decode : String -> Maybe Model`（encode と対）
- **重ねの流儀は `GoldenView.elm` にある** — `Mode = SideBySide | Overlay | Diff`、
  透過は `opacity : Float` を `HA.style "opacity"` で上の img に掛けるだけ（GoldenView.elm:360-369）。
  `view` が URL を作る関数を引数で受け、口の組み方を呼び側に残す形も踏襲する
- **ラフの絵はマスの div の並び** — `SketchPad.viewGrid` / `viewCell`（2011-2074 行）。
  色は `legend` の `fill`、空きマスは `emptyChar = '.'` で `bg-black/20`。
  ただし viewCell は `Msg`（塗る操作）を持つので**そのままは使えない**

### 作る物

1. **web/src/SketchCompare.elm**（新設）— GoldenView と同じ「窓」の形
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
   一覧が変わったときの倒れ方。GoldenView.withStatus と同じ考え方）

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

設計の正本（第 3 版・人と合意済み）:
https://claude.ai/code/artifact/c592edd2-0190-4e1b-b29a-a61498942db5

## 調べて分かっている前提（次の人が調べ直さなくていい）

- SketchPad.elm(1962 行) は **書きっぱなし** — ラフを読み戻す口が今は無い。だから版番号は
  サーバの一覧から取るしかない（1 を先に作る理由）。保存は汎用の putFile(OutSave)。
- `FileIndex.writeInside` は **draft/ 配下だけ親ディレクトリを作る**ので、
  `draft/sketch/<name>/v3.json` は新しい口を足さずに書ける（FileIndex.flix:112 allowsParentCreation）。
- 一覧に並べるのは既にある「違和感チケット」の列（Tickets.elm + Annotations.flix）。
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
- 施策2 は「違和感チケットの窓」に絞って実装済み(2026-08-12)。反映は swap-jar と swap-web
