# NOTES

## 次やること（2026-08-12 夜・ラフのバージョンは実装完了／未コミット）

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
