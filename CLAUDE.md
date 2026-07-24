## 会話ポリシー

日本語で会話してください。途中報告なども含めて、日本語で回答してください。
コメント・会話では、一般で広く使われるプログラミング、一般教養的な用語以外は、なるべく平易な中高生でも伝わるような言葉で書く。

## 設計・実装

複雑で大規模な変更の場合は、いきなり実装をせず、レビュー役を立てて、壁打ちして80~90点以上を目指してください。
実装はするだけで満足せず、レビュー役に仕様漏れ・リファクタリング余地が無いかを確認してもらいましょう。

## コーディングポリシー

コードには、How
テストコードには What
コミットログには Why
コードコメントには WhyNot

特にコードコメントは、WhyNotを重視し、How, Whatを書かないように。また、実装の由来や旧実装などの歴史背景は、記述しなくて良い。

## この repo は何か（flix_ge_studio）

ゲーム制作用エディタ「Flix GE Studio」を 1 リポにまとめた物。3 つの部品を組み合わせて
1 つの `.app` にする。3 言語が同居する。

- **server/** … Flix 製の常駐 HTTP バックエンド（`editor_server`）。対象ゲームの
  `project.json` / `*.kind.json` を読み書きし、プレビュー描画を返す。engine 本体は
  `github:ababup1192/flix_engine_core|world|tools` の 0.7.1 を fpkg 依存として引く
  （engine のソースは持たない）。GL ウィンドウは開かない（headless）。
- **web/** … Elm + Vite 製のフロント（`resource_editor`）。ブラウザ画面。スキーマがあれば
  フォームやスライダーを自動生成し、保存すると即・対象ゲームに反映される。
- **app/** … Rust + Tauri 製のランチャー。空きポートを選び、同梱 JRE で server の jar を
  起動し、`/health` を待って WebView を server の URL に向けるだけ。

### ビルド（Makefile で統合）

`jar → dist → jre → .app` を 1 本のMakefileにまとめてあるRoot で叩く。

- `make jar` … server を fatjar 化 → `server/artifact/server.jar`
- `make web` … web を Vite build → `web/dist`
- `make jre` … jlink で最小 JRE → `app/runtime/jre`（.app に同梱する自己完結 JRE）
- `make app` … jar+web+jre を `app/src-tauri/resources/` に置いてから Tauri で `.app` を作る
- `make dev` … server を `web/dist` 配信で開発起動（.app 無しの動作確認用）

Flix コンパイラは engine リポの `bin/flix` ラッパ経由で呼ぶ（`ENGINE=` で場所を変えられる）。
engine の 0.7.1 fpkg は `server/lib/` に同梱済み（オフラインでビルドできる）。

### 動いている .app に server 変更を反映する（重要・ハマりどころ）

`.app` は `editor_server.jar` を **自分の中に同梱**して起動する。だから server を直しても
`make jar` で `server/artifact/server.jar` を焼き直すだけでは **動いている .app には効かない**
（古い同梱 jar のまま）。

- **server だけ変えた**: `make swap-jar` … jar を焼き直し、インストール済みの全 `.app`
  （`/Applications` とビルド先の両方）の同梱 jar を差し替え + 再署名する。
- **web / UI も変えた**: `make app` … jar+web+jre を束ね直して `.app` を作る。

どちらの後も Studio を **Cmd+Q で完全終了**してから開き直す（ウィンドウを閉じるだけだと中の
java サーバが残り、古いままに見える）。

server が裏で叩く engine は `EDITOR_ENGINE`（既定 `$HOME/Desktop/flix_game_engine`）。
`make new-game` / bake はそこの Makefile を使う（Studio のビルド依存 fpkg とは別物）。

### ジャンルとテンプレ（Genesis）

「新しいゲーム」のジャンル 9 枚は `server/src/Genesis.flix` の families。`starter` が非空の
ジャンルは複製で始まり（`templates/<name>` を engine の `make new-game`）、札のサムネは
その `golden/title.png`（無ければ golden の最初の絵に倒れる安全網あり）。テンプレを足す
全手順は engine の CLAUDE.md「テンプレートを足す・更新する」に集約。足したら Studio 側で
`starter` を差して **`make swap-jar`**。

### テスト

- **web**: `elm-test`（Elm ロジック）と `vitest`（TS）。
- **server**: `flix test`。

いずれも**変更した部品だけ回す**。3 部品まとめて回すのはリリース直前でよい。

### Rust（app）は薄く保つ

app の役目は「プロセス管理（起動 / `/health` 待ち / 終了時の kill）と WebView を開く」だけ。
ゲームやエディタのロジックは一切書かない（それは server / web の仕事）。目安は ~300 行。
太り始めたら設計ミスなので、ロジックを server か web へ移す。

## エディタと Doc の流儀

ゲームの値は、なるべくコードに直書きせず **Doc（`*.kind.json`）に外へ出す**。外に出せば
このエディタでフォームやスライダーから触れて、**走らせながらその場で変えられる**
（保存したら即・画面に反映）。新しくゲームを作るときは、この形を基本にする。

- **外に出す物**: 見た目・数値・配置・バランス（色 / 速さ / 大きさ / HP / テーマ / 光 /
  シェーダー / レベルのタイル等）。**振る舞い（ルール・当たり判定・生成）はコードのまま**。
  Doc に振る舞いは入れない（見た目と数値は Doc、動きはコード、と分ける）。
- **Doc の形**: version・`kind.schema.json`・fail-open・note・`project.json` の editor 宣言・
  watchFile の外形に従う。1 つの種類 = 1 ファイル種 + 1 スキーマ。新機能は「Doc を1つ足す」
  形でだけ増やす。
- **リアルタイム反映**: ゲームに `App.watchFile` を配線して、Doc の保存で作り直す。
  調整のたびに再起動しないで済む。開発中は `App.reloadOn(F1)` も。
- **調整の入口**: このエディタ（`make dev` かビルド済みの `.app`）。スキーマがあれば
  自動でフォームになる。大量の数値=フォーム、見た目・手触り=走るゲームでライブ反映、と使い分ける。

## スキル一覧

以下、スキルを適宜参照してください。

| スキル | 用途 |
|--------|------|
| `/compile-fix` | Flixコンパイルエラーを診断し、既知の落とし穴と照合して修正を提案する |
| `/flix-docs` | Flixの公式ドキュメントとプロジェクト固有のスタイル確認（パイプスタイル・エフェクト構文・テスト・0.71.0固有の注意点） |
| `/quality-assurance` | テスト設計指針（モジュール新規作成時、ゲームロジック編集時） |
| `/verify` | 変更が実際に動くことを、アプリを走らせて挙動で確かめる |
