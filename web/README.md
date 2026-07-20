# flix_ge_resource_editor

flix_game_engine 用のリソース(スキーマ付き JSON)エディタ。
バックエンドは editor_server(port 8787)。flix_ge_editor(ui/hitbox エディタ)とは
プロトコル(ports 封筒 {id, kind, ok, body} と HTTP エンドポイント)だけ共有し、コードは共有しない。

## 起動(devbox 前提)

```sh
devbox run -- npm install   # 初回のみ
devbox run dev              # http://localhost:5173 (editor_server 8787 へ接続)
```

editor_server は flix_game_engine 側で起動しておく。別ポートのサーバへ繋ぐときは `?server=8793`。

## テスト・ビルド

```sh
devbox run test              # elm-test (純粋ロジック + FlowTest=配線フロー)
devbox run -- npx vitest run # vitest (docEdit の jsonc 最小編集)
devbox run build             # vite build → dist/
```

単体テストは 2 層:

- **elm-test** — Schema/Refs/Draft 等の純粋ロジックに加え、`tests/FlowTest.elm` が
  elm-program-test で配線フロー(どの操作でどの封筒が飛ぶ/飛ばないか)を検査する。
  update は自前データ `Effect`(src/Effect.elm)を返し、Cmd への変換は
  `Effect.perform` の 1 関数だけ — テストは Effect を模擬ポートへ流して封筒を覗く。
- **vitest** — docEdit(jsonc 最小編集)の TS 側。

### スモーク(実 DOM だけの決め打ち・scripts/smoke.mjs)

フォーカス保持・sl-range / sl-dialog の実挙動・盤面ドラッグ等、実ブラウザでしか
出ない ~11 項目だけを通す。editor_server(8787)と vite dev(5174)を起こしてから:

```sh
cd ../flix_ge_editor && devbox run -- node ../flix_ge_resource_editor/scripts/smoke.mjs
```

playwright は flix_ge_editor の node_modules を借りる(このリポには入れない)。
保存(PUT)はしない設計で、スモーク中の編集はページ再読込で捨てる。
回すのは UI の実装(view・イベント配線・Shoelace 部品)を触ったときだけで足りる —
封筒の出し入れの回帰は elm-test 側が毎回見ている。

## いまの範囲(C0)

- プロジェクト選択(GET /projects → POST /project)
- ファイル一覧(GET /files をならした JSON パス列)から開いて、素のテキスト(textarea)で編集
- 保存(PUT /file)+ lost update ガード: 保存前に GET /file で最新を取り、
  開いた時のテキストと違っていたら「再読込 / 構わず上書き」の 2 択を出す

次: GET /resources とスキーマ駆動フォーム(右ペインのプレースホルダに入る)。
