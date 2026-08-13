# 計画: 「作る」ボタンで AI まで一気通貫にする(one-click genesis)

状態: 未着手(2026-08-12 起案)

## なぜやるか

新しいゲーム作りは今、「カードを選ぶ → 依頼文を人がコピーする → Claude Code に貼る」という
乗り換えが 1 回ある。ここが Rosebud 系の「数分で遊べる」体験に負けている唯一の本質。
乗り換えを消せば「カードを選ぶ → 3 行書く → 塗る → 待つ → 遊ぶ」が Studio の画面から
出ずに完結し、しかも成果物は手元の普通のリポジトリのまま(ここは向こうに無い強み)。

## 使う既存部品(新しく発明しない)

- 依頼文生成: `server/src/Genesis.flix` の `genrePrompt` / `freePrompt`、
  ラフ側のプロンプト合成は `web/src/NewGame.elm` の `buildSketchPrompt`
- 進捗の型: `POST /projects/new` が 202 → `GET /projects/new/log` を 2 秒ポーリング、
  失敗時だけログ自動全文展開(NewGame.elm の進捗ミニパネル)
- プロセス起動の型: `server/src/EngineTasks.flix`(make 抜きで argv を ProcessBuilder へ、
  出力 UTF-8 固定、10 分見張り番、失敗はトースト + ⚠ログ)
- 生存管理の型: `server/src/Game.flix` の `~/.flix_ge_studio/games.json`(pid 書き残し)

## 設計方針

- server に「AI 仕事」タスクを 1 種足す: `claude -p "<依頼文>"` をヘッドレス起動し、
  stdout を `/projects/new/log` と同型のログ口で流す(EngineTasks の流儀に従う)。
- claude CLI が見つからない環境では、今まで通り「依頼文を見せてコピー」に fail-open で
  倒す。ボタンが「Studio に任せる / 自分で貼る」の 2 択になるだけで、既存フローは消さない。
- `/style-interview` の質問往復は第 1 段ではやらない。質問が来たらログにそのまま見せて
  「続きは Claude Code で」と案内する(投げて・見守って・開く、をまず完成させる)。
- web は NewGame の進捗ミニパネルを流用し、完了したらプロジェクト選択へ誘導する。

## 段階分け

1. ヘッドレス起動 + ログ表示(claude CLI 検出と fail-open 込み)
2. 完了検知と「開く」導線(誕生済みプロジェクトへの遷移)
3. (将来)質問往復 — `/style-interview` の質問を Studio のフォームに出して答えを返す

## 未確定事項(着手時に必ず確かめる)

- claude CLI のヘッドレス実行の作法: `--permission-mode` などの権限まわり、
  出力形式(stream-json か素のテキストか)、中断のさせ方。
- 長丁場(ゲーム誕生は数分〜)と 10 分見張り番の相性。延長するか、進捗があれば
  リセットするか。
- AI 仕事の途中で Studio が落ちた時の引き継ぎ(games.json と同じ手が使えるか)。
