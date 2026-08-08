# 実機再生の別 JVM 化（split run）設計メモ

状態: 設計のみ（未実装）。実現方法は 2026-08-05 に実験で裏取り済み。

## 背景

Studio の実機再生は `java -jar flix.jar run` で、**コンパイラとゲームが 1 つの JVM を
分け合う**。コンパイラが作る巨大なデータはゲーム実行中もヒープに居座り、メモリの少ない
機械（特に Windows）では遊んでいる間 GC が頻発してカクつく疑いがある。
`-XX:MaxRAMPercentage=50`（対応済み）は緩和で、根治はヒープの分離。

## 裏取り済みの事実（sokoban で実験）

- `flix build` は `build/class/` に実行可能な class 群を吐く。入口は `Main`
- 次のコマンドで**コンパイラ抜きの素の JVM** からゲームが起動し、絵と音まで正常だった:

```
java -XstartOnFirstThread \
  -cp "build/class:lib/external/*:<lib/cache の全 jar>:<flix.jar>" Main
```

- `lib/external/` はネイティブ jar のみ。LWJGL 本体は `lib/cache/`（Maven の写し）に居る
- `flix.jar` を classpath に入れるのは `dev.flix.runtime`（Flix の実行時部品）のため。
  コンパイラのクラスは読み込まれないのでヒープは太らない
- **注意: `flix build` はテストもコンパイルする**（build/class に Test*.class を実測）。
  肥大テストで MethodTooLargeException になるゲーム（flappy_bird に前例）は
  この段が落ちる

## 提案する形

`Task.Debug` を 2 段に分ける:

1. **コンパイル段**: `java -Xss16m -XX:MaxRAMPercentage=50 -Dstdout.encoding=UTF-8
   -Dstderr.encoding=UTF-8 -jar flix.jar build` — 窓なし。終わったら JVM ごと死ぬ
2. **実行段**: `java [-XstartOnFirstThread(mac のみ)] -Dstdout.encoding=UTF-8
   -Dstderr.encoding=UTF-8 -cp <組んだ classpath> Main` — env は今と同じ
   （DEBUG=true・DEBUG_HTTP_PORT）。ヒープ指定は不要（ゲームだけなら既定で足りる）

classpath の組み立て（サーバ側）:

- `build/class` + `lib/external/*`（ワイルドカードは JVM が展開）+
  `lib/cache` を歩いて集めた全 `*.jar` + `flix.jar`
- 区切りは macOS/Linux `:`、Windows `;`

## 実装箇所

| 場所 | 変更 |
|---|---|
| `server/src/EngineTasks.flix` | Debug 用に 2 段の argv を組む関数と classpath 組み立て（純粋部はテスト） |
| `server/src/Runner.flix` | 2 段ジョブ: 段 1 が exit 0 なら段 2。onStart フックへは段 2 の Process を渡す。停止要求は走っている段を殺す |
| `server/src/Game.flix` | フック経由なのでほぼ無変更（生死判定が段 2 の Process を見ることの確認だけ） |

## リスクと備え

- **段 1 が落ちるゲーム**（肥大テストの MethodTooLarge 等）: 段 1 失敗時は従来の
  `flix run`（1 JVM）へフォールバックし、ログに一言残す。フォールバックは常設
- `build/class` の配置と入口名 `Main` は flix の実装詳細: flix の版を上げるときの
  確認項目に足す
- 起動時間は今と同等（コンパイルはどのみち走る）+ JVM 起動 1 回ぶん
- 保存即反映（watchFile）・リモートデバッグはゲーム側の機構なので変わらない

## 導入の順序

1. 環境変数（例: `STUDIO_SPLIT_RUN=1`）での opt-in で実装
2. Windows 実機で「カクつきが消えるか」を確認（これが GC 犯人説の最終回答にもなる）
3. 効果が出たら既定 on（フォールバックは残す）
