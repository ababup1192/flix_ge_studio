# flix_ge_studio

[flix_game_engine](https://github.com/ababup1192/flix_game_engine) のゲームデータ
（**Doc** = `*.kind.json`）を、フォームやスライダーで編集するエディタ。

保存するとゲームにその場で反映されるので（`App.watchFile`）、**走らせながら色・数値・
配置を調整**できる。プレビューはエンジン自身が描く（＝本番と同じ絵）ので、見たままが結果になる。

## 何が嬉しい

- **コードを書かずに調整**: 見た目・数値・配置（色 / 速さ / 大きさ / HP / テーマ / 光 /
  シェーダー / レベルのタイル等）は Doc に外出しされていて、フォームから触れる。
- **保存で即反映**: 再起動しないで、走っているゲームに変更が映る。
- **壊れにくい**: スキーマにある値しか入力できない。壊れた参照は警告が出る。読み込みは
  fail-open なので、書き損じても黒画面にならず、直せば戻る。
- **1 つのエディタで全部**: そのゲームが宣言した Doc（キャラ / テーマ / 光 / シェーダー …）が
  1 画面の目次に並ぶ。ジャンルが違っても同じ形で編集できる。

Doc の決まりごと（外形6点・命名・スキーマ）は engine 側の
[docs/doc-conventions.md](https://github.com/ababup1192/flix_game_engine/blob/main/docs/doc-conventions.md)
を参照。

## 構成（3 部品）

| 部品 | 中身 | 役割 |
|---|---|---|
| `server/` | Flix・常駐 HTTP | エディタの頭脳。Doc の読み書き・**プレビュー PNG をエンジンで焼く**・参照検診。engine を fpkg 依存（`server/lib/` に同梱） |
| `web/`    | Elm + Vite | 画面。スキーマからフォームを作り、右にプレビューを出す |
| `app/`    | Rust + Tauri | ランチャー。server を起動し、JRE ごと 1 つの `.app` に包む（ダブルクリックで起動） |

データの流れ:

```
*.kind.json ──編集──▶ web フォーム ──保存──▶ server が書き込み ──▶ ゲームの watchFile が拾って即反映
                          ▲                         │
                          └──── プレビュー PNG ◀── エンジンが唯一のレンダラとして焼く
```

## 必要環境

- **devbox**（Flix / JDK 21 / node を供給）
- **[flix_game_engine](https://github.com/ababup1192/flix_game_engine) が隣にあること**。
  ビルドは engine の `bin/flix` ラッパと devbox の java を借りる。既定では
  `~/Desktop/flix_game_engine` を見る（別の場所なら `make ... ENGINE=/path/to/flix_game_engine`）。
- 対応 OS: いまは **macOS** 前提（起動中ゲーム検知に `ps`/`lsof`、Tauri の後始末に
  POSIX シグナルを使う）。Linux はほぼそのまま動く見込み・Windows は要追加対応。

engine の fpkg（0.7.1）は `server/lib/` に同梱済みなので、**clone 直後でもオフラインで
ビルドできる**。

## 使い方

### いちばん手軽（ダブルクリック起動）

```bash
make app
```

`app/src-tauri/target/release/bundle/macos/Flix GE Editor.app` ができる。これを
`/Applications` に置けばダブルクリックで起動し、プロジェクトを選ぶ画面が出る。
（自作アプリで署名していないので、初回だけ Finder で右クリック →「開く」で許可する。）

### 開発中（ブラウザで即確認）

```bash
make dev                 # server を :8787 で起動（web/dist を配信）
make dev PORT=9000       # ポートを変える
```

ブラウザで `http://localhost:<PORT>` を開くとエディタが出る。プロジェクトは画面から選ぶ。
コードを直したら `make web`（or `make dev`）で作り直すだけ（`.app` の再ビルド不要）。

特定のゲームを最初から開くなら、server に環境変数で渡す:

| 変数 | 意味 |
|---|---|
| `EDITOR_DIR` | 開くゲームのディレクトリ（未指定ならピッカー） |
| `EDITOR_PORT` | 待ち受けポート（既定 8787） |
| `EDITOR_WEB` | エディタ画面（web/dist）の配信元 |
| `EDITOR_ALLOW_ORIGINS` | CORS を許すオリジン（開発時のブラウザ用） |

### 部品ごとのビルド

```bash
make jar     # server → server/artifact/server.jar（Flix fatjar）
make web     # web    → web/dist（Vite build）
make jre     # jlink  → app/runtime/jre（同梱用の最小 JRE）
make app     # 上を全部同梱した .app
make clean   # 生成物を消す（engine fpkg の server/lib は残す）
```

## テスト

```bash
cd web && devbox run -- npm test        # elm-test + vitest
cd server && ../../flix_game_engine/bin/flix test   # server(Flix)
```

## 開発の流儀

このリポの [CLAUDE.md](CLAUDE.md) を参照（会話・コーディング・Doc の外出しなど）。
振る舞い（ルール・当たり判定・生成）はコードのまま、見た目と数値だけを Doc に置く、が原則。
