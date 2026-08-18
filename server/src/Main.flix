// エントリーポイント。環境変数で対象プロジェクトとポートを受け取り、Editor.serve を立ち上げる。
//   EDITOR_DIR  … 編集対象のゲームプロジェクト (省略可 — 未選択で起動し POST /project で選ぶ)
//   EDITOR_PORT … HTTP のポート番号 (省略時 8787)
//   EDITOR_WEB  … ビルド済みエディタ画面のディレクトリ (省略時は隣の flix_ge_editor/dist。無ければ配信無効)
//
// 引数 --sync-agents <engine> <game> のときだけ HTTP を立てず AGENTS の配布だけして終わる
// (engine の bin/fge sync-agents との一致検査 make check-agents-parity の入口)。

def main(): Unit \ {IO, Sys.Env.Env} =
    match Sys.Env.getArgs() {
        case "--sync-agents" :: engine :: game :: Nil => NewGame.distributeCli(engine, game)
        case _ => Editor.serve(Editor.dirFromEnv(), Editor.portFromEnv())
    }
