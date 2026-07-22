// 起動と配線だけ。ロジックは Elm に住む。
import "../styles.css";
// Shoelace は使う部品だけ個別に取り込む(全部入りバンドルにしない)。
// ダークテーマは <html class="sl-theme-dark"> で効く
import "@shoelace-style/shoelace/dist/themes/dark.css";
import "@shoelace-style/shoelace/dist/components/range/range.js";
import "@shoelace-style/shoelace/dist/components/dialog/dialog.js";
import "@shoelace-style/shoelace/dist/components/alert/alert.js";
import "@shoelace-style/shoelace/dist/components/color-picker/color-picker.js";
import { realApi } from "./realApi";
import { applyDocAppend, applyDocEdit, applyDocEdits, applyDocRemove, type DocEditOp, type PathSeg } from "./docEdit";
// @ts-ignore — vite-plugin-elm が Elm モジュールへ変換する
import { Elm } from "../Main.elm";

const params = new URLSearchParams(location.search);
// 接続先の決め方: ?server=8793 が最優先(検証用に別サーバへ繋ぐ)。
// vite dev(別オリジン)では 8787 の editor_server へ。
// editor_server 自身が配信しているページ(1プロセス運用)では同一オリジン=相対 URL。
const serverParam = params.get("server");
const SERVER_BASE = serverParam
  ? `http://127.0.0.1:${serverParam}`
  : import.meta.env.DEV
    ? "http://127.0.0.1:8787"
    : "";

const api = realApi(SERVER_BASE);

const app = Elm.Main.init({
  node: document.getElementById("app"),
});

// ペイン幅は見た目の好みなのでサーバでなくこの端末(localStorage)に覚える
const PANE_KEY = "flix_ge_resource_editor.paneWidths";

// 文書編集はサーバへ行かずここで解決する(封筒の流儀はサーバ往復と揃える)
function handleLocal(kind: string, payload: any): unknown | undefined {
  if (kind === "saveUiPrefs") {
    localStorage.setItem(PANE_KEY, JSON.stringify(payload));
    return {};
  }
  if (kind === "applyDocEdit") {
    const p = payload as { text: string; path: PathSeg[]; value: unknown; intField: boolean };
    return { text: applyDocEdit(p.text, p.path, p.value, p.intField) };
  }
  if (kind === "applyDocEdits") {
    const p = payload as { text: string; edits: DocEditOp[] };
    return { text: applyDocEdits(p.text, p.edits) };
  }
  if (kind === "applyDocAppend") {
    const p = payload as { text: string; path: PathSeg[]; value: unknown };
    return { text: applyDocAppend(p.text, p.path, p.value) };
  }
  if (kind === "applyDocRemove") {
    const p = payload as { text: string; path: PathSeg[] };
    return { text: applyDocRemove(p.text, p.path) };
  }
  return undefined;
}

// sl-dialog は Esc・オーバーレイクリックで勝手に閉じようとするが、開閉の正は
// Elm の model(conflict)。ここで止めないと「見た目だけ閉じたのに競合未解決」になる
document.addEventListener("sl-request-close", (e) => e.preventDefault());

// 覚えていたペイン幅を一方向の封筒で流し込む。id 0 はどの往復とも衝突しない
// (Elm の採番は 1 始まり)。読めない保存値は黙って捨てる(既定幅で開くだけ)
{
  const saved = localStorage.getItem(PANE_KEY);
  if (saved) {
    try {
      app.ports.apiResponse.send({ id: 0, kind: "uiPrefs", ok: true, body: JSON.parse(saved) });
    } catch {
      // 壊れた保存値は既定幅で開く
    }
  }
}

// 画像・音の URL の付け根を一方向の封筒で知らせる(vite dev は別オリジンのため、
// Elm 側が <img src> を組むのに要る)。id 0 はどの往復とも衝突しない
app.ports.apiResponse.send({ id: 0, kind: "serverBase", ok: true, body: { base: SERVER_BASE } });

// Elm → API(kind で振り分け、応答は id 付きの封筒 {id, kind, ok, body} で返す)
app.ports.apiRequest.subscribe(async (req: { id: number; kind: string; payload: unknown }) => {
  try {
    const body =
      handleLocal(req.kind, req.payload) ?? (await api.handle(req.kind, req.payload));
    app.ports.apiResponse.send({ id: req.id, kind: req.kind, ok: true, body });
  } catch (e) {
    app.ports.apiResponse.send({
      id: req.id,
      kind: req.kind,
      ok: false,
      body: { message: String(e) },
    });
  }
});
