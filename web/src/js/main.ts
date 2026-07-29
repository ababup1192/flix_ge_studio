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
import { applyDocAppend, applyDocEdit, applyDocEdits, applyDocRemove, rangeAt, type DocEditOp, type PathSeg } from "./docEdit";
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

// いま鳴っている効果音(試聴・ループ)。次を鳴らす前に必ず止める
let sfxAudio: HTMLAudioElement | null = null;
// 焼きたての音を指す一時 URL。次を鳴らす前に必ず捨てる(溜めるとメモリを食う)
let sfxUrl: string | null = null;

// 文書編集はサーバへ行かずここで解決する(封筒の流儀はサーバ往復と揃える)
function handleLocal(kind: string, payload: any): unknown | undefined {
  if (kind === "copyClipboard") {
    // プロンプトのコピー。navigator.clipboard が無い環境(非 https 等)は
    // 隠しテキストエリア + execCommand に倒す
    const text = String((payload as { text: string }).text ?? "");
    if (navigator.clipboard?.writeText) {
      return navigator.clipboard.writeText(text).then(() => ({}));
    }
    const ta = document.createElement("textarea");
    ta.value = text;
    ta.style.position = "fixed";
    ta.style.opacity = "0";
    document.body.appendChild(ta);
    ta.select();
    const ok = document.execCommand("copy");
    ta.remove();
    if (!ok) throw new Error("クリップボードに書き込めませんでした");
    return {};
  }
  if (kind === "previewSfx") {
    // つまみの値をそのまま渡して 1 音だけ焼いてもらい、返ってきた WAV を鳴らす。
    // 焼くのはゲーム自身のコードなので、ここで聴く音は実機の音と同じ。
    // handleLocal は同期関数なので、待ちは Promise のまま返す(封筒の作法と同じ)。
    const { name, values, loop } = payload as { name: string; values: unknown; loop: boolean };
    return fetch(`${SERVER_BASE}/sfx/preview?name=${encodeURIComponent(name)}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(values ?? {}),
    }).then((res) => {
      // 202 = 焼き係を立ち上げ中(初回だけ 1〜2 分)。鳴らさずにそのまま伝える
      if (res.status === 202) return { starting: true };
      if (!res.ok) return { starting: false, ok: false };
      return res.blob().then((blob) => {
        if (sfxAudio) {
          sfxAudio.pause();
          sfxAudio = null;
        }
        if (sfxUrl) URL.revokeObjectURL(sfxUrl);
        sfxUrl = URL.createObjectURL(blob);
        const audio = new Audio(sfxUrl);
        audio.loop = Boolean(loop);
        sfxAudio = audio;
        void audio.play().catch(() => {});
        return { starting: false, ok: true };
      });
    });
  }
  if (kind === "playSound" || kind === "stopSound") {
    // 音は Elm から鳴らせないのでここで持つ。同時に 1 つだけ鳴らす —
    // つまみを動かすたびに重なると、どれを聴いているのか分からなくなる
    if (sfxAudio) {
      sfxAudio.pause();
      sfxAudio = null;
    }
    if (kind === "stopSound") return {};
    const { name, loop } = payload as { name: string; loop: boolean };
    // 焼き直された物を確実に鳴らすため、毎回別の URL にする(ブラウザの控えを避ける)
    const url = `${SERVER_BASE}/gallery/sound?name=${encodeURIComponent(name)}&t=${Date.now()}`;
    const audio = new Audio(url);
    audio.loop = Boolean(loop);
    sfxAudio = audio;
    void audio.play().catch(() => {
      // 焼かれていない・音を出せない環境は黙って何もしない(絵と数値の編集は続く)
    });
    return {};
  }
  if (kind === "highlightJson") {
    // フォームで触っている欄を、右の JSON でも選択状態にして見せる。
    // focus は移さない(打っている欄から指が離れる方が困る)ので、選択と
    // スクロール位置だけを合わせる。見つからない・畳まれている時は何もしない。
    const p = payload as { text: string; path: PathSeg[]; id: string };
    const box = document.getElementById(p.id);
    if (!(box instanceof HTMLTextAreaElement)) return {};
    const range = rangeAt(p.text, p.path);
    if (range === null) return {};
    box.setSelectionRange(range.from, range.to);
    // 選んだ所が見えるように寄せる(行の高さは実測せず、前の改行数から割り出す)
    const line = p.text.slice(0, range.from).split("\n").length - 1;
    const lineHeight = box.scrollHeight / Math.max(1, p.text.split("\n").length);
    box.scrollTop = Math.max(0, line * lineHeight - box.clientHeight / 2);
    return {};
  }
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
    // handleLocal は Promise を返す事もある(clipboard)ので、どちらも await で均す
    const body = await (handleLocal(req.kind, req.payload) ?? api.handle(req.kind, req.payload));
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
