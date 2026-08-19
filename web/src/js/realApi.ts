// 実サーバ(editor_server)への薄い橋。
// Elm 側は kind + payload しか知らない(URL/HTTP メソッドはここに閉じる)。
// flix_ge_editor とはプロトコル(封筒 {id, kind, ok, body})だけ揃え、コードは共有しない。

export interface Api {
  handle(kind: string, payload: any): Promise<unknown>;
}

export function realApi(base: string): Api {
  // 400/404 でもサーバは {ok:false, error} の JSON を返すので、その文言を拾って投げる
  const raiseHttpError = async (res: Response, url: string): Promise<never> => {
    let detail = "";
    try {
      const body = await res.json();
      if (body && typeof body.error === "string") detail = " — " + body.error;
    } catch {
      // JSON でない失敗応答はステータスだけで伝える
    }
    throw new Error(`HTTP ${res.status}: ${url}${detail}`);
  };
  // 応答が来ないまま黙って待ち続けない — 期限切れは失敗の封筒になり、
  // Elm 側が「探しています…」を畳める。焼きの取り次ぎだけはサーバの上限
  // (180 秒)より長く待つ。
  const QUICK_MS = 20_000;
  const BAKE_MS = 200_000;
  const fetchWithin = (url: string, init: RequestInit, ms: number): Promise<Response> => {
    const abort = new AbortController();
    const timer = setTimeout(() => abort.abort(), ms);
    return fetch(url, { ...init, signal: abort.signal }).finally(() => clearTimeout(timer));
  };
  const getJson = async (url: string, ms: number = QUICK_MS) => {
    const res = await fetchWithin(url, {}, ms);
    if (!res.ok) await raiseHttpError(res, url);
    return res.json();
  };
  const sendJson = async (method: string, url: string, body: unknown, ms: number = QUICK_MS) => {
    const res = await fetchWithin(url, {
      method,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    }, ms);
    if (!res.ok) await raiseHttpError(res, url);
    return res.json();
  };
  // 400/409 が {ok:false, error:{message:日本語}} の契約の口。理由だけを
  // " — " の後ろに載せて投げる(Elm 側が理由だけ表示する)
  const sendJsonReason = async (method: string, url: string, body: unknown) => {
    const res = await fetch(url, {
      method,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!res.ok) {
      let detail = "";
      try {
        const failed = await res.json();
        const msg = failed?.error?.message ?? failed?.error;
        if (typeof msg === "string") detail = " — " + msg;
      } catch {
        // JSON でない失敗応答はステータスだけで伝える
      }
      throw new Error(`HTTP ${res.status}: ${url}${detail}`);
    }
    return res.json();
  };

  return {
    async handle(kind: string, payload: any): Promise<unknown> {
      switch (kind) {
        case "projects":
          return getJson(`${base}/projects`);
        case "workspace":
          return getJson(`${base}/workspace`);
        case "workspaceSet":
          return sendJson("POST", `${base}/workspace`, payload);
        case "selectProject": {
          // 400 でも {ok:false, error} の JSON が契約なので、投げずにそのまま Elm へ返す
          // (どの候補の失敗として出すかは Elm 側が覚えている)
          const res = await fetch(`${base}/project`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          });
          try {
            return await res.json();
          } catch {
            return await raiseHttpError(res, `${base}/project`);
          }
        }
        case "health":
          return getJson(`${base}/health`);
        case "files":
          return getJson(`${base}/files`);
        case "resources":
          return getJson(`${base}/resources`);
        case "changes":
          return getJson(`${base}/changes`);
        case "runningGames":
          // 走っているゲームの検知はサーバ側 (外部コマンド) が行う。
          // ブラウザでも .app でも同じ経路で {games:[{pid,cwd}]} が返る。
          return getJson(`${base}/running-games`);
        case "engineVersion":
          // いま使っている engine のバージョン。プロジェクト未選択でも返る。
          return getJson(`${base}/engine/version`);
        case "activeDocs":
          // ゲームが書く「いま画面に出ている Doc」。無いのは普通(Elm 側が静かに無視)
          return getJson(`${base}/file?path=${encodeURIComponent("debug/active-docs.json")}`);
        case "getFile":
          return getJson(`${base}/file?path=${encodeURIComponent(payload.path)}`);
        case "putFile": {
          // 409(ifMtime 競合)も {ok:false, currentMtime} の JSON が契約なので、
          // 投げずにそのまま Elm へ返す(競合ダイアログを出すのは Elm 側)
          const res = await fetch(`${base}/file`, {
            method: "PUT",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          });
          try {
            return await res.json();
          } catch {
            return await raiseHttpError(res, `${base}/file`);
          }
        }
        // 焼き係への取り次ぎ(Studio のサーバが 1 枚噛む)。20〜70 秒かかる
        case "bakeWake":
          return sendJson("POST", `${base}/bake/wake`, payload);
        // 実機へ「ここから演じて」と頼む(本文は text/plain。判別は Elm 側)
        case "performScene":
          return sendJson("POST", `${base}/bake/proxy`, payload, BAKE_MS);
        case "bakeCutscene":
          return sendJson("POST", `${base}/bake/proxy`, payload, BAKE_MS);
        // 1 枚焼き(選んだカットの瞬間)。宣言の口 + /frame への取り次ぎ
        case "cutsceneFrame":
          return sendJson("POST", `${base}/bake/proxy`, payload, BAKE_MS);
        // 焼き産物が既にあるか(既存の配信の口へ HEAD を撃つだけ)。X-Mtime ヘッダ
        // (その産物ファイルの mtime ms)が有れば一緒に返す — Studio を閉じて
        // 開き直しても(bakedTokens はメモリなので消える)、復元した GIF の mtime と
        // 今の JSON の mtime を比べれば「前回の焼きから何も変わっていない」を
        // ディスクの事実から言い直せる
        case "mediaExists": {
          const url = String((payload as { url: string }).url ?? "");
          return fetch(url, { method: "HEAD" })
            .then((res) => {
              const raw = res.headers.get("X-Mtime");
              const mtime = raw !== null && raw !== "" ? Number(raw) : null;
              return { exists: res.ok, mtime: mtime !== null && !Number.isNaN(mtime) ? mtime : null };
            })
            .catch(() => ({ exists: false, mtime: null }));
        }
        // 前回の焼き(pastBake)はコマ数が分からないので、置き場の *.png を数えて貰う
        // (URL は Elm 側で組む — mediaExists と同じ流儀)
        case "mediaCount": {
          const url = String((payload as { url: string }).url ?? "");
          return getJson(url);
        }
        case "referenceStatus":
          return getJson(`${base}/reference/status`);
        case "referenceUpdate":
          return sendJsonReason("POST", `${base}/reference/update`, payload);
        case "search":
          return getJson(`${base}/search?q=${encodeURIComponent(String((payload as { q: string }).q ?? ""))}`);
        // 検索結果から飛んだ欄まで画面を送る。描き終わるのは Elm の次の描画なので
        // 少しだけ待って探す(見つからなければ静かに諦める — 移動は演出であって編集ではない)
        case "scrollTo": {
          const opts = payload as { id: string; block?: ScrollLogicalPosition; flash?: boolean };
          const id = String(opts.id ?? "");
          // 既定は「真ん中へ寄せて光らせる」(検索から欄へ飛んだ時)。
          // 一覧の追従は nearest + 光らせない — 選択が動くたびに光ると読めない
          const block = opts.block ?? "center";
          const flash = opts.flash ?? true;
          const tryScroll = (left: number) => {
            const el = document.getElementById(id);
            if (el !== null) {
              el.scrollIntoView({ block });
              if (flash) {
                el.classList.add("jump-flash");
                window.setTimeout(() => el.classList.remove("jump-flash"), 1200);
              }
            } else if (left > 0) {
              window.requestAnimationFrame(() => tryScroll(left - 1));
            }
          };
          window.requestAnimationFrame(() => tryScroll(30));
          return { ok: true };
        }
        // その場編集の欄へカーソルを置く(描き終わるのを待って探す)
        case "focusId": {
          const id = String((payload as { id: string }).id ?? "");
          const tryFocus = (left: number) => {
            const el = document.getElementById(id);
            if (el instanceof HTMLInputElement) {
              el.focus();
              el.select();
            } else if (left > 0) {
              window.requestAnimationFrame(() => tryFocus(left - 1));
            }
          };
          window.requestAnimationFrame(() => tryFocus(30));
          return { ok: true };
        }
        // ファイルそのものへの動詞。断り(409 既にある / 400 プロジェクト外)は
        // サーバの理由つきで投げる — 画面には「なぜできなかったか」を出したい
        case "fileNew":
          return sendJsonReason("POST", `${base}/file/new`, payload);
        case "fileDuplicate":
          return sendJsonReason("POST", `${base}/file/duplicate`, payload);
        case "fileRename":
          return sendJsonReason("POST", `${base}/file/rename`, payload);
        case "fileDelete":
          return sendJsonReason("POST", `${base}/file/delete`, payload);
        case "journeyState":
          return getJson(`${base}/journey/state`);
        case "journeyChanges":
          // 呼ぶたびに見た目の検査が 1 目盛り進む。404(この口を持たないサーバ)は
          // Elm 側が実況とモーダルを出さないだけに倒す
          return getJson(`${base}/journey/changes`);
        case "journeyChangesSeen":
          return sendJson("POST", `${base}/journey/changes/seen`, payload);
        case "sfxWarm": {
          // 焼き係を先に立ち上げておく(最初のひと触りを待たせないため)
          const res = await fetch(`${base}/sfx/warm`, { method: "POST" });
          return res.json();
        }
        case "sfxShape":
          // 焼き上がった効果音の実測(波形の包絡と帯ごとの音量)。まだ焼かれて
          // いなければサーバが 404 を返し、エディタは絵を出さないだけ
          return getJson(
            `${base}/sfx/shape?name=${encodeURIComponent(String((payload as { name: string }).name ?? ""))}`,
          );
        case "galleryList":
          // ミニプレイヤーの場面チップとラフ比較の材料(読むだけ)
          return getJson(`${base}/gallery/list`);
        case "galleryScenes":
          // ギャラリータブ 1 枚ぶん(絵 + 場面の説明のラベル + リファレンス差分)
          return getJson(`${base}/gallery/scenes`);
        case "annotationsList":
          // 注釈チケットの一覧(読むだけ)
          return getJson(`${base}/annotations/list`);
        case "sketchList":
          // ラフ塗りの一覧(読むだけ) — 次に書くバージョン番号の元
          return getJson(`${base}/sketch/list`);
        case "annotationsComment":
          // 404/400 は日本語の理由が契約
          return sendJsonReason("POST", `${base}/annotations/comment`, payload);
        case "annotationsArchive":
          return sendJsonReason("POST", `${base}/annotations/archive`, payload);
        case "annotationsCreate":
          // ラフと見比べて気づいた事を、遊んでいて切った物と同じ棚へ足す
          return sendJsonReason("POST", `${base}/annotations/create`, payload);
        case "runnerLog":
          // プレビューの描き出しのログ(進捗パネルの末尾数行の材料)
          return getJson(`${base}/runner/log`);
        case "atelierCandidates":
          return getJson(`${base}/atelier/candidates`);
        case "promoteCandidate": {
          // 400 は {"error":{"message":…}} の日本語の理由が契約。
          // その文言だけを " — " の後ろに載せて投げる(Elm 側が理由だけ表示する)
          const url = `${base}/atelier/promote`;
          const res = await fetch(url, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          });
          if (!res.ok) {
            let detail = "";
            try {
              const body = await res.json();
              const msg = body?.error?.message ?? body?.error;
              if (typeof msg === "string") detail = " — " + msg;
            } catch {
              // JSON でない失敗応答はステータスだけで伝える
            }
            throw new Error(`HTTP ${res.status}: ${url}${detail}`);
          }
          return res.json();
        }
        case "atelierSlots":
          return getJson(`${base}/atelier/slots`);
        case "atelierArchive":
          return getJson(`${base}/atelier/archive`);
        case "atelierArchiveAdd":
          // 409(同名あり)は日本語の理由が契約
          return sendJsonReason("POST", `${base}/atelier/archive`, payload);
        case "atelierRestore":
          // 409(同じ名前の候補が既にある)は日本語の理由が契約
          return sendJsonReason("POST", `${base}/atelier/restore`, payload);
        case "promptAtelier": {
          const q = new URLSearchParams({
            slot: String(payload.slot),
            count: String(payload.count),
            direction: String(payload.direction ?? ""),
          });
          return getJson(`${base}/prompt/atelier?${q}`);
        }
        case "promptWire":
          return getJson(`${base}/prompt/wire?doc=${encodeURIComponent(payload.doc)}`);
        case "atelierCopy": {
          // 400/409 は日本語の理由が契約。promoteCandidate と同じく
          // " — " の後ろに理由だけ載せて投げる(Elm 側が理由だけ表示する)
          const url = `${base}/atelier/copy`;
          const res = await fetch(url, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          });
          if (!res.ok) {
            let detail = "";
            try {
              const body = await res.json();
              const msg = body?.error?.message ?? body?.error;
              if (typeof msg === "string") detail = " — " + msg;
            } catch {
              // JSON でない失敗応答はステータスだけで伝える
            }
            throw new Error(`HTTP ${res.status}: ${url}${detail}`);
          }
          return res.json();
        }
        case "genesisGenres":
          // ジャンル一覧。404(旧サーバ)は Elm 側がプリセット入力に倒す
          return getJson(`${base}/genesis/genres`);
        case "promptGenesis": {
          // 公式プロンプト。free だけ direction 必須(400 は日本語の理由が契約)
          const q = new URLSearchParams({ genre: String(payload.genre ?? "") });
          const direction = String(payload.direction ?? "");
          if (direction !== "") q.set("direction", direction);
          return getJson(`${base}/prompt/genesis?${q}`);
        }
        case "promptExtend": {
          // 「ゲームを広げる」のプロンプトの下書き。404(旧サーバ)は Elm 側が「準備中」に倒す
          const q = new URLSearchParams({ kind: String(payload.kind) });
          return getJson(`${base}/prompt/extend?${q}`);
        }
        case "projectNew":
          // 202 が契約。400/409 は {ok:false, error:日本語} — raiseHttpError が
          // " — 理由" の形で投げ、Elm 側が理由だけ表示する
          return sendJson("POST", `${base}/projects/new`, payload);
        case "projectNewLog":
          return getJson(`${base}/projects/new/log`);
        case "gameStatus":
          return getJson(`${base}/game/status`);
        case "gameStart": {
          // 409(既に走っている)は失敗ではなく「走っている」のメッセージ
          const url = `${base}/game/start`;
          const res = await fetch(url, { method: "POST" });
          if (res.status === 409) return { running: true };
          if (!res.ok) await raiseHttpError(res, url);
          try {
            return await res.json();
          } catch {
            return {}; // 202 が本文なしでも受理は受理
          }
        }
        case "gameStop":
          return sendJson("POST", `${base}/game/stop`, {});
        case "gameLog":
          return getJson(`${base}/game/log`);
        case "gameLogView":
          // 「ゲームのログ」モーダル用。起動実況(gameLog)と同じ口だが、応答の
          // 行き先が違うので名前を分ける(実況のポーリングと混ざらないように)
          return getJson(`${base}/game/log`);
        case "cleanLocks":
          // こまったときの掃除: 中断が lib/cache に残した *.lock を消す
          return sendJson("POST", `${base}/maintenance/clean-locks`, {});
        case "clearFontCache":
          return sendJson("POST", `${base}/maintenance/clear-font-cache`, {});
        case "previewItems":
          return sendJson("POST", `${base}/preview/items`, payload);
        case "previewUi":
          return sendJson("POST", `${base}/preview/ui`, payload);
        case "spriteColors":
          // ドット絵 legend の実色表。404(旧サーバ)は Elm 側が仮色に倒す
          return sendJson("POST", `${base}/sprite/colors`, payload);
        case "previewHitbox":
          return sendJson("POST", `${base}/preview/hitbox`, payload);
        case "previewFx":
          return sendJson("POST", `${base}/preview/fx`, payload);
        default:
          throw new Error("未知のリクエスト: " + kind);
      }
    },
  };
}
