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
  const getJson = async (url: string) => {
    const res = await fetch(url);
    if (!res.ok) await raiseHttpError(res, url);
    return res.json();
  };
  const sendJson = async (method: string, url: string, body: unknown) => {
    const res = await fetch(url, {
      method,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!res.ok) await raiseHttpError(res, url);
    return res.json();
  };

  return {
    async handle(kind: string, payload: any): Promise<unknown> {
      switch (kind) {
        case "projects":
          return getJson(`${base}/projects`);
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
        case "previewItems":
          return sendJson("POST", `${base}/preview/items`, payload);
        case "previewUi":
          return sendJson("POST", `${base}/preview/ui`, payload);
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
