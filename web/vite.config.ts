import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  // debug: false — Elm デバッガの封筒バッジ(画面右下)を dev でも出さない
  plugins: [elmPlugin({ debug: false }), tailwindcss()],
  // ポートを固定する。ずれると editor_server の CORS 許可オリジン(localhost:5174)と
  // 食い違って「サーバに繋がりません」になるため、勝手に別ポートへ逃げさせない。
  server: { port: 5174, strictPort: true },
});
