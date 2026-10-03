/** Local Worker fixture for the native two-gateway Rooms suite. */
import { build } from "esbuild";
import { Miniflare } from "miniflare";
import { readFile } from "node:fs/promises";

const bundle = await build({
  stdin: {
    contents: `import { handle } from './src/index.ts';
    export default { fetch(request, env) {
      const limiter = { limit: async () => ({ success: true }) };
      return handle(request, { ...env, ROOMS_ADDRESS_LIMIT: limiter, ROOMS_MACHINE_LIMIT: limiter });
    } };`,
    resolveDir: process.cwd(),
  },
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  write: false,
});
const worker = new Miniflare({
  host: "127.0.0.1",
  port: 0,
  workers: [
    {
      config: {
        name: "rooms",
        compatibilityDate: "2025-01-01",
        manifest: {
          mainModule: "rooms.mjs",
          modulesRoot: process.cwd(),
          modules: {
            "rooms.mjs": { type: "esm", contents: bundle.outputFiles[0].text },
          },
        },
        env: {
          ROOMS_DB: { type: "d1", id: "ROOMS_DB" },
          ROOMS_ADMIN_TOKEN: {
            type: "text",
            value: process.env.ROOMS_ADMIN_TOKEN,
          },
        },
        exports: {},
      },
    },
  ],
});
const db = await worker.getD1Database("ROOMS_DB");
for (const name of ["0001_rooms.sql", "0002_rooms_limits.sql"]) {
  const sql = await readFile(
    new URL(`../migrations/${name}`, import.meta.url),
    "utf8",
  );
  const statements = sql
    .replace(/^--.*$/gm, "")
    .split(/\n\s*\n/)
    .map((part) => part.trim())
    .filter(Boolean);
  await db.exec(
    statements.map((statement) => statement.replaceAll("\n", " ")).join("\n"),
  );
}
console.log((await worker.ready).origin);
process.on("SIGTERM", async () => {
  await worker.dispose();
  process.exit(0);
});
