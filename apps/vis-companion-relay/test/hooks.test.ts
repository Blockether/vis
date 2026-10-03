import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { Miniflare } from "miniflare";
import { build } from "esbuild";
import { readFile } from "node:fs/promises";
import { URL as NodeURL } from "node:url";
import Ajv2020 from "ajv/dist/2020.js";
import { handle } from "../src/index";
import type { Env } from "../src/types";
import schema from "../../../packages/vis-contract/resources/vis-contract/schema/automations.json";
import common from "../../../packages/vis-contract/resources/vis-contract/schema/common.json";

const limits = schema["x-vis-limits"];
const ajv = new Ajv2020({ strict: false, validateFormats: false });
ajv.addSchema(common);
ajv.addSchema(schema);
const automationId = "0b9c6f1e-2f3a-4c5d-8e9f-0a1b2c3d4e5f";
const start = 1700000000000;
let mf: Miniflare;

function expectContract(name: string, value: unknown): void {
  const check = ajv.getSchema(`${schema.$id}#/$defs/${name}`);
  if (!check) throw new Error(`missing contract ${name}`);
  expect(check(value), JSON.stringify(check.errors)).toBe(true);
}

interface Call {
  method?: string;
  token?: string;
  body?: string | Uint8Array;
  headers?: Record<string, string>;
  clock?: number;
  deny?: string;
}

async function call(path: string, options: Call = {}) {
  const response = await mf.dispatchFetch(
    `https://gateway.example.com${path}`,
    {
      method: options.method ?? "GET",
      headers: {
        ...(options.token ? { authorization: `Bearer ${options.token}` } : {}),
        "x-test-clock": String(options.clock ?? start),
        ...(options.deny ? { "x-test-deny": options.deny } : {}),
        ...options.headers,
      },
      ...(options.body === undefined ? {} : { body: options.body }),
    } as Parameters<Miniflare["dispatchFetch"]>[1],
  );
  const data = (await response.json()) as Record<string, any>;
  if (!response.ok) {
    expect(Object.keys(data)).toEqual(["error"]);
    expect(typeof data.error.code).toBe("string");
  }
  return { status: response.status, data };
}

async function createInbox(clock = start) {
  const created = await call("/v1/hooks/inboxes", { method: "POST", clock });
  expect(created.status, JSON.stringify(created.data)).toBe(201);
  expectContract("relay_inbox", created.data);
  return created.data as { inbox_id: string; token: string };
}

async function send(
  inboxId: string,
  body: string | Uint8Array,
  options: Omit<Call, "body"> = {},
) {
  return call(`/hooks/${inboxId}/${automationId}`, {
    method: "POST",
    body,
    ...options,
  });
}

async function poll(token: string, query = "", clock = start) {
  const page = await call(`/v1/hooks/inbox${query}`, { token, clock });
  expect(page.status, JSON.stringify(page.data)).toBe(200);
  expectContract("relay_page", page.data);
  return page.data.requests as Array<Record<string, any>>;
}

async function ack(token: string, ids: string[]) {
  const result = await call("/v1/hooks/inbox/ack", {
    method: "POST",
    token,
    body: JSON.stringify({ ids }),
    headers: { "content-type": "application/json" },
  });
  if (result.status === 200) expectContract("relay_ack", result.data);
  return result;
}

beforeAll(async () => {
  const bundle = await build({
    stdin: {
      contents: `import worker, { handle } from './src/index.ts';
      const limiter = (name, request) => ({
        limit: async () => ({ success: request.headers.get('x-test-deny') !== name }),
      });
      export default {
        fetch(request, env) {
          return handle(request, {
            ...env,
            HOOKS_CREATE_LIMIT: limiter('create', request),
            HOOKS_INBOX_LIMIT: limiter('inbox', request),
            HOOKS_SENDER_LIMIT: limiter('sender', request),
          }, { fetch: globalThis.fetch, now: () => Number(request.headers.get('x-test-clock')) });
        },
        scheduled(controller, env, context) {
          return worker.scheduled(controller, env, context);
        },
      };`,
      resolveDir: process.cwd(),
    },
    bundle: true,
    format: "esm",
    platform: "browser",
    target: "es2022",
    write: false,
  });
  mf = new Miniflare({
    workers: [
      {
        config: {
          name: "hooks",
          compatibilityDate: "2025-01-01",
          manifest: {
            mainModule: "hooks.mjs",
            modulesRoot: process.cwd(),
            modules: {
              "hooks.mjs": {
                type: "esm",
                contents: bundle.outputFiles[0].text,
              },
            },
          },
          env: { ROOMS_DB: { type: "d1", id: "ROOMS_DB" } },
          exports: {},
        },
      },
    ],
  });
  const db = await mf.getD1Database("ROOMS_DB");
  for (const name of [
    "0001_rooms.sql",
    "0002_rooms_limits.sql",
    "0003_hooks.sql",
  ]) {
    const sql = await readFile(
      new NodeURL(`../migrations/${name}`, import.meta.url),
      "utf8",
    );
    const statements = sql
      .replace(/^--.*$/gm, "")
      .split(/\n\s*\n/)
      .map((statement) => statement.trim())
      .filter(Boolean);
    for (const statement of statements) await db.prepare(statement).run();
  }
}, 60000);

afterAll(async () => {
  await mf?.dispose();
});

describe("webhook inboxes", () => {
  it("stores the raw body and the allowlisted headers until the gateway acknowledges them", async () => {
    const inbox = await createInbox();
    const body = new Uint8Array([0x7b, 0xff, 0x00, 0xfe, 0x7d, 0x0a]);
    const stored = await send(inbox.inbox_id, body, {
      clock: start + 5,
      headers: {
        "content-type": "application/json",
        "user-agent": "GitHub-Hookshot/test",
        "x-hub-signature-256": "sha256=abc",
        "x-github-event": "push",
        cookie: "session=private",
        "x-forwarded-for": "10.0.0.5",
      },
    });
    expect(stored.status, JSON.stringify(stored.data)).toBe(202);
    expectContract("relay_stored", stored.data);

    const [request, ...rest] = await poll(inbox.token);
    expect(rest).toEqual([]);
    expect(request.automation_id).toBe(automationId);
    expect(request.received_at).toBe(start + 5);
    expect([...Buffer.from(request.body, "base64")]).toEqual([...body]);
    expect(request.headers).toEqual({
      "content-type": "application/json",
      "user-agent": "GitHub-Hookshot/test",
      "x-hub-signature-256": "sha256=abc",
      "x-github-event": "push",
    });

    const other = await createInbox();
    expect((await ack(other.token, [request.id])).data).toEqual({ acked: 0 });
    expect((await ack(inbox.token, [request.id])).data).toEqual({ acked: 1 });
    expect(await poll(inbox.token)).toEqual([]);
    expect((await ack(inbox.token, [request.id])).data).toEqual({ acked: 0 });
  });

  it("refuses a missing or unknown inbox token", async () => {
    expect((await call("/v1/hooks/inbox")).status).toBe(401);
    expect(
      (await call("/v1/hooks/inbox", { token: "t".repeat(43) })).status,
    ).toBe(401);
    const result = await call("/v1/hooks/inbox/ack", {
      method: "POST",
      token: "t".repeat(43),
      body: JSON.stringify({ ids: ["a".repeat(22)] }),
    });
    expect(result.status).toBe(401);
  });

  it("refuses unknown inboxes, wrong methods and invalid acknowledgements", async () => {
    expect((await send("A".repeat(22), "{}")).data.error.code).toBe(
      "not_found",
    );
    expect(
      (await call(`/hooks/short/${automationId}`, { method: "POST" })).status,
    ).toBe(404);
    const inbox = await createInbox();
    expect(
      (await call(`/hooks/${inbox.inbox_id}/not-a-uuid`, { method: "POST" }))
        .status,
    ).toBe(404);
    expect(
      (await call(`/hooks/${inbox.inbox_id}/${automationId}`)).status,
    ).toBe(405);
    for (const ids of [[], ["bad"], ["a".repeat(22), "a".repeat(22)]])
      expect((await ack(inbox.token, ids)).status).toBe(400);
    expect((await call("/v1/hooks/other", { method: "POST" })).status).toBe(
      404,
    );
  });

  it("accepts a body at the limit and refuses a larger one", async () => {
    const inbox = await createInbox();
    const largest = new Uint8Array(limits.webhook_body_bytes).fill(0x61);
    expect((await send(inbox.inbox_id, largest)).status).toBe(202);
    const tooLarge = await send(
      inbox.inbox_id,
      new Uint8Array(limits.webhook_body_bytes + 1),
    );
    expect(tooLarge.status).toBe(413);
    expect(tooLarge.data.error.code).toBe("too_large");
    const [request] = await poll(inbox.token);
    expect(Buffer.from(request.body, "base64").length).toBe(
      limits.webhook_body_bytes,
    );
  });

  it("pages by request count and body bytes and stops at the byte cap", async () => {
    const inbox = await createInbox();
    const mebibyte = new Uint8Array(limits.webhook_body_bytes).fill(0x62);
    const fits = limits.relay_pending_bytes / limits.webhook_body_bytes;
    for (let index = 0; index < fits; index++) {
      const stored = await send(inbox.inbox_id, mebibyte, {
        clock: start + index,
      });
      expect(stored.status, JSON.stringify(stored.data)).toBe(202);
    }
    const full = await send(inbox.inbox_id, "{}");
    expect(full.status).toBe(429);
    expect(full.data.error.code).toBe("inbox_full");

    const perPage = limits.relay_page_bytes / limits.webhook_body_bytes;
    const page = await poll(inbox.token);
    expect(page.map((request) => request.received_at)).toEqual(
      Array.from({ length: perPage }, (_, index) => start + index),
    );
    expect((await poll(inbox.token, "?limit=2")).length).toBe(2);
    expect(
      (
        await ack(
          inbox.token,
          page.map((request) => request.id),
        )
      ).data,
    ).toEqual({ acked: perPage });
    expect((await send(inbox.inbox_id, "{}")).status).toBe(202);
    expect((await poll(inbox.token)).length).toBe(fits - perPage);
  }, 60000);

  it("stops at the pending request cap", async () => {
    const inbox = await createInbox();
    for (let index = 0; index < limits.relay_pending_requests; index++)
      expect((await send(inbox.inbox_id, "{}")).status).toBe(202);
    const full = await send(inbox.inbox_id, "{}");
    expect(full.status).toBe(429);
    expect(full.data.error.code).toBe("inbox_full");
    expect((await poll(inbox.token)).length).toBe(limits.relay_page_requests);
  }, 60000);

  it("drops expired requests and idle inboxes", async () => {
    const ttl = limits.relay_request_ttl_seconds * 1000;
    const inbox = await createInbox();
    expect((await send(inbox.inbox_id, "{}")).status).toBe(202);
    expect(await poll(inbox.token, "", start + ttl + 1)).toEqual([]);

    const idle = await createInbox();
    expect((await send(idle.inbox_id, "{}")).status).toBe(202);
    const worker = await mf.getWorker();
    await worker.scheduled({
      scheduledTime: new Date(
        start + limits.relay_inbox_idle_seconds * 1000 + 1,
      ),
    });
    expect((await call("/v1/hooks/inbox", { token: idle.token })).status).toBe(
      401,
    );
    expect((await send(idle.inbox_id, "{}")).status).toBe(404);
  });

  it("applies the edge rate limits", async () => {
    const created = await call("/v1/hooks/inboxes", {
      method: "POST",
      deny: "create",
    });
    expect(created.status).toBe(429);
    const inbox = await createInbox();
    expect(
      (await send(inbox.inbox_id, "{}", { deny: "sender" })).data.error.code,
    ).toBe("rate_limited");
    expect(
      (await call("/v1/hooks/inbox", { token: inbox.token, deny: "inbox" }))
        .status,
    ).toBe(429);
  });

  it("reports missing storage instead of failing", async () => {
    const response = await handle(
      new Request("https://gateway.example.com/v1/hooks/inboxes", {
        method: "POST",
      }),
      {} as Env,
    );
    expect(response.status).toBe(503);
    expect(((await response.json()) as any).error.code).toBe(
      "hooks_unconfigured",
    );
  });
});
