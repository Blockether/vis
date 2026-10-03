/**
 * Relay inboxes for automation webhooks. A gateway without a public address
 * creates an inbox and gives the inbox address to another service. The relay
 * stores each request with its raw body and the allowlisted headers. The
 * gateway collects the stored requests, checks each signature and acknowledges
 * them. The relay never holds an automation secret, so it cannot forge a
 * request that the gateway accepts.
 */
import schema from "../../../../packages/vis-contract/resources/vis-contract/schema/automations.json";
import { readBytes, TOO_LARGE } from "../body";
import { base64url, sha256Hex } from "../jwt";
import type { Deps, Env } from "../types";

const limits = schema["x-vis-limits"];
const definitions = schema.$defs;
const HEADER_NAMES: readonly string[] = definitions.relay_header_name.enum;
const HEADER_VALUE_CHARS =
  definitions.relay_request.properties.headers.additionalProperties.maxLength;
const INBOX_ID = new RegExp(definitions.relay_inbox_id.pattern);
const REQUEST_ID = new RegExp(definitions.relay_request_id.pattern);
/** The `uuid` format of `automation_id`. */
const AUTOMATION_ID =
  /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
/** An acknowledgement lists at most one page of request IDs. */
const ACK_BYTES = 4096;
/** A poll refreshes the idle clock of its inbox at most once an hour. */
const POLL_REFRESH_MS = 3_600_000;
/** `btoa` input in whole 3-byte groups, so the parts join without padding. */
const BASE64_CHUNK = 3 * 8192;

type Reader = (request: Request, limit: number) => Promise<unknown>;

interface Storage {
  db: D1DatabaseSession;
  create: RateLimit;
  inbox: RateLimit;
  sender: RateLimit;
}

class HookError extends Error {
  constructor(
    readonly status: number,
    readonly code: string,
    message: string,
  ) {
    super(message);
  }
}

function fail(status: number, code: string, message: string): never {
  throw new HookError(status, code, message);
}

/** Gateways and webhook senders call these routes, never a browser page. */
function json(status: number, value: unknown): Response {
  return new Response(JSON.stringify(value), {
    status,
    headers: {
      "content-type": "application/json; charset=utf-8",
      "cache-control": "no-store",
      "x-content-type-options": "nosniff",
      "referrer-policy": "no-referrer",
    },
  });
}

function storage(env: Env): Storage {
  if (
    !env.ROOMS_DB ||
    !env.HOOKS_CREATE_LIMIT ||
    !env.HOOKS_INBOX_LIMIT ||
    !env.HOOKS_SENDER_LIMIT
  ) {
    fail(
      503,
      "hooks_unconfigured",
      "inbox storage and rate limits are not configured",
    );
  }
  return {
    db: env.ROOMS_DB.withSession("first-primary"),
    create: env.HOOKS_CREATE_LIMIT,
    inbox: env.HOOKS_INBOX_LIMIT,
    sender: env.HOOKS_SENDER_LIMIT,
  };
}

async function admit(limiter: RateLimit, key: string): Promise<void> {
  if (!(await limiter.limit({ key })).success)
    fail(429, "rate_limited", "too many requests, try again later");
}

function randomId(bytes: number): string {
  return base64url(crypto.getRandomValues(new Uint8Array(bytes)));
}

function base64(bytes: Uint8Array): string {
  let text = "";
  for (let at = 0; at < bytes.length; at += BASE64_CHUNK)
    text += btoa(String.fromCharCode(...bytes.subarray(at, at + BASE64_CHUNK)));
  return text;
}

/** A header that is too long is dropped, never cut: a cut value lies. */
function keptHeaders(request: Request): Record<string, string> {
  const kept: Record<string, string> = {};
  for (const name of HEADER_NAMES) {
    const value = request.headers.get(name);
    if (value !== null && value.length <= HEADER_VALUE_CHARS)
      kept[name] = value;
  }
  return kept;
}

async function inboxOf(request: Request, store: Storage): Promise<string> {
  const header = request.headers.get("authorization") ?? "";
  const token = header.toLowerCase().startsWith("bearer ")
    ? header.slice(7).trim()
    : "";
  if (token.length < 32 || token.length > 128)
    fail(401, "unauthorized", "an inbox token is required");
  const hash = await sha256Hex(token);
  await admit(store.inbox, hash);
  const row = await store.db
    .prepare("SELECT id FROM hook_inboxes WHERE token_hash = ?")
    .bind(hash)
    .first<{ id: string }>();
  if (!row) fail(401, "unauthorized", "the inbox token is not valid");
  return row.id;
}

async function createInbox(
  request: Request,
  store: Storage,
  now: number,
): Promise<Response> {
  const address =
    request.headers.get("cf-connecting-ip") ??
    request.headers.get("x-forwarded-for") ??
    "anon";
  await admit(store.create, address);
  const inboxId = randomId(16);
  const token = randomId(32);
  await store.db
    .prepare(
      "INSERT INTO hook_inboxes (id, token_hash, created_at, polled_at) VALUES (?, ?, ?, ?)",
    )
    .bind(inboxId, await sha256Hex(token), now, now)
    .run();
  return json(201, { inbox_id: inboxId, token });
}

/**
 * One statement checks the inbox and both caps and inserts the request, so
 * two senders at the same time cannot both pass the last free slot.
 */
async function storeRequest(
  request: Request,
  store: Storage,
  now: number,
  inboxId: string,
  automationId: string,
): Promise<Response> {
  if (!INBOX_ID.test(inboxId) || !AUTOMATION_ID.test(automationId))
    fail(404, "not_found", "this inbox does not exist");
  await admit(store.sender, inboxId);
  const limit = limits.webhook_body_bytes;
  const declared = Number.parseInt(
    request.headers.get("content-length") ?? "0",
    10,
  );
  if (Number.isFinite(declared) && declared > limit)
    fail(413, "too_large", `a request body may not exceed ${limit} bytes`);
  const body = await readBytes(request, limit);
  if (body === TOO_LARGE)
    fail(413, "too_large", `a request body may not exceed ${limit} bytes`);
  if (body === null) fail(400, "invalid_request", "the request body is broken");
  const result = await store.db
    .prepare(
      `INSERT INTO hook_requests
         (id, inbox_id, automation_id, received_at, headers, body, body_bytes)
       SELECT ?1, ?2, ?3, ?4, ?5, ?6, ?7
       WHERE EXISTS (SELECT 1 FROM hook_inboxes WHERE id = ?2)
         AND (SELECT COUNT(*) FROM hook_requests WHERE inbox_id = ?2) < ?8
         AND (SELECT COALESCE(SUM(body_bytes), 0) FROM hook_requests
              WHERE inbox_id = ?2) + ?7 <= ?9`,
    )
    .bind(
      randomId(16),
      inboxId,
      automationId.toLowerCase(),
      now,
      JSON.stringify(keptHeaders(request)),
      base64(body),
      body.byteLength,
      limits.relay_pending_requests,
      limits.relay_pending_bytes,
    )
    .run();
  if (result.meta.changes === 0) {
    const inbox = await store.db
      .prepare("SELECT id FROM hook_inboxes WHERE id = ?")
      .bind(inboxId)
      .first();
    if (!inbox) fail(404, "not_found", "this inbox does not exist");
    fail(
      429,
      "inbox_full",
      "the inbox is full until its gateway collects the stored requests",
    );
  }
  return json(202, { status: "stored" });
}

/** The body column is read only for the requests that fit the page. */
async function poll(
  request: Request,
  store: Storage,
  now: number,
): Promise<Response> {
  const inboxId = await inboxOf(request, store);
  const asked = Number.parseInt(
    new URL(request.url).searchParams.get("limit") ?? "",
    10,
  );
  const count =
    Number.isFinite(asked) && asked > 0
      ? Math.min(asked, limits.relay_page_requests)
      : limits.relay_page_requests;
  await store.db.batch([
    store.db
      .prepare(
        "UPDATE hook_inboxes SET polled_at = ? WHERE id = ? AND polled_at < ?",
      )
      .bind(now, inboxId, now - POLL_REFRESH_MS),
    store.db
      .prepare(
        "DELETE FROM hook_requests WHERE inbox_id = ? AND received_at < ?",
      )
      .bind(inboxId, now - limits.relay_request_ttl_seconds * 1000),
  ]);
  const { results: sizes } = await store.db
    .prepare(
      "SELECT id, body_bytes FROM hook_requests WHERE inbox_id = ? ORDER BY received_at, id LIMIT ?",
    )
    .bind(inboxId, count)
    .all<{ id: string; body_bytes: number }>();
  const ids: string[] = [];
  let bytes = 0;
  for (const row of sizes) {
    if (ids.length > 0 && bytes + row.body_bytes > limits.relay_page_bytes)
      break;
    bytes += row.body_bytes;
    ids.push(row.id);
  }
  if (ids.length === 0) return json(200, { requests: [] });
  const { results } = await store.db
    .prepare(
      `SELECT id, automation_id, received_at, headers, body FROM hook_requests
       WHERE inbox_id = ? AND id IN (${ids.map(() => "?").join(", ")})
       ORDER BY received_at, id`,
    )
    .bind(inboxId, ...ids)
    .all<{
      id: string;
      automation_id: string;
      received_at: number;
      headers: string;
      body: string;
    }>();
  return json(200, {
    requests: results.map((row) => ({
      id: row.id,
      automation_id: row.automation_id,
      received_at: row.received_at,
      headers: JSON.parse(row.headers) as Record<string, string>,
      body: row.body,
    })),
  });
}

async function acknowledge(
  request: Request,
  store: Storage,
  readJson: Reader,
): Promise<Response> {
  const inboxId = await inboxOf(request, store);
  const body = (await readJson(request, ACK_BYTES)) as {
    ids?: unknown;
  } | null;
  const ids = body && typeof body === "object" ? body.ids : undefined;
  if (
    !Array.isArray(ids) ||
    ids.length < 1 ||
    ids.length > limits.relay_page_requests ||
    new Set(ids).size !== ids.length ||
    !ids.every((id) => typeof id === "string" && REQUEST_ID.test(id))
  ) {
    fail(
      400,
      "invalid_request",
      `ids must list 1 to ${limits.relay_page_requests} different request IDs`,
    );
  }
  const result = await store.db
    .prepare(
      `DELETE FROM hook_requests WHERE inbox_id = ? AND id IN (${ids.map(() => "?").join(", ")})`,
    )
    .bind(inboxId, ...(ids as string[]))
    .run();
  return json(200, { acked: result.meta.changes });
}

async function route(
  request: Request,
  env: Env,
  deps: Deps,
  readJson: Reader,
): Promise<Response> {
  const path = new URL(request.url).pathname.replace(/\/+$/, "");
  const method = request.method.toUpperCase();
  const sender = /^\/hooks\/([^/]+)\/([^/]+)$/.exec(path);
  if (sender) {
    if (method !== "POST")
      fail(405, "method_not_allowed", "send a webhook as a POST request");
    return storeRequest(
      request,
      storage(env),
      deps.now(),
      sender[1],
      sender[2],
    );
  }
  if (method === "POST" && path === "/v1/hooks/inboxes")
    return createInbox(request, storage(env), deps.now());
  if (method === "GET" && path === "/v1/hooks/inbox")
    return poll(request, storage(env), deps.now());
  if (method === "POST" && path === "/v1/hooks/inbox/ack")
    return acknowledge(request, storage(env), readJson);
  fail(404, "not_found", `${method} ${path} is not a route of this relay`);
}

export async function handleHooks(
  request: Request,
  env: Env,
  deps: Deps,
  readJson: Reader,
): Promise<Response> {
  try {
    return await route(request, env, deps, readJson);
  } catch (error) {
    const failure =
      error instanceof HookError
        ? error
        : new HookError(
            503,
            "hooks_unavailable",
            "the relay inbox is temporarily unavailable",
          );
    return json(failure.status, {
      error: { code: failure.code, message: failure.message },
    });
  }
}

/**
 * Remove expired requests and the inboxes that no gateway polled for the idle
 * limit. A poll also removes the expired requests of its own inbox.
 */
export async function cleanHooks(env: Env, now: number): Promise<void> {
  if (!env.ROOMS_DB) return;
  const idle = now - limits.relay_inbox_idle_seconds * 1000;
  await env.ROOMS_DB.batch([
    env.ROOMS_DB.prepare(
      "DELETE FROM hook_requests WHERE received_at < ?",
    ).bind(now - limits.relay_request_ttl_seconds * 1000),
    env.ROOMS_DB.prepare(
      "DELETE FROM hook_requests WHERE inbox_id IN (SELECT id FROM hook_inboxes WHERE polled_at < ?)",
    ).bind(idle),
    env.ROOMS_DB.prepare("DELETE FROM hook_inboxes WHERE polled_at < ?").bind(
      idle,
    ),
  ]);
}
