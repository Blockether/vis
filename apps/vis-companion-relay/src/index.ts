/**
 * vis-companion-relay — the smallest thing that lets a gateway you do NOT run
 * wake a phone running a companion you DID sign.
 *
 * APNs binds a topic to the Apple team that owns it, so the signing key can
 * only ever live on infrastructure the app's publisher controls. This Worker
 * is that infrastructure, and it is deliberately the least of it:
 *
 *   app     -> POST /v1/grants   {device_token}      => an opaque grant
 *   app     -> hands the grant to a gateway when the user says "notify me"
 *   gateway -> POST /v1/push     Bearer <grant>      => the relay signs + sends
 *   app     -> DELETE /v1/grants/<grant>             => revoked, alone
 *
 * What the relay therefore is NOT: it holds no user account, no session, no
 * transcript, and no gateway credential. The gateway never learns the device
 * token; the relay never learns which gateway pushed, only that a grant did.
 * Encrypt the alert body app-side and the relay cannot read it either.
 *
 * Abuse budget. Every route is public, so each one is metered before it can
 * cost anything: a request body is refused by Content-Length before it is
 * parsed, grant creation and push are both capped per client address, push is
 * capped per grant, and an over-limit subject is answered from a single
 * indexed read with no write at all (`consumeQuota` returns before the
 * INSERT). Nothing here accumulates for free either — `scheduled` sweeps
 * expired quota windows and grants that were minted and never used.
 */

import { apnsConfig, APNS_DEAD_REASONS, sendApns } from "./apns";
import { fcmConfig, FCM_DEAD_REASONS, sendFcm } from "./fcm";
import {
  consumeQuota,
  createGrant,
  deleteGrantById,
  findGrant,
  notePush,
  purgeExpired,
  randomGrant,
  revokeGrant,
  setEnvironment,
} from "./store";
import type { Deps, Env, Notification, Platform } from "./types";
import { PLATFORMS } from "./types";

export const defaultDeps: Deps = {
  fetch: (...args: Parameters<typeof fetch>) => fetch(...args),
  now: () => Date.now(),
  randomGrant,
};

const MAX_DEVICE_TOKEN_CHARS = 4096;
const MAX_BODY_CHARS = 4096;
/** An alert is a few hundred bytes; APNs itself refuses more than 4 KiB. */
const MAX_REQUEST_BYTES = 16384;
const MAX_DATA_KEYS = 32;

function json(status: number, payload: unknown): Response {
  return new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json; charset=utf-8" },
  });
}

function fail(status: number, code: string, message: string): Response {
  return json(status, { error: { code, message } });
}

function intVar(value: string | undefined, fallback: number): number {
  const parsed = Number.parseInt(String(value ?? ""), 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function bearer(request: Request): string {
  const header = request.headers.get("authorization") ?? "";
  return header.toLowerCase().startsWith("bearer ") ? header.slice(7).trim() : "";
}

/**
 * Cloudflare sets `cf-connecting-ip` itself and a client cannot forge or strip
 * it; `x-forwarded-for` is only the fallback for a non-Cloudflare front.
 */
function clientKey(request: Request): string {
  return request.headers.get("cf-connecting-ip") ?? request.headers.get("x-forwarded-for") ?? "anon";
}

/** A body big enough to cost CPU is refused before a single byte is parsed. */
const TOO_LARGE = Symbol("too_large");

function isOversized(request: Request): boolean {
  const declared = Number.parseInt(request.headers.get("content-length") ?? "0", 10);
  return Number.isFinite(declared) && declared > MAX_REQUEST_BYTES;
}

function oversized(): Response {
  return fail(413, "too_large", `a request body may not exceed ${MAX_REQUEST_BYTES} bytes`);
}

/**
 * `content-length` is the cheap check and a chunked body has none, so the size
 * is enforced twice: on the declaration, then on what actually arrived.
 */
async function readJson(
  request: Request,
): Promise<Record<string, unknown> | typeof TOO_LARGE | null> {
  const text = await request.text();
  if (text.length > MAX_REQUEST_BYTES) return TOO_LARGE;
  try {
    const parsed = JSON.parse(text) as unknown;
    return parsed && typeof parsed === "object" ? (parsed as Record<string, unknown>) : null;
  } catch {
    return null;
  }
}

function str(value: unknown): string {
  return typeof value === "string" ? value.trim() : "";
}

function notificationFrom(body: Record<string, unknown>): Notification | null {
  const title = str(body.title).slice(0, MAX_BODY_CHARS);
  const text = str(body.body).slice(0, MAX_BODY_CHARS);
  if (!title && !text) return null;
  const data: Record<string, string> = {};
  const raw = body.data;
  if (raw && typeof raw === "object") {
    for (const [key, value] of Object.entries(raw as Record<string, unknown>).slice(0, MAX_DATA_KEYS)) {
      data[key] = String(value).slice(0, MAX_BODY_CHARS);
    }
  }
  const badge = typeof body.badge === "number" ? body.badge : undefined;
  return {
    title: title || "Vis",
    body: text,
    data,
    threadId: str(body.thread_id) || undefined,
    collapseId: str(body.collapse_id) || undefined,
    badge,
    isMutable: body.is_mutable === true,
  };
}

async function health(env: Env): Promise<Response> {
  const apns = apnsConfig(env);
  const fcm = fcmConfig(env);
  return json(200, {
    is_ok: true,
    service: "vis-companion-relay",
    apns: {
      is_available: apns !== null,
      topic: apns?.topic ?? null,
      environment: apns?.defaultEnvironment ?? null,
    },
    fcm: { is_available: fcm !== null, project_id: fcm?.projectId ?? null },
  });
}

async function createGrantHandler(request: Request, env: Env, deps: Deps): Promise<Response> {
  const quota = await consumeQuota(
    env.DB,
    `grant-ip:${clientKey(request)}`,
    intVar(env.GRANT_RATE_LIMIT, 30),
    intVar(env.PUSH_RATE_WINDOW_MS, 3600000),
    deps.now(),
  );
  if (!quota.isAllowed) return fail(429, "rate_limited", "too many grants from this address");

  const body = await readJson(request);
  if (body === TOO_LARGE) return oversized();
  if (!body) return fail(400, "bad_request", "a JSON object body is required");

  const deviceToken = str(body.device_token);
  const platform = str(body.platform || "ios") as Platform;
  if (!deviceToken || deviceToken.length > MAX_DEVICE_TOKEN_CHARS) {
    return fail(400, "bad_request", "device_token is required");
  }
  if (!PLATFORMS.includes(platform)) {
    return fail(400, "bad_request", `platform must be one of ${PLATFORMS.join(", ")}`);
  }

  const { grant, row } = await createGrant(
    env.DB,
    {
      deviceToken,
      platform,
      environment: str(body.environment) || "production",
      label: str(body.label) || null,
    },
    deps,
    intVar(env.MAX_GRANTS_PER_DEVICE, 10),
  );

  return json(201, {
    grant,
    relay_url: new URL(request.url).origin,
    platform: row.platform,
    environment: row.environment,
    created_at: row.created_at,
  });
}

async function pushHandler(request: Request, env: Env, deps: Deps): Promise<Response> {
  /**
   * Before the grant lookup, not after: a flood of made-up grants must not buy
   * one database read each. Over the limit this answers from a single indexed
   * read and writes nothing.
   */
  const address = await consumeQuota(
    env.DB,
    `push-ip:${clientKey(request)}`,
    intVar(env.IP_PUSH_RATE_LIMIT, 600),
    intVar(env.PUSH_RATE_WINDOW_MS, 3600000),
    deps.now(),
  );
  if (!address.isAllowed) {
    return json(429, {
      error: { code: "rate_limited", message: "too many push attempts from this address" },
      reset_at: address.resetAt,
    });
  }

  const parsed = await readJson(request);
  if (parsed === TOO_LARGE) return oversized();
  const body = parsed ?? {};
  const grant = bearer(request) || str(body.grant);
  if (!grant) return fail(401, "no_grant", "an Authorization: Bearer <grant> header is required");

  const row = await findGrant(env.DB, grant);
  if (!row) return fail(404, "unknown_grant", "this grant was revoked or never existed");

  const notification = notificationFrom(body);
  if (!notification) return fail(400, "bad_request", "title or body is required");

  const quota = await consumeQuota(
    env.DB,
    `grant:${row.id}`,
    intVar(env.PUSH_RATE_LIMIT, 120),
    intVar(env.PUSH_RATE_WINDOW_MS, 3600000),
    deps.now(),
  );
  if (!quota.isAllowed) {
    return json(429, {
      error: { code: "rate_limited", message: "this grant is over its push quota" },
      reset_at: quota.resetAt,
    });
  }

  const isApple = row.platform === "ios" || row.platform === "ipados";
  const apns = isApple ? apnsConfig(env) : null;
  const fcm = isApple ? null : fcmConfig(env);
  if (isApple ? !apns : !fcm) {
    return fail(503, "provider_unconfigured", `this relay cannot push to ${row.platform}`);
  }

  const result = apns
    ? await sendApns(
        apns,
        { deviceToken: row.device_token, environment: row.environment, notification },
        deps,
      )
    : await sendFcm(fcm!, { deviceToken: row.device_token, notification }, deps);

  const isDead =
    result.status === 410 ||
    (result.status === 404 && !isApple) ||
    (isApple ? APNS_DEAD_REASONS.has(result.reason) : FCM_DEAD_REASONS.has(result.reason));

  if (isDead) {
    await deleteGrantById(env.DB, row.id);
    return json(410, {
      is_delivered: false,
      status: result.status,
      reason: result.reason,
      is_revoked: true,
    });
  }

  if (result.status !== 200) {
    return json(502, { is_delivered: false, status: result.status, reason: result.reason });
  }

  await notePush(env.DB, row.id, deps.now());
  if (result.environment && result.environment !== row.environment) {
    await setEnvironment(env.DB, row.id, result.environment);
  }
  return json(200, {
    is_delivered: true,
    status: 200,
    reason: "",
    environment: result.environment ?? row.environment,
  });
}

async function revokeHandler(grant: string, env: Env): Promise<Response> {
  return json(200, { is_revoked: await revokeGrant(env.DB, grant) });
}

export async function handle(request: Request, env: Env, deps: Deps = defaultDeps): Promise<Response> {
  const url = new URL(request.url);
  const path = url.pathname.replace(/\/+$/, "") || "/";
  const method = request.method.toUpperCase();

  if (isOversized(request)) return oversized();

  if (method === "GET" && (path === "/healthz" || path === "/")) return await health(env);
  if (method === "POST" && path === "/v1/grants") return await createGrantHandler(request, env, deps);
  if (method === "POST" && path === "/v1/push") return await pushHandler(request, env, deps);
  if (method === "DELETE" && path.startsWith("/v1/grants/")) {
    return await revokeHandler(decodeURIComponent(path.slice("/v1/grants/".length)), env);
  }
  return fail(404, "not_found", `${method} ${path} is not a route of this relay`);
}

/**
 * The cron sweep. Public grant creation means anyone can leave rows behind, so
 * rows must expire on their own: quota windows long past, and grants nobody
 * ever pushed to. A grant in real use is never touched, whatever its age.
 */
export async function sweep(env: Env, deps: Deps = defaultDeps): Promise<{
  quota: number;
  grants: number;
}> {
  return await purgeExpired(env.DB, deps.now(), {
    quotaWindowMs: intVar(env.PUSH_RATE_WINDOW_MS, 3600000),
    unusedGrantMs: intVar(env.UNUSED_GRANT_TTL_MS, 30 * 24 * 3600000),
  });
}

export default {
  fetch(request: Request, env: Env): Promise<Response> {
    return handle(request, env, defaultDeps);
  },
  async scheduled(_controller: unknown, env: Env): Promise<void> {
    await sweep(env, defaultDeps);
  },
};
