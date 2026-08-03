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
 *
 * What the relay therefore is NOT: it holds no user account, no session, no
 * transcript, no gateway credential — and, since the grant carries its own
 * sealed contents (`seal.ts`), no database either. The gateway never learns the
 * device token; the relay never learns which gateway pushed, only that a grant
 * did. Encrypt the alert body app-side and the relay cannot read it either.
 *
 * Abuse budget. Every route is public — nobody authenticates to ASK for a
 * grant, and a gateway only ever proves it holds one — so the question is never
 * "is this caller allowed" but "what does an unwelcome caller cost". A body too
 * big is refused by Content-Length before it is parsed; minting and pushing are
 * both metered per client address by a Cloudflare rate limiting binding, whose
 * counters live at the edge, so a flood is refused without a single storage
 * operation; and pushes are metered per DEVICE, not per grant, so minting a
 * thousand grants for one phone still buys the same one phone's worth of noise.
 * Nothing accumulates anywhere, so nothing has to be swept.
 */

import { APNS_DEAD_REASONS, APNS_MAX_PAYLOAD_BYTES, apnsConfig, apnsPayload, sendApns } from "./apns";
import { FCM_DEAD_REASONS, FCM_MAX_PAYLOAD_BYTES, fcmConfig, fcmPayload, sendFcm } from "./fcm";
import { sha256Hex } from "./jwt";
import { fitNotification } from "./payload";
import { seal, unseal } from "./seal";
import type { Deps, Env, Notification, Platform } from "./types";
import { PLATFORMS } from "./types";

export const defaultDeps: Deps = {
  fetch: (...args: Parameters<typeof fetch>) => fetch(...args),
  now: () => Date.now(),
};

const MAX_BODY_CHARS = 4096;
const MAX_DATA_KEYS = 32;
const DEFAULT_GRANT_TTL_DAYS = 90;

/**
 * The transport cap, and the only one there is. Cloudflare's own request body
 * limit belongs to the ACCOUNT PLAN — 100 MB on Free — and no configuration
 * lowers it; a `workers.dev` subdomain is not a zone, so no WAF rule and no
 * rate limiting rule runs in front of this Worker either. Whatever the edge
 * refuses, this file is what refuses it, which is why the refusal happens
 * before a byte of the body is pulled.
 *
 * `MAX_REQUEST_BYTES` lets an operator tighten it and nothing loosen it: a var
 * is public configuration, and configuration must not be able to hand this
 * isolate a bigger buffer than the code was reviewed for.
 */
const REQUEST_BYTES_CEILING = 16384;

function maxRequestBytes(env: Env): number {
  return Math.min(intVar(env.MAX_REQUEST_BYTES, REQUEST_BYTES_CEILING), REQUEST_BYTES_CEILING);
}

/**
 * A device token is interpolated into the APNs request path, so its shape is a
 * security boundary and not a nicety: anything outside these alphabets could
 * steer the relay's authenticated request at another APNs path. Apple's token
 * is hex; Google's is url-safe base64 with a colon.
 */
const TOKEN_SHAPES: Record<Platform, RegExp> = {
  ios: /^[0-9a-fA-F]{32,200}$/,
  ipados: /^[0-9a-fA-F]{32,200}$/,
  android: /^[A-Za-z0-9_:.-]{32,4096}$/,
};

const SAFE_HEADERS: Record<string, string> = {
  "content-type": "application/json; charset=utf-8",
  "cache-control": "no-store",
  "x-content-type-options": "nosniff",
  "referrer-policy": "no-referrer",
  /**
   * Wildcard is safe here precisely because nothing is ambient: there is no
   * cookie and no session, a caller must already hold the grant it presents,
   * and the companion is a WebView that would otherwise be refused by CORS.
   */
  "access-control-allow-origin": "*",
};

function json(status: number, payload: unknown): Response {
  return new Response(JSON.stringify(payload), { status, headers: SAFE_HEADERS });
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

/**
 * The keys that can open a grant. The first one also seals new grants, so a
 * rotation is: move the old value to `RELAY_SEAL_KEY_PREVIOUS`, put a new one
 * in `RELAY_SEAL_KEY`, and drop the previous once every app has re-registered.
 */
function sealKeys(env: Env): string[] {
  return [env.RELAY_SEAL_KEY, env.RELAY_SEAL_KEY_PREVIOUS]
    .map((value) => (value ?? "").trim())
    .filter((value) => value.length > 0);
}

function unsealed(): Response {
  return fail(503, "relay_unconfigured", "this relay has no RELAY_SEAL_KEY and cannot issue or open grants");
}

/** A body big enough to cost CPU is refused before a single byte is parsed. */
const TOO_LARGE = Symbol("too_large");

function isOversized(request: Request, limit: number): boolean {
  const declared = Number.parseInt(request.headers.get("content-length") ?? "0", 10);
  return Number.isFinite(declared) && declared > limit;
}

function oversized(limit: number): Response {
  return fail(413, "too_large", `a request body may not exceed ${limit} bytes`);
}

/**
 * `content-length` is the cheap refusal, but a chunked body declares none, so
 * the bytes are counted as they arrive and the stream is cancelled the instant
 * the cap is passed. Buffering first and measuring after would let a single
 * unauthenticated POST put Cloudflare's whole 100 MB body allowance into a
 * 128 MB isolate, and take every other request sharing it down too.
 *
 * The cancel is deliberate: it stops the upload mid-flight, which is the whole
 * point. `wrangler dev` wraps the worker in a body-draining middleware that
 * then logs "Network connection lost" and can take the local server with it —
 * a dev-only facade, absent from the deployed bundle (`deploy --dry-run
 * --outdir` contains no drainer). Do not remove the cancel to quiet it.
 */
async function readJson(
  request: Request,
  limit: number,
): Promise<Record<string, unknown> | typeof TOO_LARGE | null> {
  const stream = request.body;
  if (!stream) return null;
  const reader = stream.getReader() as ReadableStreamDefaultReader<Uint8Array>;
  const chunks: Uint8Array[] = [];
  let total = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      total += value.byteLength;
      if (total > limit) {
        await reader.cancel();
        return TOO_LARGE;
      }
      chunks.push(value);
    }
  } catch {
    return null;
  }

  const bytes = new Uint8Array(total);
  let at = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, at);
    at += chunk.byteLength;
  }
  try {
    const parsed = JSON.parse(new TextDecoder().decode(bytes)) as unknown;
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
    is_accepting_grants: sealKeys(env).length > 0,
    apns: {
      is_available: apns !== null,
      topic: apns?.topic ?? null,
      environment: apns?.defaultEnvironment ?? null,
    },
    fcm: { is_available: fcm !== null, project_id: fcm?.projectId ?? null },
    // Published so a caller can size a notification up front rather than
    // discover a cap by being refused by it.
    limits: {
      max_request_bytes: maxRequestBytes(env),
      max_apns_payload_bytes: APNS_MAX_PAYLOAD_BYTES,
      max_fcm_payload_bytes: FCM_MAX_PAYLOAD_BYTES,
    },
  });
}

async function createGrantHandler(request: Request, env: Env, deps: Deps): Promise<Response> {
  const { success } = await env.MINT_LIMIT.limit({ key: `mint:${clientKey(request)}` });
  if (!success) return fail(429, "rate_limited", "too many grants from this address");

  const keys = sealKeys(env);
  if (keys.length === 0) return unsealed();

  const limit = maxRequestBytes(env);
  const body = await readJson(request, limit);
  if (body === TOO_LARGE) return oversized(limit);
  if (!body) return fail(400, "bad_request", "a JSON object body is required");

  const deviceToken = str(body.device_token);
  const platform = str(body.platform || "ios") as Platform;
  if (!PLATFORMS.includes(platform)) {
    return fail(400, "bad_request", `platform must be one of ${PLATFORMS.join(", ")}`);
  }
  if (!TOKEN_SHAPES[platform].test(deviceToken)) {
    return fail(400, "bad_request", `device_token is not a valid ${platform} registration token`);
  }

  const expiresAt = deps.now() + intVar(env.GRANT_TTL_DAYS, DEFAULT_GRANT_TTL_DAYS) * 86_400_000;
  const environment = str(body.environment) === "sandbox" ? "sandbox" : "production";
  const grant = await seal(keys[0], { deviceToken, platform, environment, expiresAt });

  return json(201, {
    grant,
    relay_url: new URL(request.url).origin,
    platform,
    environment,
    expires_at: expiresAt,
  });
}

async function pushHandler(request: Request, env: Env, deps: Deps): Promise<Response> {
  /**
   * Before the body is read and before a byte is decrypted: a flood of made-up
   * grants must buy nothing but this one edge-local counter increment.
   */
  const address = await env.PUSH_ADDRESS_LIMIT.limit({ key: `push:${clientKey(request)}` });
  if (!address.success) {
    return fail(429, "rate_limited", "too many push attempts from this address");
  }

  const keys = sealKeys(env);
  if (keys.length === 0) return unsealed();

  const limit = maxRequestBytes(env);
  const parsed = await readJson(request, limit);
  if (parsed === TOO_LARGE) return oversized(limit);
  const body = parsed ?? {};
  const presented = bearer(request) || str(body.grant);
  if (!presented) {
    return fail(401, "no_grant", "an Authorization: Bearer <grant> header is required");
  }

  /**
   * Forged, expired, or sealed under a key that has since been rotated away are
   * one answer with one meaning for the gateway: this grant will never deliver
   * again, forget the device and let the app hand you a new one.
   */
  const grant = await unseal(keys, presented, deps.now());
  if (!grant) return fail(404, "unknown_grant", "this grant is not valid on this relay, or has expired");

  const notification = notificationFrom(body);
  if (!notification) return fail(400, "bad_request", "title or body is required");

  /**
   * Per device, not per grant: grants are free to mint, so a cap on one grant
   * would cap nothing. The fingerprint is a hash — the limiter never sees a
   * device token either.
   */
  const fingerprint = (await sha256Hex(grant.deviceToken)).slice(0, 32);
  const device = await env.PUSH_DEVICE_LIMIT.limit({ key: `device:${fingerprint}` });
  if (!device.success) {
    return fail(429, "rate_limited", "this device is over its push quota");
  }

  const isApple = grant.platform === "ios" || grant.platform === "ipados";
  const apns = isApple ? apnsConfig(env) : null;
  const fcm = isApple ? null : fcmConfig(env);
  if (isApple ? !apns : !fcm) {
    return fail(503, "provider_unconfigured", `this relay cannot push to ${grant.platform}`);
  }

  /**
   * A provider's size verdict is final — Apple's `PayloadTooLarge`, Google's
   * `INVALID_ARGUMENT`, which this relay would even read as a dead device — so
   * the bytes are measured here instead of learned there. A preview is trimmed
   * to what fits; a `data` map that does not fit is the caller's own bug and is
   * answered without spending a round trip on it.
   */
  const payloadLimit = isApple ? APNS_MAX_PAYLOAD_BYTES : FCM_MAX_PAYLOAD_BYTES;
  const fitted = fitNotification(
    notification,
    isApple ? apnsPayload : (n: Notification) => fcmPayload(grant.deviceToken, n),
    payloadLimit,
  );
  if (!fitted) {
    return fail(
      413,
      "payload_too_large",
      `a ${grant.platform} push may carry ${payloadLimit} bytes and this notification's data alone exceeds them`,
    );
  }

  const result = apns
    ? await sendApns(
        apns,
        { deviceToken: grant.deviceToken, environment: grant.environment, notification: fitted },
        deps,
      )
    : await sendFcm(fcm!, { deviceToken: grant.deviceToken, notification: fitted }, deps);

  const isDead =
    result.status === 410 ||
    (result.status === 404 && !isApple) ||
    (isApple ? APNS_DEAD_REASONS.has(result.reason) : FCM_DEAD_REASONS.has(result.reason));

  if (isDead) {
    return json(410, {
      is_delivered: false,
      status: result.status,
      reason: result.reason,
      is_dead: true,
    });
  }

  if (result.status !== 200) {
    return json(502, { is_delivered: false, status: result.status, reason: result.reason });
  }

  return json(200, {
    is_delivered: true,
    status: 200,
    reason: "",
    is_truncated: fitted !== notification,
    environment: result.environment ?? grant.environment,
  });
}

/** The companion is a WebView; a preflight it cannot pass is a broken app. */
function preflight(): Response {
  return new Response(null, {
    status: 204,
    headers: {
      ...SAFE_HEADERS,
      "access-control-allow-methods": "GET, POST, OPTIONS",
      "access-control-allow-headers": "authorization, content-type",
      "access-control-max-age": "86400",
    },
  });
}

async function route(request: Request, env: Env, deps: Deps): Promise<Response> {
  const url = new URL(request.url);
  const path = url.pathname.replace(/\/+$/, "") || "/";
  const method = request.method.toUpperCase();

  const limit = maxRequestBytes(env);
  if (isOversized(request, limit)) return oversized(limit);

  if (method === "OPTIONS") return preflight();
  if (method === "GET" && (path === "/healthz" || path === "/")) return await health(env);
  if (method === "POST" && path === "/v1/grants") return await createGrantHandler(request, env, deps);
  if (method === "POST" && path === "/v1/push") return await pushHandler(request, env, deps);
  return fail(404, "not_found", `${method} ${path} is not a route of this relay`);
}

/**
 * Nothing a caller sends may come back to them as an exception. An unhandled
 * throw is a bare 500 carrying a stack trace — a leak, and a lie about the
 * shape of every other answer this relay gives.
 */
export async function handle(
  request: Request,
  env: Env,
  deps: Deps = defaultDeps,
): Promise<Response> {
  try {
    return await route(request, env, deps);
  } catch {
    return fail(500, "internal_error", "the relay failed to handle this request");
  }
}

export default {
  fetch(request: Request, env: Env): Promise<Response> {
    return handle(request, env, defaultDeps);
  },
};
