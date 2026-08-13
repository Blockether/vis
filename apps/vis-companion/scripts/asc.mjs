#!/usr/bin/env node
// Shared App Store Connect client. One place that knows how to mint the ES256 JWT and
// how the JSON:API error shape reads, so release-notes.mjs (What to Test) and
// testflight.mjs (public link distribution) cannot drift apart.
//
// Credentials are never read here — callers pass keyId/issuerId/keyPem, which they
// resolve env-first then from the macOS login keychain (scripts/secrets.mjs), and hand
// `asc` a MINT (`() => ascToken(credentials)`) so any attempt can sign a fresh JWT.

import { createPrivateKey, sign as cryptoSign } from 'node:crypto';

const base64url = (buf) => Buffer.from(buf).toString('base64').replace(/=+$/, '').replace(/\+/g, '-').replace(/\//g, '_');

/** ES256 JWT. `dsaEncoding: 'ieee-p1363'` is the raw r||s form JOSE wants; DER is rejected. */
export const ascToken = ({ keyId, issuerId, keyPem }) => {
  const now = Math.floor(Date.now() / 1000);
  const header = base64url(JSON.stringify({ alg: 'ES256', kid: keyId, typ: 'JWT' }));
  const payload = base64url(JSON.stringify({ iss: issuerId, iat: now, exp: now + 20 * 60, aud: 'appstoreconnect-v1' }));
  const signature = base64url(
    cryptoSign('sha256', Buffer.from(`${header}.${payload}`), {
      key: createPrivateKey(keyPem),
      dsaEncoding: 'ieee-p1363',
    }),
  );
  return `${header}.${payload}.${signature}`;
};

export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── the transport ─────────────────────────────────────────────────────────────────────
// App Store Connect fails transiently often enough that calling it once is a bug, and the
// call it strands is usually the LAST one of a release: the .ipa is already uploaded, so a
// single hiccup costs the "What to Test" notes or the beta-group link rather than the
// build. Three shapes have actually been seen:
//   · `TypeError: fetch failed` — undici wrapping a dropped socket or a DNS blip. It threw
//     straight out of the notes publish of 0.1.35 (4075), seconds after the same token had
//     read the app, and the release reported `notes not published: fetch failed`.
//   · 401 on a token minted moments earlier (run 30766271157), accepted on a replay.
//   · 429 / 5xx from Apple's edge, sometimes with an HTML page instead of JSON:API errors.
// The retry therefore lives HERE, in the one function that owns the socket, and never in a
// caller: a per-caller wrapper is how the notes path ended up without one.
const API = 'https://api.appstoreconnect.apple.com';
const BACKOFF_MS = [1_000, 4_000, 10_000];
const RETRYABLE = new Set([401, 408, 425, 429, 500, 502, 503, 504]);
const RETRY_AFTER_CAP_MS = 60_000;

/** Apple's edge answers some 5xx with an HTML page; its status is then the only fact in it. */
const parseJson = (text) => {
  try {
    return text ? JSON.parse(text) : {};
  } catch {
    return {};
  }
};

/** How long Apple asked us to wait, when it bothered to say. */
const retryAfterMs = (res) => {
  const seconds = Number(res.headers?.get?.('retry-after'));
  return Number.isFinite(seconds) && seconds > 0 ? Math.min(seconds * 1_000, RETRY_AFTER_CAP_MS) : undefined;
};

/** One round trip. Transport failure and non-2xx come back as DATA, so the loop can decide. */
const attempt = async (mint, method, path, body) => {
  let res;
  let text;
  try {
    res = await fetch(`${API}${path}`, {
      method,
      headers: {
        Authorization: `Bearer ${mint()}`,
        ...(body ? { 'Content-Type': 'application/json' } : {}),
      },
      ...(body ? { body: JSON.stringify(body) } : {}),
    });
    text = await res.text();
  } catch (cause) {
    // `fetch` throws only for the transport itself — DNS, TLS, a socket that went away.
    const code = cause.cause?.code ? ` (${cause.cause.code})` : '';
    const err = new Error(`ASC ${method} ${path} → ${cause.message}${code}`);
    err.cause = cause;
    return { err, retryable: true };
  }
  const json = parseJson(text);
  if (res.ok) return { json };
  const detail = json.errors?.map((e) => `${e.title}: ${e.detail}`).join('; ') || text.trim().slice(0, 200) || res.statusText;
  const err = new Error(`ASC ${method} ${path} → ${res.status} ${detail}`);
  err.status = res.status;
  err.codes = json.errors?.map((e) => e.code) ?? [];
  return { err, retryable: RETRYABLE.has(res.status), waitMs: retryAfterMs(res) };
};

/**
 * One ASC request, retried through Apple's transient failures (four attempts, ~15s of
 * backoff at most, `Retry-After` honoured when Apple sends one).
 *
 * `mint` is a token FACTORY, never a token: every attempt signs a fresh JWT, which is what
 * makes the 401 replay work and lets a caller keep polling past Apple's 20-minute token
 * lifetime. Throws on a permanent non-2xx with Apple's own error text attached, carrying
 * `status` + `codes` so callers can treat "already exists" / "already submitted" as success
 * instead of parsing prose. `wait` exists for the tests; production never passes it.
 */
export const asc = async (mint, method, path, body, { wait = sleep } = {}) => {
  for (let n = 0; ; n += 1) {
    const { json, err, retryable, waitMs } = await attempt(mint, method, path, body);
    if (!err) return json;
    if (!retryable || n >= BACKOFF_MS.length) throw err;
    await wait(waitMs ?? BACKOFF_MS[n]);
  }
};

/** App id for a bundle id, or undefined when the API key's team does not own it. */
export const appIdFor = async (mint, bundleId) => {
  const apps = await asc(mint, 'GET', `/v1/apps?filter[bundleId]=${encodeURIComponent(bundleId)}&limit=1`);
  return apps.data?.[0]?.id;
};

/**
 * Wait for an uploaded build to become visible, and optionally for Apple to finish
 * processing it (`processingState: VALID`). Uploads are async: the build simply does not
 * exist for the first minutes, then exists as PROCESSING, and only a VALID build can be
 * linked to a beta group. `timeoutMs: 0` polls exactly once.
 */
export const waitForBuild = async (mint, { appId, build, timeoutMs = 15 * 60 * 1000, requireValid = false, log = () => {} }) => {
  const query = `/v1/builds?filter[app]=${appId}&filter[version]=${encodeURIComponent(build)}&limit=1`;
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const found = (await asc(mint, 'GET', query)).data?.[0];
    const state = found?.attributes?.processingState;
    if (found && (!requireValid || state === 'VALID')) return { id: found.id, state };
    if (found && (state === 'INVALID' || state === 'FAILED')) {
      throw new Error(`build ${build} was rejected during processing (${state}) — check App Store Connect`);
    }
    if (Date.now() >= deadline) return found ? { id: found.id, state } : undefined;
    log(found ? `waiting for App Store Connect to finish processing build ${build} (${state}) …` : `waiting for App Store Connect to ingest build ${build} …`);
    await sleep(30_000);
  }
};
