#!/usr/bin/env node
// Shared App Store Connect client. One place that knows how to mint the ES256 JWT and
// how the JSON:API error shape reads, so release-notes.mjs (What to Test) and
// testflight.mjs (public link distribution) cannot drift apart.
//
// Credentials are never read here — callers pass keyId/issuerId/keyPem, which they
// resolve env-first then from the macOS login keychain (scripts/secrets.mjs).

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

/**
 * One ASC request. Throws on a non-2xx with Apple's own error text attached, and carries
 * `status` + `codes` so callers can treat "already exists" / "already submitted" as success
 * instead of parsing prose.
 */
export const asc = async (token, method, path, body) => {
  const res = await fetch(`https://api.appstoreconnect.apple.com${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${token}`,
      ...(body ? { 'Content-Type': 'application/json' } : {}),
    },
    ...(body ? { body: JSON.stringify(body) } : {}),
  });
  const text = await res.text();
  const json = text ? JSON.parse(text) : {};
  if (!res.ok) {
    const detail = json.errors?.map((e) => `${e.title}: ${e.detail}`).join('; ') || text;
    const err = new Error(`ASC ${method} ${path} → ${res.status} ${detail}`);
    err.status = res.status;
    err.codes = json.errors?.map((e) => e.code) ?? [];
    throw err;
  }
  return json;
};

export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/** App id for a bundle id, or undefined when the API key's team does not own it. */
export const appIdFor = async (token, bundleId) => {
  const apps = await asc(token, 'GET', `/v1/apps?filter[bundleId]=${encodeURIComponent(bundleId)}&limit=1`);
  return apps.data?.[0]?.id;
};

/**
 * Wait for an uploaded build to become visible, and optionally for Apple to finish
 * processing it (`processingState: VALID`). Uploads are async: the build simply does not
 * exist for the first minutes, then exists as PROCESSING, and only a VALID build can be
 * linked to a beta group. `timeoutMs: 0` polls exactly once.
 */
export const waitForBuild = async (token, { appId, build, timeoutMs = 15 * 60 * 1000, requireValid = false, log = () => {} }) => {
  const query = `/v1/builds?filter[app]=${appId}&filter[version]=${encodeURIComponent(build)}&limit=1`;
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const found = (await asc(token, 'GET', query)).data?.[0];
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
