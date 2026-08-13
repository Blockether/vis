#!/usr/bin/env node
// Google Play Developer API v3 client — the Play half of what App Store Connect does for
// iOS. Deliberately plain `fetch` + `node:crypto` instead of fastlane/googleapis: the
// whole protocol is four calls, and this keeps the release path free of a Ruby toolchain
// and of a dependency that would have to be audited for credential handling.
//
// Auth is a self-signed service-account JWT (RS256) exchanged for an access token — the
// same grant googleapis uses internally. The service-account JSON never touches disk: it
// is read from the login keychain (scripts/secrets.mjs) or an env var and stays in memory.
//
// Track names Play understands: internal, alpha (closed testing), beta (OPEN testing —
// the public one, the true TestFlight-public analogue), production. A release names as many
// as it likes: they are written in ONE edit, so every tester channel serves one build.

import { createPrivateKey, sign as cryptoSign } from 'node:crypto';

const SCOPE = 'https://www.googleapis.com/auth/androidpublisher';
const API = 'https://androidpublisher.googleapis.com/androidpublisher/v3';
const UPLOAD = 'https://androidpublisher.googleapis.com/upload/androidpublisher/v3';

const base64url = (buf) => Buffer.from(buf).toString('base64').replace(/=+$/, '').replace(/\+/g, '-').replace(/\//g, '_');

/** Service-account JWT → OAuth2 access token. Valid an hour; a release never outlives it. */
export const playToken = async (serviceAccount) => {
  const sa = typeof serviceAccount === 'string' ? JSON.parse(serviceAccount) : serviceAccount;
  if (sa.type !== 'service_account' || !sa.private_key || !sa.client_email) {
    throw new Error('that JSON is not a Google service-account key (needs type, client_email, private_key)');
  }
  const now = Math.floor(Date.now() / 1000);
  const header = base64url(JSON.stringify({ alg: 'RS256', typ: 'JWT' }));
  const claims = base64url(
    JSON.stringify({ iss: sa.client_email, scope: SCOPE, aud: sa.token_uri ?? 'https://oauth2.googleapis.com/token', iat: now, exp: now + 3600 }),
  );
  const signature = base64url(cryptoSign('RSA-SHA256', Buffer.from(`${header}.${claims}`), createPrivateKey(sa.private_key)));
  const assertion = `${header}.${claims}.${signature}`;

  const res = await fetch(sa.token_uri ?? 'https://oauth2.googleapis.com/token', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({ grant_type: 'urn:ietf:params:oauth:grant-type:jwt-bearer', assertion }),
  });
  const json = await res.json();
  if (!res.ok) throw new Error(`Google OAuth → ${res.status} ${json.error_description ?? json.error ?? ''}`);
  return { token: json.access_token, account: sa.client_email };
};

const call = async (token, method, path, { body, contentType = 'application/json', base = API } = {}) => {
  const res = await fetch(`${base}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${token}`,
      ...(body === undefined ? {} : { 'Content-Type': contentType }),
    },
    ...(body === undefined ? {} : { body: contentType === 'application/json' ? JSON.stringify(body) : body }),
  });
  const text = await res.text();
  const json = text ? JSON.parse(text) : {};
  if (!res.ok) {
    const err = new Error(`Play ${method} ${path} → ${res.status} ${json.error?.message ?? text}`);
    err.status = res.status;
    err.reason = json.error?.errors?.[0]?.reason;
    throw err;
  }
  return json;
};

/** Every track Play knows, lowest to highest. */
export const PLAY_TRACKS = ['internal', 'alpha', 'beta', 'production'];

/**
 * The default fan-out. A build testers can install is the SAME build on every tester
 * channel: publishing to one track and leaving the rest behind is how `internal` ended up
 * serving 0.1.21 while `beta` served 0.1.35. `production` is never implied — it is asked for.
 */
export const TESTING_TRACKS = ['internal', 'alpha', 'beta'];

/**
 * `internal,beta`, `['internal', 'beta']` or nothing at all → the tracks to write, unique and
 * ordered internal → production. Asking for nothing is asking for the tester fan-out.
 */
export const parseTracks = (values) => {
  const asked = (Array.isArray(values) ? values : [values])
    .flatMap((v) => (v == null ? [] : String(v).split(',')))
    .map((v) => v.trim())
    .filter(Boolean);
  if (!asked.length) return [...TESTING_TRACKS];
  const unknown = asked.find((t) => !PLAY_TRACKS.includes(t));
  if (unknown) throw new Error(`unknown track "${unknown}" (${PLAY_TRACKS.join(' | ')})`);
  return PLAY_TRACKS.filter((t) => asked.includes(t));
};

/**
 * Everything decided before Play is touched: which tracks the release lands on, its status,
 * and how to shape it once a version code exists. Publish and promote both plan through here
 * so the rules are stated once, and the CLI can plan BEFORE a ten-minute Gradle build and
 * refuse an impossible combination in a second instead of after the .aab is signed.
 *
 * `userFraction` staged-rolls the release (0 < f < 1 ⇒ status inProgress); omit it for a full
 * rollout. `draft: true` uploads without releasing — the Play Console "draft" state.
 */
export const planRelease = ({ tracks, releaseName, notes, locale = 'en-US', userFraction, draft = false }) => {
  const wanted = parseTracks(tracks);
  // Play stages a rollout per track, so one fraction shared by several tracks is not a thing
  // the API can express — and guessing which track the fraction meant is worse than refusing.
  if (userFraction && wanted.length > 1) throw new Error(`a staged rollout targets exactly one track, not ${wanted.join(', ')}`);
  const status = draft ? 'draft' : userFraction ? 'inProgress' : 'completed';
  return {
    tracks: wanted,
    status,
    release: (versionCode) => ({
      name: releaseName ?? String(versionCode),
      versionCodes: [String(versionCode)],
      status,
      ...(userFraction && !draft ? { userFraction: Number(userFraction) } : {}),
      ...(notes?.trim() ? { releaseNotes: [{ language: locale, text: notes.trim().slice(0, 500) }] } : {}),
    }),
  };
};

/**
 * Run `body` inside ONE Play edit, then commit it. Play edits are transactional, so either
 * the commit lands with the bundle AND every track assignment, or nothing changed at all: a
 * half-published release — the new build on internal, an older one on beta — is not a state
 * this can produce.
 *
 * An abandoned edit is not merely untidy: Play refuses a NEW edit while a stale one holds the
 * app, so the next release would fail on a failure that already happened.
 */
const withEdit = async ({ token, packageName, log }, body) => {
  const edit = await call(token, 'POST', `/applications/${packageName}/edits`);
  log(`· edit ${edit.id}`);

  let committed = false;
  try {
    const result = await body(edit.id);
    await call(token, 'POST', `/applications/${packageName}/edits/${edit.id}:commit`);
    committed = true;
    return { ...result, editId: edit.id };
  } finally {
    if (!committed) {
      await call(token, 'DELETE', `/applications/${packageName}/edits/${edit.id}`).catch(() => {});
      log('· edit rolled back (nothing was published)');
    }
  }
};

/** Put the one release on every planned track, inside the caller's edit. */
const assignTracks = async ({ token, packageName, editId, plan, versionCode, log }) => {
  const release = plan.release(versionCode);
  for (const track of plan.tracks) {
    await call(token, 'PUT', `/applications/${packageName}/edits/${editId}/tracks/${track}`, { body: { track, releases: [release] } });
    log(`· track ${track} ← ${release.name} (${release.status})`);
  }
  return { ok: true, versionCode: String(versionCode), tracks: plan.tracks, status: plan.status };
};

/** Upload an .aab and roll it out on every planned track, atomically. */
export const publishBundle = async ({ serviceAccount, packageName, aab, log = console.log, ...plan }) => {
  const planned = planRelease(plan);
  const { token, account } = await playToken(serviceAccount);
  log(`· authenticated as ${account}`);

  return withEdit({ token, packageName, log }, async (editId) => {
    const uploaded = await call(token, 'POST', `/applications/${packageName}/edits/${editId}/bundles?uploadType=media`, {
      body: aab,
      contentType: 'application/octet-stream',
      base: UPLOAD,
    });
    log(`· uploaded versionCode ${uploaded.versionCode} (sha1 ${uploaded.sha1?.slice(0, 12) ?? '?'}…)`);
    return assignTracks({ token, packageName, editId, plan: planned, versionCode: uploaded.versionCode, log });
  });
};

/**
 * Roll an ALREADY-uploaded bundle onto tracks without re-uploading its version code: the
 * recovery path after an upload landed on the wrong track, and the cheap way to line a
 * lagging track up with the build the others already serve.
 */
export const promoteBundle = async ({ serviceAccount, packageName, versionCode, log = console.log, ...plan }) => {
  if (!/^\d+$/.test(String(versionCode))) throw new Error(`versionCode must be a positive integer, got "${versionCode}"`);
  const planned = planRelease(plan);
  const { token, account } = await playToken(serviceAccount);
  log(`· authenticated as ${account}`);

  return withEdit({ token, packageName, log }, async (editId) => {
    log(`· reusing versionCode ${versionCode}`);
    return assignTracks({ token, packageName, editId, plan: planned, versionCode, log });
  });
};

/** Read back what each track currently serves — the post-release proof, and a dry-run probe. */
export const tracks = async ({ serviceAccount, packageName }) => {
  const { token } = await playToken(serviceAccount);
  const edit = await call(token, 'POST', `/applications/${packageName}/edits`);
  try {
    const { tracks: found = [] } = await call(token, 'GET', `/applications/${packageName}/edits/${edit.id}/tracks`);
    return found.map((t) => ({
      track: t.track,
      releases: (t.releases ?? []).map((r) => ({ name: r.name, status: r.status, versionCodes: r.versionCodes, userFraction: r.userFraction })),
    }));
  } finally {
    await call(token, 'DELETE', `/applications/${packageName}/edits/${edit.id}`).catch(() => {});
  }
};
