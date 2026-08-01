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
// the public one, the true TestFlight-public analogue), production.

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

/**
 * Upload an .aab and roll it out on `track`, atomically: Play edits are transactional, so
 * either the commit lands with the bundle AND the track assignment, or nothing changed at
 * all. A half-published release is therefore not a state this can produce.
 *
 * `userFraction` staged-rolls the release (0 < f < 1 ⇒ status inProgress); omit it for a
 * full rollout. `draft: true` uploads without releasing — the Play Console "draft" state.
 */
export const publishBundle = async ({
  serviceAccount,
  packageName,
  aab,
  track = 'internal',
  releaseName,
  notes,
  locale = 'en-US',
  userFraction,
  draft = false,
  log = console.log,
}) => {
  const { token, account } = await playToken(serviceAccount);
  log(`· authenticated as ${account}`);

  const edit = await call(token, 'POST', `/applications/${packageName}/edits`);
  log(`· edit ${edit.id}`);

  let committed = false;
  try {
    const uploaded = await call(token, 'POST', `/applications/${packageName}/edits/${edit.id}/bundles?uploadType=media`, {
      body: aab,
      contentType: 'application/octet-stream',
      base: UPLOAD,
    });
    const versionCode = uploaded.versionCode;
    log(`· uploaded versionCode ${versionCode} (sha1 ${uploaded.sha1?.slice(0, 12) ?? '?'}…)`);

    const status = draft ? 'draft' : userFraction ? 'inProgress' : 'completed';
    const release = {
      name: releaseName ?? String(versionCode),
      versionCodes: [String(versionCode)],
      status,
      ...(userFraction && !draft ? { userFraction: Number(userFraction) } : {}),
      ...(notes?.trim() ? { releaseNotes: [{ language: locale, text: notes.trim().slice(0, 500) }] } : {}),
    };
    await call(token, 'PUT', `/applications/${packageName}/edits/${edit.id}/tracks/${track}`, { body: { track, releases: [release] } });
    log(`· track ${track} ← ${release.name} (${status})`);

    await call(token, 'POST', `/applications/${packageName}/edits/${edit.id}:commit`);
    committed = true;
    return { ok: true, versionCode, track, status, editId: edit.id };
  } finally {
    // An abandoned edit is not merely untidy: Play refuses a NEW edit while a stale one
    // holds the app, so the next release would fail on a failure that already happened.
    if (!committed) {
      await call(token, 'DELETE', `/applications/${packageName}/edits/${edit.id}`).catch(() => {});
      log('· edit rolled back (nothing was published)');
    }
  }
};

/**
 * Roll an already-uploaded bundle onto another track without re-uploading its versionCode.
 * This is the safe recovery path after a successful upload landed on the wrong track.
 */
export const promoteBundle = async ({
  serviceAccount,
  packageName,
  versionCode,
  track = 'beta',
  releaseName,
  notes,
  locale = 'en-US',
  userFraction,
  draft = false,
  log = console.log,
}) => {
  if (!/^\d+$/.test(String(versionCode))) throw new Error(`versionCode must be a positive integer, got "${versionCode}"`);

  const { token, account } = await playToken(serviceAccount);
  log(`· authenticated as ${account}`);

  const edit = await call(token, 'POST', `/applications/${packageName}/edits`);
  log(`· edit ${edit.id}`);

  let committed = false;
  try {
    const status = draft ? 'draft' : userFraction ? 'inProgress' : 'completed';
    const release = {
      name: releaseName ?? String(versionCode),
      versionCodes: [String(versionCode)],
      status,
      ...(userFraction && !draft ? { userFraction: Number(userFraction) } : {}),
      ...(notes?.trim() ? { releaseNotes: [{ language: locale, text: notes.trim().slice(0, 500) }] } : {}),
    };
    await call(token, 'PUT', `/applications/${packageName}/edits/${edit.id}/tracks/${track}`, { body: { track, releases: [release] } });
    log(`· existing versionCode ${versionCode} → track ${track} (${status})`);

    await call(token, 'POST', `/applications/${packageName}/edits/${edit.id}:commit`);
    committed = true;
    return { ok: true, versionCode: String(versionCode), track, status, editId: edit.id };
  } finally {
    if (!committed) {
      await call(token, 'DELETE', `/applications/${packageName}/edits/${edit.id}`).catch(() => {});
      log('· edit rolled back (nothing was published)');
    }
  }
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
