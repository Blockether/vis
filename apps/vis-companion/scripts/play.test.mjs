import { generateKeyPairSync } from 'node:crypto';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { parseTracks, planRelease, promoteBundle, publishBundle, TESTING_TRACKS } from './play.mjs';

// `play.mjs` is the ONE place that talks to the Play Developer API, so the only thing worth
// stubbing is the transport: every release path inherits whatever policy lives here.
const respond = (entry) => {
  if (entry instanceof Error) throw entry;
  const body = entry.body ?? {};
  return {
    ok: entry.status >= 200 && entry.status < 300,
    status: entry.status,
    text: async () => JSON.stringify(body),
    json: async () => body,
  };
};

/** Queue one outcome per request, in order; returns the log of what Play was asked to do. */
const transport = (...queue) => {
  const seen = [];
  globalThis.fetch = vi.fn(async (url, init) => {
    seen.push({ url, method: init.method, body: init.body });
    if (!queue.length) throw new Error(`unexpected extra request to ${url}`);
    return respond(queue.shift());
  });
  return seen;
};

const ok = (body) => ({ status: 200, body });
const token = () => ok({ access_token: 'tok' });
const edit = () => ok({ id: 'edit-1' });

// A real RSA key: playToken signs the assertion for real, so the test covers the signing path
// instead of a stub of it. The identity is a placeholder — no real credential is involved.
const { privateKey } = generateKeyPairSync('rsa', {
  modulusLength: 2048,
  privateKeyEncoding: { type: 'pkcs8', format: 'pem' },
  publicKeyEncoding: { type: 'spki', format: 'pem' },
});
const serviceAccount = JSON.stringify({
  type: 'service_account',
  client_email: 'releases@example.com',
  private_key: privateKey,
  token_uri: 'https://oauth2.example.com/token',
});

const packageName = 'com.example.companion';
const release = (extra = {}) => ({ serviceAccount, packageName, log: () => {}, ...extra });
const trackPuts = (seen) =>
  seen.filter((r) => r.method === 'PUT').map((r) => ({ url: r.url, ...JSON.parse(r.body) }));

const realFetch = globalThis.fetch;
afterEach(() => {
  globalThis.fetch = realFetch;
  vi.restoreAllMocks();
});

describe('parseTracks', () => {
  it('defaults to every tester channel and keeps them in Play order', () => {
    expect(parseTracks(undefined)).toEqual(['internal', 'alpha', 'beta']);
    expect(parseTracks([])).toEqual(TESTING_TRACKS);
    expect(parseTracks('beta,internal')).toEqual(['internal', 'beta']);
  });

  it('takes a repeated flag, trims it, and drops the duplicates', () => {
    expect(parseTracks(['internal', ' beta , internal '])).toEqual(['internal', 'beta']);
  });

  it('refuses a track Play does not have', () => {
    expect(() => parseTracks('nightly')).toThrow(/unknown track "nightly"/);
  });
});

describe('planRelease', () => {
  it('names the release, caps the notes at what Play accepts, and marks a full rollout', () => {
    const plan = planRelease({ tracks: 'internal', releaseName: '0.1.35 (4090)', notes: 'x'.repeat(700) });
    expect(plan.status).toBe('completed');
    expect(plan.release(4090)).toMatchObject({ name: '0.1.35 (4090)', versionCodes: ['4090'] });
    expect(plan.release(4090).releaseNotes[0].text).toHaveLength(500);
  });

  it('stages a fraction as inProgress, and a draft never carries one', () => {
    expect(planRelease({ tracks: 'beta', userFraction: '0.1' }).release(1)).toMatchObject({ status: 'inProgress', userFraction: 0.1 });
    expect(planRelease({ tracks: 'beta', userFraction: '0.1', draft: true }).release(1)).toEqual(
      expect.not.objectContaining({ userFraction: expect.anything() }),
    );
  });

  // Play stages a rollout per track: one fraction spread over three tracks has no meaning,
  // and the CLI plans before the build so this costs a second instead of a signed .aab.
  it('refuses a staged rollout aimed at more than one track', () => {
    expect(() => planRelease({ userFraction: '0.1' })).toThrow(/exactly one track/);
  });
});

describe('publishBundle', () => {
  // Regression: releases went out one track at a time, so internal served 0.1.21 (2861) while
  // beta already served 0.1.35 (4075) — which build a tester got depended on their list, and
  // lining the tracks up again took a second, manual promote.
  it('uploads once and puts that build on every tester track inside ONE edit', async () => {
    const seen = transport(token(), edit(), ok({ versionCode: 4090, sha1: 'abcdef123456' }), ok({}), ok({}), ok({}), ok({}));

    const res = await publishBundle(release({ aab: Buffer.from('aab'), releaseName: '0.1.35 (4090)', notes: 'fixes' }));

    expect(res).toMatchObject({ ok: true, versionCode: '4090', tracks: ['internal', 'alpha', 'beta'], status: 'completed', editId: 'edit-1' });
    expect(trackPuts(seen).map((p) => p.track)).toEqual(['internal', 'alpha', 'beta']);
    // Same edit, same build, same notes on all three — nothing can drift between tracks.
    for (const put of trackPuts(seen)) {
      expect(put.url).toContain('/edits/edit-1/tracks/');
      expect(put.releases[0]).toMatchObject({ name: '0.1.35 (4090)', versionCodes: ['4090'], status: 'completed' });
    }
    expect(seen.filter((r) => r.url.endsWith(':commit'))).toHaveLength(1);
    expect(seen.filter((r) => r.url.includes('/bundles?'))).toHaveLength(1);
  });

  it('honours an explicit subset', async () => {
    const seen = transport(token(), edit(), ok({ versionCode: 4090 }), ok({}), ok({}), ok({}));

    const res = await publishBundle(release({ aab: Buffer.from('aab'), tracks: 'production,internal' }));

    expect(res.tracks).toEqual(['internal', 'production']);
    expect(trackPuts(seen).map((p) => p.track)).toEqual(['internal', 'production']);
  });

  // A half-published release — the new build on internal, the old one still on beta — is
  // exactly what the single edit exists to prevent, so a refused track must publish NOTHING.
  it('rolls the whole edit back when one track is refused, and commits nothing', async () => {
    const seen = transport(token(), edit(), ok({ versionCode: 4090 }), ok({}), { status: 403, body: { error: { message: 'no access' } } }, ok({}));

    await expect(publishBundle(release({ aab: Buffer.from('aab') }))).rejects.toThrow(/403 no access/);

    expect(seen.filter((r) => r.url.endsWith(':commit'))).toHaveLength(0);
    expect(seen.at(-1)).toMatchObject({ method: 'DELETE' });
  });

  it('plans before it authenticates, so an impossible ask costs no request', async () => {
    const seen = transport();
    await expect(publishBundle(release({ aab: Buffer.from('aab'), userFraction: '0.1' }))).rejects.toThrow(/exactly one track/);
    expect(seen).toEqual([]);
  });
});

describe('promoteBundle', () => {
  it('lines the named tracks up with an existing build without re-uploading it', async () => {
    const seen = transport(token(), edit(), ok({}), ok({}), ok({}));

    const res = await promoteBundle(release({ versionCode: '4090', tracks: 'alpha,beta', releaseName: '0.1.35 (4090)' }));

    expect(res).toMatchObject({ versionCode: '4090', tracks: ['alpha', 'beta'], status: 'completed' });
    expect(seen.some((r) => r.url.includes('/bundles?'))).toBe(false);
    expect(trackPuts(seen).map((p) => p.releases[0].versionCodes)).toEqual([['4090'], ['4090']]);
  });

  it('refuses anything that is not a version code', async () => {
    const seen = transport();
    await expect(promoteBundle(release({ versionCode: 'latest' }))).rejects.toThrow(/versionCode must be a positive integer/);
    expect(seen).toEqual([]);
  });
});
