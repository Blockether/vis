import { describe, expect, it, vi } from 'vitest';

// The transport is stubbed, never the wiring: what this file pins is HOW publishNotes calls
// App Store Connect, because that is what silently diverged from the distribution path.
let localizations = [];
const calls = [];
vi.mock('./asc.mjs', () => ({
  ascToken: ({ keyId }) => `jwt-for-${keyId}`,
  appIdFor: async (mint) => {
    calls.push({ op: 'appIdFor', mint });
    return 'app-1';
  },
  waitForBuild: async (mint) => {
    calls.push({ op: 'waitForBuild', mint });
    return { id: 'build-1', state: 'VALID' };
  },
  asc: async (mint, method, path, body) => {
    calls.push({ op: `${method} ${path}`, mint, body });
    return path.includes('betaBuildLocalizations?') ? { data: localizations } : {};
  },
}));

import { publishNotes } from './release-notes.mjs';

const publish = (notes = '• Something a tester can see') =>
  publishNotes({
    keyId: 'K1',
    issuerId: 'I1',
    keyPem: 'PEM',
    bundleId: 'com.blockether.viscompanion',
    version: '0.1.35',
    build: '4075',
    notes,
    log: () => {},
  });

const reset = () => {
  calls.length = 0;
  localizations = [];
};

describe('publishNotes', () => {
  it('rewrites the en-US What to Test the build already has', async () => {
    reset();
    localizations = [{ id: 'loc-1', attributes: { locale: 'en-US' } }];

    await expect(publish('• Fixed the flicker')).resolves.toEqual({ ok: true, buildId: 'build-1', version: '0.1.35', build: '4075' });

    expect(calls.at(-1)).toMatchObject({
      op: 'PATCH /v1/betaBuildLocalizations/loc-1',
      body: { data: { type: 'betaBuildLocalizations', id: 'loc-1', attributes: { whatsNew: '• Fixed the flicker' } } },
    });
  });

  it('creates the localization when the build has none', async () => {
    reset();

    await expect(publish('• First notes')).resolves.toMatchObject({ ok: true });

    expect(calls.at(-1)).toMatchObject({
      op: 'POST /v1/betaBuildLocalizations',
      body: {
        data: {
          attributes: { locale: 'en-US', whatsNew: '• First notes' },
          relationships: { build: { data: { type: 'builds', id: 'build-1' } } },
        },
      },
    });
  });

  // Regression, session 0ec1e9f3-23d5-4070-a17e-46f8e7f514e8: this path minted ONE token and
  // handed the STRING to every call, so it could neither replay a 401 nor retry a dropped
  // socket the way the distribution path could — and one `fetch failed` from Apple lost the
  // "What to Test" text of build 4075, which was already uploaded and unrepeatable.
  it('hands every App Store Connect call a token MINT, never a token', async () => {
    reset();

    await publish();

    expect(calls.map((c) => c.op)).toEqual([
      'appIdFor',
      'waitForBuild',
      'GET /v1/builds/build-1/betaBuildLocalizations?limit=50',
      'POST /v1/betaBuildLocalizations',
    ]);
    for (const { op, mint } of calls) {
      expect(typeof mint, op).toBe('function');
      expect(mint(), op).toBe('jwt-for-K1');
    }
  });

  it('refuses without credentials or notes instead of calling Apple', async () => {
    reset();

    await expect(publish('   ')).resolves.toEqual({ ok: false, reason: 'no notes' });
    await expect(publishNotes({ bundleId: 'x', notes: '• a', log: () => {} })).resolves.toEqual({
      ok: false,
      reason: 'no App Store Connect API key',
    });
    expect(calls).toEqual([]);
  });
});
