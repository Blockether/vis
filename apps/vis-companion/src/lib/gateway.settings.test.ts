// A settings catalog carries a revision. The client refuses a catalog without one
// and caches nothing, so a stale gateway cannot fill the settings screen.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { SettingsResponse, SettingsTarget } from './types';

const conn = { url: 'http://gateway.example.com:7890' };
const target: SettingsTarget = { scope: 'session', target_id: 'a/b' };

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { 'Content-Type': 'application/json' },
  });

const catalog = (revision: string | undefined, enabled: boolean) =>
  ({
    revision,
    scope: 'session',
    target_id: 'a/b',
    groups: [
      {
        id: 'agent',
        title: 'Agent',
        toggles: [{ id: 'plans', label: 'Plans', type: 'boolean', enabled, source: 'session' }],
      },
    ],
  }) as SettingsResponse;

async function relaunch(fetching: typeof fetch) {
  vi.resetModules();
  vi.stubGlobal('fetch', fetching);
  return await import('./gateway');
}

beforeEach(() => {
  localStorage.clear();
  // A `node` test file has no window; the client reaches for its timers.
  vi.stubGlobal('window', globalThis);
  vi.resetModules();
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('settings catalog revision', () => {
  it('refuses a catalog without a revision and caches nothing', async () => {
    const fetching = vi.fn(() => Promise.resolve(json(catalog(undefined, false))));
    const { GatewayClient } = await relaunch(fetching as unknown as typeof fetch);
    const client = new GatewayClient(conn);

    await expect(client.settings(undefined, target)).rejects.toThrow(
      'Update the gateway and reconnect.',
    );
    expect(client.cachedSettings(target)).toBeNull();
  });
});

describe('extension reload', () => {
  // #302: reload sends only the settings owner; reading the catalog never runs code.
  it('posts the settings target and returns the load counts', async () => {
    const fetching = vi.fn(() => Promise.resolve(json({ loaded: 2, failed: 1 })));
    const { GatewayClient } = await relaunch(fetching as unknown as typeof fetch);
    const client = new GatewayClient(conn);

    await expect(
      client.reloadExtensions({ scope: 'project', target_id: 'p', label: 'Workspace' }),
    ).resolves.toEqual({ loaded: 2, failed: 1 });
    await client.reloadExtensions();

    const calls = fetching.mock.calls as unknown as [RequestInfo | URL, RequestInit][];
    const posts = calls.filter(([url]) => String(url).endsWith('/v1/extensions/reload'));
    expect(posts.map(([, init]) => [init.method, JSON.parse(String(init.body))])).toEqual([
      ['POST', { scope: 'project', target_id: 'p' }],
      ['POST', { scope: 'global' }],
    ]);
  });
});
