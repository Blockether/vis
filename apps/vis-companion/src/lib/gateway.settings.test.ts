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
        toggles: [
          { id: 'subagents', label: 'Subagents', type: 'boolean', enabled, source: 'session' },
        ],
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

  // #393: extension code registers providers, so a reload must not leave the old
  // fleet in the router cache for `ROUTER_TTL_MS`.
  it('drops the cached provider fleet and tells mounted lists, also when the reload fails', async () => {
    let fleet = ['anthropic'];
    let reloadStatus = 200;
    const fetching = vi.fn((url: RequestInfo | URL) =>
      Promise.resolve(
        String(url).endsWith('/v1/router')
          ? json({ providers: fleet.map((id) => ({ id })) })
          : json({ loaded: 1, failed: 0 }, reloadStatus),
      ),
    );
    const { GatewayClient, onRouterStale } = await relaunch(fetching as unknown as typeof fetch);
    const client = new GatewayClient(conn);
    const stale = vi.fn();
    const stop = onRouterStale(client.base, stale);

    expect((await client.router()).map((row) => row.id)).toEqual(['anthropic']);
    fleet = ['anthropic', 'token-provider'];
    await client.reloadExtensions({ scope: 'project', target_id: 'p', label: 'Workspace' });
    expect(stale).toHaveBeenCalledTimes(1);
    expect(client.cachedRouter()).toBeNull();
    expect((await client.router()).map((row) => row.id)).toEqual(['anthropic', 'token-provider']);

    reloadStatus = 500;
    await expect(client.reloadExtensions()).rejects.toThrow();
    expect(stale).toHaveBeenCalledTimes(2);
    expect(client.isRouterFresh()).toBe(false);

    stop();
    await client.reloadExtensions().catch(() => undefined);
    expect(stale).toHaveBeenCalledTimes(2);
  });
});
