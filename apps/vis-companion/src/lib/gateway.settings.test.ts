// Settings batches are versioned: the client sends the catalog revision with the
// typed changes, names the open session for context, and replaces its snapshot only
// after the gateway accepts the whole batch.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { SettingsResponse, SettingsTarget } from './types';

const conn = { url: 'http://gateway.example.com:7890' };
const target: SettingsTarget = { scope: 'session', target_id: 'a/b' };
const plansOn = [{ id: 'plans', action: 'value' as const, value: true }];

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

describe('versioned settings batches', () => {
  it('sends the revision, the typed changes and the open session in one PATCH', async () => {
    const fetching = vi.fn(() => Promise.resolve(json(catalog('revision-2', true))));
    const { GatewayClient } = await relaunch(fetching as unknown as typeof fetch);
    const client = new GatewayClient(conn);

    const saved = await client.applySettings('revision-1', plansOn, target, 'session-7');

    const [url, init] = fetching.mock.calls[0] as unknown as [string, RequestInit];
    expect(url).toBe('http://gateway.example.com:7890/v1/settings');
    expect(init.method).toBe('PATCH');
    expect(JSON.parse(String(init.body))).toEqual({
      scope: 'session',
      target_id: 'a/b',
      revision: 'revision-1',
      changes: plansOn,
      context_session_id: 'session-7',
    });
    expect(saved.revision).toBe('revision-2');
    expect(client.cachedSettings(target)).toEqual(saved);
  });

  it('keeps the last accepted catalog when the gateway rejects a stale batch', async () => {
    const fetching = vi
      .fn()
      .mockResolvedValueOnce(json(catalog('revision-1', false)))
      .mockResolvedValueOnce(
        json({ error: 'Settings changed. Review the newer values and try again.' }, 409),
      );
    const { GatewayClient } = await relaunch(fetching as unknown as typeof fetch);
    const client = new GatewayClient(conn);
    await client.settings(undefined, target);

    await expect(client.applySettings('revision-0', plansOn, target)).rejects.toMatchObject({
      status: 409,
    });
    expect(client.cachedSettings(target)).toEqual(catalog('revision-1', false));
  });

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
