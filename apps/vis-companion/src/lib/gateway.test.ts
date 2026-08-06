import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

class MemoryStorage implements Storage {
  private readonly rows = new Map<string, string>();

  get length(): number {
    return this.rows.size;
  }

  clear(): void {
    this.rows.clear();
  }

  getItem(key: string): string | null {
    return this.rows.get(key) ?? null;
  }

  key(index: number): string | null {
    return Array.from(this.rows.keys())[index] ?? null;
  }

  removeItem(key: string): void {
    this.rows.delete(key);
  }

  setItem(key: string, value: string): void {
    this.rows.set(key, value);
  }
}

const storage = new MemoryStorage();
const conn = { url: 'http://gateway.example.com:7890' };
const sessions = [{ id: 'session-1', title: 'Cached session' }];

beforeEach(() => {
  storage.clear();
  vi.resetModules();
  vi.stubGlobal('localStorage', storage);
  vi.stubGlobal('window', globalThis);
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

// Regression: a cold-start client used to re-download the complete session list.
describe('GatewayClient session-list validators', () => {
  it('revalidates a persisted session snapshot with its head ETag after a cold start', async () => {
    const firstFetch = vi.fn().mockResolvedValue(
      new Response(
        JSON.stringify({ sessions, total: 1, has_more: false }),
        { headers: { ETag: '"sessions-v1"' } },
      ),
    );
    vi.stubGlobal('fetch', firstFetch);
    const first = await import('./gateway');

    await new first.GatewayClient(conn).listSessions();
    first.persistGatewayCaches();

    vi.resetModules();
    const secondFetch = vi.fn().mockImplementation((_url: string, init: RequestInit) => {
      expect(new Headers(init.headers).get('If-None-Match')).toBe('"sessions-v1"');
      return Promise.resolve(new Response(null, { status: 304 }));
    });
    vi.stubGlobal('fetch', secondFetch);
    const second = await import('./gateway');
    const client = new second.GatewayClient(conn);
    const cached = client.cachedSessions();

    await expect(client.listSessions()).resolves.toBe(cached);
    expect(secondFetch).toHaveBeenCalledOnce();
  });
});

// Regression: slash discovery used a gateway-global route, so the palette was resolved
// against the daemon's launch directory instead of the open session's nested project.
describe('GatewayClient session slash palette', () => {
  it('requests slash commands in the session scope', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ commands: [{ name: '/impeccable init', doc: 'Initialize' }] })),
    );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await client.slashes('session-1');

    expect(fetchMock).toHaveBeenCalledOnce();
    expect(String(fetchMock.mock.calls[0]?.[0])).toContain(
      '/v1/sessions/session-1/slashes',
    );
  });
});
