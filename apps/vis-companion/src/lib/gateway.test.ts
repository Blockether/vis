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

// Regression: the gateway scoped POST /v1/sessions/:sid/cancel-current to the
// idempotency_key its submitter sent, and the app sent none — so every Stop in the
// mobile app and the web answered 409 :not-owner and the turn kept running.
describe('GatewayClient turn cancellation', () => {
  it('cancels the current turn under the correlation id it submitted with', async () => {
    const fetchMock = vi
      .fn()
      .mockImplementation(() =>
        Promise.resolve(new Response(JSON.stringify({ turn_id: 'turn-1' }))),
      );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await client.submitTurn('session-1', 'hello');
    await client.cancelCurrentTurn('session-1');

    const submitBody = JSON.parse(String(fetchMock.mock.calls[0]?.[1]?.body));
    const cancelBody = JSON.parse(String(fetchMock.mock.calls[1]?.[1]?.body));
    expect(String(fetchMock.mock.calls[1]?.[0])).toContain(
      '/v1/sessions/session-1/cancel-current',
    );
    expect(submitBody.idempotency_key).toBeTruthy();
    expect(cancelBody.idempotency_key).toBe(submitBody.idempotency_key);
  });

  it('captures per-turn submission options', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ turn_id: 'turn-fast' })),
    );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');

    await new GatewayClient(conn).submitTurn('session-1', 'hello', {
      extraBody: { service_tier: 'priority' },
      turnFeatures: { voice_projection: true },
    });

    const body = JSON.parse(String(fetchMock.mock.calls[0]?.[1]?.body));
    expect(body.extra_body).toEqual({ service_tier: 'priority' });
    expect(body.turn_features).toEqual({ voice_projection: true });
  });

  it('cancels a known turn by id, which needs no correlation id', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(JSON.stringify({})));
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');

    await new GatewayClient(conn).cancelTurn('session-1', 'turn-1');

    expect(String(fetchMock.mock.calls[0]?.[0])).toContain(
      '/v1/sessions/session-1/turns/turn-1/cancel',
    );
  });
});

// Regression, user report: saving a new revision of an artifact reported the new
// version, but the artifacts sheet kept listing the old cut and the comments and
// highlights just written were nowhere on screen. A revision is appended to an
// iteration that ALREADY exists, so the session row never moves, the transcript
// revalidation is a no-op, and the turns the sheet is derived from stay stale.
describe('GatewayClient artifact revisions', () => {
  const held = () => [
    {
      id: 'turn-1',
      iterations: [
        {
          id: 'iteration-1',
          attachments: [
            {
              index: 0,
              iteration_id: 'iteration-1',
              filename: 'notes.md',
              media_type: 'text/markdown',
              version: 1,
            },
          ],
        },
      ],
    },
  ];

  const revisionFetch = () =>
    vi.fn().mockImplementation((input: unknown) =>
      Promise.resolve(
        String(input).includes('/attachments')
          ? new Response(
              JSON.stringify({
                index: 1,
                iteration_id: 'iteration-1',
                filename: 'notes.md',
                media_type: 'text/markdown',
                version: 2,
              }),
              { status: 201 },
            )
          : new Response(JSON.stringify({ turns: held() })),
      ),
    );

  it('folds a saved revision into the transcript it holds and hands it back', async () => {
    vi.stubGlobal('fetch', revisionFetch());
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);
    await client.transcript('session-1');

    const seen: unknown[] = [];
    const stop = client.onArtifactRevision('session-1', (turns) => seen.push(turns));
    const saved = await client.saveArtifactText(
      'session-1',
      'iteration-1',
      'notes.md',
      'text/markdown',
      '# annotated',
    );
    stop();

    expect(saved.version).toBe(2);
    expect(saved.iteration_id).toBe('iteration-1');
    const cached = client.cachedTranscript('session-1');
    expect(cached?.[0].iterations?.[0].attachments).toHaveLength(2);
    expect(cached?.[0].iterations?.[0].attachments?.[1].version).toBe(2);
    expect(seen).toEqual([cached]);
  });

  it('stops telling a screen that left', async () => {
    vi.stubGlobal('fetch', revisionFetch());
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);
    await client.transcript('session-1');

    const seen: unknown[] = [];
    client.onArtifactRevision('session-1', (turns) => seen.push(turns))();
    await client.saveArtifactText(
      'session-1',
      'iteration-1',
      'notes.md',
      'text/markdown',
      '# annotated',
    );

    expect(seen).toEqual([]);
  });
});
