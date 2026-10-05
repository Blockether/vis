// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import './gateway';

beforeEach(() => { localStorage.clear(); vi.resetModules(); });
afterEach(() => { vi.unstubAllGlobals(); vi.restoreAllMocks(); });

describe('bounded reads during session switches', () => {
  it('shares overlapping reads and requests only recent iterations', async () => {
    let release!: (response: Response) => void;
    const pending = new Promise<Response>((resolve) => { release = resolve; });
    const fetched = vi.fn((_input: RequestInfo | URL) => pending);
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const other = new GatewayClient({ url: 'http://gateway.example.com' });
    const controller = new AbortController();
    const first = client.transcript('s1', controller.signal);
    const second = other.openingTranscript('s1');
    await vi.waitFor(() => expect(fetched).toHaveBeenCalledOnce());
    controller.abort();
    const url = new URL(String(fetched.mock.calls[0][0]));
    expect(url.searchParams.get('iteration_limit')).toBe('8');
    expect(url.searchParams.get('limit')).toBe('24');
    const rows = [{ turn_id: 't1', status: 'running', iterations: [{ position: 100 }], iterations_offset: 99 }];
    release(new Response(JSON.stringify({ turns: rows, total: 1, offset: 0 })));
    expect(await first).toEqual(rows);
    expect(await second).toEqual(rows);
    expect(client.cachedTranscript('s1')).toEqual(rows);
    expect(fetched).toHaveBeenCalledOnce();
  });

  it('retries a failed shared read rather than retaining its rejection', async () => {
    const fetched = vi.fn().mockResolvedValueOnce(new Response('', { status: 403 }))
      .mockResolvedValueOnce(new Response(JSON.stringify({ turns: [], total: 0, offset: 0 })));
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    await expect(client.transcript('s1')).rejects.toThrow();
    await expect(client.transcript('s1')).resolves.toEqual([]);
    expect(fetched).toHaveBeenCalledTimes(2);
  });

  it('reuses a pending warm read even when the returned turn is still running', async () => {
    let release!: (response: Response) => void;
    const fetched = vi.fn(() => new Promise<Response>((resolve) => { release = resolve; }));
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const row = { id: 's1', live: true, turn_count: 1, modified_at: '2026-08-15T12:00:00Z' };
    client.warmTranscript(row as import('./types').Session);
    await vi.waitFor(() => expect(fetched).toHaveBeenCalledOnce());
    const foreground = client.transcriptIfMoved('s1', row as import('./types').Session);
    release(new Response(JSON.stringify({ turns: [
      { turn_id: 't1', status: 'running', iterations: [{ position: 100 }], iterations_offset: 99 },
    ], total: 1, offset: 0 })));
    const foregroundRows = await foreground;
    expect(foregroundRows).toEqual(client.cachedTranscript('s1'));
    expect(client.cachedTranscript('s1')?.[0].iterations_offset).toBe(99);
    expect(fetched).toHaveBeenCalledOnce();
  });

  // Regression, user report: each visit grew a long turn in view a moment after the session opened.
  it('keeps a complete turn trace for the next visit', async () => {
    const steps = [{ position: 1 }, { position: 2 }, { position: 3 }];
    const fetched = vi.fn(async () => new Response(JSON.stringify({ iterations: steps })));
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const turn: import('./types').TranscriptTurn = {
      turn_id: 't1', status: 'completed', iterations: [{ position: 3 }], iterations_offset: 2, iterations_total: 3,
    };
    expect(client.cachedTurnTrace('s1', turn)).toBeNull();
    expect(await client.turnTrace('s1', 't1')).toEqual(steps);
    expect(client.cachedTurnTrace('s1', turn)).toEqual(steps);
    // A trace that was read while the turn ran is short of the settled turn.
    expect(client.cachedTurnTrace('s1', { ...turn, iterations_total: 4 })).toBeNull();
    client.forgetSession('s1');
    expect(client.cachedTurnTrace('s1', turn)).toBeNull();
    expect(fetched).toHaveBeenCalledOnce();
  });

  it('keeps only the newest turn traces', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ iterations: [{ position: 2 }] }))));
    const { GatewayClient, SESSION_CACHE_LIMIT } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const turn = (turn_id: string) => ({ turn_id, iterations: [], iterations_offset: 1 });
    for (let index = 0; index <= SESSION_CACHE_LIMIT; index += 1) await client.turnTrace('s1', `t${index}`);
    expect(client.cachedTurnTrace('s1', turn('t0'))).toBeNull();
    expect(client.cachedTurnTrace('s1', turn(`t${SESSION_CACHE_LIMIT}`))).toHaveLength(1);
  });
});
