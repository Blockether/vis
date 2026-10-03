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
});
