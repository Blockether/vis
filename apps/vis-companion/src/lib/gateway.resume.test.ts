// What a reconnect and a relaunch must not throw away: the facts only the list
// row carries, and how far this device's event stream already got.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { Session } from './types';

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

beforeEach(() => {
  storage.clear();
  vi.resetModules();
  vi.stubGlobal('localStorage', storage);
  vi.stubGlobal('window', globalThis);
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

/** One SSE response body, delivered in a single chunk. */
function sseBody(frames: Array<Record<string, unknown>>): ReadableStream<Uint8Array> {
  return new ReadableStream<Uint8Array>({
    start(controller) {
      controller.enqueue(
        new TextEncoder().encode(
          frames.map((frame) => `data: ${JSON.stringify(frame)}\n\n`).join(''),
        ),
      );
    },
  });
}

// Regression, duplicate project header: `GET /v1/sessions/:sid` answers a LEAN row
// — against a live gateway it omits `workspace`, `is_unread` and `unread_answers`
// — and the reconcile took it wholesale, so opening a session DELETED the
// workspace the list had established. The row then grouped under the empty path
// and its project grew a second, nameless header beside the real one.
describe('a lean single-session payload', () => {
  const workspace = { root: '/repo/a/apps/web', repo_root: '/repo/a' };

  const listRow: Session = {
    id: 's1',
    title: 'Row',
    live: false,
    current_turn_id: null,
    turn_count: 3,
    server_time_ms: 0,
    workspace,
    is_unread: true,
    unread_answers: 2,
  };

  /** Exactly what the gateway answers for one session: the list row, less those keys. */
  function leanRow(extra: Partial<Session> = {}): Record<string, unknown> {
    const { workspace: _w, is_unread: _u, unread_answers: _a, ...lean } = listRow;
    return { ...lean, ...extra };
  }

  async function warmed(detail: Record<string, unknown>) {
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async (input: string) =>
          new Response(
            JSON.stringify(
              new URL(input).pathname === '/v1/sessions' ? { sessions: [listRow] } : detail,
            ),
          ),
      ),
    );
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);
    await client.listSessions();
    return client;
  }

  it('never deletes the workspace the list established', async () => {
    const client = await warmed(leanRow());
    const merged = await client.session('s1');

    expect(merged.workspace).toEqual(workspace);
    expect(client.cachedSession('s1')?.workspace).toEqual(workspace);
    // The list snapshot is what the project headers are grouped from.
    expect(client.cachedSessions()?.[0]?.workspace).toEqual(workspace);
    expect(merged.is_unread).toBe(true);
    expect(merged.unread_answers).toBe(2);
  });

  it('still wins on every fact it does carry', async () => {
    const client = await warmed(leanRow({ title: 'Renamed', turn_count: 4 }));
    const merged = await client.session('s1');

    expect(merged.title).toBe('Renamed');
    expect(merged.turn_count).toBe(4);
    expect(merged.workspace).toEqual(workspace);
  });

  it('clears a workspace the gateway reports as gone', async () => {
    const client = await warmed(leanRow({ workspace: null }));

    await expect(client.session('s1')).resolves.toMatchObject({ workspace: null });
  });

  // The full chain the duplicate header came down: open the session (lean row
  // cached), then star it — the PATCH echo is lean too, and absorbing it wrote
  // that row back into the LIST snapshot, which is what the headers group by.
  it('survives an echoed row being absorbed back into the list', async () => {
    const client = await warmed(leanRow({ favorite_rank: 1 }));
    await client.session('s1');
    const starred = await client.setSessionFavorite('s1', true);

    expect(starred.favorite_rank).toBe(1);
    expect(starred.workspace).toEqual(workspace);
    expect(client.cachedSessions()?.[0]?.workspace).toEqual(workspace);
  });
});

// Regression: `-1` is the gateway's REWIND sentinel, not a live-only subscribe —
// `resolve-sse-cursor` answers it with the running turn's whole replay. Cursors
// lived only in the hub's in-memory Map, so every cold start asked for it on
// every watched session: measured at 6,062,080 bytes for three running turns,
// against 98,304 bytes for an in-range resume over the same window.
describe('a relaunched event stream', () => {
  async function streamOnce(cursors: Map<string, number>, frames: Array<Record<string, unknown>>) {
    const fetches = vi.fn(async (_input: string) => new Response(sseBody(frames)));
    vi.stubGlobal('fetch', fetches);
    const gateway = await import('./gateway');
    const client = new gateway.GatewayClient(conn);
    const stop = client.streamSessionEvents(cursors, () => {});
    await vi.advanceTimersByTimeAsync(0);
    stop();
    return { gateway, client, asked: new URL(fetches.mock.calls[0]![0]).searchParams.get('sids') };
  }

  it('resumes from the cursor it was served instead of rewinding', async () => {
    vi.useFakeTimers();
    const warm = await streamOnce(new Map([['s1', -1]]), [
      { type: 'subscription.ready', session_id: 's1', cursor: 40 },
      { type: 'content.block.delta', session_id: 's1', seq: 42 },
    ]);
    // Nothing was remembered yet, so the first connect is the honest rewind.
    expect(warm.asked).toBe('s1:-1');
    expect(warm.client.cachedSessionCursor('s1')).toBe(42);
    warm.gateway.persistGatewayCaches();

    vi.resetModules();
    const cold = await streamOnce(new Map([['s1', -1]]), []);
    expect(cold.client.cachedSessionCursor('s1')).toBe(42);
    expect(cold.asked).toBe('s1:42');
  });

  it('leaves a cursor the caller already holds alone', async () => {
    vi.useFakeTimers();
    const warm = await streamOnce(new Map([['s1', -1]]), [
      { type: 'subscription.ready', session_id: 's1', cursor: 7 },
    ]);
    warm.gateway.persistGatewayCaches();

    vi.resetModules();
    // A hub that stayed alive across a reconnect knows better than the store.
    const live = await streamOnce(new Map([['s1', 9]]), []);
    expect(live.asked).toBe('s1:9');
  });

  it('rewinds again once the session is deleted', async () => {
    vi.useFakeTimers();
    const warm = await streamOnce(new Map([['s1', -1]]), [
      { type: 'subscription.ready', session_id: 's1', cursor: 5 },
    ]);
    warm.client.forgetDeletedSession('s1');
    expect(warm.client.cachedSessionCursor('s1')).toBeNull();
    warm.gateway.persistGatewayCaches();

    vi.resetModules();
    const cold = await streamOnce(new Map([['s2', -1]]), []);
    expect(cold.client.cachedSessionCursor('s1')).toBeNull();
    expect(cold.asked).toBe('s2:-1');
  });
});
