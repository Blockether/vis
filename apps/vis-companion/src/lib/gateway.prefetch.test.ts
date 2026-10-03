// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import type { Session } from './types';
import './gateway';

const row = {
  id: 'news-session',
  title: 'Ready answer',
  live: false,
  status: 'idle',
  turn_count: 1,
  answer_count: 1,
  is_unread: true,
  unread_answers: 1,
  modified_at: '2026-08-15T12:00:00Z',
} as Session;
const turns = [{
  turn_id: 'turn-1',
  request: 'A new question',
  content: [{ id: 'answer-1', type: 'prose', markdown: 'A ready answer' }],
  status: 'completed',
  iterations: [],
}];
const page = () => new Response(JSON.stringify({ turns, total: 1, offset: 0, has_more: false }));

beforeEach(() => {
  localStorage.clear();
  vi.resetModules();
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

// Regression: first-seen unread rows and project pages bypassed transcript warming.
describe('preparing NEW sessions before opening', () => {
  it.each(['fleet', 'project', 'grouped'] as const)(
    'warms a first-seen %s answer before publishing its row',
    async (surface) => {
      let release!: (response: Response) => void;
      const body = new Promise<Response>((resolve) => { release = resolve; });
      const reads: string[] = [];
      vi.stubGlobal('fetch', vi.fn(async (input: string) => {
        const path = new URL(String(input)).pathname;
        reads.push(path);
        return path.endsWith('/transcript') ? body : new Response(JSON.stringify({
          sessions: surface === 'grouped' ? [] : [row],
          grouped: surface === 'grouped' ? [row] : [],
          total: 1,
        }));
      }));
      const { GatewayClient } = await import('./gateway');
      const client = new GatewayClient({ url: 'http://gateway.example.com' });
      let published = false;
      const read = () => surface === 'fleet'
        ? client.listSessions()
        : client.listProjectPage(
            '/project', 15, '', new Map(), undefined, true, 'exclude', undefined, true,
          );
      const list = read().then((result) => {
        published = true;
        return result;
      });

      await vi.waitFor(() => expect(reads).toContain('/v1/sessions/news-session/transcript'));
      expect(published).toBe(false);
      release(page());
      await list;
      expect(client.cachedTranscript(row.id)).toEqual(turns);
      await expect(client.transcriptIfMoved(row.id, row)).resolves.toBeNull();
      await read();
      expect(reads.filter((path) => path.endsWith('/transcript'))).toHaveLength(1);
    },
  );

  it('does not warm unread bodies for pages fetched ahead of the visible page', async () => {
    const fetched = vi.fn(async () => new Response(JSON.stringify({ sessions: [row], total: 1 })));
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    await client.listProjectPage('/project', 15, 'next-page', new Map());
    expect(client.cachedTranscript(row.id)).toBeNull();
    expect(fetched).toHaveBeenCalledOnce();
  });

  it('warms a page first fetched as metadata when a 304 confirms it on screen', async () => {
    let listed = false;
    const fetched = vi.fn(async (input: string) => {
      if (new URL(String(input)).pathname.endsWith('/transcript')) return page();
      if (listed) return new Response(null, { status: 304 });
      listed = true;
      return new Response(JSON.stringify({ sessions: [row], total: 1 }), {
        headers: { ETag: 'page' },
      });
    });
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const pins = new Map();
    const held = await client.listProjectPage('/project', 15, '', pins);
    expect(client.cachedTranscript(row.id)).toBeNull();
    const ready = await client.listProjectPage(
      '/project', 15, '', pins, undefined, true, 'exclude', undefined, true,
    );
    expect(ready).toBe(held);
    expect(client.cachedTranscript(row.id)).toEqual(turns);
    expect(fetched).toHaveBeenCalledTimes(3);
  });

  it('keeps a project page unchanged after a failed warm and retries before publishing NEW', async () => {
    let changed = false;
    let fail = true;
    const previous = { ...row, is_unread: false, unread_answers: 0, turn_count: 0 };
    vi.stubGlobal('fetch', vi.fn(async (input: string) => {
      if (new URL(String(input)).pathname.endsWith('/transcript'))
        return fail ? new Response('Unavailable', { status: 503 }) : page();
      return new Response(JSON.stringify({ sessions: [changed ? row : previous], total: 1 }), {
        headers: { ETag: changed ? 'new-page' : 'old-page' },
      });
    }));
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    const pins = new Map();
    const read = () => client.listProjectPage(
      '/project', 15, '', pins, undefined, true, 'exclude', undefined, true,
    );
    const held = await read();
    changed = true;
    expect(await read()).toBe(held);
    expect(client.cachedTranscript(row.id)).toBeNull();
    fail = false;
    expect((await read()).rows[0]?.is_unread).toBe(true);
    expect(client.cachedTranscript(row.id)).toEqual(turns);
  });

  it('bounds warming to ten unique sessions, including grouped rows', async () => {
    const rows = Array.from({ length: 12 }, (_, index) => ({ ...row, id: `session-${index}` }));
    const bodies: string[] = [];
    vi.stubGlobal('fetch', vi.fn(async (input: string) => {
      const path = new URL(String(input)).pathname;
      if (path.endsWith('/transcript')) {
        bodies.push(path);
        return page();
      }
      return new Response(JSON.stringify({ sessions: rows.slice(0, 2), grouped: rows, total: 2 }));
    }));
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient({ url: 'http://gateway.example.com' });
    await client.listProjectPage(
      '/project', 15, '', new Map(), undefined, true, 'exclude', undefined, true,
    );
    expect(bodies).toHaveLength(10);
    expect(new Set(bodies).size).toBe(10);
    expect(client.cachedTranscript('session-0')).toEqual(turns);
    expect(client.cachedTranscript('session-11')).toBeNull();
  });
});
