/** @vitest-environment jsdom */
import { afterEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';

const conn = { url: 'http://gateway.example.com' };

afterEach(() => vi.unstubAllGlobals());

const stub = () => {
  const fetcher = vi.fn(
    async (_input: RequestInfo | URL, _init?: RequestInit) =>
      new Response(JSON.stringify([]), { headers: { 'Content-Type': 'application/json' } }),
  );
  vi.stubGlobal('fetch', fetcher);
  return fetcher;
};

const sent = (fetcher: ReturnType<typeof stub>) => {
  const [input, init] = fetcher.mock.calls[0];
  return {
    path: new URL(String(input)).pathname,
    method: init?.method,
    body: JSON.parse(String(init?.body)) as unknown,
  };
};

describe('Subagent control client', () => {
  it('reads the timeline of one child of the leader', async () => {
    const fetcher = vi.fn(
      async (_input: RequestInfo | URL, _init?: RequestInit) =>
        new Response(JSON.stringify({ session_id: 'child', items: [{ at: 1, kind: 'started', text: 'Go' }] }), {
          headers: { 'Content-Type': 'application/json' },
        }),
    );
    vi.stubGlobal('fetch', fetcher);
    const timeline = await new GatewayClient(conn).agentTimeline('lead/1', 'child/2');
    expect(timeline.items).toEqual([{ at: 1, kind: 'started', text: 'Go' }]);
    const [input, init] = fetcher.mock.calls[0];
    expect(new URL(String(input)).pathname).toBe('/v1/sessions/lead%2F1/agents/child%2F2/timeline');
    expect(init?.method).toBe('GET');
  });

  it('stops a child for its leader', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).cancelAgent('lead/1', 'child');
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead%2F1/agents/cancel',
      method: 'POST',
      body: { session_id: 'child' },
    });
  });

  it('keeps a refused stop as an error with its status', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () =>
          new Response(JSON.stringify({ error: 'invalid', message: 'No such agent' }), {
            status: 400,
            headers: { 'Content-Type': 'application/json' },
          }),
      ),
    );
    await expect(new GatewayClient(conn).cancelAgent('lead', 'child')).rejects.toMatchObject({ status: 400 });
  });
});
