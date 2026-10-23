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
  it('answers the open question of a child for its leader', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).answerAgent('lead/1', 'child', 'Use the main branch');
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead%2F1/agents/answer',
      method: 'POST',
      body: { session_id: 'child', text: 'Use the main branch' },
    });
  });

  it('gives a child more iterations', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).extendAgent('lead', 'child', 10);
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead/agents/extend',
      method: 'POST',
      body: { session_id: 'child', iterations: 10 },
    });
  });

  it('refuses an open request with a reason', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).denyAgent('lead', 'child', 'Not now');
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead/agents/deny',
      method: 'POST',
      body: { session_id: 'child', reason: 'Not now' },
    });
  });

  it('grants the requested files', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).grantAgent('lead', 'child', { write: ['src/a/**'] });
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead/agents/grant',
      method: 'POST',
      body: { session_id: 'child', files: { write: ['src/a/**'] } },
    });
  });

  it('keeps a refused answer as an error with its status', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () =>
          new Response(JSON.stringify({ error: 'invalid', message: 'No open question' }), {
            status: 400,
            headers: { 'Content-Type': 'application/json' },
          }),
      ),
    );
    await expect(
      new GatewayClient(conn).answerAgent('lead', 'child', 'Yes'),
    ).rejects.toMatchObject({ status: 400 });
  });
});
