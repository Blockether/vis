// @vitest-environment jsdom
// Regression, user report ("it says Reconnecting every few moments"): the
// gateway was healthy and answering every poll. A webview gives an HTTP/1.1
// origin about six sockets, and a resumed screen fired its whole poll set at
// once, so the live session and fleet streams reopened behind that burst and
// never got their headers inside the stream connect timeout. The phone's
// diagnostics show the aborted stream opens sitting at a median of thirteen
// concurrent requests, against six for the ones that connected.
import { afterEach, describe, expect, it, vi } from 'vitest';

const startGatewayRequestDiagnostic = vi.hoisted(() =>
  vi.fn(() => ({ request_id: 'req-test', finish: vi.fn() })),
);
vi.mock('./diagnostics', () => ({ startGatewayRequestDiagnostic }));

import { GatewayClient } from './gateway';

const conn = { url: 'https://gateway.example.com', token: 'private-token' };

/** Let every pending microtask and queued continuation run. */
const settle = () => new Promise((resolve) => setTimeout(resolve, 0));

/** A gateway that answers only when this test says so. */
function heldGateway() {
  const pending: Array<(body: Response) => void> = [];
  const wire = vi.fn(
    async () =>
      await new Promise<Response>((resolve) => {
        pending.push(resolve);
      }),
  );
  vi.stubGlobal('fetch', wire);
  return {
    /** Requests actually put on the wire so far. */
    get started() {
      return wire.mock.calls.length;
    },
    async answerOne() {
      pending.shift()?.(
        new Response('{}', { status: 200, headers: { 'Content-Type': 'application/json' } }),
      );
      await settle();
    },
    async answerAll() {
      while (pending.length) await this.answerOne();
    },
  };
}

afterEach(() => {
  vi.unstubAllGlobals();
});

describe('requests to one gateway', () => {
  it('leaves the live streams their sockets by queueing the rest', async () => {
    const gateway = heldGateway();
    const client = new GatewayClient(conn);
    const polls = Array.from({ length: 7 }, () => client.status().catch(() => null));
    await settle();

    expect(gateway.started).toBe(4);

    // A slot is handed straight to the next waiter: the cap holds, and nothing
    // stalls behind a request that has already been answered.
    await gateway.answerOne();
    expect(gateway.started).toBe(5);
    await gateway.answerOne();
    expect(gateway.started).toBe(6);

    await gateway.answerAll();
    expect(gateway.started).toBe(7);
    await Promise.all(polls);
  });

  it('puts a request nothing is holding up on the wire in the same tick', async () => {
    const gateway = heldGateway();
    const poll = new GatewayClient(conn).status().catch(() => null);
    // A tap on `Retry` reaches the machine now, not on some later turn of the
    // event loop: the gate only ever makes a request WAIT for a busy gateway.
    expect(gateway.started).toBe(1);
    await gateway.answerAll();
    await poll;
  });

  it('opens the gate again once the burst is over', async () => {
    const gateway = heldGateway();
    const client = new GatewayClient(conn);
    const first = Array.from({ length: 4 }, () => client.status().catch(() => null));
    await settle();
    await gateway.answerAll();
    await Promise.all(first);

    const later = client.status().catch(() => null);
    await settle();
    expect(gateway.started).toBe(5);
    await gateway.answerAll();
    await later;
  });
});

describe('probing a candidate address', () => {
  it('gives it a few seconds, not the whole request budget', async () => {
    vi.useFakeTimers();
    const aborted = vi.fn();
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async (_input: RequestInfo | URL, init?: RequestInit) =>
          await new Promise<Response>((_resolve, reject) => {
            init?.signal?.addEventListener('abort', () => {
              aborted();
              reject(new DOMException('aborted', 'AbortError'));
            });
          }),
      ),
    );
    try {
      const answered = new GatewayClient({ url: 'http://192.168.0.31:7890', token: 't' }).ping();
      await vi.advanceTimersByTimeAsync(4_000);
      expect(aborted).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(1_000);
      expect(await answered).toBe(false);
      expect(aborted).toHaveBeenCalled();
    } finally {
      vi.useRealTimers();
    }
  });
});
