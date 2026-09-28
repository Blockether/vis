// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import { abortableDelay, GatewayClient, linkSignals } from './gateway';

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

/** Listeners added to `signal` and not yet removed, through its public methods. */
function netListeners(signal: AbortSignal): () => number {
  const added = vi.spyOn(signal, 'addEventListener');
  const removed = vi.spyOn(signal, 'removeEventListener');
  return () => added.mock.calls.length - removed.mock.calls.length;
}

/** Hide the platform's `AbortSignal.any`, as in WebViews that predate it. */
async function withoutNativeAny(run: () => void | Promise<void>) {
  const native = Object.getOwnPropertyDescriptor(AbortSignal, 'any');
  Object.defineProperty(AbortSignal, 'any', { value: undefined, configurable: true, writable: true });
  try {
    await run();
  } finally {
    if (native) Object.defineProperty(AbortSignal, 'any', native);
    else delete (AbortSignal as { any?: unknown }).any;
  }
}

// Memory report: every request, stream reconnect and retry left an abort listener,
// and the combined controller behind it, on the caller's long-lived signal.
describe('linkSignals', () => {
  it('aborts when any input aborts', () => {
    const stream = new AbortController();
    const attempt = new AbortController();
    const link = linkSignals([stream.signal, attempt.signal]);
    expect(link.signal.aborted).toBe(false);
    attempt.abort();
    expect(link.signal.aborted).toBe(true);
  });

  it('keeps no listener on a long-lived input per link', () => {
    const stream = new AbortController();
    const listeners = netListeners(stream.signal);
    for (let attempt = 0; attempt < 50; attempt += 1) {
      linkSignals([stream.signal, new AbortController().signal]).release();
    }
    expect(listeners()).toBe(0);
  });

  it('falls back to links that detach from every input once one aborts', async () => {
    await withoutNativeAny(() => {
      const stream = new AbortController();
      const attempt = new AbortController();
      const listeners = netListeners(stream.signal);
      const link = linkSignals([stream.signal, attempt.signal]);
      expect(listeners()).toBe(1);
      attempt.abort();
      expect(link.signal.aborted).toBe(true);
      expect(listeners()).toBe(0);
    });
  });

  // A weakly held fallback could be collected mid-request and lose the timeout's
  // abort, so the links stay strong until the work lets go of them.
  it('falls back to links that stay until they are released', async () => {
    await withoutNativeAny(() => {
      const stream = new AbortController();
      const listeners = netListeners(stream.signal);
      for (let attempt = 0; attempt < 50; attempt += 1) {
        linkSignals([stream.signal, new AbortController().signal]).release();
      }
      expect(listeners()).toBe(0);
      const link = linkSignals([stream.signal, new AbortController().signal]);
      expect(listeners()).toBe(1);
      stream.abort();
      expect(link.signal.aborted).toBe(true);
    });
  });

  it('is aborted from the start when an input already aborted', async () => {
    const done = new AbortController();
    done.abort();
    expect(linkSignals([done.signal, new AbortController().signal]).signal.aborted).toBe(true);
    await withoutNativeAny(() => {
      expect(linkSignals([new AbortController().signal, done.signal]).signal.aborted).toBe(true);
    });
  });
});

describe('gateway requests', () => {
  const calls: Array<[string, (client: GatewayClient, signal: AbortSignal) => Promise<unknown>]> = [
    ['a probe', (client, signal) => client.ping(signal)],
    ['a JSON request', (client, signal) => client.health(signal)],
    ['a body request', (client, signal) => client.transcriptMd('s1', signal)],
  ];
  for (const [name, call] of calls) {
    it(`let go of the caller signal once ${name} is answered`, async () => {
      await withoutNativeAny(async () => {
        vi.stubGlobal(
          'fetch',
          vi.fn(async () => new Response('{"ok":true}', { status: 200 })),
        );
        const caller = new AbortController();
        const listeners = netListeners(caller.signal);
        await call(new GatewayClient({ url: 'http://gateway.example.com:7890' }), caller.signal);
        expect(listeners()).toBe(0);
      });
    });
  }
});

describe('abortableDelay', () => {
  it('removes its abort listener once the delay has passed', async () => {
    const stream = new AbortController();
    const listeners = netListeners(stream.signal);
    const waited = abortableDelay(5, stream.signal);
    expect(listeners()).toBe(1);
    await waited;
    expect(listeners()).toBe(0);
  });

  it('ends early when the signal aborts', async () => {
    const stream = new AbortController();
    const waited = abortableDelay(60_000, stream.signal);
    stream.abort();
    await expect(waited).resolves.toBeUndefined();
  });
});
