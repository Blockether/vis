// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

const finishRequestDiagnostic = vi.hoisted(() => vi.fn());
const startGatewayRequestDiagnostic = vi.hoisted(() =>
  vi.fn(() => ({ request_id: 'req-test', finish: finishRequestDiagnostic })),
);
vi.mock('./diagnostics', () => ({ startGatewayRequestDiagnostic }));

import { GatewayClient, GatewayError } from './gateway';

const conn = { url: 'https://gateway.example.com', token: 'private-token' };

describe('gateway request diagnostics', () => {
  beforeEach(() => {
    startGatewayRequestDiagnostic.mockClear();
    finishRequestDiagnostic.mockClear();
  });

  it('records an explicit session id without credentials or payloads', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{"deleted":true}', {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    })));

    await new GatewayClient(conn).deleteSession('session /42');

    expect(startGatewayRequestDiagnostic).toHaveBeenCalledWith({
      gateway: 'https://gateway.example.com',
      method: 'DELETE',
      path: '/v1/sessions/session%20%2F42',
      session_id: 'session /42',
      transport: 'fetch',
    });
    expect(finishRequestDiagnostic).toHaveBeenCalledWith('info', {
      outcome: 'success',
      status: 200,
    });
    const serialized = JSON.stringify([
      startGatewayRequestDiagnostic.mock.calls,
      finishRequestDiagnostic.mock.calls,
    ]);
    expect(serialized).not.toContain('private-token');
    expect(serialized).not.toContain('deleted');
  });

  it('times a fully consumed binary response without logging its request body', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => new Response(new Uint8Array([1, 2, 3]), { status: 200 })),
    );

    const result = await new GatewayClient(conn).speakText(
      'session-audio',
      'private spoken text',
    );

    expect(result.size).toBe(3);
    expect(startGatewayRequestDiagnostic).toHaveBeenCalledWith({
      gateway: 'https://gateway.example.com',
      method: 'POST',
      path: '/v1/sessions/session-audio/speech',
      session_id: 'session-audio',
      transport: 'fetch',
    });
    expect(finishRequestDiagnostic).toHaveBeenCalledWith('info', {
      outcome: 'success',
      status: 200,
    });
    expect(JSON.stringify(startGatewayRequestDiagnostic.mock.calls)).not.toContain(
      'private spoken text',
    );
  });

  it('preserves exact transcript text while recording its session request', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('123', { status: 200 })));

    const markdown = await new GatewayClient(conn).transcriptMd('session-md');

    expect(markdown).toBe('123');
    expect(startGatewayRequestDiagnostic).toHaveBeenCalledWith({
      gateway: 'https://gateway.example.com',
      method: 'GET',
      path: '/v1/sessions/session-md/transcript.md',
      session_id: 'session-md',
      transport: 'fetch',
    });
    expect(finishRequestDiagnostic).toHaveBeenCalledWith('info', {
      outcome: 'success',
      status: 200,
    });
  });

  it('identifies every session on an SSE attempt and records its cancellation', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(
        (_input: RequestInfo | URL, init?: RequestInit) =>
          new Promise<Response>((_resolve, reject) => {
            const signal = init?.signal;
            signal?.addEventListener(
              'abort',
              () => reject(signal.reason ?? new DOMException('Aborted', 'AbortError')),
              { once: true },
            );
          }),
      ),
    );
    const stop = new GatewayClient(conn).streamSessionEvents(
      new Map([
        ['session-one', -1],
        ['session-two', 12],
      ]),
      () => undefined,
    );

    await vi.waitFor(() =>
      expect(startGatewayRequestDiagnostic).toHaveBeenCalledWith({
        gateway: 'https://gateway.example.com',
        method: 'GET',
        path: '/v1/events',
        transport: 'sse',
        stream: 'sessions',
        attempt: 1,
        session_ids: ['session-one', 'session-two'],
      }),
    );
    stop();
    await vi.waitFor(() =>
      expect(finishRequestDiagnostic).toHaveBeenCalledWith(
        'info',
        expect.objectContaining({ outcome: 'cancelled', status: 0 }),
      ),
    );
  });

  it('records failed HTTP exchanges before surfacing the gateway error', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{"error":"unavailable"}', {
      status: 503,
      headers: { 'Content-Type': 'application/json' },
    })));

    await expect(new GatewayClient(conn).status()).rejects.toBeInstanceOf(GatewayError);
    expect(finishRequestDiagnostic).toHaveBeenCalledWith('error', {
      status: 503,
      outcome: 'http_error',
      error: 'HTTP 503',
    });
    expect(JSON.stringify(finishRequestDiagnostic.mock.calls)).not.toContain('unavailable');
  });

  it('does not repeat credentials from a malformed gateway address', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => {
      throw new Error('offline');
    }));

    await expect(
      new GatewayClient({ url: 'not-a-url?token=address-private' }).status(),
    ).rejects.toBeInstanceOf(GatewayError);

    expect(startGatewayRequestDiagnostic).toHaveBeenCalledWith(
      expect.objectContaining({ gateway: 'invalid gateway' }),
    );
    expect(JSON.stringify(startGatewayRequestDiagnostic.mock.calls)).not.toContain('address-private');
  });
});
