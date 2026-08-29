// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

const recordDiagnostic = vi.hoisted(() => vi.fn());
vi.mock('./diagnostics', () => ({ recordDiagnostic }));

import { GatewayClient, GatewayError } from './gateway';

const conn = { url: 'https://gateway.example.com', token: 'private-token' };

describe('gateway request diagnostics', () => {
  beforeEach(() => {
    recordDiagnostic.mockClear();
  });

  it('records the route, status and duration without credentials or payloads', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{"sessions":1}', {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    })));

    await new GatewayClient(conn).status();

    expect(recordDiagnostic).toHaveBeenCalledWith(
      'info',
      'gateway',
      'request',
      expect.objectContaining({
        gateway: 'https://gateway.example.com',
        method: 'GET',
        path: '/v1/admin/status',
        status: 200,
        duration_ms: expect.any(Number),
      }),
    );
    const serialized = JSON.stringify(recordDiagnostic.mock.calls);
    expect(serialized).not.toContain('private-token');
    expect(serialized).not.toContain('sessions');
  });

  it('records failed HTTP exchanges before surfacing the gateway error', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{"error":"unavailable"}', {
      status: 503,
      headers: { 'Content-Type': 'application/json' },
    })));

    await expect(new GatewayClient(conn).status()).rejects.toBeInstanceOf(GatewayError);
    expect(recordDiagnostic).toHaveBeenCalledWith(
      'error',
      'gateway',
      'request',
      expect.objectContaining({ status: 503, error: 'unavailable' }),
    );
  });

  it('does not repeat credentials from a malformed gateway address', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => {
      throw new Error('offline');
    }));

    await expect(
      new GatewayClient({ url: 'not-a-url?token=address-private' }).status(),
    ).rejects.toBeInstanceOf(GatewayError);

    expect(recordDiagnostic).toHaveBeenCalledWith(
      'error',
      'gateway',
      'request',
      expect.objectContaining({ gateway: 'invalid gateway' }),
    );
    expect(JSON.stringify(recordDiagnostic.mock.calls)).not.toContain('address-private');
  });
});
