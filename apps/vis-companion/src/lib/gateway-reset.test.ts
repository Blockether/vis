// @vitest-environment jsdom
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';
import type { GatewayConn } from './types';

const conn = (name: string) => ({ id: name, name, url: `https://${name}.example.com`, token: 'test-pairing' }) as GatewayConn;
afterEach(() => { vi.unstubAllGlobals(); localStorage.clear(); });

it('retains one account-scoped attempt across an unknown response and a new client', async () => {
  const bodies: unknown[] = [];
  const fetch = vi.fn(async (_url: unknown, init?: RequestInit) => {
    bodies.push(JSON.parse(String(init?.body)));
    return new Response(JSON.stringify(bodies.length === 1 ? { outcome: 'unknown' } : { outcome: 'already_redeemed' }), { status: 200 });
  });
  vi.stubGlobal('fetch', fetch);
  const first = new GatewayClient(conn('reset-retry'));
  await expect(first.consumeProviderResetCredit('openai-codex', 'account-1')).rejects.toThrow(/confirm/i);
  const retry = new GatewayClient(conn('reset-retry'));
  expect(retry.hasPendingProviderReset('openai-codex', 'account-1')).toBe(true);
  expect(retry.hasPendingProviderReset('openai-codex', 'account-2')).toBe(false);
  await expect(retry.consumeProviderResetCredit('openai-codex', 'account-1')).resolves.toBe('already_redeemed');
  expect(bodies[0]).toEqual(bodies[1]);
  expect(bodies[0]).toMatchObject({ account_id: 'account-1', idempotency_key: expect.any(String) });
  expect(retry.hasPendingProviderReset('openai-codex', 'account-1')).toBe(false);
  expect(fetch.mock.calls[0][0]).toBe('https://reset-retry.example.com/v1/providers/openai-codex/reset-credits/consume');
});

it('coalesces double presses but creates a new key for a later confirmed reset', async () => {
  const bodies: unknown[] = [];
  let finish!: (response: Response) => void;
  vi.stubGlobal('fetch', vi.fn((_url, init) => {
    bodies.push(JSON.parse(String(init.body)));
    return new Promise<Response>(resolve => { finish = resolve; });
  }));
  const client = new GatewayClient(conn('reset-double'));
  const first = client.consumeProviderResetCredit('openai-codex', 'account');
  const duplicate = client.consumeProviderResetCredit('openai-codex', 'account');
  await vi.waitFor(() => expect(bodies).toHaveLength(1));
  finish(new Response(JSON.stringify({ outcome: 'reset' })));
  expect(await first).toBe('reset');
  expect(await duplicate).toBe('reset');
  const next = client.consumeProviderResetCredit('openai-codex', 'account');
  await vi.waitFor(() => expect(bodies).toHaveLength(2));
  finish(new Response(JSON.stringify({ outcome: 'no_credit' })));
  expect(await next).toBe('no_credit');
  expect(bodies[0]).not.toEqual(bodies[1]);
});

it('scopes live limits updates to the gateway and detaches subscribers', async () => {
  const client = new GatewayClient(conn('limits-subscription'));
  const sameGateway = new GatewayClient(conn('limits-subscription'));
  const otherGateway = new GatewayClient(conn('limits-other'));
  const receive = vi.fn();
  const other = vi.fn();
  const stop = sameGateway.onProviderLimits(receive);
  const stopOther = otherGateway.onProviderLimits(other);
  const report = { status: 'ok', dynamic: { limits: [{ used: 0, limit: 100 }] } };
  vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify({ report }))));
  try {
    await client.providerLimits('openai-codex');
    expect(receive).toHaveBeenCalledWith('openai-codex', report);
    expect(other).not.toHaveBeenCalled();
    stop();
    await client.providerLimits('openai-codex');
    expect(receive).toHaveBeenCalledTimes(1);
  } finally {
    stop();
    stopOther();
  }
});
