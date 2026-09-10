// @vitest-environment jsdom
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { clientAuthFlow, watchAuth } from './oauth';
import type { SignInFlow } from './types';
import { act, cleanup, renderHook } from '@testing-library/react';
import { useProviderAuth } from '../components/ProviderAuth';
import type { GatewayClient } from './gateway';
import type { RouterProvider } from './types';
const host = vi.hoisted(() => ({ available: true, authorize: vi.fn(), cancel: vi.fn(), reopen: vi.fn() }));
vi.mock('@capacitor/core', () => ({ Capacitor: {
  isNativePlatform: () => host.available, isPluginAvailable: () => host.available,
}, registerPlugin: () => host }));
const redirect = 'http://localhost:53692/callback';
const flow: SignInFlow = { flow_id: 'native-flow', kind: 'pkce', callback_mode: 'loopback', redirect_uri: redirect,
  url: `https://gateway.example.com/authorize?state=test-state&redirect_uri=${encodeURIComponent(redirect)}` };
const callback = `${redirect}?state=test-state&code=test-code`;
const remote = 'https://gateway.example.com';
function transport() { return { poll: vi.fn().mockResolvedValue({ status: 'pending' }),
  complete: vi.fn().mockResolvedValue({ status: 'ok' }), cancel: vi.fn().mockResolvedValue(undefined) }; }
beforeEach(() => {
  vi.useFakeTimers(); host.available = true; host.authorize.mockReset(); host.cancel.mockResolvedValue(undefined);
  host.reopen.mockResolvedValue(undefined); vi.spyOn(window, 'open').mockReturnValue(null);
});
afterEach(() => { vi.clearAllTimers(); vi.useRealTimers(); vi.restoreAllMocks(); vi.clearAllMocks(); });

it('receives a fixed loopback callback on the initiating native device, without rewriting the registered URI', async () => {
  host.authorize.mockResolvedValue({ url: callback });
  const client = transport(); const verdict = vi.fn(); const presented = clientAuthFlow(flow, remote);
  expect(presented.callback_mode).toBe('loopback');
  const watcher = watchAuth(presented, client, verdict); await vi.advanceTimersByTimeAsync(0);
  expect(host.authorize).toHaveBeenCalledWith(expect.objectContaining({ flowId: flow.flow_id,
    authorizationUrl: flow.url, redirectUri: redirect, state: 'test-state' }));
  expect(window.open).not.toHaveBeenCalled();
  expect(client.complete).toHaveBeenCalledExactlyOnceWith(callback);
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'ok' })); watcher.stop();
});
it('treats a gateway-side bind failure the same way and keeps a web host honest', () => {
  expect(clientAuthFlow({ ...flow, callback_mode: 'manual' }, remote).callback_mode).toBe('loopback');
  host.available = false; expect(clientAuthFlow(flow, remote).callback_mode).toBe('manual');
});
it('reopens and cancels only the same native attempt, ignoring a late return', async () => {
  let complete!: (value: { url: string }) => void;
  host.authorize.mockReturnValue(new Promise(resolve => { complete = resolve; }));
  const client = transport(); const verdict = vi.fn(); const watcher = watchAuth(clientAuthFlow(flow, remote), client, verdict);
  await vi.advanceTimersByTimeAsync(0); watcher.open(); await vi.advanceTimersByTimeAsync(0);
  expect(host.reopen).toHaveBeenCalledExactlyOnceWith({ flowId: flow.flow_id });
  watcher.stop(); complete({ url: callback }); await vi.advanceTimersByTimeAsync(0);
  expect(host.cancel).toHaveBeenCalledWith({ flowId: flow.flow_id });
  expect(client.complete).not.toHaveBeenCalled(); expect(verdict).not.toHaveBeenCalled();
});
it.each([callback.replace('test-state', 'wrong'), callback.replace('53692', '1455'), `${callback}&code=other`])(
  'revalidates native returns before contacting the paired gateway', async url => {
    host.authorize.mockResolvedValue({ url }); const client = transport(); const verdict = vi.fn();
    const watcher = watchAuth(clientAuthFlow(flow, remote), client, verdict); await vi.advanceTimersByTimeAsync(0);
    expect(client.complete).not.toHaveBeenCalled();
    expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' })); watcher.stop();
  });
it('never binds for device/API-key flows or non-loopback redirects', () => {
  for (const other of [{ ...flow, kind: 'device' as const }, { ...flow, kind: 'api-key' as const },
    { ...flow, redirect_uri: 'http://10.0.0.5/callback' }]) {
    expect(clientAuthFlow(other, remote).callback_mode).not.toBe('loopback');
  }
 });
it('fails closed before opening when the authorization URL substitutes a different redirect', async () => {
  const url = new URL(flow.url!); url.searchParams.set('redirect_uri', 'http://10.0.0.5/callback');
  const client = transport(); const verdict = vi.fn();
  const watcher = watchAuth(clientAuthFlow({ ...flow, url: url.toString() }, remote), client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  expect(host.authorize).not.toHaveBeenCalled(); expect(window.open).not.toHaveBeenCalled();
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' })); watcher.stop();
});
it('expires a native browser independently of a stalled native callback', async () => {
  host.authorize.mockReturnValue(new Promise(() => {}));
  const client = transport(); const verdict = vi.fn();
  const watcher = watchAuth(clientAuthFlow({ ...flow, expires_at: Date.now() + 100 }, remote), client, verdict);
  await vi.advanceTimersByTimeAsync(101);
  expect(host.cancel).toHaveBeenCalledWith({ flowId: flow.flow_id });
  expect(client.cancel).toHaveBeenCalledOnce();
  expect(verdict).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ status: 'error' })); watcher.stop();
 });
it('closes the actual provider UI and refreshes the same remote gateway after a native return', async () => {
  host.authorize.mockResolvedValue({ url: callback });
  const provider = { id: 'generic-oauth' } as RouterProvider;
  const client = { base: remote, cachedRouter: () => [], router: vi.fn().mockResolvedValue([]),
    onProviderLimits: () => () => {},
    startProviderAuth: vi.fn().mockResolvedValue({ ...flow, provider_id: provider.id }),
    completeProviderAuth: vi.fn().mockResolvedValue({ status: 'ok' }),
    pollProviderAuth: vi.fn().mockResolvedValue({ status: 'pending' }), cancelProviderAuth: vi.fn().mockResolvedValue(undefined) };
  const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
  try {
    await act(async () => { await result.current.signIn(provider); await vi.advanceTimersByTimeAsync(0); });
    expect(client.completeProviderAuth).toHaveBeenCalledExactlyOnceWith(provider.id, flow.flow_id, callback);
    expect(result.current.flow).toBeNull();
    expect(client.router).toHaveBeenCalledWith(undefined, { force: true });
    expect(window.open).not.toHaveBeenCalled();
  } finally { cleanup(); }
});
