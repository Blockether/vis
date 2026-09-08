// @vitest-environment jsdom
import { act, cleanup, renderHook } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from '../lib/gateway';
import type { AuthFlow, RouterProvider } from '../lib/types';
import { useProviderAuth } from './ProviderAuth';

vi.mock('@capacitor/core', () => ({ Capacitor: { isNativePlatform: () => true, isPluginAvailable: () => false } }));
afterEach(() => { cleanup(); vi.useRealTimers(); vi.restoreAllMocks(); vi.unstubAllGlobals(); });

const provider = { id: 'openai-codex' } as RouterProvider;
const flow: AuthFlow = { flow_id: 'test-flow', provider_id: provider.id, kind: 'pkce',
  url: 'https://gateway.example.com/authorize' };
function clientFor(started = flow) {
  return { base: 'http://127.0.0.1:7890', cachedRouter: () => [], router: vi.fn().mockResolvedValue([]),
    startProviderAuth: vi.fn().mockResolvedValue(started),
    pollProviderAuth: vi.fn().mockResolvedValue({ status: 'ok' }),
    completeProviderAuth: vi.fn().mockResolvedValue({ status: 'ok' }),
    cancelProviderAuth: vi.fn().mockResolvedValue({ status: 'cancelled' }),
  };
}

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => { resolve = done; });
  return { promise, resolve };
}

it('starts provider sign-in on a paired HTTP gateway without another consent dialog', async () => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
  const fetch = vi.fn().mockImplementation(async () => new Response(JSON.stringify(flow)));
  vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  vi.spyOn(client, 'cachedRouter').mockReturnValue([]);
  vi.spyOn(client, 'router').mockResolvedValue([]);
  const { result } = renderHook(() => useProviderAuth(client));
  await act(async () => result.current.signIn(provider));
  expect(confirm).not.toHaveBeenCalled();
  expect(fetch.mock.calls[0]![0]).toBe('http://10.0.0.5:7890/v1/providers/openai-codex/auth/start');
  expect(result.current.flow?.flow_id).toBe('test-flow');
  expect(window.open).toHaveBeenCalledOnce();
});

describe('browser-flow lifecycle in the shared Companion UI', () => {
  it('polls a fresh PKCE flow, closes the sign-in form and refreshes the fleet automatically', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    await act(async () => { await vi.advanceTimersByTimeAsync(5000); });
    expect(client.pollProviderAuth).toHaveBeenCalledWith(provider.id, flow.flow_id);
    expect(result.current.flow).toBeNull();
    expect(client.router).toHaveBeenCalledWith(undefined, { force: true });
    expect(client.completeProviderAuth).not.toHaveBeenCalled();
  });
  it('cancels an active gateway flow on unmount', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    const { result, unmount } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    unmount();
    expect(client.cancelProviderAuth).toHaveBeenCalledExactlyOnceWith(provider.id, flow.flow_id);
    await vi.advanceTimersByTimeAsync(5000);
    expect(client.pollProviderAuth).not.toHaveBeenCalled();
  });

  it('ignores a poll that returns after cancellation', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    const response = deferred<{ status: string }>();
    client.pollProviderAuth.mockReturnValue(response.promise);
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); });
    await act(async () => result.current.cancelFlow());
    await act(async () => response.resolve({ status: 'ok' }));
    expect(result.current.flow).toBeNull();
    expect(result.current.note).toBeNull();
    expect(client.router).not.toHaveBeenCalledWith(undefined, { force: true });
  });

  it('cancels a superseded start without opening its browser', async () => {
    vi.useFakeTimers(); const opened = vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    const response = deferred<AuthFlow>();
    client.startProviderAuth.mockReturnValueOnce(response.promise);
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    let first!: Promise<void>;
    act(() => { first = result.current.signIn(provider); });
    await act(async () => result.current.signIn(provider));
    await act(async () => { response.resolve({ ...flow, flow_id: 'old-flow' }); await first; });
    expect(result.current.flow?.flow_id).toBe(flow.flow_id);
    expect(opened).toHaveBeenCalledTimes(1);
    expect(client.cancelProviderAuth).toHaveBeenCalledWith(provider.id, 'old-flow');
  });

  it('does not restart polling after a cancelled manual completion fails', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    const response = deferred<{ status: string; message: string }>();
    client.completeProviderAuth.mockReturnValue(response.promise);
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    act(() => result.current.setRedirectUrl('http://localhost:1455/auth/callback?code=test&state=test'));
    let completion!: Promise<void>;
    act(() => { completion = result.current.finishPkce(); });
    await act(async () => result.current.cancelFlow());
    await act(async () => { response.resolve({ status: 'error', message: 'Rejected' }); await completion; });
    await act(async () => { await vi.advanceTimersByTimeAsync(5000); });
    expect(client.pollProviderAuth).not.toHaveBeenCalled();
    expect(result.current.err).toBeNull();
    expect(result.current.pending).toBeNull();
  });

  it('recovers polling after a temporary connection loss', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor();
    client.pollProviderAuth.mockRejectedValueOnce(new Error('Connection unavailable'));
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); });
    expect(result.current.flow?.flow_id).toBe(flow.flow_id);
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); });
    expect(result.current.flow).toBeNull();
    expect(result.current.err).toBeNull();
    expect(client.router).toHaveBeenCalledWith(undefined, { force: true });
  });
  it('does not advertise a remote gateway loopback as automatic return on this device', async () => {
    vi.useFakeTimers(); vi.spyOn(window, 'open').mockReturnValue(null);
    const client = clientFor({ ...flow, callback_mode: 'loopback' });
    client.base = 'https://gateway.example.com';
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    expect(result.current.flow?.callback_mode).toBe('manual');
  });
  it.each(['url', 'verification_uri'] as const)('opens and polls a remote device flow from %s', async (urlField) => {
    vi.useFakeTimers();
    const opened = vi.spyOn(window, 'open').mockReturnValue(null);
    const url = 'https://gateway.example.com/device';
    const started: AuthFlow = { flow_id: 'device-flow', provider_id: provider.id,
      kind: 'device', user_code: 'ABCD-EFGH', interval_ms: 1000, [urlField]: url };
    const client = clientFor(started);
    client.base = 'https://gateway.example.com';
    client.pollProviderAuth.mockResolvedValueOnce({ status: 'pending' });
    const { result } = renderHook(() => useProviderAuth(client as unknown as GatewayClient));
    await act(async () => result.current.signIn(provider));
    expect(opened).toHaveBeenCalledWith(url, '_blank', 'noopener,noreferrer');
    expect(result.current.flow?.user_code).toBe('ABCD-EFGH');
    expect(result.current.flow?.url).toBe(url);
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); });
    expect(result.current.flow?.user_code).toBe('ABCD-EFGH');
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); });
    expect(result.current.flow).toBeNull();
    expect(client.router).toHaveBeenCalledWith(undefined, { force: true });
    expect(client.completeProviderAuth).not.toHaveBeenCalled();
  });
});
