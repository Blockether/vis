// @vitest-environment jsdom
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import type { SignInFlow } from './types';
import { clientCallbackMode, watchAuth } from './oauth';

const native = vi.hoisted(() => ({ on: true, handler: (_: { url: string }) => {}, remove: vi.fn() }));
vi.mock('@capacitor/core', () => ({ Capacitor: { isNativePlatform: () => native.on, isPluginAvailable: () => false } }));
vi.mock('@capacitor/app', () => ({ App: { addListener: vi.fn(async (_event, callback) => {
  native.handler = callback; return { remove: native.remove };
}) } }));
const flow: SignInFlow = { flow_id: 'test-flow', kind: 'pkce',
  callback_mode: 'app', redirect_uri: 'com.blockether.viscompanion://oauth/callback',
  url: 'https://gateway.example.com/authorize?state=test-state&redirect_uri=com.blockether.viscompanion%3A%2F%2Foauth%2Fcallback' };
const callback = `${flow.redirect_uri}?state=test-state&code=test-code`;
function gateway() {
  return { poll: vi.fn().mockResolvedValue({ status: 'pending' }), complete: vi.fn().mockResolvedValue({ status: 'ok' }),
    cancel: vi.fn().mockResolvedValue(undefined) };
}
beforeEach(() => { vi.useFakeTimers(); native.on = true; native.remove.mockReset(); vi.spyOn(window, 'open').mockReturnValue(null); });
afterEach(() => { vi.clearAllTimers(); vi.useRealTimers(); vi.restoreAllMocks(); });

it('selects a local app callback only on a native host', () => {
  expect(clientCallbackMode()).toBe('app'); native.on = false; expect(clientCallbackMode()).toBe('loopback');
});
it('opens after subscribing and forwards only to the initiating paired gateway, once', async () => {
  const client = gateway(); const verdict = vi.fn(); const { stop } = watchAuth(flow, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  expect(window.open).toHaveBeenCalledWith(flow.url, '_blank', 'noopener,noreferrer');
  native.handler({ url: callback }); native.handler({ url: callback });
  await vi.advanceTimersByTimeAsync(0);
  expect(client.complete).toHaveBeenCalledExactlyOnceWith(callback);
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'ok' }));
  expect(native.remove).toHaveBeenCalledOnce(); stop();
});
it('ignores wrong state, destination, ambiguous queries and non-callback links', async () => {
  const client = gateway(); const { stop } = watchAuth(flow, client, vi.fn());
  await vi.advanceTimersByTimeAsync(0);
  for (const url of [callback.replace('test-state', 'wrong'), callback.replace('oauth/callback', 'gateway/callback'),
    callback.replace('com.blockether.viscompanion:', 'https:'), `${callback}&state=test-state`,
    `${callback}&error=`, `${callback}#fragment`, `${callback}&code=other`, 'vis://gateway?url=https://gateway.example.com']) native.handler({ url });
  await vi.advanceTimersByTimeAsync(0);
  expect(client.complete).not.toHaveBeenCalled(); stop();
});
it('recovers a lost completion response through polling, without another exchange', async () => {
  const client = gateway(); const verdict = vi.fn();
  client.complete.mockRejectedValue(new Error('connection lost'));
  const { stop } = watchAuth(flow, client, verdict); await vi.advanceTimersByTimeAsync(0);
  client.poll.mockResolvedValue({ status: 'ok' });
  native.handler({ url: callback }); await vi.advanceTimersByTimeAsync(0);
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'ok' }));
  expect(client.complete).toHaveBeenCalledOnce(); stop();
});
it('retries a callback after a transient disconnect, never via another endpoint', async () => {
  const client = gateway(); const verdict = vi.fn(); client.complete.mockRejectedValueOnce(new Error('offline'));
  const { stop } = watchAuth(flow, client, verdict); await vi.advanceTimersByTimeAsync(0);
  native.handler({ url: callback }); await vi.advanceTimersByTimeAsync(2100);
  expect(client.complete).toHaveBeenCalledTimes(2);
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'ok' })); stop();
});
it('drops late returns on cancellation and cancels the gateway flow', async () => {
  const client = gateway(); const verdict = vi.fn(); const { stop } = watchAuth(flow, client, verdict);
  await vi.advanceTimersByTimeAsync(0); stop(); native.handler({ url: callback });
  await vi.advanceTimersByTimeAsync(5000);
  expect(client.complete).not.toHaveBeenCalled(); expect(verdict).not.toHaveBeenCalled();
  expect(client.cancel).toHaveBeenCalledOnce();
});
it('expires without another callback exchange and reports a sanitized error', async () => {
  const client = gateway(); const verdict = vi.fn();
  const { stop } = watchAuth({ ...flow, expires_at: Date.now() + 100 }, client, verdict);
  await vi.advanceTimersByTimeAsync(101); native.handler({ url: callback }); await vi.advanceTimersByTimeAsync(2100);
  expect(client.complete).not.toHaveBeenCalled();
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' })); stop();
});
it('fails closed if a gateway substitutes an HTTPS callback for the app scheme', async () => {
  const client = gateway(); const verdict = vi.fn();
  const { stop } = watchAuth({ ...flow, redirect_uri: 'https://gateway.example.com/callback' }, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  expect(window.open).not.toHaveBeenCalled(); expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' })); stop();
});
it('refuses a different callback hidden inside the authorization URL', async () => {
  const client = gateway(); const verdict = vi.fn();
  const url = new URL(flow.url!); url.searchParams.set('redirect_uri', 'https://gateway.example.com/callback');
  const { stop } = watchAuth({ ...flow, url: url.toString() }, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  expect(window.open).not.toHaveBeenCalled();
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' })); stop();
 });

it('expires independently of a hung gateway poll and ignores its late verdict', async () => {
  const client = gateway(); const verdict = vi.fn();
  let resolve!: (value: { status: string }) => void;
  client.poll.mockReturnValue(new Promise(done => { resolve = done; }));
  const watcher = watchAuth({ ...flow, callback_mode: 'manual', expires_at: Date.now() + 3000 }, client, verdict);
  await vi.advanceTimersByTimeAsync(3100);
  expect(client.cancel).toHaveBeenCalledOnce();
  expect(verdict).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ status: 'error' }));
  resolve({ status: 'ok' }); await vi.advanceTimersByTimeAsync(0);
  expect(verdict).toHaveBeenCalledOnce(); watcher.stop();
});

it('serializes manual submissions with polling and with each other', async () => {
  const client = gateway(); let release!: (value: { status: string }) => void;
  client.poll.mockReturnValueOnce(new Promise(done => { release = done; }));
  client.complete.mockImplementation(async () => { await new Promise(done => setTimeout(done, 10)); return { status: 'ok' }; });
  const watcher = watchAuth({ ...flow, callback_mode: 'manual' }, client, vi.fn());
  await vi.advanceTimersByTimeAsync(2000);
  const first = watcher.complete('manual'); const second = watcher.complete('manual');
  release({ status: 'pending' }); await vi.advanceTimersByTimeAsync(20); await Promise.all([first, second]);
  expect(client.complete).toHaveBeenCalledOnce(); watcher.stop();
});
