// @vitest-environment jsdom
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import type { SignInFlow } from './types';
import { reserveAuthTab, watchAuth } from './oauth';

// A web host: no native receiver, so the browser opens by URL and the gateway's own
// loopback listener settles the flow through polling. The native receiver is
// covered by oauth.native.test.ts.
vi.mock('@capacitor/core', () => ({
  Capacitor: { isNativePlatform: () => false, isPluginAvailable: () => false },
}));
const redirect = 'http://127.0.0.1:53692/mcp-callback';
const flow: SignInFlow = {
  flow_id: 'test-flow',
  kind: 'pkce',
  callback_mode: 'loopback',
  redirect_uri: redirect,
  url: `https://gateway.example.com/authorize?state=test-state&redirect_uri=${encodeURIComponent(redirect)}`,
};
const callback = `${redirect}?state=test-state&code=test-code`;
function gateway() {
  return {
    poll: vi.fn().mockResolvedValue({ status: 'pending' }),
    complete: vi.fn().mockResolvedValue({ status: 'ok' }),
    cancel: vi.fn().mockResolvedValue(undefined),
  };
}
beforeEach(() => {
  vi.useFakeTimers();
  vi.spyOn(window, 'open').mockReturnValue(null);
});
afterEach(() => {
  vi.clearAllTimers();
  vi.useRealTimers();
  vi.restoreAllMocks();
});

it('opens the browser at once and settles through polling', async () => {
  const client = gateway();
  const verdict = vi.fn();
  const { stop } = watchAuth(flow, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  expect(window.open).toHaveBeenCalledWith(flow.url, '_blank', 'noopener,noreferrer');
  client.poll.mockResolvedValue({ status: 'ok' });
  await vi.advanceTimersByTimeAsync(2100);
  expect(client.poll).toHaveBeenCalledOnce();
  expect(verdict).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ status: 'ok' }));
  expect(client.complete).not.toHaveBeenCalled();
  stop();
});
it('reopens the same browser page on request, never a second flow', async () => {
  const client = gateway();
  const watcher = watchAuth(flow, client, vi.fn());
  await vi.advanceTimersByTimeAsync(0);
  watcher.open();
  expect(window.open).toHaveBeenCalledTimes(2);
  expect(window.open).toHaveBeenLastCalledWith(flow.url, '_blank', 'noopener,noreferrer');
  watcher.stop();
});
it('recovers a lost completion response through polling, without another exchange', async () => {
  const client = gateway();
  const verdict = vi.fn();
  client.complete.mockRejectedValue(new Error('connection lost'));
  const watcher = watchAuth(flow, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  client.poll.mockResolvedValue({ status: 'ok' });
  await expect(watcher.complete(callback)).rejects.toThrow('connection lost');
  await vi.advanceTimersByTimeAsync(2100);
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'ok' }));
  expect(client.complete).toHaveBeenCalledOnce();
  watcher.stop();
});
it('drops late polls on cancellation and cancels the gateway flow', async () => {
  const client = gateway();
  const verdict = vi.fn();
  const { stop } = watchAuth(flow, client, verdict);
  await vi.advanceTimersByTimeAsync(0);
  stop();
  client.poll.mockResolvedValue({ status: 'ok' });
  await vi.advanceTimersByTimeAsync(5000);
  expect(client.poll).not.toHaveBeenCalled();
  expect(verdict).not.toHaveBeenCalled();
  expect(client.cancel).toHaveBeenCalledOnce();
});
it('expires without another exchange and reports a sanitized error', async () => {
  const client = gateway();
  const verdict = vi.fn();
  const watcher = watchAuth({ ...flow, expires_at: Date.now() + 100 }, client, verdict);
  await vi.advanceTimersByTimeAsync(101);
  await watcher.complete(callback);
  await vi.advanceTimersByTimeAsync(2100);
  expect(client.complete).not.toHaveBeenCalled();
  expect(verdict).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ status: 'error' }));
  watcher.stop();
});

it('expires independently of a hung gateway poll and ignores its late verdict', async () => {
  const client = gateway();
  const verdict = vi.fn();
  let resolve!: (value: { status: string }) => void;
  client.poll.mockReturnValue(
    new Promise((done) => {
      resolve = done;
    }),
  );
  const watcher = watchAuth(
    { ...flow, callback_mode: 'manual', expires_at: Date.now() + 3000 },
    client,
    verdict,
  );
  await vi.advanceTimersByTimeAsync(3100);
  expect(client.cancel).toHaveBeenCalledOnce();
  expect(verdict).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({ status: 'error' }));
  resolve({ status: 'ok' });
  await vi.advanceTimersByTimeAsync(0);
  expect(verdict).toHaveBeenCalledOnce();
  watcher.stop();
});

it('serializes manual submissions with polling and with each other', async () => {
  const client = gateway();
  let release!: (value: { status: string }) => void;
  client.poll.mockReturnValueOnce(
    new Promise((done) => {
      release = done;
    }),
  );
  client.complete.mockImplementation(async () => {
    await new Promise((done) => setTimeout(done, 10));
    return { status: 'ok' };
  });
  const watcher = watchAuth({ ...flow, callback_mode: 'manual' }, client, vi.fn());
  await vi.advanceTimersByTimeAsync(2000);
  const first = watcher.complete('manual');
  const second = watcher.complete('manual');
  release({ status: 'pending' });
  await vi.advanceTimersByTimeAsync(20);
  await Promise.all([first, second]);
  expect(client.complete).toHaveBeenCalledOnce();
  watcher.stop();
});

// Regression: on WKWebView and behind desktop popup blockers the open that followed the
// flow fetch was silently dropped, so tapping Sign in showed no browser at all.
it('navigates a tab reserved inside the tap instead of opening a new one later', async () => {
  const reserved = { location: { href: '' }, opener: {}, close: vi.fn() };
  vi.mocked(window.open).mockReturnValueOnce(reserved as unknown as Window);
  const tab = reserveAuthTab();
  expect(window.open).toHaveBeenCalledExactlyOnceWith('', '_blank');
  expect(reserved.opener).toBeNull();
  const client = gateway();
  const watcher = watchAuth(flow, client, vi.fn(), undefined, tab);
  await vi.advanceTimersByTimeAsync(0);
  expect(reserved.location.href).toBe(flow.url);
  expect(window.open).toHaveBeenCalledOnce();
  expect(reserved.close).not.toHaveBeenCalled();
  watcher.stop();
});

it('closes a reserved tab the flow can never use', async () => {
  const reserved = { location: { href: '' }, opener: {}, close: vi.fn() };
  vi.mocked(window.open).mockReturnValueOnce(reserved as unknown as Window);
  const tab = reserveAuthTab();
  const verdict = vi.fn();
  watchAuth({ ...flow, expires_at: Date.now() - 1 }, gateway(), verdict, undefined, tab);
  await vi.advanceTimersByTimeAsync(0);
  expect(reserved.close).toHaveBeenCalledOnce();
  expect(reserved.location.href).toBe('');
  expect(verdict).toHaveBeenCalledWith(expect.objectContaining({ status: 'error' }));
});

it('reserves nothing when the browser refuses the popup', () => {
  expect(reserveAuthTab()).toBeUndefined();
});
