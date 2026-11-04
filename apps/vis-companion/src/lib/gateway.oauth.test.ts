// @vitest-environment jsdom
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
  vi.restoreAllMocks();
});

// Pairing chooses the transport for the whole app, including OAuth. No separate VPN consent.
it.each([
  'https://gateway.example.com',
  'http://10.0.0.5:7890',
  'http://100.64.0.10:7890',
  'http://example.ts.net:7890',
  'http://gateway.example.com',
])('uses the paired transport unchanged: %s', async (base) => {
  const fetch = vi.fn().mockImplementation(async () => new Response('{}'));
  vi.stubGlobal('fetch', fetch);
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
  const client = new GatewayClient({ url: base, token: 'test-paired-token' });
  await client.mcpAuthStart('work');
  await client.mcpAuthComplete('work', 'test-flow', 'test-code');
  await client.mcpAuthPoll('work', 'test-flow');
  await client.mcpAuthCancel('work', 'test-flow');
  await client.startProviderAuth('openai-codex');
  await client.completeProviderAuth('openai-codex', 'test-flow', 'test-code');
  await client.pollProviderAuth('openai-codex', 'test-flow');
  await client.cancelProviderAuth('openai-codex', 'test-flow');
  expect(fetch.mock.calls.map(([url]) => url)).toEqual(
    ['mcp/servers/work', 'providers/openai-codex'].flatMap((owner) =>
      ['start', 'complete', 'poll', 'cancel'].map((action) => `${base}/v1/${owner}/auth/${action}`),
    ),
  );
  for (const [, options] of fetch.mock.calls) {
    expect(options.headers.get('Authorization')).toBe('Bearer test-paired-token');
    expect(options.redirect).toBe('error');
    expect(options.cache).toBe('no-store');
  }
  expect(JSON.parse(fetch.mock.calls[0]![1].body)).toEqual({ callback_mode: 'loopback' });
  expect(confirm).not.toHaveBeenCalled();
});

it.each(['127.0.0.1', 'localhost', '[::1]'])(
  'still permits unpaired loopback: %s',
  async (host) => {
    const fetch = vi.fn().mockImplementation(async () => new Response('{}'));
    vi.stubGlobal('fetch', fetch);
    const client = new GatewayClient({ url: `http://${host}:7890` });
    await client.mcpAuthStart('work');
    expect(fetch).toHaveBeenCalledOnce();
    expect(fetch.mock.calls[0]![1].headers.has('Authorization')).toBe(false);
  },
);

// Issue #441: a reverse proxy can add the gateway token on the server side, so a browser without
// a token must still reach the OAuth routes. The gateway enforces `--require-token` itself.
it.each([undefined, '', '   '])('sends unpaired remote OAuth requests to the gateway (%s)', async (token) => {
  const fetch = vi.fn().mockImplementation(async () => new Response('{}'));
  vi.stubGlobal('fetch', fetch);
  for (const url of ['http://10.0.0.5:7890', 'https://gateway.example.com']) {
    const client = new GatewayClient({ url, token });
    await client.mcpAuthStart('work');
    await client.startProviderAuth('github-copilot');
  }
  expect(fetch.mock.calls.map(([url]) => url)).toEqual([
    'http://10.0.0.5:7890/v1/mcp/servers/work/auth/start',
    'http://10.0.0.5:7890/v1/providers/github-copilot/auth/start',
    'https://gateway.example.com/v1/mcp/servers/work/auth/start',
    'https://gateway.example.com/v1/providers/github-copilot/auth/start',
  ]);
  for (const [, options] of fetch.mock.calls) expect(options.headers.has('Authorization')).toBe(false);
});

it('asks to pair when the gateway refuses an OAuth request without a token', async () => {
  vi.stubGlobal(
    'fetch',
    vi.fn().mockImplementation(async () => new Response('{"error":"unauthorized"}', { status: 401 })),
  );
  const client = new GatewayClient({ url: 'https://gateway.example.com' });
  await expect(client.startProviderAuth('github-copilot')).rejects.toMatchObject({
    reason: 'pairing-required',
    status: 401,
    message: 'Pair this gateway before starting sign-in.',
  });
});

it('refuses a non-HTTP scheme before sending credentials', async () => {
  const fetch = vi.fn();
  vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({
    url: 'ftp://gateway.example.com',
    token: 'test-paired-token',
  });
  await expect(client.mcpAuthStart('work')).rejects.toMatchObject({ reason: 'invalid-address' });
  expect(fetch).not.toHaveBeenCalled();
});

it('does not impose a separate transport-consent timeout on a paired HTTP client', async () => {
  vi.useFakeTimers();
  const fetch = vi.fn().mockImplementation(async () => new Response('{}'));
  vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  await client.mcpAuthStart('work');
  vi.setSystemTime(Date.now() + 900_001);
  await client.mcpAuthStart('work');
  expect(fetch).toHaveBeenCalledTimes(2);
});

it('does not downgrade or replay a failed HTTPS request', async () => {
  const fetch = vi.fn().mockRejectedValue(new TypeError('offline'));
  vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({
    url: 'https://gateway.example.com',
    token: 'test-paired-token',
  });
  await expect(client.mcpAuthStart('work')).rejects.toThrow('network error');
  expect(fetch).toHaveBeenCalledOnce();
  expect(fetch.mock.calls[0]![0]).toBe(
    'https://gateway.example.com/v1/mcp/servers/work/auth/start',
  );
});
