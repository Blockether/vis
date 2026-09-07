// @vitest-environment jsdom
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayOAuthError } from './gateway';
import { startGatewayAuth } from './oauth';
afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers(); vi.restoreAllMocks(); });
it('sends an app callback request directly to the paired HTTPS gateway without redirects or cache', async () => {
  const fetch = vi.fn().mockResolvedValue(new Response('{}'));
  vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'https://gateway.example.com', token: 'test-paired-token' });
  await client.mcpAuthStart('work', 'app');
  const [url, options] = fetch.mock.calls[0]!;
  expect(url).toBe('https://gateway.example.com/v1/mcp/servers/work/auth/start');
  expect(options.redirect).toBe('error'); expect(options.cache).toBe('no-store');
  expect(options.headers.get('Authorization')).toBe('Bearer test-paired-token');
  expect(JSON.parse(options.body)).toEqual({ callback_mode: 'app' });
});
it('refuses model and MCP OAuth over remote cleartext, before issuing any request', async () => {
  const fetch = vi.fn(); vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  await expect(client.mcpAuthStart('work', 'app')).rejects.toThrow(/HTTPS/);
  await expect(client.completeProviderAuth('anthropic-coding-plan', 'test-flow', 'test-code')).rejects.toThrow(/HTTPS/);
  expect(fetch).not.toHaveBeenCalled();
});
it('still permits local development on literal loopback', async () => {
  const fetch = vi.fn().mockImplementation(async () => new Response('{}')); vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  await client.mcpAuthStart('work'); expect(fetch).toHaveBeenCalledOnce();
});
it.each(['http://10.0.0.5:7890', 'http://100.64.0.10:7890', 'http://example.ts.net:7890',
  'http://gateway.example.com'])('does not infer encryption from a paired address: %s', async url => {
  const fetch = vi.fn(); vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url, token: 'test-paired-token' });
  const operations = [
    () => client.mcpAuthStart('work', 'app'),
    () => client.mcpAuthComplete('work', 'test-flow', 'test-code'),
    () => client.mcpAuthPoll('work', 'test-flow'),
    () => client.mcpAuthCancel('work', 'test-flow'),
    () => client.startProviderAuth('openai-codex'),
    () => client.completeProviderAuth('openai-codex', 'test-flow', 'test-code'),
    () => client.pollProviderAuth('openai-codex', 'test-flow'),
    () => client.cancelProviderAuth('openai-codex', 'test-flow'),
  ];
  for (const operation of operations) {
    await expect(operation()).rejects.toMatchObject({ reason: 'vpn-required' });
  }
  expect(fetch).not.toHaveBeenCalled();
});
it('keeps VPN approval only on the initiating URL/token client, and expires it', async () => {
  vi.useFakeTimers();
  const fetch = vi.fn().mockImplementation(async () => new Response('{}')); vi.stubGlobal('fetch', fetch);
  const conn = { url: 'http://10.0.0.5:7890', token: 'test-paired-token' };
  const client = new GatewayClient(conn); client.confirmVpnForOAuth();
  await client.startProviderAuth('openai-codex');
  await client.completeProviderAuth('openai-codex', 'test-flow', 'test-code');
  expect(fetch).toHaveBeenCalledTimes(2);
  for (const other of [conn, { ...conn, url: 'http://gateway.example.com' }, { ...conn, token: 'other-test-token' }]) {
    await expect(new GatewayClient(other).mcpAuthStart('work')).rejects.toMatchObject({ reason: 'vpn-required' });
  }
  await vi.advanceTimersByTimeAsync(900_000);
  await expect(client.mcpAuthComplete('work', 'test-flow', 'test-code')).rejects.toMatchObject({ reason: 'vpn-required' });
  expect(fetch).toHaveBeenCalledTimes(2);
});
it.each([undefined, '', '   '])('VPN approval never substitutes for pairing (%s)', async token => {
  const fetch = vi.fn(); vi.stubGlobal('fetch', fetch);
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(true);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token });
  client.confirmVpnForOAuth();
  await expect(startGatewayAuth(client, () => client.mcpAuthStart('work'), () => true))
    .rejects.toMatchObject({ reason: 'pairing-required' });
  expect(fetch).not.toHaveBeenCalled(); expect(confirm).not.toHaveBeenCalled();
});
it('never authorizes a non-HTTP scheme even after VPN confirmation', async () => {
  const fetch = vi.fn(); vi.stubGlobal('fetch', fetch);
  const client = new GatewayClient({ url: 'ftp://gateway.example.com', token: 'test-paired-token' });
  client.confirmVpnForOAuth();
  await expect(client.mcpAuthStart('work')).rejects.toMatchObject({ reason: 'invalid-address' });
  expect(fetch).not.toHaveBeenCalled();
});
it('does not prompt or retry an abandoned transport refusal', async () => {
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(true);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  const start = vi.fn().mockRejectedValue(new GatewayOAuthError('vpn-required'));
  expect(await startGatewayAuth(client, start, () => false)).toBeNull();
  expect(start).toHaveBeenCalledOnce(); expect(confirm).not.toHaveBeenCalled();
});
it('rechecks the initiating screen after the confirmation dialog closes', async () => {
  let current = true;
  vi.spyOn(window, 'confirm').mockImplementation(() => { current = false; return true; });
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  const start = vi.fn().mockRejectedValue(new GatewayOAuthError('vpn-required'));
  expect(await startGatewayAuth(client, start, () => current)).toBeNull();
  expect(start).toHaveBeenCalledOnce();
  await expect(client.mcpAuthStart('work')).rejects.toMatchObject({ reason: 'vpn-required' });
});
it('never treats a remote or unknown failure as permission to retry over HTTP', async () => {
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(true);
  const client = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  const error = new Error('Sign-in needs HTTPS or a confirmed encrypted VPN connection.');
  const start = vi.fn().mockRejectedValue(error);
  await expect(startGatewayAuth(client, start, () => true)).rejects.toBe(error);
  expect(start).toHaveBeenCalledOnce(); expect(confirm).not.toHaveBeenCalled();
});
