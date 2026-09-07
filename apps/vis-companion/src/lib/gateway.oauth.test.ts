// @vitest-environment jsdom
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';
afterEach(() => { vi.unstubAllGlobals(); });
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
