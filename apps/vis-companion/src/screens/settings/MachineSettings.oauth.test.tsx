// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { McpServersPanel } from './MachineSettings';
import { GatewayClient, GatewayError, GatewayOAuthError } from '../../lib/gateway';
import type { McpServer } from '../../lib/types';
const native = vi.hoisted(() => ({ handler: (_: { url: string }) => {}, remove: vi.fn() }));
vi.mock('@capacitor/core', async importOriginal => ({ ...await importOriginal<typeof import('@capacitor/core')>(),
  Capacitor: { isNativePlatform: () => true, isPluginAvailable: () => false, getPlatform: () => 'ios' } }));
vi.mock('@capacitor/app', () => ({ App: { addListener: vi.fn(async (_event, handler) => {
  native.handler = handler; return { remove: native.remove };
}) } }));
const flow = { server: 'work', flow_id: 'test-flow', kind: 'pkce', status: 'pending', callback_mode: 'app',
  redirect_uri: 'com.blockether.viscompanion://oauth/callback',
  url: 'https://gateway.example.com/authorize?state=test-state&redirect_uri=com.blockether.viscompanion%3A%2F%2Foauth%2Fcallback' };
function client() {
  const servers = [{ name: 'work', transport: 'streamable_http', url: 'https://gateway.example.com/mcp', tools: 0, enabled: true }];
  return { cachedMcpServers: () => servers, mcpServers: vi.fn().mockResolvedValue(servers),
    mcpAuthStart: vi.fn().mockResolvedValue(flow), mcpAuthComplete: vi.fn().mockResolvedValue({ ...flow, status: 'ok' }),
    mcpAuthPoll: vi.fn().mockResolvedValue(flow), mcpAuthCancel: vi.fn().mockResolvedValue(undefined) };
}
afterEach(() => { cleanup(); vi.restoreAllMocks(); vi.unstubAllGlobals(); });
it('starts native MCP auth, returns directly, closes the pending UI and refreshes the server', async () => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  const gateway = client(); render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in' }));
  await screen.findByText('Waiting for authorization…');
  expect(gateway.mcpAuthStart).toHaveBeenCalledWith('work', 'app');
  expect(screen.queryByRole('textbox')).toBeNull();
  await act(async () => native.handler({ url: `${flow.redirect_uri}?state=test-state&code=test-code` }));
  await waitFor(() => expect(screen.queryByText('Waiting for authorization…')).toBeNull());
  expect(gateway.mcpAuthComplete).toHaveBeenCalledExactlyOnceWith('work', 'test-flow', `${flow.redirect_uri}?state=test-state&code=test-code`);
  expect(gateway.mcpServers).toHaveBeenCalledTimes(2);
});
it('cancels a start that returns after unmount without opening the browser', async () => {
  const opened = vi.spyOn(window, 'open').mockReturnValue(null);
  const gateway = client(); let release!: (value: typeof flow) => void;
  gateway.mcpAuthStart.mockReturnValue(new Promise(resolve => { release = resolve; }));
  const view = render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in' })); view.unmount();
  await act(async () => release(flow));
  expect(opened).not.toHaveBeenCalled(); expect(gateway.mcpAuthCancel).toHaveBeenCalledWith('work', 'test-flow');
});
it('does not send an old callback to a different paired gateway', async () => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  const first = client(); const second = client();
  const view = render(<McpServersPanel client={first as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in' })); await screen.findByText('Waiting for authorization…');
  view.rerender(<McpServersPanel client={second as unknown as GatewayClient} />);
  await act(async () => native.handler({ url: `${flow.redirect_uri}?state=test-state&code=test-code` }));
  expect(second.mcpAuthComplete).not.toHaveBeenCalled(); expect(first.mcpAuthComplete).not.toHaveBeenCalled();
  expect(first.mcpAuthCancel).toHaveBeenCalledWith('work', 'test-flow');
});

// Regression: an HTTP gateway reached through an encrypted VPN was refused before fetch,
// and the UI replaced that reason with a generic connection error.
function pairedHttpClient() {
  const gateway = new GatewayClient({ url: 'http://10.0.0.5:7890', token: 'test-paired-token' });
  const servers = client().cachedMcpServers() as McpServer[];
  vi.spyOn(gateway, 'cachedMcpServers').mockReturnValue(servers);
  vi.spyOn(gateway, 'mcpServers').mockResolvedValue(servers);
  return gateway;
}
it('starts native MCP sign-in on a paired HTTP gateway without another consent dialog', async () => {
  const opened = vi.spyOn(window, 'open').mockReturnValue(null);
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
  const fetch = vi.fn().mockImplementation(async (url: string) => new Response(JSON.stringify({
    ...flow, status: url.endsWith('/complete') ? 'ok' : 'pending',
  })));
  vi.stubGlobal('fetch', fetch);
  const gateway = pairedHttpClient();
  render(<McpServersPanel client={gateway} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in' }));
  await screen.findByText('Waiting for authorization…');
  expect(confirm).not.toHaveBeenCalled();
  await waitFor(() => expect(opened).toHaveBeenCalledWith(flow.url, '_blank', 'noopener,noreferrer'));
  await act(async () => native.handler({ url: `${flow.redirect_uri}?state=test-state&code=test-code` }));
  await waitFor(() => expect(screen.queryByText('Waiting for authorization…')).toBeNull());
  const completion = fetch.mock.calls.find(([url]) => url.endsWith('/complete'))!;
  expect(completion[0]).toBe('http://10.0.0.5:7890/v1/mcp/servers/work/auth/complete');
  expect(JSON.parse(completion[1].body)).toEqual({ flow_id: 'test-flow', input: `${flow.redirect_uri}?state=test-state&code=test-code` });
  for (const [url, options] of fetch.mock.calls) {
    expect(url).toMatch(/^http:\/\/10\.0\.0\.5:7890\/v1\/mcp\/servers\/work\/auth\//);
    expect(options.headers.get('Authorization')).toBe('Bearer test-paired-token');
    expect(options.redirect).toBe('error');
    expect(options.cache).toBe('no-store');
  }
  expect(confirm).not.toHaveBeenCalled();
});
it.each([
  [new GatewayOAuthError('pairing-required'), 'Pair this gateway before starting sign-in.'],
  [new GatewayError(400, 'test-sensitive-callback-value'),
    'Cannot start sign-in. Check the gateway and MCP server settings, then try again.'],
])('shows safe local start errors without exposing remote error text', async (error, message) => {
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(true);
  const gateway = client(); gateway.mcpAuthStart.mockRejectedValue(error);
  render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in' }));
  await screen.findByText(message as string);
  expect(screen.queryByText('test-sensitive-callback-value')).toBeNull();
  expect(confirm).not.toHaveBeenCalled();
});
