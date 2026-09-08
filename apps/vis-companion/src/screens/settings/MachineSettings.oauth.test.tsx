// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { McpServersPanel } from './MachineSettings';
import { GatewayClient, GatewayError, GatewayOAuthError } from '../../lib/gateway';
import type { McpServer } from '../../lib/types';
// The native loopback receiver: it binds the gateway-issued port on this device and
// opens the system browser itself, so nothing here depends on a browser gesture.
const host = vi.hoisted(() => ({ authorize: vi.fn(), cancel: vi.fn(), reopen: vi.fn() }));
vi.mock('@capacitor/core', async importOriginal => ({ ...await importOriginal<typeof import('@capacitor/core')>(),
  Capacitor: { isNativePlatform: () => true, isPluginAvailable: () => true, getPlatform: () => 'ios' },
  registerPlugin: () => host }));
const redirect = 'http://127.0.0.1:53692/mcp-callback';
const flow = { server: 'work', flow_id: 'test-flow', kind: 'pkce', status: 'pending', callback_mode: 'loopback',
  redirect_uri: redirect,
  url: `https://gateway.example.com/authorize?state=test-state&redirect_uri=${encodeURIComponent(redirect)}` };
const callback = `${redirect}?state=test-state&code=test-code`;
function client() {
  const servers = [{ name: 'work', transport: 'streamable_http', url: 'https://gateway.example.com/mcp', tools: 0, enabled: true }];
  return { cachedMcpServers: () => servers, mcpServers: vi.fn().mockResolvedValue(servers),
    mcpAuthStart: vi.fn().mockResolvedValue(flow), mcpAuthComplete: vi.fn().mockResolvedValue({ ...flow, status: 'ok' }),
    mcpAuthPoll: vi.fn().mockResolvedValue(flow), mcpAuthCancel: vi.fn().mockResolvedValue(undefined) };
}
/** The browser holds the sign-in until the test hands back its callback. */
function browser() {
  let done!: (value: { url: string }) => void;
  host.authorize.mockReturnValue(new Promise<{ url: string }>(resolve => { done = resolve; }));
  return { returns: () => act(async () => done({ url: callback })) };
}
beforeEach(() => { host.authorize.mockReset(); host.cancel.mockResolvedValue(undefined); host.reopen.mockResolvedValue(undefined); });
afterEach(() => { cleanup(); vi.restoreAllMocks(); vi.unstubAllGlobals(); });
it('opens the browser at once, without a panel, and finishes when the callback returns', async () => {
  const opened = vi.spyOn(window, 'open').mockReturnValue(null);
  const { returns } = browser();
  const gateway = client(); render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(host.authorize).toHaveBeenCalledWith(expect.objectContaining({
    flowId: 'test-flow', authorizationUrl: flow.url, redirectUri: redirect, state: 'test-state' })));
  expect(gateway.mcpAuthStart).toHaveBeenCalledExactlyOnceWith('work');
  expect(opened).not.toHaveBeenCalled();
  // The row says so; there is no explanatory block under it and nothing to paste.
  expect(screen.getByText('signing in')).toBeVisible();
  expect(screen.queryByText(/Waiting for authorization/)).toBeNull();
  expect(screen.queryByRole('textbox')).toBeNull();
  expect(screen.getByRole('button', { name: 'Cancel signing in to work' })).toBeVisible();
  await returns();
  await waitFor(() => expect(screen.queryByText('signing in')).toBeNull());
  expect(gateway.mcpAuthComplete).toHaveBeenCalledExactlyOnceWith('work', 'test-flow', callback);
  expect(gateway.mcpServers).toHaveBeenCalledTimes(2);
});
it('takes the sign-in back from the same slot it started in', async () => {
  browser();
  const gateway = client(); render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await screen.findByRole('button', { name: 'Cancel signing in to work' });
  fireEvent.click(screen.getByRole('button', { name: 'Cancel signing in to work' }));
  await waitFor(() => expect(gateway.mcpAuthCancel).toHaveBeenCalledWith('work', 'test-flow'));
  expect(host.cancel).toHaveBeenCalledWith({ flowId: 'test-flow' });
  expect(screen.getByRole('button', { name: 'Sign in to work' })).toBeVisible();
  expect(gateway.mcpAuthComplete).not.toHaveBeenCalled();
});
it('cancels a start that returns after unmount without opening the browser', async () => {
  const opened = vi.spyOn(window, 'open').mockReturnValue(null);
  const gateway = client(); let release!: (value: typeof flow) => void;
  gateway.mcpAuthStart.mockReturnValue(new Promise(resolve => { release = resolve; }));
  const view = render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' })); view.unmount();
  await act(async () => release(flow));
  expect(opened).not.toHaveBeenCalled(); expect(host.authorize).not.toHaveBeenCalled();
  expect(gateway.mcpAuthCancel).toHaveBeenCalledWith('work', 'test-flow');
});
it('does not send an old callback to a different paired gateway', async () => {
  const { returns } = browser();
  const first = client(); const second = client();
  const view = render(<McpServersPanel client={first as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(host.authorize).toHaveBeenCalledOnce());
  view.rerender(<McpServersPanel client={second as unknown as GatewayClient} />);
  await returns();
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
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
  const fetch = vi.fn().mockImplementation(async (url: string) => new Response(JSON.stringify({
    ...flow, status: url.endsWith('/complete') ? 'ok' : 'pending',
  })));
  vi.stubGlobal('fetch', fetch);
  const { returns } = browser();
  const gateway = pairedHttpClient();
  render(<McpServersPanel client={gateway} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(host.authorize).toHaveBeenCalledOnce());
  expect(confirm).not.toHaveBeenCalled();
  await returns();
  await waitFor(() => expect(screen.queryByText('signing in')).toBeNull());
  const start = fetch.mock.calls.find(([url]) => url.endsWith('/start'))!;
  expect(JSON.parse(start[1].body)).toEqual({ callback_mode: 'loopback' });
  const completion = fetch.mock.calls.find(([url]) => url.endsWith('/complete'))!;
  expect(completion[0]).toBe('http://10.0.0.5:7890/v1/mcp/servers/work/auth/complete');
  expect(JSON.parse(completion[1].body)).toEqual({ flow_id: 'test-flow', input: callback });
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
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await screen.findByText(message as string);
  expect(screen.queryByText('test-sensitive-callback-value')).toBeNull();
  expect(confirm).not.toHaveBeenCalled();
});
