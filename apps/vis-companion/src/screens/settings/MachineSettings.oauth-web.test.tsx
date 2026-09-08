// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { McpServersPanel } from './MachineSettings';
import type { GatewayClient } from '../../lib/gateway';
// A web or desktop host: no native receiver, so the page itself must open the browser.
// Regression: the open ran after the flow fetch, outside the tap, and WKWebView and
// popup blockers dropped it silently, so Sign in appeared to do nothing.
vi.mock('@capacitor/core', async importOriginal => ({ ...await importOriginal<typeof import('@capacitor/core')>(),
  Capacitor: { isNativePlatform: () => false, isPluginAvailable: () => false, getPlatform: () => 'web' } }));
const redirect = 'http://127.0.0.1:53692/mcp-callback';
const flow = { server: 'work', flow_id: 'test-flow', kind: 'pkce', status: 'pending', callback_mode: 'loopback',
  redirect_uri: redirect,
  url: `https://gateway.example.com/authorize?state=test-state&redirect_uri=${encodeURIComponent(redirect)}` };
function client(start: () => Promise<typeof flow>) {
  const servers = [{ name: 'work', transport: 'streamable_http', url: 'https://gateway.example.com/mcp', tools: 0, enabled: true }];
  return { cachedMcpServers: () => servers, mcpServers: vi.fn().mockResolvedValue(servers),
    mcpAuthStart: vi.fn().mockImplementation(start), mcpAuthComplete: vi.fn().mockResolvedValue({ ...flow, status: 'ok' }),
    mcpAuthPoll: vi.fn().mockResolvedValue(flow), mcpAuthCancel: vi.fn().mockResolvedValue(undefined) };
}
function reserved() { return { location: { href: '' }, opener: {}, close: vi.fn() }; }
afterEach(() => { cleanup(); vi.restoreAllMocks(); });

it('claims the tab inside the tap and navigates it once the gateway issues the URL', async () => {
  const tab = reserved();
  const opened = vi.spyOn(window, 'open').mockReturnValue(tab as unknown as Window);
  let issue!: (value: typeof flow) => void;
  const gateway = client(() => new Promise(resolve => { issue = resolve; }));
  render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  expect(opened).toHaveBeenCalledExactlyOnceWith('', '_blank');
  expect(tab.location.href).toBe('');
  issue(flow);
  await waitFor(() => expect(tab.location.href).toBe(flow.url));
  expect(opened).toHaveBeenCalledOnce();
  expect(screen.getByText('signing in')).toBeVisible();
});

it('closes the claimed tab when the gateway cannot start the flow', async () => {
  const tab = reserved();
  vi.spyOn(window, 'open').mockReturnValue(tab as unknown as Window);
  const gateway = client(() => Promise.reject(new Error('down')));
  render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(tab.close).toHaveBeenCalledOnce());
  expect(tab.location.href).toBe('');
  expect(screen.getByText(/Cannot start sign-in/)).toBeVisible();
});

it('falls back to a plain open when the browser refuses the popup', async () => {
  const opened = vi.spyOn(window, 'open').mockReturnValue(null);
  const gateway = client(() => Promise.resolve(flow));
  render(<McpServersPanel client={gateway as unknown as GatewayClient} />);
  fireEvent.click(screen.getByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(opened).toHaveBeenLastCalledWith(flow.url, '_blank', 'noopener,noreferrer'));
});
