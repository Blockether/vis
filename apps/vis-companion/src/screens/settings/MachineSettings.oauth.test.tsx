// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { McpServersPanel } from './MachineSettings';
import type { GatewayClient } from '../../lib/gateway';
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
afterEach(() => { cleanup(); vi.restoreAllMocks(); });
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
