// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import type { GatewayClient } from '../../lib/gateway';
import type { McpServer } from '../../lib/types';
import { McpServersPanel } from './MachineSettings';

const server: McpServer = {
  name: 'opennews',
  transport: 'stdio',
  enabled: true,
  is_connected: true,
  is_managed: false,
  is_killed: false,
  tools: 24,
};

async function showServer(spec: McpServer) {
  const client = {
    cachedMcpServers: () => [spec],
    mcpServers: vi.fn().mockResolvedValue([spec]),
  };
  render(<McpServersPanel client={client as unknown as GatewayClient} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  return screen.getByRole('button', { name: /^opennews/ });
}

// Regression: a full home prefix and break-all split the executable name in Settings.
it.each([
  ['/Users/ana', '/'],
  ['/home/ana', '/'],
  ['C:\\Users\\Ana', '\\'],
])('homeifies %s in the summary and details without losing the original paths', async (home, sep) => {
  const cwd = [home, '.vis', 'mcp-servers', 'opennews'].join(sep);
  const command = [cwd, '.venv', 'bin', 'opennews-mcp'].join(sep);
  const row = await showServer({ ...server, command, cwd });
  const compactCommand = '~/.vis/mcp-servers/opennews/.venv/bin/opennews-mcp';
  expect(within(row).getByText(compactCommand)).toHaveAttribute('title', command);
  fireEvent.click(row);
  const details = screen.getByRole('region', { name: 'opennews details' });
  expect(within(details).getByText(compactCommand)).toHaveAttribute('title', command);
  expect(within(details).getByText('~/.vis/mcp-servers/opennews')).toHaveAttribute('title', cwd);
  expect(row).toHaveAttribute('aria-expanded', 'true');
  fireEvent.click(row);
  expect(screen.queryByRole('region', { name: 'opennews details' })).toBeNull();
  expect(row).toHaveAttribute('aria-expanded', 'false');
});

it.each(['uvx', './bin/opennews-mcp', '/opt/mcp/bin/opennews-mcp', '~/bin/opennews-mcp'])(
  'leaves non-home command %s and argument text unchanged',
  async (command) => {
    const args = ['--directory', '/Users/ana/news', '--label', 'daily news'];
    const row = await showServer({ ...server, command, args, cwd: '/opt/mcp' });
    expect(within(row).getByText(command)).toBeVisible();
    fireEvent.click(row);
    const details = within(screen.getByRole('region', { name: 'opennews details' }));
    expect(details.getByText(command)).toBeVisible();
    expect(details.getByText('/opt/mcp')).toBeVisible();
    expect(details.getByText(args.join(' '))).toBeVisible();
  },
);

it('leaves HTTP endpoints unchanged', async () => {
  const url = 'https://gateway.example.com/Users/ana/mcp';
  const row = await showServer({ ...server, transport: 'streamable_http', url, is_authorized: true });
  expect(within(row).getByText(url)).toBeVisible();
  fireEvent.click(row);
  const details = within(screen.getByRole('region', { name: 'opennews details' }));
  expect(details.getByText(url)).toBeVisible();
  expect(details.getByText('Signed in')).toBeVisible();
});

it('keeps absolute paths in the edit form', async () => {
  const command = '/Users/ana/.vis/mcp-servers/opennews/.venv/bin/opennews-mcp';
  const cwd = '/Users/ana/.vis/mcp-servers/opennews';
  await showServer({ ...server, is_managed: true, command, cwd });
  fireEvent.click(screen.getByRole('button', { name: 'Edit opennews' }));
  expect(screen.getByDisplayValue(command)).toBeVisible();
  expect(screen.getByDisplayValue(cwd)).toBeVisible();
});
