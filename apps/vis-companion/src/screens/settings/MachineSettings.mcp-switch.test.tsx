// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../../lib/gateway';
import type { McpServer } from '../../lib/types';
import { McpServersPanel } from './MachineSettings';

const SERVER: McpServer = {
  name: 'linear',
  transport: 'streamable_http',
  enabled: true,
  is_connected: true,
  is_managed: true,
  is_killed: false,
  is_authorized: true,
  tools: 66,
  url: 'https://mcp.linear.app/mcp',
};

afterEach(cleanup);

async function openServer(patch: Partial<McpServer> = {}) {
  let server = { ...SERVER, ...patch };
  const client = {
    cachedMcpServers: () => [server],
    mcpServers: vi.fn(async () => [server]),
    setMcpServerEnabled: vi.fn(async (_name: string, enabled: boolean) => {
      server = { ...server, enabled };
      return server;
    }),
  };
  render(<McpServersPanel client={client as unknown as GatewayClient} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  return {
    client,
    control: screen.getByRole('switch', { name: /^linear MCP server:/ }),
    row: screen.getByRole('button', { name: /^linear/ }),
  };
}

// Regression: the requested leading switch replaces the status mark, rather than
// remaining after the disclosure and overflow actions at the far right.
it('puts one independent switch before the server details without a status icon', async () => {
  const { client, control, row } = await openServer();
  expect(control.compareDocumentPosition(row) & Node.DOCUMENT_POSITION_FOLLOWING).not.toBe(0);
  expect(screen.getAllByRole('switch')).toHaveLength(1);
  expect(row.contains(control)).toBe(false);
  expect(row.querySelector('svg[class*="lucide-circle"]')).toBeNull();
  expect(control).toHaveAttribute('aria-checked', 'true');
  fireEvent.click(control);
  await waitFor(() => expect(control).toHaveAttribute('aria-checked', 'false'));
  expect(client.setMcpServerEnabled).toHaveBeenCalledExactlyOnceWith('linear', false);
  expect(row).toHaveAttribute('aria-expanded', 'false');
  fireEvent.click(row);
  expect(screen.getByRole('region', { name: 'linear details' })).toBeVisible();
  expect(client.setMcpServerEnabled).toHaveBeenCalledTimes(1);
});

it.each([true, false])('keeps a config-file switch read-only when enabled=%s', async (enabled) => {
  const { client, control, row } = await openServer({ is_managed: false, enabled });
  expect(control).toBeDisabled();
  expect(control).toHaveAttribute('aria-checked', String(enabled));
  expect(control).toHaveAccessibleDescription(/config file; edit it there/i);
  expect(row).toHaveTextContent('Config file');
  expect(row.querySelector('svg[class*="lucide-circle"]')).toBeNull();
  fireEvent.click(control);
  expect(client.setMcpServerEnabled).not.toHaveBeenCalled();
  fireEvent.click(row);
  expect(screen.getByRole('region', { name: 'linear details' })).toBeVisible();
});

it.each<[Partial<McpServer>, string, string]>([
  [{}, 'Connected', '66 tools'],
  [{ is_killed: true }, 'Killed — start it to reconnect', 'killed'],
  [{ enabled: false }, 'Disabled', ''],
  [{ is_connected: false, is_authorized: false }, 'Not signed in — sign in to connect', 'sign in'],
  [{ is_connected: false }, 'Connecting', 'connecting'],
])('preserves connection status without a leading mark: %s', async (patch, label, word) => {
  const { control, row } = await openServer(patch);
  expect(row).toHaveAccessibleDescription(label);
  expect(control).toHaveAccessibleDescription(label);
  if (word) expect(row).toHaveTextContent(word);
});

it('keeps the switch busy while saving and preserves its value after a failure', async () => {
  const { client, control, row } = await openServer();
  let reject!: (error: Error) => void;
  client.setMcpServerEnabled.mockImplementationOnce(
    () =>
      new Promise((_resolve, fail) => {
        reject = fail;
      }),
  );
  fireEvent.click(control);
  expect(control).toHaveAttribute('aria-busy', 'true');
  expect(control).toBeDisabled();
  fireEvent.click(control);
  expect(client.setMcpServerEnabled).toHaveBeenCalledTimes(1);
  reject(new Error('Could not save MCP server'));
  await screen.findByText('Could not save MCP server');
  expect(control).toHaveAttribute('aria-checked', 'true');
  expect(control).not.toBeDisabled();
  expect(row).toHaveAttribute('aria-expanded', 'false');
  fireEvent.click(control);
  await waitFor(() => expect(control).toHaveAttribute('aria-checked', 'false'));
  expect(client.setMcpServerEnabled).toHaveBeenCalledTimes(2);
  expect(screen.queryByText('Could not save MCP server')).toBeNull();
});
