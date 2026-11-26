// @vitest-environment jsdom
import { cleanup, fireEvent, screen, waitFor } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import { afterEach, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../../lib/gateway';
import type { McpServer, McpServerInput, SettingsTarget } from '../../lib/types';
import { McpServersPanel } from './MachineSettings';

const PROJECT: SettingsTarget = { scope: 'project', target_id: 'p1' };

const SERVER: McpServer = {
  name: 'work',
  transport: 'streamable_http',
  enabled: true,
  is_connected: false,
  status: 'disconnected',
  is_managed: true,
  is_killed: false,
  is_authorized: false,
  tools: 0,
  url: 'https://mcp.example.com/mcp',
  source: 'project',
};

afterEach(cleanup);

function gateway(server: McpServer) {
  return {
    cachedMcpServers: () => [server],
    mcpServers: vi.fn(async () => [server]),
    mcpAuthStart: vi.fn(() => new Promise(() => {})),
    testMcpServer: vi.fn(async (name: string) => ({ name, is_connected: true, needs_auth: false, tools: [] })),
    saveMcpServer: vi.fn(async (_name: string, _server: McpServerInput) => server),
  };
}

// Regression: project, group and session settings hid every OAuth verb, so a
// scoped server that asked for a sign-in could never connect.
it('offers sign-in for a project server that asks for it, and signs in at that scope', async () => {
  vi.spyOn(window, 'open').mockReturnValue(null);
  const client = gateway({ ...SERVER, needs_auth: true });
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  fireEvent.click(await screen.findByRole('button', { name: 'Sign in to work' }));
  await waitFor(() => expect(client.mcpAuthStart).toHaveBeenCalledExactlyOnceWith('work', PROJECT));
});

// Regression: a server with its own bearer token, or a public one, was told to sign in.
it.each([
  ['a bearer token', { header_names: ['Authorization'], needs_auth: false }],
  ['a public server', { needs_auth: false }],
  ['a connected public server', { needs_auth: false, is_connected: true, status: 'connected' }],
])('does not ask %s to sign in', async (_kind, patch) => {
  const client = gateway({ ...SERVER, ...patch } as McpServer);
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  expect(screen.queryByRole('button', { name: 'Sign in to work' })).toBeNull();
  expect(screen.queryByRole('button', { name: 'Sign in to work with OAuth', hidden: true })).toBeNull();
});

// Regression: there was no bearer token field, and scoped forms had no headers at all.
it('saves a bearer token as the Authorization header of a project server', async () => {
  const client = gateway(SERVER);
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
  fireEvent.click(screen.getByRole('button', { name: 'Streamable HTTP' }));
  fireEvent.change(screen.getByLabelText('Server name'), { target: { value: 'api' } });
  fireEvent.change(screen.getByLabelText('Streamable HTTP endpoint'), {
    target: { value: 'https://api.example.com/mcp' },
  });
  fireEvent.change(screen.getByLabelText(/^Bearer token/), { target: { value: 'Bearer kb-test-token' } });
  fireEvent.change(screen.getByLabelText(/^Other headers/), { target: { value: 'X-Team: blue' } });
  fireEvent.click(screen.getByRole('button', { name: 'Save here' }));
  const spec = {
    transport: 'streamable_http',
    url: 'https://api.example.com/mcp',
    headers: { 'X-Team': 'blue', Authorization: 'Bearer kb-test-token' },
  };
  await waitFor(() => expect(client.saveMcpServer).toHaveBeenCalledExactlyOnceWith('api', spec, PROJECT));
  expect(client.testMcpServer).toHaveBeenCalledExactlyOnceWith('api', spec, PROJECT);
});

// Regression: removing the stored token while entering new headers kept the token.
it('removes a stored token when the other headers are replaced', async () => {
  const client = gateway({ ...SERVER, header_names: ['Authorization'], needs_auth: false });
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  fireEvent.click(screen.getByRole('button', { name: 'Edit work', hidden: true }));
  fireEvent.change(screen.getByLabelText(/^Other headers/), { target: { value: 'X-Team: red' } });
  fireEvent.click(screen.getByText('Remove the stored token and headers'));
  fireEvent.click(screen.getByRole('button', { name: 'Save here' }));
  await waitFor(() =>
    expect(client.saveMcpServer).toHaveBeenCalledWith(
      'work',
      expect.objectContaining({ headers: { 'X-Team': 'red', Authorization: '' } }),
      PROJECT,
    ),
  );
});

// Regression: a working server with its key in the URL said "Not signed in",
// and the screen showed the key.
it('shows no sign-in for a key-in-URL server and hides the key', async () => {
  const url = 'https://mcp.example.com/?access=kb-url-key';
  const client = gateway({ ...SERVER, url, needs_auth: false, is_connected: true, status: 'connected', tools: 4 } as McpServer);
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  fireEvent.click(screen.getByText('work'));
  const details = await screen.findByRole('region', { name: 'work details' });
  expect(details).toHaveTextContent('Tools');
  expect(details).not.toHaveTextContent('Sign-in');
  expect(document.body).not.toHaveTextContent('kb-url-key');
  expect(document.body.innerHTML).not.toContain('kb-url-key');
});

// Regression: on a phone, closing the add form after a save left the page below
// the list, so the new server was out of view.
it('scrolls the saved server into view, open', async () => {
  const added: McpServer = { ...SERVER, name: 'api', url: 'https://api.example.com/mcp', is_connected: true, status: 'connected', tools: 3 };
  const client = gateway(SERVER);
  client.mcpServers.mockResolvedValueOnce([SERVER]).mockResolvedValue([SERVER, added]);
  const scroll = vi.spyOn(Element.prototype, 'scrollIntoView');
  renderOpenBands(<McpServersPanel client={client as unknown as GatewayClient} target={PROJECT} />);
  await waitFor(() => expect(client.mcpServers).toHaveBeenCalledOnce());
  fireEvent.click(screen.getByRole('button', { name: 'Add an MCP server' }));
  fireEvent.click(screen.getByRole('button', { name: 'Streamable HTTP' }));
  fireEvent.change(screen.getByLabelText('Server name'), { target: { value: 'api' } });
  fireEvent.change(screen.getByLabelText('Streamable HTTP endpoint'), { target: { value: added.url } });
  fireEvent.click(screen.getByRole('button', { name: 'Save here' }));
  expect(await screen.findByRole('region', { name: 'api details' })).toHaveTextContent('3');
  await waitFor(() =>
    expect(scroll.mock.contexts).toContainEqual(document.querySelector('[data-mcp-server="api"]')),
  );
  scroll.mockRestore();
});
