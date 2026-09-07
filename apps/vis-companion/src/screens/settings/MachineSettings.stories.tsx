import { useMemo } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import type { McpServer } from '../../lib/types';
import { McpServersPanel } from './MachineSettings';

/**
 * ONE MACHINE'S MCP SERVERS, in the rhythm every other list in Settings keeps:
 * a reach mark on the name's line, the transport target under it, the state or
 * the tool count at the end, and the verbs under a slide (a hover strip with a
 * pointer). The enable switch is the row's one permanent control, as it is on
 * every other on/off setting in the dialog; a config-file server has no switch
 * because this API never rewrites a hand-written tier.
 *
 * The gateway is replaced at the client's own boundary by a fixture that
 * answers the list from memory and echoes every action back into it.
 */
const SERVERS: McpServer[] = [
  {
    name: 'filesystem',
    transport: 'stdio',
    enabled: true,
    is_connected: true,
    is_managed: true,
    is_killed: false,
    tools: 14,
    command: 'npx',
    args: ['-y', '@modelcontextprotocol/server-filesystem', '/workspace'],
    cwd: '/workspace',
  },
  {
    name: 'linear',
    transport: 'streamable_http',
    enabled: true,
    is_connected: false,
    is_managed: true,
    is_killed: false,
    is_authorized: false,
    tools: 0,
    url: 'https://mcp.linear.app/mcp',
  },
  {
    name: 'company-tools',
    transport: 'streamable_http',
    enabled: true,
    is_connected: true,
    is_managed: true,
    is_killed: false,
    is_authorized: true,
    tools: 1,
    url: 'https://gateway.example.com/mcp',
    timeout_ms: 30_000,
  },
  {
    name: 'scratch',
    transport: 'stdio',
    enabled: true,
    is_connected: false,
    is_managed: true,
    is_killed: true,
    tools: 0,
    command: 'python',
    args: ['-m', 'scratch_mcp'],
  },
  {
    name: 'docs',
    transport: 'stdio',
    enabled: false,
    is_connected: false,
    is_managed: true,
    is_killed: false,
    tools: 0,
    command: 'uvx',
    args: ['mcp-server-docs'],
  },
  {
    name: 'sentry',
    transport: 'streamable_http',
    enabled: true,
    is_connected: false,
    is_managed: false,
    is_killed: false,
    is_authorized: true,
    tools: 0,
    url: 'https://mcp.sentry.dev/mcp',
  },
];

/** A gateway that answers from memory. Every verb lands in the list it serves. */
function fixtureClient(initial: McpServer[]): Parameters<typeof McpServersPanel>[0]['client'] {
  let servers = initial.map((server) => ({ ...server }));
  const update = (name: string, patch: Partial<McpServer>) => {
    servers = servers.map((server) => (server.name === name ? { ...server, ...patch } : server));
    return servers.find((server) => server.name === name)!;
  };
  const client = {
    cachedMcpServers: () => servers,
    mcpServers: async () => servers,
    setMcpServerEnabled: async (name: string, enabled: boolean) => update(name, { enabled }),
    killMcpServer: async (name: string) => update(name, { is_killed: true, is_connected: false }),
    startMcpServer: async (name: string) => update(name, { is_killed: false, is_connected: true }),
    deleteMcpServer: async (name: string) => {
      servers = servers.filter((server) => server.name !== name);
    },
    mcpAuthLogout: async (name: string) => update(name, { is_authorized: false, is_connected: false }),
    testMcpServer: async (name: string) => ({ name, tools: [] }),
    saveMcpServer: async (name: string, spec: McpServer) => update(name, spec),
  };
  return client as unknown as Parameters<typeof McpServersPanel>[0]['client'];
}

/** One fixture gateway per mount, so a story's presses never leak into the next. */
function Panel({ servers }: { servers: McpServer[] }) {
  const client = useMemo(() => fixtureClient(servers), [servers]);
  return <McpServersPanel client={client} />;
}

const meta = {
  title: 'Screens/MCP servers panel',
  component: Panel,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof Panel>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Every reach a server can have: connected, waiting for sign-in, killed, off, and a config-file tier. */
export const Fleet: Story = {
  args: { servers: SERVERS },
};

/** Desktop: the verbs wait at the trailing edge and show on hover. */
export const FleetDesktop: Story = {
  ...Fleet,
  parameters: { viewport: { defaultViewport: 'desktop' } },
};

/** A row opens on the whole spec its one line cannot carry. */
export const Details: Story = {
  ...Fleet,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: /^filesystem/ }));
    const details = canvas.getByRole('region', { name: 'filesystem details' });
    await expect(details).toHaveTextContent('/workspace');
    await expect(details).toHaveTextContent('server-filesystem');
  },
};

/** Removing asks in the row itself, at the row's own height. */
export const RemoveAsks: Story = {
  ...Fleet,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Remove scratch from this machine' }));
    await expect(canvas.getByRole('group', { name: 'Remove scratch?' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'No, keep' }));
    await expect(canvas.getByRole('button', { name: /^scratch/ })).toBeVisible();
  },
};

/** Nothing configured yet: the list says so, and the band's ＋ is the way in. */
export const Empty: Story = {
  args: { servers: [] },
};
