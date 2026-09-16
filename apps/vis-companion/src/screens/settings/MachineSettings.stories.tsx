import { useMemo } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import type { McpServer } from '../../lib/types';
import { McpServersPanel } from './MachineSettings';

/**
 * MCP servers start with their enable switch, followed by the name and endpoint.
 * The trailing word reports connection status or the available tool count.
 * Config-file switches show their saved state but cannot edit that file; runtime
 * actions remain available through the row's swipe drawer or overflow menu.
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
    mcpAuthLogout: async (name: string) =>
      update(name, { is_authorized: false, is_connected: false }),
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

/** Names lead while endpoints, provenance and tool counts stay readable and secondary. */
export const Typography: Story = {
  args: { servers: SERVERS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const name = await canvas.findByText('filesystem', { exact: true });
    await expect(getComputedStyle(name).fontSize).toBe('13px');
    await expect(getComputedStyle(name).fontWeight).toBe('500');
    for (const element of [
      canvas.getByText('npx', { exact: true }),
      canvas.getByText('Config file', { exact: true }),
      canvas.getByText('14 tools', { exact: true }),
    ]) {
      const style = getComputedStyle(element);
      await expect(style.fontSize).toBe('11px');
      await expect(style.lineHeight).toBe('16px');
      await expect(style.fontWeight).toBe('400');
      await expect(style.textTransform).toBe('none');
    }
  },
};

export const TypographyPointer: Story = {
  ...Typography,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Every reach a server can have: connected, waiting for sign-in, killed, off, and a config-file tier. */
export const Fleet: Story = {
  args: { servers: SERVERS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const isMouse = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    let previousTargetBottom: number | undefined;
    for (const server of SERVERS) {
      const control = canvas.getByRole('switch', {
        name: `${server.name} MCP server: ${server.enabled ? 'on' : 'off'}`,
      });
      const row = canvas.getByRole('button', { name: new RegExp(`^${server.name}`) });
      const switchBox = control.getBoundingClientRect();
      const rowBox = row.getBoundingClientRect();
      const reach = getComputedStyle(control, '::after');
      const targetTop = switchBox.top + parseFloat(reach.top);
      const targetBottom = switchBox.bottom - parseFloat(reach.bottom);
      if (previousTargetBottom !== undefined) {
        await expect(targetTop - previousTargetBottom).toBeGreaterThanOrEqual(8);
      }
      previousTargetBottom = targetBottom;
      // The switch replaces the leading mark, outside the details button. Its
      // invisible touch reach remains separate from that button's target.
      await expect(switchBox.right + 8).toBeLessThanOrEqual(rowBox.left);
      await expect(
        Math.abs(switchBox.top + switchBox.height / 2 - rowBox.top - rowBox.height / 2),
      ).toBeLessThan(1);
      await expect(
        switchBox.height - parseFloat(reach.top) - parseFloat(reach.bottom),
      ).toBeGreaterThanOrEqual(isMouse ? 28 : 44);
      await expect(switchBox.width).toBeGreaterThanOrEqual(isMouse ? 28 : 44);
      await expect(row.contains(control)).toBe(false);
      await expect(row.querySelector('svg[class*="lucide-circle"]')).toBeNull();
      await expect(control).toHaveAttribute('aria-checked', String(server.enabled));
      if (!server.is_managed) {
        await expect(control).toBeDisabled();
        await expect(control).toHaveAccessibleDescription(/config file; edit it there/i);
      }
    }
    const control = canvas.getByRole('switch', { name: 'filesystem MCP server: on' });
    const row = canvas.getByRole('button', { name: /^filesystem/ });
    control.focus();
    await userEvent.keyboard('[Space]');
    await waitFor(() => expect(control).toHaveAttribute('aria-checked', 'false'));
    await waitFor(() => expect(control).not.toBeDisabled());
    await expect(row).toHaveAttribute('aria-expanded', 'false');
    control.focus();
    await userEvent.keyboard('[Enter]');
    await waitFor(() => expect(control).toHaveAttribute('aria-checked', 'true'));
    await userEvent.click(row);
    await expect(canvas.getByRole('region', { name: 'filesystem details' })).toBeVisible();
    await expect(control).toHaveAttribute('aria-checked', 'true');
    await userEvent.click(row);
  },
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
    // The request timeout is the gateway's own 30 s and is neither shown nor edited here.
    await expect(details).not.toHaveTextContent('Timeout');
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

/** Home paths stay compact, and long detail values wrap without breaking every word. */
export const HomePaths: Story = {
  args: {
    servers: [
      {
        name: 'opennews',
        transport: 'stdio',
        enabled: true,
        is_connected: true,
        is_managed: false,
        is_killed: false,
        tools: 24,
        command: '/Users/ana/.vis/mcp-servers/opennews/.venv/bin/opennews-mcp',
        args: ['--label', 'daily news', '--collection', 'international-market-news-archive'.repeat(4)],
        cwd: '/Users/ana/.vis/mcp-servers/opennews',
      },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const row = canvas.getByRole('button', { name: /^opennews/ });
    await userEvent.click(row);
    const details = canvas.getByRole('region', { name: 'opennews details' });
    const command = within(details).getByText(/opennews-mcp$/);
    await expect(getComputedStyle(command).wordBreak).toBe('normal');
    await expect(getComputedStyle(command).overflowWrap).toBe('anywhere');
    await expect(command).toHaveTextContent('~/.vis/mcp-servers/opennews/.venv/bin/opennews-mcp');
    // Prefer folder boundaries over a short first line ending at the first hyphen.
    await expect(command.querySelectorAll('wbr')).toHaveLength(6);
    await expect(
      within(row).getByText('~/.vis/mcp-servers/opennews/.venv/bin/opennews-mcp'),
    ).toBeVisible();
    await expect(details).toHaveTextContent('~/.vis/mcp-servers/opennews');
    for (const value of details.querySelectorAll('span:nth-child(even)')) {
      await expect(value.scrollWidth).toBeLessThanOrEqual(value.clientWidth + 1);
    }
    await expect(details.scrollWidth).toBeLessThanOrEqual(details.clientWidth + 1);
    await userEvent.click(row);
    await expect(canvas.queryByRole('region', { name: 'opennews details' })).not.toBeInTheDocument();
    await userEvent.click(row);
  },
};

/** Nothing configured yet: the list says so, and the band's ＋ is the way in. */
export const Empty: Story = {
  args: { servers: [] },
};
