// @vitest-environment jsdom
// #302: settings show extension load failures and offer an explicit refresh and reload.
import { cleanup, screen, within } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { MachineSettings } from './MachineSettings';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

const catalog = (origin: 'global' | 'project', path: string): SettingsResponse => ({
  revision: 'extensions-1',
  groups: [
    {
      id: 'agent',
      title: 'Agent',
      toggles: [
        { id: 'subagents', label: 'Subagents', type: 'boolean', enabled: true, source: origin },
      ],
    },
    {
      id: 'extension:broken.py',
      title: 'broken.py',
      extension: { name: 'broken.py', origin, path: `${path}/broken.py`, status: 'failed', error: 'SyntaxError: invalid syntax' },
      toggles: [],
    },
    {
      id: 'extension:foundation-mcp',
      title: 'foundation-mcp',
      extension: { name: 'foundation-mcp', origin: 'built_in', status: 'loaded' },
      toggles: [{ id: 'mcp_tools', label: 'MCP tools', type: 'boolean', enabled: true, source: 'default' }],
    },
    {
      id: 'extension:notifier',
      title: 'notifier',
      extension: { name: 'notifier', origin, path: `${path}/notifier.py`, status: 'stale', error: 'NameError: sound' },
      toggles: [{ id: 'notifier_enabled', label: 'Desktop alerts', type: 'boolean', enabled: true, source: origin }],
    },
  ],
});

/** The Extensions band, its extension names at `level`, and the scope word of one extension. */
function extensionsBand(level: number) {
  const band = screen.getByRole('heading', { name: 'Extensions' }).closest('section')!;
  const names = within(band).getAllByRole('heading', { level }).map((heading) => heading.textContent);
  const scope = (name: string) =>
    within(within(band).getByRole('region', { name })).queryByText(/^(global|project)$/)?.textContent ?? null;
  return { band, names, scope };
}

beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'rooms').mockResolvedValue({ configured: false, relays: [] });
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => new Response('{}', { headers: { 'Content-Type': 'application/json' } })),
  );
});

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

it('keeps a failed project extension visible and runs its code only on request', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'project', target_id: 'p1', label: 'Workspace' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  const read = vi.spyOn(client, 'settings').mockResolvedValue({
    ...catalog('project', '.vis/extensions'),
    scope: 'project',
    target_id: 'p1',
  });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  let finishReload: (counts: Awaited<ReturnType<GatewayClient['reloadExtensions']>>) => void = () => {};
  const reload = vi.spyOn(client, 'reloadExtensions').mockImplementation(
    () =>
      new Promise((resolve) => {
        finishReload = resolve;
      }),
  );
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

  expect(await screen.findByText(/Extension failed to load/)).toHaveTextContent('SyntaxError: invalid syntax');
  const { band, names, scope } = extensionsBand(5);
  expect(names).toEqual(['broken.py', 'foundation-mcp', 'notifier']);
  expect(scope('broken.py')).toBe('project');
  expect(scope('foundation-mcp')).toBeNull();
  expect(band).not.toHaveTextContent('.vis/extensions');
  expect(band).not.toContainElement(screen.getByRole('heading', { name: 'Agent' }));
  const notifier = within(band).getByRole('region', { name: 'notifier' });
  expect(within(notifier).getByText(/Vis uses the last loaded version/)).toHaveTextContent('NameError: sound');
  expect(within(notifier).getByRole('switch', { name: 'Desktop alerts: on' })).toBeInTheDocument();

  expect(screen.queryByRole('button', { name: 'Refresh list' })).not.toBeInTheDocument();
  const button = screen.getByRole('button', { name: 'Reload extensions' });
  expect(band.querySelector('header')).toContainElement(button);
  expect(reload).not.toHaveBeenCalled();

  const reads = read.mock.calls.length;
  await user.click(button);
  // The result stands as plain text in the header band, not in a box below it.
  const header = band.querySelector('header')!;
  expect(within(header).getByRole('status')).toHaveTextContent('Reloading…');
  finishReload({ loaded: 1, failed: 1 });
  expect(
    await within(header).findByText('1 loaded, 1 failed. Each failed extension shows its error.'),
  ).toHaveAttribute('role', 'status');
  expect(reload).toHaveBeenCalledWith({ scope: 'project', target_id: 'p1' });
  expect(read.mock.calls.length).toBeGreaterThan(reads);
});

it('keeps matching extensions under Extensions while a search hides the actions', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'project', target_id: 'p1', label: 'Workspace' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockResolvedValue({ ...catalog('project', '.vis/extensions'), scope: 'project', target_id: 'p1' });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

  await user.type(await screen.findByRole('searchbox', { name: 'Search settings' }), 'desktop');
  const { names, scope } = extensionsBand(5);
  expect(names).toEqual(['notifier']);
  expect(scope('notifier')).toBe('project');
  expect(screen.queryByRole('button', { name: 'Reload extensions' })).not.toBeInTheDocument();

  await user.clear(screen.getByRole('searchbox', { name: 'Search settings' }));
  await user.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'subagents');
  expect(screen.queryByRole('heading', { name: 'Extensions' })).not.toBeInTheDocument();
});

it('reloads machine extensions from machine settings and explains an older gateway', async () => {
  const user = userEvent.setup();
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(catalog('global', '~/.vis/extensions'));
  const reload = vi
    .spyOn(GatewayClient.prototype, 'reloadExtensions')
    .mockResolvedValueOnce({ loaded: 3, failed: 0 })
    .mockRejectedValueOnce(
      new GatewayError(404, 'no such route', { error: { type: 'not-found', message: 'no such route' } }),
    );
  renderOpenBands(
    <MachineSettings
      gateway={{ id: 'extensions-test', url: 'http://127.0.0.1:7890', token: 'test' }}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
    />,
  );

  expect(await screen.findByText(/Extension failed to load/)).toBeInTheDocument();
  const { band, names, scope } = extensionsBand(6);
  expect(names).toEqual(['broken.py', 'foundation-mcp', 'notifier']);
  expect(scope('broken.py')).toBe('global');
  expect(scope('notifier')).toBe('global');
  expect(scope('foundation-mcp')).toBeNull();
  expect(band).not.toHaveTextContent('Machine extension');
  expect(band).not.toContainElement(screen.getByRole('heading', { name: 'Agent' }));
  const header = band.querySelector('header')!;
  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(await within(header).findByText('3 loaded, 0 failed.')).toBeInTheDocument();
  expect(reload).toHaveBeenLastCalledWith(undefined);

  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(
    await within(header).findByText('This gateway does not support extension reload. Update Vis on that machine.'),
  ).toBeInTheDocument();
});
it('names each extension once, on the row of its Auto/On/Off choice', async () => {
  // Settings showed each extension name twice: as a heading and again as its choice row.
  const engine = (name: string): Toggle => ({
    id: `engines_${name}`,
    label: name,
    type: 'enum',
    choices: ['auto', 'on', 'off'],
    value: 'auto',
    description: 'Auto detects applicability; On stays active; Off denies tools.',
    source: 'global',
  });
  const target: SettingsTarget = { scope: 'project', target_id: 'p1', label: 'Workspace' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockResolvedValue({
    revision: 'extensions-2',
    scope: 'project',
    target_id: 'p1',
    groups: [
      {
        id: 'extension:vis-optmem',
        title: 'vis-optmem',
        extension: { name: 'vis-optmem', origin: 'global', status: 'loaded' },
        toggles: [engine('vis-optmem')],
      },
      {
        id: 'extension:vis-spel',
        title: 'vis-spel',
        extension: { name: 'vis-spel', origin: 'global', status: 'loaded' },
        toggles: [
          engine('vis-spel'),
          { id: 'spel_headless', label: 'Headless', type: 'boolean', enabled: false, source: 'global' },
          { id: 'skills_1', label: 'vis-spel/browser', type: 'boolean', enabled: true, source: 'global' },
        ],
      },
    ],
  });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

  await screen.findByRole('heading', { name: 'vis-spel' });
  const { band, names, scope } = extensionsBand(5);
  expect(names).toEqual(['vis-optmem', 'vis-spel']);
  expect(within(band).getAllByText('vis-spel')).toHaveLength(1);
  expect(scope('vis-spel')).toBe('global');
  const spel = within(band).getByRole('region', { name: 'vis-spel' });
  expect(within(spel).getByRole('combobox', { name: 'vis-spel' })).toBeInTheDocument();
  // The extension's own settings come first, and its skills stand under their own Skills heading.
  const skills = within(spel).getByRole('region', { name: 'Skills' });
  expect(within(skills).getByRole('switch', { name: 'browser: on' })).toBeInTheDocument();
  expect(within(spel).getByRole('switch', { name: 'Headless: off' })).toBeInTheDocument();
  expect(within(skills).queryByRole('switch', { name: 'Headless: off' })).toBeNull();
  expect(band).not.toHaveTextContent('vis-spel/browser');
  // The Auto/On/Off explanation is not shown: neither above the extensions nor on each one.
  expect(within(band).queryByText(/^Auto detects applicability/)).toBeNull();
});
