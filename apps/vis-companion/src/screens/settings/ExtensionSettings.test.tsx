// @vitest-environment jsdom
// #302: settings show extension load failures and offer an explicit refresh and reload.
import { cleanup, screen, within } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { OpenScopedSettings } from '../../lib/open-settings';
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
      id: 'extension:language-clojure',
      title: 'language-clojure',
      extension: { name: 'language-clojure', origin: 'built_in', status: 'loaded' },
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
  expect(names).toEqual(['broken.py', 'language-clojure', 'notifier']);
  expect(scope('broken.py')).toBe('project');
  expect(scope('language-clojure')).toBeNull();
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
  // #373: the result stands as a note below the header, not beside its button.
  const header = band.querySelector('header')!;
  expect(within(header).queryByRole('status')).toBeNull();
  // #348: the report names each scanned scope. #411: its directory and names are details.
  finishReload({
    loaded: 1,
    failed: 1,
    scopes: [
      { scope: 'global', dirs: ['~/.vis/extensions'], loaded: 0, failed: 0, extensions: [] },
      { scope: 'project', dirs: ['/work/app/.vis/extensions'], loaded: 1, failed: 1, extensions: ['notifier'] },
    ],
    reload_id: 'reload-1',
  });
  const summary = 'No machine extensions. Project extensions: 1 loaded, 1 failed. Each failed extension shows its error.';
  const note = (await within(band).findByText(summary)).closest('[role="status"]') as HTMLElement;
  expect([...note.querySelectorAll('p')].map((line) => line.textContent)).toEqual([summary]);
  expect(note).not.toHaveTextContent('/work/app/.vis/extensions');
  await user.click(within(note).getByRole('button', { name: 'Details' }));
  expect([...note.querySelectorAll('li')].map((line) => line.textContent)).toEqual([
    'Machine extensions in ~/.vis/extensions: none.',
    'Project extensions in /work/app/.vis/extensions: notifier.',
  ]);
  expect(reload).toHaveBeenCalledWith({ scope: 'project', target_id: 'p1' });
  expect(read.mock.calls.length).toBeGreaterThan(reads);
});

it('reloads machine extensions from machine settings and explains an older gateway', async () => {
  const user = userEvent.setup();
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(catalog('global', '~/.vis/extensions'));
  const reload = vi
    .spyOn(GatewayClient.prototype, 'reloadExtensions')
    .mockResolvedValueOnce({
      loaded: 0,
      failed: 0,
      scopes: [{ scope: 'global', dirs: ['~/.vis/extensions'], loaded: 0, failed: 0, extensions: [] }],
      reload_id: 'reload-1',
    })
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
  expect(names).toEqual(['broken.py', 'language-clojure', 'notifier']);
  expect(scope('broken.py')).toBe('global');
  expect(scope('notifier')).toBe('global');
  expect(scope('language-clojure')).toBeNull();
  expect(band).not.toHaveTextContent('Machine extension');
  expect(band).not.toContainElement(screen.getByRole('heading', { name: 'Agent' }));
  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  // #348, #373: an empty machine reload says where project extensions reload. #411: where it
  // looked is a detail.
  const note = (
    await within(band).findByText('No machine extensions. Project extensions reload in Project settings.')
  ).closest('[role="status"]') as HTMLElement;
  await user.click(within(note).getByRole('button', { name: 'Details' }));
  expect(within(note).getByText('Machine extensions in ~/.vis/extensions: none.')).toBeInTheDocument();
  expect(reload).toHaveBeenLastCalledWith(undefined);

  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(
    await within(band).findByText('This gateway does not support extension reload. Update Vis on that machine.'),
  ).toBeInTheDocument();
});
// #373: a machine reload counts the project extensions of the open session and opens Project settings.
// #411: the summary stays short; the project inventory is a detail.
it('counts project extensions after a machine reload and opens Project settings', async () => {
  const user = userEvent.setup();
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
    ...catalog('global', '~/.vis/extensions'),
    project_extensions: { target_id: 'p1', label: 'Workspace', count: 1, names: ['notifier'] },
  });
  vi.spyOn(GatewayClient.prototype, 'reloadExtensions').mockResolvedValue({
    loaded: 0,
    failed: 0,
    scopes: [{ scope: 'global', dirs: ['~/.vis/extensions'], loaded: 0, failed: 0, extensions: [] }],
    reload_id: 'reload-1',
  });
  const openScoped = vi.fn();
  renderOpenBands(
    <OpenScopedSettings.Provider value={openScoped}>
      <MachineSettings
        gateway={{ id: 'extensions-test', url: 'http://127.0.0.1:7890', token: 'test' }}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
        contextSessionId="s1"
      />
    </OpenScopedSettings.Provider>,
  );

  await screen.findByText(/Extension failed to load/);
  expect(screen.queryByRole('button', { name: 'Open Project settings' })).toBeNull();
  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  const summary = 'No machine extensions. 1 project extension reloads in Project settings.';
  const note = (await screen.findByText(summary)).closest('[role="status"]') as HTMLElement;
  expect([...note.querySelectorAll('p')].map((line) => line.textContent)).toEqual([summary]);
  expect(note).not.toHaveTextContent('notifier');
  const details = within(note).getByRole('button', { name: 'Details' });
  expect(details).toHaveAttribute('aria-expanded', 'false');
  await user.click(details);
  expect(details).toHaveAttribute('aria-expanded', 'true');
  expect([...note.querySelectorAll('li')].map((line) => line.textContent)).toEqual([
    'Machine extensions in ~/.vis/extensions: none.',
    'Project extensions of Workspace: notifier.',
  ]);
  await user.click(within(note).getByRole('button', { name: 'Open Project settings' }));
  expect(openScoped).toHaveBeenCalledWith(expect.any(GatewayClient), { scope: 'project', target_id: 'p1', label: 'Workspace' });
});
