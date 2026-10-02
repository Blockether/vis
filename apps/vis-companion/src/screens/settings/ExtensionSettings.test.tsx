// @vitest-environment jsdom
// #302: settings show extension load failures and offer an explicit refresh and reload.
import { cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { MachineSettings } from './MachineSettings';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

const catalog = (origin: 'global' | 'project', path: string): SettingsResponse => ({
  revision: 'extensions-1',
  groups: [
    {
      id: 'extension:broken.py',
      title: 'broken.py',
      extension: { name: 'broken.py', origin, path: `${path}/broken.py`, status: 'failed', error: 'SyntaxError: invalid syntax' },
      toggles: [],
    },
    {
      id: 'extension:notifier',
      title: 'notifier',
      extension: { name: 'notifier', origin, path: `${path}/notifier.py`, status: 'stale', error: 'NameError: sound' },
      toggles: [{ id: 'notifier_enabled', label: 'Desktop alerts', type: 'boolean', enabled: true, source: origin }],
    },
  ],
});

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
  const reload = vi.spyOn(client, 'reloadExtensions').mockResolvedValue({ loaded: 1, failed: 1 });
  render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

  expect(await screen.findByText(/Extension failed to load/)).toHaveTextContent('SyntaxError: invalid syntax');
  expect(screen.getByText('Project extension · .vis/extensions/broken.py')).toBeInTheDocument();
  expect(screen.getByText(/Vis uses the last loaded version/)).toHaveTextContent('NameError: sound');
  expect(screen.getByRole('switch', { name: 'Desktop alerts: on' })).toBeInTheDocument();

  const reads = read.mock.calls.length;
  await user.click(screen.getByRole('button', { name: 'Refresh list' }));
  expect(await screen.findByText('List refreshed. No extension code ran.')).toBeInTheDocument();
  expect(reload).not.toHaveBeenCalled();
  expect(read.mock.calls.length).toBeGreaterThan(reads);

  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(
    await screen.findByText('1 loaded, 1 failed. Each failed extension shows its error.'),
  ).toBeInTheDocument();
  expect(reload).toHaveBeenCalledWith({ scope: 'project', target_id: 'p1' });
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
  render(
    <MachineSettings
      gateway={{ id: 'extensions-test', url: 'http://127.0.0.1:7890', token: 'test' }}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
    />,
  );

  expect(await screen.findByText(/Extension failed to load/)).toBeInTheDocument();
  expect(screen.getByText('Machine extension · ~/.vis/extensions/broken.py')).toBeInTheDocument();
  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(await screen.findByText('3 loaded, 0 failed.')).toBeInTheDocument();
  expect(reload).toHaveBeenLastCalledWith(undefined);

  await user.click(screen.getByRole('button', { name: 'Reload extensions' }));
  expect(
    await screen.findByText('This gateway does not support extension reload. Update Vis on that machine.'),
  ).toBeInTheDocument();
});
