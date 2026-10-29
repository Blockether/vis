// @vitest-environment jsdom
import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { OpenMachineSettings, OpenScopedSettings } from '../../lib/open-settings';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

it('writes and inherits only the selected owner, without carrying form state to another session', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  let own = false;
  const response = (owner?: SettingsTarget): SettingsResponse => ({
    revision: 'scoped-1',
    scope: 'session', target_id: owner?.target_id,
    groups: [{ id: 'agent', title: 'Agent', toggles: [{
      id: 'subagents', label: `Subagents ${owner?.target_id}`, type: 'boolean',
      enabled: false, scopes: ['global', 'session'], source: own ? 'session' : 'global',
      is_override: own,
    }] }],
  });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  const read = vi.spyOn(client, 'settings').mockImplementation(async (_signal, owner) => response(owner));
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  const write = vi.spyOn(client, 'setSetting').mockImplementation(async (_id, action) => {
    own = action !== 'inherit';
    return response(target).groups[0].toggles[0];
  });
  const view = renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByText('Inherited from global');
  await user.click(screen.getByRole('switch', { name: 'Subagents a: off' }));
  await screen.findByText('Set here');
  expect(write).toHaveBeenLastCalledWith('subagents', 'toggle', undefined, target);
  await user.click(screen.getByRole('button', { name: 'Use inherited value' }));
  await screen.findByText('Inherited from global');
  expect(write).toHaveBeenLastCalledWith('subagents', 'inherit', undefined, target);
  await user.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'missing');
  view.rerender(<ScopedSettingsDialog client={client} target={{ scope: 'session', target_id: 'b' }} onClose={() => {}} />);
  await screen.findByRole('switch', { name: 'Subagents b: off' });
  expect(screen.queryByText('Subagents a')).toBeNull();
  expect(screen.getByRole('searchbox', { name: 'Search settings' })).toHaveValue('');
  await waitFor(() => expect(read).toHaveBeenLastCalledWith(expect.any(AbortSignal), { scope: 'session', target_id: 'b' }));
});
it('removes a session secret with a confirmation, and an inherited secret offers no Remove', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  let own = true;
  const response = (): SettingsResponse => ({
    revision: 'scoped-secret',
    scope: 'session', target_id: 'a',
    groups: [{ id: 'extension:graphify', title: 'graphify', toggles: [{
      id: 'graphify_kb_token', label: 'Knowledge base token', type: 'string', editor: 'secret',
      is_set: true, max_length: 4096, scope: 'session', scopes: ['global', 'session'],
      source: own ? 'session' : 'global', is_override: own, inherited_source: 'global',
    }] }],
  });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockImplementation(async () => response());
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  const write = vi.spyOn(client, 'setSetting').mockImplementation(async (_id, action) => {
    own = action !== 'inherit';
    return response().groups[0].toggles[0];
  });
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByText('Set here');
  expect(screen.getByText('Saved')).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Use inherited value' })).toBeNull();
  await user.click(screen.getByRole('button', { name: 'Remove' }));
  const question = screen.getByRole('group', { name: 'Remove Knowledge base token?' });
  await user.click(within(question).getByRole('button', { name: 'Remove' }));
  await screen.findByText('Inherited from global');
  expect(write).toHaveBeenLastCalledWith('graphify_kb_token', 'inherit', undefined, target);
  expect(screen.getByText('Saved')).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Remove' })).toBeNull();
  expect(screen.getByRole('button', { name: 'Replace' })).toBeEnabled();
});
// Every settings band opens folded, so the dialog first shows only group names. A
// search opens every band again, so a folded band cannot hide a match.
it('opens with every band folded and opens them all for a search', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockResolvedValue({
    revision: 'scoped-1',
    scope: 'session', target_id: 'a',
    groups: [{ id: 'agent', title: 'Agent', toggles: [{
      id: 'subagents', label: 'Subagents', type: 'boolean',
      enabled: false, scopes: ['global', 'session'], source: 'global', is_override: false,
    }] }],
  });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByRole('heading', { name: 'Agent' });
  expect(screen.queryByRole('switch', { name: 'Subagents: off' })).toBeNull();
  await user.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'agent');
  expect(await screen.findByRole('switch', { name: 'Subagents: off' })).toBeInTheDocument();
});

it('keeps gateway caches and writes separate for equal setting ids in different scopes', async () => {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (input, init) => {
    const url = new URL(String(input));
    return Response.json({ id: 'subagents', label: 'Subagents', type: 'boolean',
      enabled: url.searchParams.get('target_id') === 'a', ...(init?.body ? JSON.parse(String(init.body)) : {}) });
  });
  const a: SettingsTarget = { scope: 'session', target_id: 'a' };
  const b: SettingsTarget = { scope: 'group', target_id: 'a' };
  await client.setting('subagents', undefined, a);
  expect(client.cachedSetting('subagents', a)?.enabled).toBe(true);
  expect(client.cachedSetting('subagents', b)).toBeNull();
  await client.setSetting('subagents', 'value', false, b);
  const body = JSON.parse(String(fetcher.mock.calls.at(-1)?.[1]?.body));
  expect(body).toMatchObject({ id: 'subagents', action: 'value', value: false, ...b });
});

it('does not claim a failed search before one exists when the catalog has no toggles', async () => {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  const target: SettingsTarget = { scope: 'group', target_id: 'wallet' };
  const data: SettingsResponse = { revision: 'scoped-1', scope: 'group', target_id: 'wallet', groups: [] };
  vi.spyOn(client, 'cachedSettings').mockReturnValue(data);
  vi.spyOn(client, 'settings').mockResolvedValue(data);
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  expect(screen.getByRole('heading', { name: 'MCP servers' })).toBeInTheDocument();
  expect(screen.queryByText(/No settings match/)).toBeNull();
});

// Reported on a phone: with a slow gateway, project and group settings jumped as
// each answer arrived, and the sheet showed a half-built Tools section under the
// loading text.
it('keeps one phone-height sheet and shows only a loading panel until the catalog arrives', async () => {
  const target: SettingsTarget = { scope: 'group', target_id: 'wallet', label: 'Wallet work' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  let answer: (response: SettingsResponse) => void = () => {};
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockImplementation(() => new Promise((resolve) => { answer = resolve; }));
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue(null);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByRole('status', { name: 'Loading settings' });
  for (const name of ['Tools', 'MCP servers', 'Extensions']) {
    expect(screen.queryByRole('heading', { name })).toBeNull();
  }
  answer({
    revision: 'wallet-1', scope: 'group', target_id: 'wallet',
    groups: [{ id: 'agent', title: 'Agent', toggles: [{
      id: 'subagents', label: 'Subagents', type: 'boolean', enabled: false,
      scopes: ['global', 'group'], source: 'global', is_override: false,
    }] }],
  });
  await screen.findByRole('heading', { name: 'Agent' });
  expect(screen.queryByRole('status', { name: 'Loading settings' })).toBeNull();
  expect(screen.getByRole('heading', { name: 'Tools' })).toBeInTheDocument();
});

it('offers a retry after a failed first read and drops the failure when the gateway answers', async () => {
  const target: SettingsTarget = { scope: 'project', target_id: '/work/wallet' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings')
    .mockRejectedValueOnce(new Error('gateway timed out'))
    .mockResolvedValue({
      revision: 'wallet-1', scope: 'project', target_id: '/work/wallet',
      groups: [{ id: 'agent', title: 'Agent', toggles: [{
        id: 'subagents', label: 'Subagents', type: 'boolean', enabled: false,
        scopes: ['global', 'project'], source: 'global', is_override: false,
      }] }],
    });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByText('gateway timed out');
  expect(screen.queryByRole('heading', { name: 'Tools' })).toBeNull();
  expect(screen.getByRole('button', { name: 'Retry' })).toBeInTheDocument();
  // The next poll answers, so the failure goes without a tap.
  await screen.findByRole('heading', { name: 'Agent' }, { timeout: 4500 });
  expect(screen.queryByText('gateway timed out')).toBeNull();
  expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
});

// Regression for #373: session settings hid the extension settings that a wider scope
// decides. They show read-only, name that scope and open its settings in place of this dialog.
it('locks inherited extension rows and opens the settings that decide them', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockResolvedValue({
    revision: 'scoped-1',
    scope: 'session', target_id: 'a',
    groups: [{
      id: 'extension:demo', title: 'demo',
      extension: { name: 'demo', origin: 'project', status: 'loaded' },
      toggles: [
        { id: 'demo_machine', label: 'Machine flag', type: 'boolean', enabled: true, scope: 'session',
          scopes: ['global'], source: 'global', decided_in: { scope: 'global', label: 'Machine' } },
        { id: 'demo_project', label: 'Project flag', type: 'boolean', enabled: false, scope: 'session',
          scopes: ['global', 'project'], source: 'default',
          decided_in: { scope: 'project', target_id: 'p1', label: 'Workspace' } },
      ],
    }],
  });
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  const onClose = vi.fn();
  const openMachine = vi.fn();
  const openScoped = vi.fn();
  renderOpenBands(
    <OpenMachineSettings.Provider value={openMachine}>
      <OpenScopedSettings.Provider value={openScoped}>
        <ScopedSettingsDialog client={client} target={target} onClose={onClose} />
      </OpenScopedSettings.Provider>
    </OpenMachineSettings.Provider>,
  );

  expect(await screen.findByRole('switch', { name: 'Machine flag: on' })).toBeDisabled();
  expect(screen.getByRole('switch', { name: 'Project flag: off' })).toBeDisabled();
  expect(screen.getByText('Set in Machine settings. You cannot change it per session.')).toBeInTheDocument();
  expect(screen.getByText('Set in Project settings. You cannot change it per session.')).toBeInTheDocument();
  // The lock note names the deciding scope, so the inherited-from line does not repeat it.
  expect(screen.queryByText(/Inherited from/)).toBeNull();

  await user.click(screen.getByRole('button', { name: 'Open Project settings' }));
  expect(onClose).toHaveBeenCalledTimes(1);
  expect(openScoped).toHaveBeenCalledWith(client, { scope: 'project', target_id: 'p1', label: 'Workspace' });
  await user.click(screen.getByRole('button', { name: 'Open Machine settings' }));
  expect(openMachine).toHaveBeenCalledWith('http://127.0.0.1:7890');
});
