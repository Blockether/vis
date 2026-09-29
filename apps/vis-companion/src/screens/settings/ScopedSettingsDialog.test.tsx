// @vitest-environment jsdom
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

it('writes and inherits only the selected owner, without carrying form state to another session', async () => {
  const user = userEvent.setup();
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  let own = false;
  const response = (owner?: SettingsTarget): SettingsResponse => ({
    scope: 'session', target_id: owner?.target_id,
    groups: [{ id: 'agent', title: 'Agent', toggles: [{
      id: 'plans', label: `Plans ${owner?.target_id}`, type: 'boolean',
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
  const view = render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  await screen.findByText('Inherited from global');
  await user.click(screen.getByRole('switch', { name: 'Plans a: off' }));
  await screen.findByText('Set here');
  expect(write).toHaveBeenLastCalledWith('plans', 'toggle', undefined, target);
  await user.click(screen.getByRole('button', { name: 'Use inherited value' }));
  await screen.findByText('Inherited from global');
  expect(write).toHaveBeenLastCalledWith('plans', 'inherit', undefined, target);
  await user.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'missing');
  view.rerender(<ScopedSettingsDialog client={client} target={{ scope: 'session', target_id: 'b' }} onClose={() => {}} />);
  await screen.findByRole('switch', { name: 'Plans b: off' });
  expect(screen.queryByText('Plans a')).toBeNull();
  expect(screen.getByRole('searchbox', { name: 'Search settings' })).toHaveValue('');
  await waitFor(() => expect(read).toHaveBeenLastCalledWith(expect.any(AbortSignal), { scope: 'session', target_id: 'b' }));
});

it('keeps gateway caches and writes separate for equal setting ids in different scopes', async () => {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (input, init) => {
    const url = new URL(String(input));
    return Response.json({ id: 'plans', label: 'Plans', type: 'boolean',
      enabled: url.searchParams.get('target_id') === 'a', ...(init?.body ? JSON.parse(String(init.body)) : {}) });
  });
  const a: SettingsTarget = { scope: 'session', target_id: 'a' };
  const b: SettingsTarget = { scope: 'group', target_id: 'a' };
  await client.setting('plans', undefined, a);
  expect(client.cachedSetting('plans', a)?.enabled).toBe(true);
  expect(client.cachedSetting('plans', b)).toBeNull();
  await client.setSetting('plans', 'value', false, b);
  const body = JSON.parse(String(fetcher.mock.calls.at(-1)?.[1]?.body));
  expect(body).toMatchObject({ id: 'plans', action: 'value', value: false, ...b });
});

it.each(['session', 'group', 'project'] as const)(
  'keeps %s settings search compact and easy to recover',
  async (scope) => {
    const user = userEvent.setup();
    const target: SettingsTarget = { scope, target_id: 'item', label: 'Wallet work' };
    const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
    vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
    vi.spyOn(client, 'settings').mockResolvedValue({
      scope, target_id: 'item',
      groups: [{ id: 'agent', title: 'Agent', toggles: [{
        id: 'plans', label: 'Plans', description: 'Track work', type: 'boolean',
        enabled: false, scopes: ['global', scope], source: 'global', is_override: false,
      }] }],
    });
    vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
    vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
    render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

    const dialog = screen.getByRole('dialog', { name: `${scope[0].toUpperCase()}${scope.slice(1)} settings` });
    expect(dialog.parentElement).toHaveClass('sm:h-auto', 'sm:max-w-4xl', 'mouse:max-w-6xl');
    const search = screen.getByRole('searchbox', { name: 'Search settings' });
    expect(search.parentElement?.querySelector('svg.lucide-search')).toBeInTheDocument();
    await screen.findByRole('switch', { name: 'Plans: off' });
    await user.type(search, 'agent');
    expect(screen.getByRole('switch', { name: 'Plans: off' })).toBeInTheDocument();
    expect(screen.getByRole('status')).toHaveTextContent('1 setting found');
    await user.click(screen.getByRole('button', { name: 'Clear settings search' }));
    expect(search).toHaveValue('');
    expect(search).toHaveFocus();
    await user.type(search, 'dd');
    expect(screen.getByRole('heading', { name: 'No settings match “dd”' })).toBeInTheDocument();
    expect(screen.getByText(/Try a different name or description/)).toBeInTheDocument();
    expect(screen.queryByRole('switch')).toBeNull();
    await user.click(screen.getByRole('button', { name: 'Clear search' }));
    expect(search).toHaveValue('');
    expect(screen.getByRole('switch', { name: 'Plans: off' })).toBeInTheDocument();
    await user.type(search, 'missing');
    await user.keyboard('{Escape}');
    expect(dialog).toBeInTheDocument();
    expect(search).toHaveValue('');
  },
);

it('does not claim a failed search before one exists when the catalog has no toggles', async () => {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  const target: SettingsTarget = { scope: 'group', target_id: 'wallet' };
  const data: SettingsResponse = { scope: 'group', target_id: 'wallet', groups: [] };
  vi.spyOn(client, 'cachedSettings').mockReturnValue(data);
  vi.spyOn(client, 'settings').mockResolvedValue(data);
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  expect(screen.getByRole('heading', { name: 'MCP servers' })).toBeInTheDocument();
  expect(screen.queryByText(/No settings match/)).toBeNull();
});

it.each(['session', 'group', 'project'] as const)(
  'separates every %s settings section, including MCP servers, without orphan rules after filtering',
  async (scope) => {
    const user = userEvent.setup();
    const target: SettingsTarget = { scope, target_id: 'item' };
    const data: SettingsResponse = { scope, target_id: 'item', groups: [
      { id: 'agent', title: 'Agent', toggles: [{
        id: 'plans', label: 'Plans', type: 'boolean', enabled: false, scopes: ['global', scope],
        source: 'global', is_override: false,
      }] },
      { id: 'experimental', title: 'Experimental', toggles: [{
        id: 'draft_backend', label: 'Draft backend', type: 'boolean', enabled: false,
        scopes: ['global', scope], source: 'global', is_override: false,
      }] },
    ] };
    const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
    vi.spyOn(client, 'cachedSettings').mockReturnValue(data);
    vi.spyOn(client, 'settings').mockResolvedValue(data);
    vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
    vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
    render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);

    const panel = (title: string) => screen.getByRole('heading', { name: title }).closest('section');
    const sections = panel('Agent')?.parentElement;
    expect(sections).toHaveClass('divide-y', 'divide-dialog-edge');
    expect(Array.from(sections?.children ?? [])).toEqual([
      panel('Agent'), panel('Experimental'), panel('MCP servers'),
    ]);

    await user.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'draft');
    expect(panel('Experimental')?.parentElement).toHaveClass('divide-y', 'divide-dialog-edge');
    expect(panel('Experimental')?.parentElement?.children).toHaveLength(1);
    expect(screen.queryByRole('heading', { name: 'Agent' })).toBeNull();
    expect(screen.queryByRole('heading', { name: 'MCP servers' })).toBeNull();
  },
);
