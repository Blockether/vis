// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
function mockClient() {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  return client;
}
it('writes and inherits only the selected owner and isolates a newly opened session', async () => {
  const target: SettingsTarget = { scope: 'session', target_id: 'a' };
  const client = mockClient();
  let own = false;
  const response = (owner?: SettingsTarget): SettingsResponse => ({
    revision: own ? 'own-2' : 'base-1',
    scope: owner?.scope,
    target_id: owner?.target_id,
    groups: [
      {
        id: 'agent',
        title: 'Agent',
        toggles: [
          {
            id: 'plans',
            label: `Plans ${owner?.target_id}`,
            type: 'boolean',
            enabled: own,
            is_override: own,
            source: own ? 'session' : 'global',
            inherited_value: false,
            inherited_source: 'global',
          },
        ],
      },
    ],
  });
  const read = vi
    .spyOn(client, 'settings')
    .mockImplementation(async (_signal, owner) => response(owner));
  const write = vi
    .spyOn(client, 'applySettings')
    .mockImplementation(async (_revision, changes, owner) => {
      own = changes[0].action !== 'inherit';
      return response(owner);
    });
  const view = render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  fireEvent.click(await screen.findByRole('switch', { name: 'Plans a: off' }));
  expect(write).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await waitFor(() =>
    expect(write).toHaveBeenCalledWith(
      'base-1',
      [{ id: 'plans', action: 'value', value: true }],
      target,
      undefined,
    ),
  );
  fireEvent.click(screen.getByRole('button', { name: 'Use inherited value' }));
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await waitFor(() =>
    expect(write).toHaveBeenLastCalledWith(
      'own-2',
      [{ id: 'plans', action: 'inherit' }],
      target,
      undefined,
    ),
  );
  await userEvent.type(screen.getByRole('searchbox', { name: 'Search settings' }), 'missing');
  view.rerender(
    <ScopedSettingsDialog
      client={client}
      target={{ scope: 'session', target_id: 'b' }}
      onClose={() => {}}
    />,
  );
  await screen.findByRole('switch', { name: 'Plans b: off' });
  expect(screen.queryByText('Plans a')).toBeNull();
  expect(screen.getByRole('searchbox', { name: 'Search settings' })).toHaveValue('');
  expect(read).toHaveBeenLastCalledWith(
    expect.any(AbortSignal),
    { scope: 'session', target_id: 'b' },
    undefined,
  );
});
it('keeps gateway caches separate for equal setting IDs and applies batches only after success', async () => {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  const a: SettingsTarget = { scope: 'session', target_id: 'a' };
  const b: SettingsTarget = { scope: 'group', target_id: 'a' };
  const fetcher = vi.spyOn(globalThis, 'fetch').mockImplementation(async (input, init) => {
    const url = new URL(String(input));
    const body = init?.body ? JSON.parse(String(init.body)) : {};
    if (init?.method === 'PATCH')
      return Response.json({
        revision: 'group-2',
        scope: 'group',
        groups: [
          {
            id: 'agent',
            title: 'Agent',
            toggles: [{ id: 'plans', label: 'Plans', type: 'boolean', enabled: false }],
          },
        ],
        ...body,
      });
    return Response.json({
      id: 'plans',
      label: 'Plans',
      type: 'boolean',
      enabled: url.searchParams.get('target_id') === 'a',
    });
  });
  await client.setting('plans', undefined, a);
  expect(client.cachedSetting('plans', a)?.enabled).toBe(true);
  expect(client.cachedSetting('plans', b)).toBeNull();
  await client.applySettings('group-1', [{ id: 'plans', action: 'value', value: false }], b);
  expect(fetcher.mock.calls.at(-1)?.[1]?.method).toBe('PATCH');
  expect(JSON.parse(String(fetcher.mock.calls.at(-1)?.[1]?.body))).toMatchObject({
    revision: 'group-1',
    changes: [{ id: 'plans', action: 'value', value: false }],
    ...b,
  });
  expect(client.cachedSetting('plans', a)?.enabled).toBe(true);
  expect(client.cachedSetting('plans', b)?.enabled).toBe(false);
});
it.each(['session', 'group', 'project'] as const)(
  'searches all %s categories and clears search before Escape closes',
  async (scope) => {
    const client = mockClient();
    vi.spyOn(client, 'settings').mockResolvedValue({
      revision: 'search-1',
      groups: [
        {
          id: 'experimental',
          title: 'Experimental',
          toggles: [
            {
              id: 'plans',
              label: 'Plans',
              description: 'Track work',
              type: 'boolean',
              enabled: false,
            },
          ],
        },
      ],
    });
    const close = vi.fn();
    render(
      <ScopedSettingsDialog
        client={client}
        target={{ scope, target_id: 'item' }}
        onClose={close}
      />,
    );
    const search = screen.getByRole('searchbox', { name: 'Search settings' });
    await userEvent.type(search, 'plans');
    await screen.findByRole('switch', { name: 'Plans: off' });
    expect(screen.getByText('1 settings found across all categories')).toBeInTheDocument();
    await userEvent.clear(search);
    await userEvent.type(search, 'missing');
    expect(screen.getByText('No settings match “missing”')).toBeInTheDocument();
    await userEvent.keyboard('{Escape}');
    expect(close).not.toHaveBeenCalled();
    expect(search).toHaveValue('');
    await userEvent.keyboard('{Escape}');
    expect(close).toHaveBeenCalledOnce();
  },
);
it('opens resource workflows only in their category and keeps an empty catalog readable', async () => {
  const client = mockClient();
  vi.spyOn(client, 'settings').mockResolvedValue({ revision: 'empty-1', groups: [] });
  render(
    <ScopedSettingsDialog
      client={client}
      target={{ scope: 'group', target_id: 'item' }}
      onClose={() => {}}
    />,
  );
  await screen.findByText('No configuration fields in this category.');
  expect(screen.queryByText(/No settings match/)).toBeNull();
  await userEvent.click(screen.getByRole('combobox', { name: 'Settings category' }));
  await userEvent.click(screen.getByRole('option', { name: 'Tools and integrations' }));
  await screen.findByRole('heading', { name: 'MCP servers' });
});
