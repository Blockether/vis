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
  await user.type(screen.getByRole('textbox', { name: 'Search settings' }), 'missing');
  view.rerender(<ScopedSettingsDialog client={client} target={{ scope: 'session', target_id: 'b' }} onClose={() => {}} />);
  await screen.findByRole('switch', { name: 'Plans b: off' });
  expect(screen.queryByText('Plans a')).toBeNull();
  expect(screen.getByRole('textbox', { name: 'Search settings' })).toHaveValue('');
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
