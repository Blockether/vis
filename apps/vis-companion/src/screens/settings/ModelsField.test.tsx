// @vitest-environment jsdom
import { cleanup, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import { renderOpenBands } from '../../test-settings';
import { GatewayClient } from '../../lib/gateway';
import type { RouterProvider, SettingValue, SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';
import fixture from '../../../../../packages/vis-contract/resources/vis-contract/fixtures/setting-models.json';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

const target: SettingsTarget = { scope: 'project', target_id: 'p' };
const added = fixture.write.value[2];
const provider: RouterProvider = {
  id: added.provider,
  label: 'Anthropic',
  is_managed: false,
  models: [fixture.row.value[0].model, added.model],
  is_default: true,
  default_model: null,
  is_fallback: false,
  fallback_model: null,
  status: { is_authenticated: true, auth_state: 'verified' },
};

function open() {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  let row = fixture.row as unknown as Toggle;
  const response = (): SettingsResponse => ({
    revision: 'models-1', scope: 'project', target_id: 'p',
    groups: [{ id: 'council', title: 'Council', toggles: [row] }],
  });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockImplementation(async () => response());
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  vi.spyOn(client, 'cachedRouter').mockReturnValue(null);
  vi.spyOn(client, 'router').mockResolvedValue([provider]);
  const write = vi.spyOn(client, 'setSetting').mockImplementation(async (_id, _action, value) => {
    row = { ...row, value: value as SettingValue };
    return row;
  });
  renderOpenBands(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  return write;
}
const asWrite = (call: unknown[]) => ({ id: call[0], action: call[1], value: call[2], scope: (call[3] as SettingsTarget).scope });
const name = (entry: { provider: string; model: string }) => `${entry.provider}/${entry.model}`;

it('lists the models with the first as default, adds one from the session picker, reorders and clears', async () => {
  const user = userEvent.setup();
  const write = open();
  const list = await screen.findByLabelText(`${fixture.row.label} value`);
  expect(list).toHaveTextContent(`${name(fixture.row.value[0])} (default)`);
  expect(list).toHaveTextContent(name(fixture.row.value[1]));

  await user.click(screen.getByRole('button', { name: `Add a model to ${fixture.row.label}` }));
  const picker = screen.getByRole('dialog', { name: 'Model' });
  await user.click(await within(picker).findByRole('button', { name: /Anthropic/ }));
  await user.click(within(picker).getByRole('button', { name: added.model }));
  expect(asWrite(write.mock.calls[0])).toEqual(fixture.write);
  expect(screen.queryByRole('dialog', { name: 'Model' })).toBeNull();

  await user.click(await screen.findByRole('button', { name: `Make ${name(added)} the default` }));
  expect(write.mock.calls[1][2]).toEqual([added, fixture.row.value[0], fixture.row.value[1]]);

  for (const entry of [added, fixture.row.value[0], fixture.row.value[1]])
    await user.click(await screen.findByRole('button', { name: `Remove ${name(entry)}` }));
  expect(asWrite(write.mock.calls[4])).toEqual(fixture.clear_write);
  await waitFor(() => expect(screen.queryByRole('list', { name: `${fixture.row.label} value` })).toBeNull());
});
