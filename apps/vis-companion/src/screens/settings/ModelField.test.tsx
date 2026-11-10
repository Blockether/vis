// @vitest-environment jsdom
import { cleanup, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import { renderOpenBands } from '../../test-settings';
import { GatewayClient } from '../../lib/gateway';
import type { RouterProvider, SettingValue, SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';
import fixture from '../../../../../packages/vis-contract/resources/vis-contract/fixtures/setting-model.json';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });

const target: SettingsTarget = { scope: 'project', target_id: 'p' };
const provider: RouterProvider = {
  id: fixture.write.value.provider,
  label: 'Anthropic',
  is_managed: false,
  models: ['claude-haiku-4-5', fixture.write.value.model],
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
    revision: 'model-1', scope: 'project', target_id: 'p',
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
const value = () => screen.getByLabelText(`${fixture.row.label} value`);

it('shows the chosen model, clears it with the first entry and picks a model from the session picker', async () => {
  const user = userEvent.setup();
  const write = open();
  expect(await screen.findByLabelText(`${fixture.row.label} value`))
    .toHaveTextContent(`${fixture.row.value.provider}/${fixture.row.value.model}`);

  await user.click(screen.getByRole('button', { name: `Change ${fixture.row.label}` }));
  await user.click(within(screen.getByRole('dialog', { name: 'Model' })).getByRole('button', { name: 'Not set' }));
  expect(asWrite(write.mock.calls[0])).toEqual(fixture.clear_write);
  expect(screen.queryByRole('dialog', { name: 'Model' })).toBeNull();
  expect(await screen.findByText('Not set')).toBe(value());

  await user.click(screen.getByRole('button', { name: `Change ${fixture.row.label}` }));
  const picker = screen.getByRole('dialog', { name: 'Model' });
  await user.click(await within(picker).findByRole('button', { name: /Anthropic/ }));
  await user.click(within(picker).getByRole('button', { name: fixture.write.value.model }));
  expect(asWrite(write.mock.calls[1])).toEqual(fixture.write);
  expect(await screen.findByText(`${fixture.write.value.provider}/${fixture.write.value.model}`)).toBe(value());
});
