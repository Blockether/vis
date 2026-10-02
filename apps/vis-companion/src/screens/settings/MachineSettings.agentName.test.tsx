// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { SettingsResponse } from '../../lib/types';
import { MachineSettings } from './MachineSettings';

const gateway = { id: 'agent-name-settings', url: 'http://127.0.0.1:7890', token: 'test' };
const setting = {
  id: 'agent_name',
  label: 'Agent name',
  type: 'string' as const,
  value: 'Ada',
  max_length: 80,
  is_override: true,
};
const initial: SettingsResponse = {
  revision: 'name-1',
  groups: [{ id: 'agent', title: 'Agent', toggles: [setting] }],
};
beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(initial);
  vi.spyOn(GatewayClient.prototype, 'applySettings').mockResolvedValue({
    ...initial,
    revision: 'name-2',
    groups: [{ ...initial.groups[0], toggles: [{ ...setting, value: 'Grace' }] }],
  });
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
async function open() {
  render(
    <MachineSettings
      gateway={gateway}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
    />,
  );
  return screen.findByRole<HTMLInputElement>('textbox', { name: 'Agent name' });
}
it('keeps the batch disabled until the name changes and discards a draft', async () => {
  const field = await open();
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  fireEvent.focus(field);
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  fireEvent.change(field, { target: { value: 'Grace' } });
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeEnabled();
  fireEvent.click(screen.getByRole('button', { name: 'Discard changes' }));
  expect(field).toHaveValue('Ada');
});
it('saves to this gateway as one batch and adopts its normalized response', async () => {
  let finish!: (value: SettingsResponse) => void;
  const save = vi.mocked(GatewayClient.prototype.applySettings).mockImplementation(function (
    this: GatewayClient,
  ) {
    expect(this.base).toBe(gateway.url);
    return new Promise((resolve) => {
      finish = resolve;
    });
  });
  const field = await open();
  expect(field).toHaveAttribute('maxlength', '80');
  fireEvent.change(field, { target: { value: '  Grace  ' } });
  expect(save).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  expect(save).toHaveBeenCalledWith(
    'name-1',
    [{ id: 'agent_name', action: 'value', value: '  Grace  ' }],
    { scope: 'global', target_id: undefined },
    undefined,
  );
  expect(field).toBeDisabled();
  finish({
    ...initial,
    revision: 'name-2',
    groups: [{ ...initial.groups[0], toggles: [{ ...setting, value: 'Grace' }] }],
  });
  await waitFor(() => expect(field).toHaveValue('Grace'));
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
});
it('rejects a blank name locally and preserves a failed write for retry', async () => {
  const save = vi
    .mocked(GatewayClient.prototype.applySettings)
    .mockRejectedValueOnce(new Error('Gateway could not save the name; retry.'));
  const field = await open();
  fireEvent.change(field, { target: { value: '   ' } });
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  fireEvent.change(field, { target: { value: 'Grace' } });
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await screen.findByText('Gateway could not save the name; retry.');
  expect(field).toHaveValue('Grace');
  expect(field).toBeEnabled();
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await waitFor(() => expect(save).toHaveBeenCalledTimes(2));
});
