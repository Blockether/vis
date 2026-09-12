// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import { MachineSettings } from './MachineSettings';

const gateway = { id: 'agent-name-settings', url: 'http://127.0.0.1:7890', token: 'test' };
const setting = {
  id: 'agent_name',
  label: 'Agent name',
  type: 'string' as const,
  value: 'Ada',
  max_length: 80,
};
beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
    groups: [{ id: 'agent', title: 'Agent', toggles: [setting] }],
  });
  vi.stubGlobal(
    'fetch',
    vi.fn(
      async () =>
        new Response('{}', {
          headers: { 'Content-Type': 'application/json' },
        }),
    ),
  );
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
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
it('shows Save only after the agent name changes', async () => {
  const field = await open();
  expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Cancel' })).not.toBeInTheDocument();
  fireEvent.focus(field);
  expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();

  fireEvent.change(field, { target: { value: 'Grace' } });
  expect(screen.getByRole('button', { name: 'Save' })).toBeEnabled();
  expect(screen.getByRole('button', { name: 'Cancel' })).toBeEnabled();

  fireEvent.change(field, { target: { value: 'Ada' } });
  expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Cancel' })).not.toBeInTheDocument();
});

it('saves explicitly to this gateway and adopts its normalized response', async () => {
  let finish!: (value: typeof setting) => void;
  const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockImplementation(function (
    this: GatewayClient,
  ) {
    expect(this.base).toBe(gateway.url);
    return new Promise((resolve) => {
      finish = resolve;
    });
  });
  const field = await open();
  expect(field).toHaveValue('Ada');
  expect(field).toHaveAttribute('maxlength', '80');
  fireEvent.change(field, { target: { value: '  Grace  ' } });
  expect(save).not.toHaveBeenCalled();
  fireEvent.submit(field.closest('form')!);
  expect(save).toHaveBeenCalledExactlyOnceWith('agent_name', 'value', '  Grace  ');
  expect(field).toBeDisabled();
  expect(screen.getByRole('button', { name: 'Saving…' })).toBeDisabled();
  finish({ ...setting, value: 'Grace' });
  await waitFor(() =>
    expect(screen.getByRole('textbox', { name: 'Agent name' })).toHaveValue('Grace'),
  );
  expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
});
it('cancels a draft, rejects a blank name locally and retries a failed write', async () => {
  const save = vi
    .spyOn(GatewayClient.prototype, 'setSetting')
    .mockRejectedValueOnce(new Error('Gateway could not save the name; retry.'))
    .mockResolvedValueOnce({ ...setting, value: 'Grace' });
  const field = await open();
  fireEvent.change(field, { target: { value: 'Other' } });
  fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  expect(field).toHaveValue('Ada');
  expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
  fireEvent.change(field, { target: { value: '   ' } });
  expect(screen.getByRole('button', { name: 'Save' })).toBeDisabled();
  fireEvent.change(field, { target: { value: 'Grace' } });
  fireEvent.click(screen.getByRole('button', { name: 'Save' }));
  await screen.findByText('Gateway could not save the name; retry.');
  expect(field).toHaveValue('Grace');
  expect(field).not.toBeDisabled();
  fireEvent.click(screen.getByRole('button', { name: 'Save' }));
  await waitFor(() =>
    expect(screen.queryByText('Gateway could not save the name; retry.')).toBeNull(),
  );
  expect(save).toHaveBeenCalledTimes(2);
});
