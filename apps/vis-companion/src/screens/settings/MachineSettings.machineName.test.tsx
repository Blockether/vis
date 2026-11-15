// @vitest-environment jsdom
import { cleanup, fireEvent, screen, waitFor } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import { MachineSettings } from './MachineSettings';

const gateway = { id: 'machine-name-settings', url: 'http://127.0.0.1:7890', token: 'test' };
const setting = {
  id: 'machine_name',
  label: 'Machine name',
  type: 'string' as const,
  value: 'Studio',
  max_length: 64,
  is_optional: true,
};
beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
    revision: 'machine-name-1',
    groups: [{ id: 'agent', title: 'Agent', toggles: [setting] }],
  });
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

// The machine name is optional: a blank value removes it. A required text row
// kept Save disabled, so a person could not remove a name that was set.
it('saves a blank machine name, which removes the name', async () => {
  const save = vi
    .spyOn(GatewayClient.prototype, 'setSetting')
    .mockResolvedValue({ ...setting, value: '' });
  renderOpenBands(
    <MachineSettings
      gateway={gateway}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
    />,
  );
  const field = await screen.findByRole<HTMLInputElement>('textbox', { name: 'Machine name' });
  expect(field).toHaveValue('Studio');
  expect(field).not.toBeRequired();
  fireEvent.change(field, { target: { value: '' } });
  fireEvent.click(screen.getByRole('button', { name: 'Save' }));
  expect(save).toHaveBeenCalledExactlyOnceWith('machine_name', 'value', '');
  await waitFor(() => expect(screen.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument());
  expect(screen.getByRole('textbox', { name: 'Machine name' })).toHaveValue('');
});
