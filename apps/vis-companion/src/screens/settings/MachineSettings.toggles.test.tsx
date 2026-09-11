// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { Toggle } from '../../lib/types';
import { MachineSettings } from './MachineSettings';

const backend: Toggle = {
  id: 'draft_backend',
  label: 'Draft backend',
  type: 'enum',
  description: 'Choose how drafts are isolated.',
  choices: ['auto', 'worktree', 'rift', 'off'],
  value: 'auto',
};
const gateway = { id: 'draft-dropdown-test', url: 'http://127.0.0.1:7890', token: 'test' };

beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
    groups: [
      {
        id: 'sandbox',
        title: 'Sandbox',
        toggles: [backend, { id: 'council', label: 'Council', type: 'boolean', enabled: true }],
      },
    ],
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

async function openSettings() {
  render(
    <MachineSettings
      gateway={gateway}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
    />,
  );
  return await screen.findByRole<HTMLSelectElement>('combobox', { name: 'Draft backend' });
}

describe('draft backend dropdown', () => {
  it('lists the configured choices and saves the chosen value, not a cycle', async () => {
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockResolvedValue({ ...backend, value: 'rift' });
    const select = await openSettings();
    expect(select.value).toBe('auto');
    expect(
      within(select)
        .getAllByRole('option')
        .map((option) => option.textContent),
    ).toEqual(backend.choices);
    expect(screen.queryByRole('button', { name: 'rift' })).toBeNull();
    fireEvent.change(select, { target: { value: 'rift' } });
    await waitFor(() => expect(select.value).toBe('rift'));
    expect(save).toHaveBeenCalledExactlyOnceWith('draft_backend', 'value', 'rift');
    expect(select.disabled).toBe(false);
  });

  it("blocks another choice while saving, then adopts the gateway's response", async () => {
    let finish!: (toggle: Toggle) => void;
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockReturnValue(
      new Promise((resolve) => {
        finish = resolve;
      }),
    );
    const select = await openSettings();
    fireEvent.change(select, { target: { value: 'worktree' } });
    expect(select.disabled).toBe(true);
    expect(save).toHaveBeenCalledTimes(1);
    finish({ ...backend, value: 'worktree' });
    await waitFor(() => expect(select.value).toBe('worktree'));
    expect(select.disabled).toBe(false);
  });

  it('keeps the saved value after a refusal and allows retry', async () => {
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockRejectedValueOnce(new Error('Setting could not be saved'))
      .mockResolvedValueOnce({ ...backend, value: 'off' });
    const select = await openSettings();
    fireEvent.change(select, { target: { value: 'off' } });
    await screen.findByText('Setting could not be saved');
    expect(select.value).toBe('auto');
    expect(select.disabled).toBe(false);
    fireEvent.change(select, { target: { value: 'off' } });
    await waitFor(() => expect(select.value).toBe('off'));
    expect(save).toHaveBeenCalledTimes(2);
    expect(screen.queryByText('Setting could not be saved')).toBeNull();
  });
});
