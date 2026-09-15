// @vitest-environment jsdom
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
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
  return await screen.findByRole('combobox', { name: 'Draft backend' });
}

describe('draft backend dropdown', () => {
  // #242 and #243: opening Settings must not opt the user into draft isolation.
  it('shows off without saving and lets the user enable and disable drafts', async () => {
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      groups: [{ id: 'sandbox', title: 'Sandbox', toggles: [{ ...backend, value: 'off' }] }],
    });
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockImplementation(
      async (_id, _action, value) => ({ ...backend, value }),
    );
    const select = await openSettings();
    expect(select).toHaveTextContent('off');
    expect(select).toBeEnabled();
    expect(save).not.toHaveBeenCalled();
    for (const value of ['auto', 'off']) {
      await userEvent.click(select);
      await userEvent.click(screen.getByRole('option', { name: value }));
      await waitFor(() => expect(select).toHaveTextContent(value));
      expect(save).toHaveBeenLastCalledWith('draft_backend', 'value', value);
    }
  });

  it('lists the configured choices and saves the chosen value, not a cycle', async () => {
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockResolvedValue({ ...backend, value: 'rift' });
    const select = await openSettings();
    expect(select).toHaveTextContent('auto');
    await userEvent.click(select);
    expect(screen.getAllByRole('option').map((option) => option.textContent)).toEqual(
      backend.choices,
    );
    await userEvent.click(screen.getByRole('option', { name: 'rift' }));
    await waitFor(() => expect(select).toHaveTextContent('rift'));
    expect(save).toHaveBeenCalledExactlyOnceWith('draft_backend', 'value', 'rift');
    expect(select).toBeEnabled();
  });

  it("blocks another choice while saving, then adopts the gateway's response", async () => {
    let finish!: (toggle: Toggle) => void;
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockReturnValue(
      new Promise((resolve) => {
        finish = resolve;
      }),
    );
    const select = await openSettings();
    await userEvent.click(select);
    await userEvent.click(screen.getByRole('option', { name: 'worktree' }));
    expect(select).toBeDisabled();
    expect(save).toHaveBeenCalledTimes(1);
    finish({ ...backend, value: 'worktree' });
    await waitFor(() => expect(select).toHaveTextContent('worktree'));
    expect(select).toBeEnabled();
  });

  it('keeps the saved value after a refusal and allows retry', async () => {
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockRejectedValueOnce(new Error('Setting could not be saved'))
      .mockResolvedValueOnce({ ...backend, value: 'off' });
    const select = await openSettings();
    await userEvent.click(select);
    await userEvent.click(screen.getByRole('option', { name: 'off' }));
    await screen.findByText('Setting could not be saved');
    expect(select).toHaveTextContent('auto');
    expect(select).toBeEnabled();
    await userEvent.click(select);
    await userEvent.click(screen.getByRole('option', { name: 'off' }));
    await waitFor(() => expect(select).toHaveTextContent('off'));
    expect(save).toHaveBeenCalledTimes(2);
    expect(screen.queryByText('Setting could not be saved')).toBeNull();
  });
});

describe('experimental feature flags', () => {
  it('renders badges from metadata and refreshes dependent rows after a flip', async () => {
    const feature: Toggle = {
      id: 'improve',
      label: 'Improve',
      type: 'boolean',
      enabled: false,
      is_experimental: true,
    };
    const mode: Toggle = {
      id: 'improve_mode',
      label: 'Improve mode',
      type: 'enum',
      value: 'human',
      choices: ['off', 'human', 'automatic'],
      is_experimental: true,
    };
    let enabled = false;
    const settings = vi.mocked(GatewayClient.prototype.settings).mockImplementation(async () => ({
      groups: [{
        id: 'experimental',
        title: 'Experimental',
        toggles: [{ ...feature, enabled }, ...(enabled ? [mode] : [])],
      }],
    }));
    vi.spyOn(GatewayClient.prototype, 'setSetting').mockImplementation(async () => {
      enabled = !enabled;
      return { ...feature, enabled };
    });
    render(
      <MachineSettings
        gateway={gateway}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
      />,
    );
    const toggle = await screen.findByRole('switch', { name: /^Improve:/ });
    expect(toggle).not.toBeChecked();
    expect(screen.getAllByText('Experimental')).toHaveLength(2);
    expect(screen.queryByRole('combobox', { name: 'Improve mode' })).toBeNull();
    await userEvent.click(toggle);
    await screen.findByRole('combobox', { name: 'Improve mode' });
    expect(toggle).toBeChecked();
    expect(screen.getAllByText('Experimental')).toHaveLength(3);
    await userEvent.click(toggle);
    await waitFor(() => expect(screen.queryByRole('combobox', { name: 'Improve mode' })).toBeNull());
    expect(settings).toHaveBeenCalledTimes(3);
  });
});
