// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from '../../lib/gateway';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import type { SettingsResponse, Toggle } from '../../lib/types';
import { MachineSettings } from './MachineSettings';

const backend: Toggle = {
  id: 'draft_backend',
  label: 'Draft backend',
  type: 'enum',
  choices: ['auto', 'worktree', 'rift', 'off'],
  value: 'auto',
  inherited_value: 'off',
  is_override: true,
};
const gateway = { id: 'draft-dropdown-test', url: 'http://127.0.0.1:7890', token: 'test' };
const catalog = (toggles: Toggle[], revision = 'draft-1'): SettingsResponse => ({
  revision,
  groups: [{ id: 'sandbox', title: 'Sandbox', toggles }],
});
beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue(catalog([backend]));
  vi.spyOn(GatewayClient.prototype, 'applySettings').mockResolvedValue(
    catalog([{ ...backend, value: 'rift' }], 'draft-2'),
  );
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
});
async function openSettings(contextSessionId?: string) {
  render(
    <MachineSettings
      gateway={gateway}
      speechPrefs={DEFAULT_SPEECH_PREFS}
      onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
      category="advanced"
      contextSessionId={contextSessionId}
    />,
  );
  return screen.findByRole('combobox', { name: 'Draft backend' });
}
describe('settings provenance', () => {
  it('keeps the inherited value visible and stages removing an explicit value', async () => {
    await openSettings();
    expect(screen.getByText(/Without this override: off/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Use inherited value' }));
    expect(screen.getByRole('combobox', { name: 'Draft backend' })).toHaveTextContent('off');
    expect(GatewayClient.prototype.applySettings).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
    await waitFor(() =>
      expect(GatewayClient.prototype.applySettings).toHaveBeenCalledWith(
        'draft-1',
        [{ id: 'draft_backend', action: 'inherit' }],
        { scope: 'global', target_id: undefined },
        undefined,
      ),
    );
  });
  it('warns about a more specific winner without locking the ancestor', async () => {
    vi.mocked(GatewayClient.prototype.settings).mockResolvedValue(
      catalog([{ ...backend, overridden_by: { scope: 'group', value: 'off' } }]),
    );
    const select = await openSettings('s1');
    expect(select).toBeEnabled();
    expect(screen.getByText(/This session uses its group override/)).toBeInTheDocument();
    expect(GatewayClient.prototype.settings).toHaveBeenCalledWith(
      expect.any(AbortSignal),
      { scope: 'global', target_id: undefined },
      's1',
    );
  });
});
describe('draft backend dropdown', () => {
  // #242 and #243: opening Settings must not opt the user into draft isolation.
  it('shows off without saving and stages an explicit choice, not a cycle', async () => {
    vi.mocked(GatewayClient.prototype.settings).mockResolvedValue(
      catalog([{ ...backend, value: 'off', is_override: false }]),
    );
    const select = await openSettings();
    expect(select).toHaveTextContent('off');
    expect(GatewayClient.prototype.applySettings).not.toHaveBeenCalled();
    await userEvent.click(select);
    expect(screen.getAllByRole('option').map((option) => option.textContent)).toEqual(
      backend.choices,
    );
    await userEvent.click(screen.getByRole('option', { name: 'rift' }));
    expect(select).toHaveTextContent('rift');
    expect(GatewayClient.prototype.applySettings).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
    await waitFor(() =>
      expect(GatewayClient.prototype.applySettings).toHaveBeenCalledWith(
        'draft-1',
        [{ id: 'draft_backend', action: 'value', value: 'rift' }],
        { scope: 'global', target_id: undefined },
        undefined,
      ),
    );
  });
  it('blocks another choice only while the batch saves and keeps a failed draft', async () => {
    let reject!: (error: Error) => void;
    vi.mocked(GatewayClient.prototype.applySettings).mockReturnValueOnce(
      new Promise((_, fail) => {
        reject = fail;
      }),
    );
    const select = await openSettings();
    await userEvent.click(select);
    await userEvent.click(screen.getByRole('option', { name: 'rift' }));
    expect(select).toBeEnabled();
    fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
    expect(select).toBeDisabled();
    reject(new Error('Setting could not be saved'));
    await screen.findByText('Setting could not be saved');
    expect(select).toHaveTextContent('rift');
    expect(select).toBeEnabled();
    fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
    await waitFor(() => expect(GatewayClient.prototype.applySettings).toHaveBeenCalledTimes(2));
  });
});
