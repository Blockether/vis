// @vitest-environment jsdom
import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
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
    revision: 'toggles-1',
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

describe('global settings provenance', () => {
  // Global settings are the root scope: an explicit value there overrides nothing, so its
  // row shows no "Set here" caption and offers no "Use inherited value" reset.
  it('shows no provenance caption or reset action, even for explicit values', async () => {
    const plans: Toggle = { id: 'plans', label: 'Plans', type: 'boolean', enabled: false };
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'toggles-1',
      groups: [{
        id: 'sandbox', title: 'Sandbox', toggles: [
          { ...backend, source: 'default', is_override: false },
          { ...plans, source: 'global', is_override: true },
        ],
      }],
    });

    await openSettings();
    expect(await screen.findByRole('switch', { name: 'Plans: off' })).toBeEnabled();
    expect(screen.queryByText('Set here')).toBeNull();
    expect(screen.queryByText(/^Inherited from/)).toBeNull();
    expect(screen.queryByRole('button', { name: 'Use inherited value' })).toBeNull();
  });
});
describe('settings the open session decides elsewhere', () => {
  // A project `vis.yml` with `shell: false` decides Shell for its sessions, so the
  // global row stays locked while such a session is open: flipping it changes nothing.
  it('locks those rows and says where to change them', async () => {
    const settings = vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'toggles-1',
      groups: [{
        id: 'sandbox', title: 'Sandbox', toggles: [
          { ...backend, overridden_by: { scope: 'group', value: 'off' } },
          {
            id: 'shell', label: 'Shell commands', type: 'boolean', enabled: true,
            source: 'global', is_override: true, overridden_by: { scope: 'project', enabled: false },
          },
          { id: 'council', label: 'Council', type: 'boolean', enabled: true },
        ],
      }],
    });
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting');
    render(
      <MachineSettings
        gateway={gateway}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
        contextSessionId="s1"
      />,
    );
    const shell = await screen.findByRole('switch', { name: 'Shell commands: on' });
    expect(settings).toHaveBeenCalledWith(undefined, undefined, 's1');
    expect(shell).toBeDisabled();
    expect(
      screen.getByText('Locked: Project settings turn this off for this session. Change it in Project settings.'),
    ).toBeInTheDocument();
    expect(screen.getByRole('combobox', { name: 'Draft backend' })).toBeDisabled();
    expect(
      screen.getByText('Locked: Group settings set this to off for this session. Change it in Group settings.'),
    ).toBeInTheDocument();
    expect(screen.getByRole('switch', { name: 'Council: on' })).toBeEnabled();
    await userEvent.click(shell);
    expect(save).not.toHaveBeenCalled();
  });
});

describe('draft backend dropdown', () => {
  // #242 and #243: opening Settings must not opt the user into draft isolation.
  it('shows off without saving and lets the user enable and disable drafts', async () => {
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'toggles-1',
      groups: [{ id: 'sandbox', title: 'Sandbox', toggles: [{ ...backend, value: 'off' }] }],
    });
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting').mockImplementation(
      async (_id, _action, value) => ({ ...backend, value: typeof value === 'string' ? value : undefined }),
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
      revision: 'toggles-1',
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

describe('typed settings', () => {
  const turns: Toggle = { id: 'max_turns', label: 'Maximum turns', type: 'number', value: 30 };
  const filesystem: Toggle = {
    id: 'jail_filesystem',
    label: 'Filesystem access',
    type: 'object',
    editor: 'filesystem',
    value: { allow: ['~/docs'], deny_read: ['~/private'] },
  };
  const renderTyped = async (name: string, role: 'spinbutton' | 'textbox') => {
    vi.mocked(GatewayClient.prototype.settings).mockResolvedValue({
      revision: 'typed-1',
      groups: [{ id: 'limits', title: 'Limits', toggles: [turns, filesystem] }],
    });
    render(
      <MachineSettings
        gateway={gateway}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
      />,
    );
    const field = await screen.findByRole(role, { name });
    return { field, form: within(field.closest('form')!) };
  };

  it('sends a number only from Save and keeps text that is not a number in the form', async () => {
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockResolvedValue({ ...turns, value: 45 });
    const { field, form } = await renderTyped('Maximum turns', 'spinbutton');
    expect(form.queryByRole('button', { name: 'Save' })).toBeNull();

    await userEvent.clear(field);
    expect(form.getByRole('alert')).toHaveTextContent('Enter a number.');
    expect(form.getByRole('button', { name: 'Save' })).toBeDisabled();

    await userEvent.type(field, '45');
    expect(form.queryByRole('alert')).toBeNull();
    expect(save).not.toHaveBeenCalled();
    await userEvent.click(form.getByRole('button', { name: 'Save' }));
    expect(save).toHaveBeenCalledWith('max_turns', 'value', 45);
    await waitFor(() => expect(screen.getByRole('spinbutton', { name: 'Maximum turns' })).toHaveValue(45));
    expect(screen.queryByRole('button', { name: 'Save' })).toBeNull();
  });

  it('keeps guided object edits local, and Cancel restores the saved paths', async () => {
    const save = vi.spyOn(GatewayClient.prototype, 'setSetting');
    const { field, form } = await renderTyped('Allowed paths 1', 'textbox');
    expect(screen.queryByText('Advanced JSON')).toBeNull();

    await userEvent.clear(field);
    await userEvent.type(field, '~/project');
    expect(field).toHaveValue('~/project');
    expect(form.getByRole('button', { name: 'Save' })).toBeEnabled();
    expect(save).not.toHaveBeenCalled();

    await userEvent.click(form.getByRole('button', { name: 'Cancel' }));
    expect(field).toHaveValue('~/docs');
    expect(form.getByRole('textbox', { name: 'Blocked read paths 1' })).toHaveValue('~/private');
    expect(form.queryByRole('alert')).toBeNull();
    expect(form.queryByRole('button', { name: 'Save' })).toBeNull();
    expect(save).not.toHaveBeenCalled();
  });

  it('saves a guided object edit as a typed value and keeps the other paths', async () => {
    const value = { allow: ['~/project'], deny_read: ['~/private'] };
    const save = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockResolvedValue({ ...filesystem, value });
    const { field, form } = await renderTyped('Allowed paths 1', 'textbox');

    await userEvent.clear(field);
    await userEvent.type(field, '~/project');
    expect(save).not.toHaveBeenCalled();
    await userEvent.click(form.getByRole('button', { name: 'Save' }));
    expect(save).toHaveBeenCalledWith('jail_filesystem', 'value', value);
    await waitFor(() => expect(field).toHaveValue('~/project'));
    expect(form.queryByRole('button', { name: 'Save' })).toBeNull();
  });
});
