// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient, GatewayError } from '../../lib/gateway';
import type { SettingsResponse } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';
import { SettingsEditor } from './SettingsEditor';

const initial: SettingsResponse = {
  revision: 'revision-1',
  scope: 'session',
  target_id: 'draft-test',
  groups: [
    {
      id: 'agent',
      title: 'Agent',
      toggles: [
        {
          id: 'plans',
          label: 'Plans',
          type: 'boolean',
          enabled: false,
          source: 'global',
          is_override: false,
          inherited_value: false,
          inherited_source: 'global',
        },
        {
          id: 'agent_name',
          label: 'Agent name',
          type: 'string',
          value: 'Ada',
          source: 'global',
          is_override: true,
          max_length: 80,
          inherited_value: 'Default',
        },
      ],
    },
  ],
};
const target = { scope: 'session', target_id: 'draft-test' } as const;
function mockClient(data: SettingsResponse = initial) {
  const client = new GatewayClient({ url: 'http://127.0.0.1:7890' });
  vi.spyOn(client, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(client, 'settings').mockResolvedValue(data);
  vi.spyOn(client, 'cachedMcpServers').mockReturnValue([]);
  vi.spyOn(client, 'mcpServers').mockResolvedValue([]);
  vi.spyOn(client, 'setSetting').mockResolvedValue({
    id: 'plans',
    label: 'Plans',
    type: 'boolean',
    enabled: true,
  });
  vi.spyOn(client, 'applySettings').mockResolvedValue({ ...data, revision: 'revision-2' });
  return client;
}
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.useRealTimers();
});

it('keeps configuration changes local until Apply and sends explicit values', async () => {
  const client = mockClient();
  render(<ScopedSettingsDialog client={client} target={target} onClose={() => {}} />);
  fireEvent.click(await screen.findByRole('switch', { name: 'Plans: off' }));
  expect(client.setSetting).not.toHaveBeenCalled();
  expect(client.applySettings).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await waitFor(() =>
    expect(client.applySettings).toHaveBeenCalledExactlyOnceWith(
      'revision-1',
      [{ id: 'plans', action: 'value', value: true }],
      target,
      undefined,
    ),
  );
  await screen.findByText('Changes applied.');
});
it('discards a text draft and protects closing by Escape', async () => {
  const close = vi.fn();
  const client = mockClient();
  render(<ScopedSettingsDialog client={client} target={target} onClose={close} />);
  const name = await screen.findByRole('textbox', { name: 'Agent name' });
  fireEvent.change(name, { target: { value: 'Grace' } });
  fireEvent.keyDown(window, { key: 'Escape' });
  expect(close).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Keep editing' }));
  expect(name).toHaveValue('Grace');
  fireEvent.click(screen.getByRole('button', { name: 'Discard changes' }));
  expect(name).toHaveValue('Ada');
  fireEvent.keyDown(window, { key: 'Escape' });
  expect(close).toHaveBeenCalledOnce();
  expect(client.applySettings).not.toHaveBeenCalled();
});
it('preserves text through polling, warns about conflicts, and rebases only on review', async () => {
  vi.useFakeTimers();
  const client = mockClient();
  render(<SettingsEditor client={client} target={target} />);
  await act(async () => {});
  const name = screen.getByRole('textbox', { name: 'Agent name' });
  fireEvent.change(name, { target: { value: 'My draft' } });
  vi.mocked(client.settings).mockResolvedValue({
    ...initial,
    revision: 'external-2',
    groups: [
      {
        ...initial.groups[0],
        toggles: initial.groups[0].toggles.map((setting) =>
          setting.id === 'agent_name' ? { ...setting, value: 'External' } : setting,
        ),
      },
    ],
  });
  await act(async () => {
    await vi.advanceTimersByTimeAsync(3000);
  });
  expect(name).toHaveValue('My draft');
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  fireEvent.click(screen.getByRole('button', { name: 'Review latest and keep draft' }));
  expect(name).toHaveValue('My draft');
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await act(async () => {});
  expect(client.applySettings).toHaveBeenCalledWith(
    'external-2',
    [{ id: 'agent_name', action: 'value', value: 'My draft' }],
    target,
    undefined,
  );
});
it('shows field errors without losing the draft and retries explicit values', async () => {
  const client = mockClient();
  vi.mocked(client.applySettings).mockRejectedValueOnce(
    new GatewayError(400, 'Invalid agent name', {
      error: 'Invalid agent name',
      field_errors: { agent_name: 'Choose another name.' },
    }),
  );
  render(<SettingsEditor client={client} target={target} />);
  const name = await screen.findByRole('textbox', { name: 'Agent name' });
  fireEvent.change(name, { target: { value: 'Grace' } });
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await screen.findByText('Choose another name.');
  expect(name).toHaveValue('Grace');
  fireEvent.click(screen.getByRole('button', { name: 'Apply changes' }));
  await waitFor(() => expect(client.applySettings).toHaveBeenCalledTimes(2));
});
it('shows more-specific overrides as warnings without locking an ancestor', async () => {
  const client = mockClient({
    ...initial,
    groups: [
      {
        ...initial.groups[0],
        toggles: [
          { ...initial.groups[0].toggles[0], overridden_by: { scope: 'session', enabled: true } },
        ],
      },
    ],
  });
  render(
    <SettingsEditor client={client} target={{ scope: 'global' }} contextSessionId="draft-test" />,
  );
  const plans = await screen.findByRole('switch', { name: 'Plans: off' });
  expect(plans).toBeEnabled();
  expect(screen.getByText(/This session uses its session override/)).toBeInTheDocument();
  fireEvent.click(plans);
  expect(client.setSetting).not.toHaveBeenCalled();
});
it('renders access booleans and choices without JSON and requires permission review', async () => {
  const data: SettingsResponse = {
    revision: 'access-1',
    groups: [
      {
        id: 'access',
        title: 'Files and permissions',
        toggles: [
          {
            id: 'jail_enabled',
            label: 'Process jail',
            type: 'boolean',
            enabled: false,
            schema: '#/jail/enabled',
          },
          {
            id: 'jail_environment',
            label: 'Process environment',
            type: 'enum',
            value: 'declared',
            choices: ['declared', 'inherit'],
            schema: '#/jail/environment',
          },
        ],
      },
    ],
  };
  const client = mockClient(data);
  render(<SettingsEditor client={client} target={target} category="access" />);
  fireEvent.click(await screen.findByRole('switch', { name: 'Process jail: off' }));
  expect(screen.getByRole('combobox', { name: 'Process environment' })).toBeInTheDocument();
  expect(
    screen.queryByRole('textbox', { name: /(Process jail|Process environment) JSON/ }),
  ).not.toBeInTheDocument();
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  fireEvent.click(screen.getByRole('checkbox', { name: /I reviewed the permission changes/ }));
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeEnabled();
});
it('keeps invalid advanced JSON local and blocks Apply until it is repaired', async () => {
  const client = mockClient({
    revision: 'access-1',
    groups: [
      {
        id: 'access',
        title: 'Files and permissions',
        toggles: [
          {
            id: 'jail_filesystem',
            label: 'Filesystem access',
            type: 'object',
            editor: 'filesystem',
            value: { allow: ['/tmp'] },
            schema: '#/jail/filesystem',
          },
        ],
      },
    ],
  });
  render(<SettingsEditor client={client} target={target} category="access" />);
  const json = await screen.findByRole('textbox', { name: 'Filesystem access JSON', hidden: true });
  fireEvent.change(json, { target: { value: '{' } });
  expect(screen.getByRole('button', { name: 'Apply changes' })).toBeDisabled();
  expect(client.applySettings).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Discard changes' }));
  expect(json).toHaveValue(JSON.stringify({ allow: ['/tmp'] }, null, 2));
});
