// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent, { PointerEventsCheckLevel } from '@testing-library/user-event';
import { afterEach, expect, it, vi } from 'vitest';
import type { GatewayClient } from '../../lib/gateway';
import type { SettingValue, Toggle } from '../../lib/types';
import { SettingField, SettingMachine } from './SettingField';

afterEach(cleanup);

function renderField(setting: Toggle, value: SettingValue) {
  const onChange = vi.fn();
  const onRawChange = vi.fn();
  render(
    <SettingField
      setting={setting}
      value={value}
      disabled={false}
      onChange={onChange}
      onRawChange={onRawChange}
    />,
  );
  return { onChange, onRawChange };
}

it('removes a cleared optional workspace name instead of saving an empty string', () => {
  const { onChange } = renderField(
    { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
    [
      {
        id: 'docs',
        path: '~/docs',
        access: 'read-only',
        python_name: 'docs_path',
        description: 'Project notes',
      },
    ],
  );
  fireEvent.change(screen.getByRole('textbox', { name: 'Python name 1' }), {
    target: { value: '' },
  });
  expect(onChange).toHaveBeenLastCalledWith([
    { id: 'docs', path: '~/docs', access: 'read-only', description: 'Project notes' },
  ]);
  fireEvent.change(screen.getByRole('textbox', { name: 'Root description 1' }), {
    target: { value: '' },
  });
  expect(onChange).toHaveBeenLastCalledWith([
    { id: 'docs', path: '~/docs', access: 'read-only', python_name: 'docs_path' },
  ]);
});

it('edits the allowed requests of one host rule and drops a cleared path', () => {
  const docs = {
    host: 'gateway.example.com',
    access: 'read-only',
    allow: [{ method: 'GET', path: '/v1/*' }],
  };
  const api = { host: '10.0.0.5', access: 'read-write', allow: [{ method: 'POST' }] };
  const { onChange } = renderField(
    { id: 'network', label: 'Network', type: 'object', editor: 'network' },
    { rules: [docs, api] },
  );
  fireEvent.change(screen.getByRole('textbox', { name: 'Rule 1 request 1 path' }), {
    target: { value: '' },
  });
  expect(onChange).toHaveBeenLastCalledWith({
    rules: [{ ...docs, allow: [{ method: 'GET' }] }, api],
  });
  fireEvent.click(screen.getByRole('button', { name: 'Add allowed request to rule 2' }));
  expect(onChange).toHaveBeenLastCalledWith({
    rules: [docs, { ...api, allow: [{ method: 'POST' }, { method: 'GET' }] }],
  });
  fireEvent.click(screen.getByRole('button', { name: 'Remove rule 1 request 1' }));
  expect(onChange).toHaveBeenLastCalledWith({ rules: [{ ...docs, allow: [] }, api] });
});

it('removes many host rules with named icon actions, not numbered text buttons', () => {
  const docs = { host: 'gateway.example.com', access: 'read-only', methods: ['GET'] };
  const api = { host: '10.0.0.5', access: 'read-write' };
  const { onChange } = renderField(
    { id: 'network', label: 'Network', type: 'object', editor: 'network' },
    { rules: [docs, api] },
  );
  expect(screen.queryByText(/Remove host rule/)).toBeNull();
  expect(screen.queryByRole('button', { name: /^Add$/ })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Add host rule' }));
  expect(onChange).toHaveBeenLastCalledWith({ rules: [docs, api, { host: '', access: 'read-only' }] });
  expect(screen.getAllByText('Methods')).toHaveLength(2);
  fireEvent.click(screen.getByRole('button', { name: 'Remove host rule 2' }));
  expect(onChange).toHaveBeenLastCalledWith({ rules: [docs] });
});

it('removes a workspace root with a named icon action', () => {
  const docs = { id: 'docs', path: '~/docs', access: 'read-only' };
  const code = { id: 'code', path: '~/code', access: 'read-write' };
  const { onChange } = renderField(
    { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
    [docs, code],
  );
  expect(screen.queryByText(/Remove root/)).toBeNull();
  expect(screen.getAllByText('Skip when missing')).toHaveLength(2);
  expect(screen.queryByText('Optional')).toBeNull();
  expect(screen.getAllByRole('combobox', { name: 'Root draft mode 1' })[0]).toHaveTextContent(
    'Edit the real folder',
  );
  fireEvent.click(screen.getByRole('button', { name: 'Remove root 1' }));
  expect(onChange).toHaveBeenLastCalledWith([code]);
});

it('picks request methods from a list instead of typed text', async () => {
  const docs = { host: 'gateway.example.com', access: 'read-only', allow: [{ method: 'GET' }] };
  const { onChange } = renderField(
    { id: 'network', label: 'Network', type: 'object', editor: 'network' },
    { rules: [docs] },
  );
  expect(screen.queryByRole('textbox', { name: 'Rule 1 request 1 method' })).toBeNull();
  await userEvent.click(screen.getByRole('combobox', { name: 'Rule 1 request 1 method' }));
  await userEvent.click(screen.getByRole('option', { name: 'POST' }));
  expect(onChange).toHaveBeenLastCalledWith({ rules: [{ ...docs, allow: [{ method: 'POST' }] }] });
  await userEvent.click(screen.getByRole('combobox', { name: 'Rule 1 methods' }));
  await userEvent.click(screen.getByRole('option', { name: 'PUT' }));
  expect(onChange).toHaveBeenLastCalledWith({ rules: [{ ...docs, methods: ['PUT'] }] });
});

it('fills the root name from the folder until someone types another name', () => {
  const docs = { id: 'docs', path: '~/docs', access: 'read-only' };
  const named = { id: 'notes', path: '~/docs', access: 'read-only' };
  const { onChange } = renderField(
    { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
    [docs, named],
  );
  fireEvent.change(screen.getByRole('textbox', { name: 'Root path 1' }), { target: { value: '~/code/vis' } });
  expect(onChange).toHaveBeenLastCalledWith([{ ...docs, id: 'vis', path: '~/code/vis' }, named]);
  fireEvent.change(screen.getByRole('textbox', { name: 'Root path 2' }), { target: { value: '~/code/vis' } });
  expect(onChange).toHaveBeenLastCalledWith([docs, { ...named, path: '~/code/vis' }]);
  expect(screen.getByRole('textbox', { name: 'Python name 1' })).toHaveAttribute('placeholder', 'docs_path');
});

it('adds a workspace root from a folder chosen on the machine', async () => {
  const browse = vi.fn(async (asked?: string) => {
    const path = !asked || asked === '~' ? '/Users/me' : asked;
    return {
      path,
      parent: path === '/Users/me' ? '/Users' : '/Users/me',
      home: '/Users/me',
      is_truncated: false,
      entries:
        path === '/Users/me' ? [{ name: 'notes', path: '/Users/me/notes', entry_count: 3, is_repo: false }] : [],
    };
  });
  const onChange = vi.fn();
  render(
    <SettingMachine.Provider value={{ client: { browse } as unknown as GatewayClient, label: 'tower' }}>
      <SettingField
        setting={{ id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' }}
        value={[]}
        disabled={false}
        onChange={onChange}
        onRawChange={vi.fn()}
      />
    </SettingMachine.Provider>,
  );
  await userEvent.click(screen.getByRole('button', { name: 'Add workspace root' }));
  expect(await screen.findByRole('dialog', { name: 'Choose folder on tower' })).toBeInTheDocument();
  await userEvent.click(await screen.findByText('notes/'));
  await userEvent.click(await screen.findByRole('button', { name: /^Use folder/ }));
  await waitFor(() =>
    expect(onChange).toHaveBeenLastCalledWith([
      { id: 'notes', path: '/Users/me/notes', access: 'read-write' },
    ]),
  );
});
it('limits numbers by the setting schema and keeps invalid text local', () => {
  const { onChange, onRawChange } = renderField(
    {
      id: 'interval_minutes',
      label: 'Review interval',
      type: 'number',
      editor: 'number',
      schema: 'config.json#/$defs/config/properties/improve/properties/interval_minutes',
    },
    60,
  );
  const input = screen.getByRole('spinbutton', { name: 'Review interval' });
  expect(input.getAttribute('min')).toBe('1');
  expect(input.getAttribute('max')).toBe('1440');
  expect(input.getAttribute('step')).toBe('1');
  for (const [text, error] of [
    ['0', 'Use at least 1.'],
    ['1441', 'Use at most 1440.'],
    ['1.5', 'Enter a whole number.'],
    ['', 'Enter a number.'],
  ]) {
    fireEvent.change(input, { target: { value: text } });
    expect(onRawChange).toHaveBeenLastCalledWith(text, error);
  }
  expect(onChange).not.toHaveBeenCalled();
  fireEvent.change(input, { target: { value: '90' } });
  expect(onRawChange).toHaveBeenLastCalledWith('90', undefined);
  expect(onChange).toHaveBeenCalledExactlyOnceWith(90, true);
});

it.each([
  ['paths', 'array', []],
  ['filesystem', 'object', {}],
  ['network', 'object', {}],
  ['list', 'array', []],
] satisfies [Toggle['editor'], Toggle['type'], SettingValue][])(
  'offers only guided controls for %s settings',
  (editor, type, value) => {
    renderField({ id: 'guided', label: 'Guided setting', editor, type }, value);
    expect(screen.queryByText('Advanced JSON')).toBeNull();
    expect(screen.queryByRole('textbox', { name: 'Guided setting JSON' })).toBeNull();
    expect(screen.queryAllByRole('group').length).toBeGreaterThan(0);
  },
);

it('edits filesystem paths without changing the other access lists', () => {
  const { onChange, onRawChange } = renderField(
    { id: 'jail_filesystem', label: 'Filesystem access', type: 'object', editor: 'filesystem' },
    { allow: ['~/docs'], deny_read: ['~/private'], deny_write: ['~/readonly'] },
  );
  fireEvent.change(screen.getByRole('textbox', { name: 'Allowed paths 1' }), {
    target: { value: '~/project' },
  });
  expect(onChange).toHaveBeenLastCalledWith({
    allow: ['~/project'],
    deny_read: ['~/private'],
    deny_write: ['~/readonly'],
  });
  expect(onRawChange).not.toHaveBeenCalled();
});

it('edits list entries with ordinary controls', () => {
  const { onChange, onRawChange } = renderField(
    { id: 'jail_deny_exec', label: 'Denied executables', type: 'array', editor: 'list' },
    ['curl'],
  );
  fireEvent.change(screen.getByRole('textbox', { name: 'Denied executables 1' }), {
    target: { value: 'wget' },
  });
  expect(onChange).toHaveBeenLastCalledWith(['wget']);
  fireEvent.click(screen.getByRole('button', { name: 'Add denied executables' }));
  expect(onChange).toHaveBeenLastCalledWith(['curl', '']);
  fireEvent.click(screen.getByRole('button', { name: 'Remove Denied executables 1' }));
  expect(onChange).toHaveBeenLastCalledWith([]);
  expect(onRawChange).not.toHaveBeenCalled();
});

it('directs unsupported settings to the configuration file instead of a JSON editor', () => {
  const { onChange, onRawChange } = renderField(
    { id: 'custom', label: 'Custom configuration', type: 'object' },
    {},
  );
  expect(screen.getByText('Edit this setting in your configuration file.')).toBeInTheDocument();
  expect(screen.queryByRole('textbox')).toBeNull();
  expect(onChange).not.toHaveBeenCalled();
  expect(onRawChange).not.toHaveBeenCalled();
});

it.each(['Root access 1', 'Root draft mode 1', 'Rule access 1'])(
  'keeps %s closed after a second touch without changing the setting',
  async (label) => {
    const user = userEvent.setup({ pointerEventsCheck: PointerEventsCheckLevel.Never });
    const { onChange, onRawChange } = label.startsWith('Root')
      ? renderField(
          { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
          [{ id: 'docs', path: '~/docs', access: 'read-only', draft: 'shared' }],
        )
      : renderField(
          { id: 'network', label: 'Network', type: 'object', editor: 'network' },
          { rules: [{ host: 'gateway.example.com', access: 'read-only' }] },
        );
    const trigger = screen.getByRole('combobox', { name: label });
    const value = trigger.textContent;
    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.getByRole('listbox', { name: label })).toBeVisible();

    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    expect(trigger).toHaveAttribute('aria-expanded', 'false');
    expect(trigger.textContent).toBe(value);
    expect(onChange).not.toHaveBeenCalled();
    expect(onRawChange).not.toHaveBeenCalled();
    await waitFor(() => expect(trigger).toHaveFocus());

    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.getByRole('listbox', { name: label })).toBeVisible();
  },
);

const rootAccessLabel: Record<string, string> = { 'read-only': 'Read only', 'read-write': 'Read and write' };
const ruleAccessLabel: Record<string, string> = {
  'read-only': 'Read only',
  'read-write': 'All methods',
  none: 'Blocked',
};

const accessAliases: [string, string][] = [
  ['read-only', 'read-only'],
  ['readonly', 'read-only'],
  ['ro', 'read-only'],
  ['read-write', 'read-write'],
  ['readwrite', 'read-write'],
  ['rw', 'read-write'],
];

it.each(accessAliases)('shows unique workspace access choices for %s', async (access, canonical) => {
  const entry = { id: 'docs', path: '~/docs', access };
  const { onChange } = renderField(
    { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
    [entry],
  );
  const select = screen.getByRole('combobox', { name: 'Root access 1' });
  expect(select).toHaveTextContent(rootAccessLabel[canonical]);
  expect(onChange).not.toHaveBeenCalled();
  await userEvent.click(select);
  expect(screen.getAllByRole('option').map((option) => option.textContent)).toEqual([
    'Read only',
    'Read and write',
  ]);
  const next = canonical === 'read-only' ? 'read-write' : 'read-only';
  await userEvent.click(screen.getByRole('option', { name: rootAccessLabel[next] }));
  expect(onChange).toHaveBeenLastCalledWith([{ ...entry, access: next }]);
});

it.each([
  ...accessAliases,
  ['full', 'read-write'],
  ['all', 'read-write'],
  ['none', 'none'],
  ['deny', 'none'],
  ['closed', 'none'],
])('shows unique network access choices for %s', async (access, canonical) => {
  const entry = { host: 'gateway.example.com', access };
  const { onChange } = renderField(
    { id: 'network', label: 'Network', type: 'object', editor: 'network' },
    { allowed_domains: ['gateway.example.com'], rules: [entry] },
  );
  const select = screen.getByRole('combobox', { name: 'Rule access 1' });
  expect(select).toHaveTextContent(ruleAccessLabel[canonical]);
  expect(onChange).not.toHaveBeenCalled();
  await userEvent.click(select);
  expect(screen.getAllByRole('option').map((option) => option.textContent)).toEqual([
    'Read only',
    'All methods',
    'Blocked',
  ]);
  const next = canonical === 'read-only' ? 'read-write' : 'read-only';
  await userEvent.click(screen.getByRole('option', { name: ruleAccessLabel[next] }));
  expect(onChange).toHaveBeenLastCalledWith({
    allowed_domains: ['gateway.example.com'],
    rules: [{ ...entry, access: next }],
  });
});
