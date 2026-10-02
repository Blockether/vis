// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import type { SettingValue, Toggle } from '../../lib/types';
import { SettingField } from './SettingField';

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
