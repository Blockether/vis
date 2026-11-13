// @vitest-environment jsdom
import { fireEvent, render, screen, within } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import type { Toggle } from '../../lib/types';
import { ValueSetting } from './MachineSettings';

// Regression: the row and the list editor both showed the label and the description.
it.each(['list', 'tools', 'paths'] as const)('shows the label and description of a %s setting once', (editor) => {
  const toggle: Toggle = {
    id: 'tools_deny',
    label: 'Turned-off tools',
    description: 'These tools are off.',
    type: 'array',
    editor,
    value: [],
  };
  render(<ValueSetting toggle={toggle} busy={false} onSave={vi.fn()} />);
  expect(screen.getAllByText('Turned-off tools')).toHaveLength(1);
  expect(screen.getAllByText('These tools are off.')).toHaveLength(1);
});

// Regression: the name of an access editor looked like one more list legend under it.
// User request: its lists stay folded until the person opens the editor.
it.each([
  ['filesystem', 'Filesystem access', 'Allowed paths'],
  ['network', 'Network access', 'Allowed domains'],
] as const)('names the %s editor as the folded group of its lists', (editor, label, list) => {
  const toggle: Toggle = { id: `jail_${editor}`, label, type: 'object', editor, value: {} };
  render(<ValueSetting toggle={toggle} busy={false} onSave={vi.fn()} />);
  const group = screen.getByRole('group', { name: label });
  expect(within(group).queryByRole('group', { name: new RegExp(`^${list}`) })).toBeNull();
  fireEvent.click(within(group).getByRole('button', { name: label, expanded: false }));
  expect(within(group).getByRole('group', { name: new RegExp(`^${list}`) })).toBeTruthy();
  expect(screen.getAllByText(label)).toHaveLength(1);
});
