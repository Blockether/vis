// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
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
