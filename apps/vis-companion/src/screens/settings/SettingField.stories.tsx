import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, within } from 'storybook/test';
import type { SettingValue, Toggle } from '../../lib/types';
import { SettingField } from './SettingField';

/**
 * Permission editors with many entries. Each root or host rule is one card with a
 * small `n / total` caption, and every remove action is a named trash icon.
 */
function Field({ setting, initial }: { setting: Toggle; initial: SettingValue }) {
  const [value, setValue] = useState(initial);
  return (
    <SettingField
      setting={setting}
      value={value}
      disabled={false}
      onChange={setValue}
      onRawChange={() => {}}
    />
  );
}

const meta = {
  title: 'Screens/Setting field',
  component: Field,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof Field>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Several workspace roots stack as numbered cards without numbered button text. */
export const WorkspaceRoots: Story = {
  args: {
    setting: { id: 'workspaces', label: 'Workspace roots', type: 'array', editor: 'paths' },
    initial: [
      { id: 'vis', path: '~/vis', access: 'read-write' },
      { id: 'docs', path: '~/docs', access: 'read-only', description: 'Project notes' },
      { id: 'scratch', path: '/tmp/scratch', access: 'read-write', optional: true },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole('button', { name: 'Remove root 3' })).toBeVisible();
    await expect(canvas.queryByText(/Remove root/)).toBeNull();
  },
};

/** Host rules keep their methods, ports and requests inside one card each. */
export const NetworkRules: Story = {
  args: {
    setting: { id: 'network', label: 'Network', type: 'object', editor: 'network' },
    initial: {
      allowed_domains: ['gateway.example.com'],
      rules: [
        {
          host: 'gateway.example.com',
          access: 'read-only',
          methods: ['GET', 'HEAD'],
          allow: [{ method: 'GET', path: '/v1/*' }],
        },
        { host: '10.0.0.5', access: 'read-write', ports: [8080] },
      ],
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole('button', { name: 'Remove host rule 2' })).toBeVisible();
    await expect(canvas.queryByText(/Remove host rule|Rule 1 methods/)).toBeNull();
  },
};
