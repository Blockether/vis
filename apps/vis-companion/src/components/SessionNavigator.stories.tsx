import type { Meta, StoryObj } from '@storybook/react-vite';

import { MachineSwitcher } from './SessionNavigator';

const meta = {
  title: 'Session/Navigator surfaces',
  component: MachineSwitcher,
  globals: { theme: 'blockether-light' },
  args: {
    children: <span className="font-mono text-meta text-white">Machine</span>,
  },
} satisfies Meta<typeof MachineSwitcher>;

export default meta;
type Story = StoryObj<typeof meta>;

export const FlatMachineSwitcher: Story = {
};
