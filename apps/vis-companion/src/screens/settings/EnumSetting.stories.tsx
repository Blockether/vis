import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { EnumSetting } from './MachineSettings';

const meta = {
  title: 'Settings/Draft backend',
  component: EnumSetting,
  parameters: { layout: 'padded' },
  args: {
    toggle: {
      id: 'draft_backend',
      label: 'Draft backend',
      type: 'enum',
      choices: ['auto', 'worktree', 'rift', 'off'],
      value: 'auto',
    },
    onPick: () => {},
  },
  render: function BackendSetting(args) {
    const [value, setValue] = useState(args.toggle.value);
    return <EnumSetting {...args} toggle={{ ...args.toggle, value }} onPick={setValue} />;
  },
} satisfies Meta<typeof EnumSetting>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {};
export const Saving: Story = { args: { busy: true } };
export const ChooseBackend: Story = {
  play: async ({ canvasElement }) => {
    const select = within(canvasElement).getByRole('combobox', { name: 'Draft backend' });
    // The application reset removes native arrows unless this control opts back in.
    await expect(getComputedStyle(select).appearance).toBe('auto');
    await userEvent.tab();
    await expect(select).toHaveFocus();
    await userEvent.selectOptions(select, 'rift');
    await expect(select).toHaveValue('rift');
    await userEvent.selectOptions(select, 'off');
    await expect(select).toHaveValue('off');
  },
};
