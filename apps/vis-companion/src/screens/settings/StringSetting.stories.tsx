import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { SettingField } from './SettingField';

const meta = {
  title: 'Settings/Agent name',
  component: SettingField,
  parameters: { layout: 'padded' },
  args: {
    setting: { id: 'agent_name', label: 'Agent name', type: 'string', max_length: 80 },
    value: 'Ada',
    disabled: false,
    onChange: () => {},
    onRawChange: () => {},
  },
  render: function AgentName(args) {
    const [value, setValue] = useState(args.value);
    return <SettingField {...args} value={value} onChange={setValue} />;
  },
} satisfies Meta<typeof SettingField>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Default: Story = {};
export const Saving: Story = {
  args: { disabled: true },
  play: async ({ canvasElement }) => {
    await expect(within(canvasElement).getByRole('textbox', { name: 'Agent name' })).toBeDisabled();
  },
};
export const Rename: Story = {
  play: async ({ canvasElement }) => {
    const field = within(canvasElement).getByRole('textbox', { name: 'Agent name' });
    await userEvent.clear(field);
    await userEvent.type(field, 'Grace');
    await expect(field).toHaveValue('Grace');
    await expect(
      within(canvasElement).queryByRole('button', { name: 'Save' }),
    ).not.toBeInTheDocument();
  },
};
