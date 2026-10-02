import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { SettingField } from './SettingField';

const meta = {
  title: 'Settings/Draft backend',
  component: SettingField,
  parameters: { layout: 'padded' },
  args: {
    setting: {
      id: 'draft_backend',
      label: 'Draft backend',
      type: 'enum',
      choices: ['auto', 'worktree', 'rift', 'off'],
    },
    value: 'off',
    disabled: false,
    onChange: () => {},
    onRawChange: () => {},
  },
  render: function BackendSetting(args) {
    const [value, setValue] = useState(args.value);
    return <SettingField {...args} value={value} onChange={setValue} />;
  },
} satisfies Meta<typeof SettingField>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Default: Story = {};
export const Saving: Story = { args: { disabled: true } };
export const ChooseBackend: Story = {
  play: async ({ canvasElement }) => {
    const select = within(canvasElement).getByRole('combobox', { name: 'Draft backend' });
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.tab();
    await expect(select).toHaveFocus();
    await userEvent.keyboard('{ArrowDown}');
    await expect(page.getByRole('option', { name: 'off' })).toHaveFocus();
    await userEvent.keyboard('r{Enter}');
    await expect(select).toHaveTextContent('rift');
    await expect(select).toHaveFocus();
    await userEvent.click(select);
    await userEvent.click(page.getByRole('option', { name: 'off' }));
    await expect(select).toHaveTextContent('off');
  },
};
