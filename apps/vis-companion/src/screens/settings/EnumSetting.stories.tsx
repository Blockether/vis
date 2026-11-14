import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { EnumSetting } from './MachineSettings';

const meta = {
  title: 'Settings/Fields/Choice',
  component: EnumSetting,
  parameters: { layout: 'padded' },
  args: {
    toggle: {
      id: 'sample_mode',
      label: 'Sample mode',
      type: 'enum',
      choices: ['auto', 'fast', 'safe', 'off'],
      value: 'off',
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

export const Saving: Story = { args: { busy: true } };
export const Choosing: Story = {
  play: async ({ canvasElement }) => {
    const select = within(canvasElement).getByRole('combobox', { name: 'Sample mode' });
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.tab();
    await expect(select).toHaveFocus();
    await userEvent.keyboard('{ArrowDown}');
    await expect(page.getByRole('option', { name: 'off' })).toHaveFocus();
    await userEvent.keyboard('s{Enter}');
    await expect(select).toHaveTextContent('safe');
    await expect(select).toHaveFocus();
    await userEvent.click(select);
    await userEvent.click(page.getByRole('option', { name: 'off' }));
    await expect(select).toHaveTextContent('off');
  },
};
