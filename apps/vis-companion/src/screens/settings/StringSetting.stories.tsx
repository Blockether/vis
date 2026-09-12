import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { StringSetting } from './MachineSettings';

const meta = {
  title: 'Settings/Agent name',
  component: StringSetting,
  parameters: { layout: 'padded' },
  args: {
    toggle: {
      id: 'agent_name',
      label: 'Agent name',
      type: 'string',
      value: 'Ada',
      max_length: 80,
      description: 'Shared by all clients of this gateway. Overrides project names.',
    },
    busy: false,
    onSave: async () => true,
  },
  render: function AgentName(args) {
    const [value, setValue] = useState(args.toggle.value);
    return (
      <StringSetting
        key={value}
        {...args}
        toggle={{ ...args.toggle, value }}
        onSave={async (next) => {
          setValue(next.trim());
          return true;
        }}
      />
    );
  },
} satisfies Meta<typeof StringSetting>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Default: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const field = canvas.getByRole('textbox', { name: 'Agent name' });
    await expect(canvas.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
    await expect(canvas.queryByRole('button', { name: 'Cancel' })).not.toBeInTheDocument();
    await userEvent.click(field);
    await expect(canvas.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const box = field.getBoundingClientRect();
    // Visible fields match across forms without reducing their effective touch reach.
    const reach =
      box.height +
      (pointer
        ? 0
        : parseFloat(getComputedStyle(field.parentElement!, '::before').height) +
          parseFloat(getComputedStyle(field.parentElement!, '::after').height));
    await expect(reach).toBeGreaterThanOrEqual(pointer ? 28 : 44);
    await expect(field).toHaveValue('Ada');
  },
};
export const Saving: Story = {
  args: { busy: true },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole('button', { name: 'Saving…' })).toBeDisabled();
    await expect(canvas.getByRole('textbox', { name: 'Agent name' })).toBeDisabled();
  },
};
export const Rename: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const field = canvas.getByRole('textbox', { name: 'Agent name' });
    await userEvent.clear(field);
    await userEvent.type(field, 'Grace');
    const save = canvas.getByRole('button', { name: 'Save' });
    await expect(save).toBeEnabled();
    await expect(field.getBoundingClientRect().height).toBe(save.getBoundingClientRect().height);
    await userEvent.click(canvas.getByRole('button', { name: 'Cancel' }));
    await expect(field).toHaveValue('Ada');
    await expect(canvas.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
    await userEvent.clear(field);
    await userEvent.type(field, 'Grace{Enter}');
    await expect(canvas.getByRole('textbox', { name: 'Agent name' })).toHaveValue('Grace');
    await expect(canvas.queryByRole('button', { name: 'Save' })).not.toBeInTheDocument();
  },
};
