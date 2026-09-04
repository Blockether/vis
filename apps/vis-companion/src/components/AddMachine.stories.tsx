import type { Meta, StoryObj } from '@storybook/react-vite';

import { AddMachine } from './Machines';

/**
 * The pairing page, idle: three numbered steps that read across where the plane is
 * wide and down one left edge where it is a column. It is the component's own width
 * that decides, so both are stories of the same args inside different frames.
 */
const meta = {
  title: 'Components/Add a machine',
  component: AddMachine,
  parameters: { layout: 'padded' },
  args: { onAdd: async () => {} },
} satisfies Meta<typeof AddMachine>;

export default meta;
type Story = StoryObj<typeof meta>;

/** The connect page on a desktop: one plane, the steps side by side. */
export const Page: Story = {
  decorators: [
    (Story) => (
      <div className="border border-dialog-edge bg-panel p-5">
        <Story />
      </div>
    ),
  ],
};

/** A phone, or the settings dialog: the same steps stacked on one edge. */
export const Column: Story = {
  decorators: [
    (Story) => (
      <div className="w-[358px] border border-dialog-edge bg-panel p-4">
        <Story />
      </div>
    ),
  ],
};
