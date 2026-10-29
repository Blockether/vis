import type { Meta, StoryObj } from '@storybook/react-vite';

import { AddMachine } from './Machines';

/**
 * The pairing page, idle. Under a mouse it is three numbered steps that read across
 * where the plane is wide and down one left edge where it is a column — the
 * component's own width decides, so both are stories of the same args inside
 * different frames. In the hand it is a decision between the two ways the code
 * gets here, which is the pointer's word, not the frame's.
 */
const meta = {
  title: 'Settings/Machines/Add machine',
  component: AddMachine,
  parameters: { layout: 'padded' },
  args: { onAdd: async () => {} },
} satisfies Meta<typeof AddMachine>;

export default meta;
type Story = StoryObj<typeof meta>;

/** The connect page on a desktop: one plane, the steps side by side. */
export const Default: Story = {
  decorators: [
    (Story) => (
      <div className="border border-dialog-edge bg-panel p-5">
        <Story />
      </div>
    ),
  ],
};
