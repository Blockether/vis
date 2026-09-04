import type { Meta, StoryObj } from '@storybook/react-vite';
import { useLayoutEffect, useState, type ReactNode } from 'react';

import { AddMachine } from './Machines';

/**
 * The pairing page, idle. Under a mouse it is three numbered steps that read across
 * where the plane is wide and down one left edge where it is a column — the
 * component's own width decides, so both are stories of the same args inside
 * different frames. In the hand it is a decision between the two ways the code
 * gets here, which is the pointer's word, not the frame's.
 */
const meta = {
  title: 'Components/Add a machine',
  component: AddMachine,
  parameters: { layout: 'padded' },
  args: { onAdd: async () => {} },
} satisfies Meta<typeof AddMachine>;

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * The device in your hand answers `pointer: coarse`. The gallery browser has a mouse,
 * so this frame answers that one query for it before the component asks, and gives
 * the real `matchMedia` back when the story leaves.
 */
function InHand({ children }: { children: ReactNode }) {
  const [held, setHeld] = useState(false);
  useLayoutEffect(() => {
    const real = window.matchMedia;
    window.matchMedia = ((query: string) =>
      query.includes('pointer: fine')
        ? { ...real.call(window, query), matches: false }
        : real.call(window, query)) as typeof window.matchMedia;
    setHeld(true);
    return () => {
      window.matchMedia = real;
    };
  }, []);
  return held ? <>{children}</> : null;
}

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

/** A phone: scan the code, OR paste the link — two ways, no order, one primary. */
export const Phone: Story = {
  decorators: [
    (Story) => (
      <InHand>
        <div className="w-[358px] border border-dialog-edge bg-panel p-4">
          <Story />
        </div>
      </InHand>
    ),
  ],
};
