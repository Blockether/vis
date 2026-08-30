import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { STORY_GATEWAYS, STORY_GATEWAY_HEALTH } from '../dev/story-data';
import { MachineRows } from './Machines';

/** The paired fleet, including one machine whose latest probe failed. */
const meta = {
  title: 'Components/Machine rows',
  component: MachineRows,
  parameters: { layout: 'padded' },
  args: {
    conns: STORY_GATEWAYS,
    primaryUrl: STORY_GATEWAYS[0].url,
    health: STORY_GATEWAY_HEALTH,
    onPick: () => {},
    actionLabel: 'Open',
  },
} satisfies Meta<typeof MachineRows>;

export default meta;
type Story = StoryObj<typeof meta>;

const pick = fn();

/** Rank, latency, long name and failure all compete in the same one-line rail. */
export const Fleet: Story = {
  args: { onPick: pick },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: /tower/i }));
    await expect(args.onPick).toHaveBeenCalledWith(STORY_GATEWAYS[0]);
  },
};

/** One machine pays no width for fleet rank. */
export const OneMachine: Story = {
  args: {
    conns: STORY_GATEWAYS.slice(0, 1),
    health: { [STORY_GATEWAYS[0].url]: STORY_GATEWAY_HEALTH[STORY_GATEWAYS[0].url] },
  },
};
