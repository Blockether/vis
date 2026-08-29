import type { Meta, StoryObj } from '@storybook/react-vite';
import { ACTIVITY_FAILED, ACTIVITY_RUNNING, ACTIVITY_SETTLED } from '../dev/story-data';
import { ActivityPanel } from './ActivityPanel';

/**
 * WHAT THE MODEL IS DOING, WHILE IT IS DOING IT.
 *
 * The panel has one job the transcript cannot do: report a bounded run — how many
 * calls, which one is moving, what it produced — without the reader opening
 * anything. The three states below are the three sentences it can say, and they
 * are the reason it is drawn here rather than described: `running` has a rail that
 * pulses, `succeeded` has to go quiet without disappearing, and `failed` has to be
 * findable in a settled transcript scrolled past.
 *
 * The payloads are the ENGINE's own (`activityProjectionFromWire`), so a wire
 * change breaks this sheet before it reaches a screen.
 */
const meta = {
  title: 'Components/Activity panel',
  component: ActivityPanel,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof ActivityPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A turn in flight: one call answered, one still running. */
export const Running: Story = {
  args: { activity: ACTIVITY_RUNNING, isSettled: false },
};

/** The same panel opened, which is where the evidence lives. */
export const RunningExpanded: Story = {
  args: { activity: ACTIVITY_RUNNING, isSettled: false, initiallyExpanded: true },
};

/** Settled and read: three calls, a diff among them, nothing moving. */
export const Settled: Story = {
  args: { activity: ACTIVITY_SETTLED, isSettled: true, initiallyExpanded: true },
};

/** A failure has to survive being scrolled past, so it keeps its rail. */
export const Failed: Story = {
  args: { activity: ACTIVITY_FAILED, isSettled: true, initiallyExpanded: true },
};

/** No projection at all — the panel a turn gets before its first tool call. */
export const Idle: Story = {
  args: { isSettled: false },
};
