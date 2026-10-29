import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import {
  ACTIVITY_ALL_GROUPS,
  ACTIVITY_CHRONOLOGY,
  ACTIVITY_FAILED,
  ACTIVITY_SETTLED,
} from '../dev/story-data';
import { ActivityHistoryContext, ActivityPanel } from './ActivityPanel';
import { activityHistoryPage } from '../dev/activity-history';

/**
 * WHAT THE MODEL IS DOING, WHILE IT IS DOING IT.
 *
 * The axis has one job the transcript cannot do: report a bounded run — how many
 * calls, which one is moving, what it produced — in one reading, with nothing
 * inside it behind a second chevron. It is what the invocation's own band opens
 * onto, and the reason a reader opens that band at all. The states below are the
 * sentences it can say, and they are drawn here rather than described, because
 * `running` has to read as one moving thread, `succeeded` has to go quiet
 * without disappearing, and `failed` has to be findable in a settled transcript
 * scrolled past — with the head of its output already on the page.
 *
 * The payloads are the ENGINE's own (`activityProjectionFromWire`), so a wire
 * change breaks this sheet before it reaches a screen.
 */
const meta = {
  title: 'Session/Activity',
  component: ActivityPanel,
  parameters: { layout: 'fullscreen' },
  // Match the transcript gutter and the execution band's inset around this panel.
  decorators: [
    (Story) => (
      <div className="mx-auto w-full max-w-3xl px-3.5 pt-4 sm:px-6 sm:pt-6">
        <div className="px-3">
          <Story />
        </div>
      </div>
    ),
  ],
} satisfies Meta<typeof ActivityPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Settled and read: three calls, a diff among them, nothing moving. */
export const Settled: Story = {
  args: { activity: ACTIVITY_SETTLED },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const toggle = canvas.getByRole('button', { name: 'Expand Activity' });
    const summary = within(toggle).getByText((_, node) =>
      /^\d+ mutations?$/.test(node?.textContent ?? ''),
    ).parentElement!;
    for (const expanded of [false, true]) {
      await expect(toggle).toHaveAttribute('aria-expanded', String(expanded));
      // The count remains part of the same full-row pointer and keyboard target.
      if (!expanded) await userEvent.click(summary);
    }
    toggle.focus();
    await userEvent.keyboard('{Enter}');
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
  },
};

/** Failure counts remain visible; details wait for explicit expansion. */
export const Failed: Story = {
  args: { activity: ACTIVITY_FAILED },
  play: Settled.play,
};

/** The thread doing its job: reads, a patch, a failed check, one step still moving. */
export const Running: Story = {
  args: { activity: ACTIVITY_CHRONOLOGY },
};

/** All groups are available immediately; individual step details still fold independently. */
export const AllGroups: Story = {
  args: { activity: ACTIVITY_ALL_GROUPS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const groups = canvas.getByRole('list', { name: 'Operation groups' });
    await expect(groups.children).toHaveLength(7);
    await expect(
      canvas.queryByRole('button', {
        name: /(?:show|hide).*(?:more|fewer).*groups?/i,
      }),
    ).not.toBeInTheDocument();
    const lastStep = within(groups.children[6] as HTMLElement).getByRole('button', {
      expanded: false,
    });
    await expect(lastStep).toBeVisible();
    lastStep.focus();
    await userEvent.keyboard('{Enter}');
    await expect(lastStep).toHaveAttribute('aria-expanded', 'true');
    await expect(canvas.getByText('Build completed')).toBeVisible();
    await userEvent.click(lastStep);
    await expect(lastStep).toHaveAttribute('aria-expanded', 'false');
  },
};

/** #212: the data source stands in for authenticated paged retrieval, not the UI. */
export const RetainedHistory: Story = {
  args: { activity: activityHistoryPage() },
  decorators: [
    (Story) => (
      <ActivityHistoryContext.Provider
        value={{
          load: async (_id, after, query) => activityHistoryPage(after, query),
        }}
      >
        <Story />
      </ActivityHistoryContext.Provider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const user = userEvent.setup();
    const copy = canvas.getByRole('button', { name: 'Copy activity' });
    const toggle = canvas.getByRole('button', { name: 'Expand Activity' });
    await user.click(copy);
    await expect(await canvas.findByRole('button', { name: 'Copied' })).toBeVisible();
    const copied = await navigator.clipboard.readText();
    await expect(copied).toContain('retained-1\n');
    await expect(copied).toContain('retained-160\n');
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await user.click(toggle);
    await expect(canvas.queryByRole('searchbox')).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Copy all activity' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Export all activity' })).toBeNull();
    await expect(await canvas.findByText('Operation 160')).toBeVisible();
    await expect(canvas.getByText('Operation 1')).toBeVisible();
    await expect(canvas.getByText('Operation 159')).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Show more operations' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Show earlier operations' })).toBeNull();
  },
};
